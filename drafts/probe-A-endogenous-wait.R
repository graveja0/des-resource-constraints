#############################################################################
# PROBE: "A-endogenous-wait-event"
#
# Goal: model finite-resource contention (treatment capacity c << demand) in
# the hand-rolled DES event loop (main_loop.R architecture) WITHOUT a blocking
# seize of the contended resource (which would freeze the patient's competing-
# risk clock). Instead, "treatment becomes available" is a REGISTRY EVENT whose
# time_to_event is ENDOGENOUS: it is derived from LIVE occupancy of a capacity-c
# treatment resource at scheduling time.
#
# Mechanism, in one breath:
#  - "A" is a real simmer resource, capacity = c. It is the slot bookkeeper.
#  - When a patient enters S1 they JOIN A WAITLIST (a plain integer tally we
#    maintain ourselves: 'wl_count' + a FIFO of join-times) and are stamped with
#    a queue position. They do NOT block.
#  - The "Get Treatment A" event's time_to_event queries get_server_count(env, "A"),
#    get_capacity(env, "A") and the patient's position in the waitlist, and converts
#    congestion into an expected wait (M/M/c-flavoured: if a slot is free, wait
#    ~ 0; otherwise wait ~ (#ahead_of_me_blocking + 1)/(c * mu), where mu is the
#    treatment service rate). That wait competes head-to-head with death /
#    progression / recovery in next_event(). DISEASE PROGRESSES WHILE WAITING.
#  - When the event finally fires, the patient seizes A (a slot is, in
#    expectation, free), leaves the waitlist, and starts treatment. Treatment
#    lasts an exp(mu) duration after which A is released (a separate timed
#    'finish treatment' event) -> a slot frees for the next waiter.
#  - Recovery to Healthy / death also release A & leave the waitlist (no leaks).
#
# Endogeneity test: shrink c or raise N and check mean realized wait rises.
# Face-validity test: c small vs c = Inf -> more deaths / more S2-time / lower QALY.
#############################################################################

suppressMessages({
  library(simmer)
})

set.seed(1)

# ---------------------------------------------------------------------------
# We replicate main_loop.R's helpers INLINE (so the probe is self-contained and
# uses the SAME next_event / branch+rollback architecture). 'env' is a global,
# exactly as in the real model.
# ---------------------------------------------------------------------------
create_counters <- function(env, counters) {
  sapply(counters, function(counter) env <- add_resource(env, counter, Inf, 0))
  env
}
mark <- function(traj, counter) traj |> seize(counter,1) |> timeout(0) |> release(counter,1)

assign_events <- function(traj, inputs) {
  sapply(event_registry, function(event) {
    traj <<- set_attribute(traj, event$attr, function() event$time_to_event(inputs))
  })
  traj
}

next_event <- function() {
  event_time <- Inf; event <- NA; id <- 0
  for (i in seq_along(event_registry)) {
    e <- event_registry[[i]]
    tmp <- get_attribute(env, e$attr)
    if (tmp < event_time) { event <- e; event_time <- tmp; id <- i }
  }
  list(event=event, event_time=event_time, id=id)
}

process_events <- function(traj, env, inputs) {
  traj <- timeout(traj, function() {
    ne <- next_event(); ne[['event_time']] - now(env)
  })
  args <- lapply(event_registry, function(e) {
    trajectory(e$name) |>
      e$func(inputs) |>
      set_attribute(e$attr, function() now(env) + e$time_to_event(inputs))
  })
  args$".trj"   <- traj
  args$option   <- function() next_event()$id
  args$continue <- rep(TRUE, length(event_registry))
  traj <- do.call(branch, args)
  # reactive reschedule
  lapply(event_registry[sapply(event_registry, function(x) x$reactive)], function(e) {
    traj <<- set_attribute(traj, e$attr, function() now(env) + e$time_to_event(inputs))
  })
  traj
}

des <- function(env, inputs) {
  trajectory("Patient") |>
    initialize_patient(inputs) |>
    assign_events(inputs) |>
    branch(function() 1, continue=TRUE,
           trajectory("main_loop") |> process_events(env, inputs)) |>
    rollback(1, 100)
}

# ---------------------------------------------------------------------------
# THE WAITLIST: a global FIFO we maintain ourselves. This is the
# "separate bookkeeping mechanism" that lets us compute an ENDOGENOUS wait
# without blocking the patient. Keyed by simmer arrival name.
# Reset at the start of every run.
# ---------------------------------------------------------------------------
WL <- new.env()
reset_waitlist <- function() {
  WL$join_order <- character(0)  # FIFO of names currently waiting for A
}
wl_join <- function(nm)  if (!(nm %in% WL$join_order)) WL$join_order <- c(WL$join_order, nm)
wl_leave <- function(nm) WL$join_order <- setdiff(WL$join_order, nm)
wl_position <- function(nm) match(nm, WL$join_order)  # 1 = front of queue, NA if not waiting

# convenience: current arrival's simmer name (env is global)
cur_name <- function() get_name(env)

# ---------------------------------------------------------------------------
# Sick-sicker states: Healthy(0) -> Sick1(1) -> Sick2(2) -> Dead(3)
# Treatment A: seized in S1; while ON A, S1->S2 progression is BLOCKED
# (treatment halts progression). A frees on recovery, death, or end-of-course.
# ---------------------------------------------------------------------------

initialize_patient <- function(traj, inputs) {
  traj |>
    seize("time_in_model") |>
    set_attribute("State", 0) |>
    seize("healthy") |>
    set_attribute("onA", 0) |>          # currently occupying a slot of A
    set_attribute("waiting", 0) |>      # on the waitlist for A
    set_attribute("nm_set", 0)          # have we recorded our name attr yet
}

# When entering S1, JOIN the waitlist (no blocking).
sick1 <- function(traj, inputs) {
  traj |>
    set_attribute("State", 1) |>
    release("healthy") |>
    seize("sick1") |>
    set_attribute("waiting", 1) |>
    # record join in our FIFO (function-of-attributes runs at sim time)
    set_attribute("joined", function() { wl_join(get_name(env)); 1 })
}

# ENDOGENOUS wait. Query live occupancy of A + our queue position.
years_till_treatmentA <- function(inputs) {
  if (get_attribute(env, "State") != 1) return(inputs$horizon + 1)   # only S1 seeks A
  if (get_attribute(env, "onA")   == 1) return(inputs$horizon + 1)   # already on it
  if (get_attribute(env, "waiting")!= 1) return(inputs$horizon + 1)

  cap    <- get_capacity(env, "A")
  busy   <- get_server_count(env, "A")          # slots currently occupied
  free   <- cap - busy
  pos    <- wl_position(get_name(env))     # 1 = front
  if (is.na(pos)) pos <- 1
  mu     <- inputs$mu_A                     # treatment service rate (per year)

  if (free >= pos) {
    # A slot is (in expectation) available for me right now: tiny wait.
    rexp(1, rate = inputs$rate_admit_when_free)
  } else {
    # All slots busy and (pos - free) people block ahead of me.
    # M/M/c-flavoured: departures happen at rate c*mu; I must wait for
    # (pos - free) departures before a slot is mine. Mean wait = (pos-free)/(c*mu).
    n_ahead <- pos - free
    rexp(1, rate = (cap * mu) / n_ahead)
  }
}

# Fire: actually seize a slot of A, start a treatment course.
get_treatmentA <- function(traj, inputs) {
  traj |>
    # Guard: only seize if still S1, waiting, and a slot is genuinely free.
    branch(
      function() {
        ok <- get_attribute(env,"State")==1 &&
              get_attribute(env,"waiting")==1 &&
              get_attribute(env,"onA")==0 &&
              (get_server_count(env, "A") < get_capacity(env, "A"))
        ok + 1L
      },
      continue = rep(TRUE, 2),
      trajectory(),                                   # 1: slot not free / not eligible -> skip, will reschedule
      trajectory() |>                                 # 2: take the slot
        seize("A", 1) |>
        set_attribute("onA", 1) |>
        set_attribute("waiting", 0) |>
        set_attribute("left", function(){ wl_leave(get_name(env)); 1 }) |>
        # schedule end of treatment course = release slot
        set_attribute("tEndA", function() now(env) + rexp(1, inputs$mu_A))
    )
}

# End-of-course: release the slot so the next waiter can be admitted.
years_till_endA <- function(inputs) {
  if (get_attribute(env, "onA") == 1) {
    max(0, get_attribute(env, "tEndA") - now(env))
  } else inputs$horizon + 1
}
end_treatmentA <- function(traj, inputs) {
  traj |> branch(
    function() (get_attribute(env,"onA")==1) + 1L,
    continue = rep(TRUE,2),
    trajectory(),
    trajectory() |> release("A",1) |> set_attribute("onA", 0)
  )
}

healthy <- function(traj, inputs) {
  traj |>
    set_attribute("State", 0) |>
    set_attribute("waiting", 0) |>
    set_attribute("leftwl", function(){ wl_leave(get_name(env)); 1 }) |>
    seize("healthy") |>
    release("sick1") |>
    branch(  # release A if on it
      function() (get_attribute(env,"onA")==1) + 1L,
      continue = rep(TRUE,2),
      trajectory(),
      trajectory() |> release("A",1) |> set_attribute("onA", 0)
    )
}

sick2 <- function(traj, inputs) {
  traj |>
    set_attribute("State", 2) |>
    set_attribute("waiting", 0) |>
    set_attribute("leftwl", function(){ wl_leave(get_name(env)); 1 }) |>
    release("sick1") |>
    seize("sick2") |>
    branch(  # if somehow on A when progressing, free it (shouldn't normally happen)
      function() (get_attribute(env,"onA")==1) + 1L,
      continue = rep(TRUE,2),
      trajectory(),
      trajectory() |> release("A",1) |> set_attribute("onA", 0)
    )
}

death <- function(traj, inputs) {
  traj |>
    set_attribute("leftwl", function(){ wl_leave(get_name(env)); 1 }) |>
    branch(  # release A if held, BEFORE terminating
      function() (get_attribute(env,"onA")==1) + 1L,
      continue = rep(TRUE,2),
      trajectory(),
      trajectory() |> release("A",1) |> set_attribute("onA", 0)
    ) |>
    branch(function() 1, continue=c(FALSE),
           trajectory("Death") |> mark("death") |> terminate_simulation(inputs))
}

terminate_simulation <- function(traj, inputs) {
  traj |> branch(function() 1, continue=FALSE,
                 trajectory() |> cleanup_on_termination(inputs))
}
cleanup_on_termination <- function(traj, inputs) {
  traj |>
    release("time_in_model") |>
    set_attribute("leftwl", function(){ wl_leave(get_name(env)); 1 }) |>
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 3),
      trajectory() |> release("healthy"),
      trajectory() |> release("sick1"),
      trajectory() |> release("sick2")
    ) |>
    # end-of-horizon patients may still hold a slot of A -> release it so the
    # resource monitor closes cleanly (no "leaving without releasing" warning).
    branch(
      function() (get_attribute(env,"onA")==1) + 1L,
      continue = rep(TRUE,2),
      trajectory(),
      trajectory() |> release("A",1) |> set_attribute("onA", 0)
    )
}

# ---- competing risks (rexp), identical pattern to model10 ------------------
years_till_death <- function(inputs) {
  state <- get_attribute(env, "State")
  rate  <- inputs$r.HD
  if (state == 1) rate <- rate * inputs$hr.S1D
  if (state == 2) rate <- rate * inputs$hr.S2D
  rexp(1, rate)
}
years_till_sick1 <- function(inputs) {
  if (get_attribute(env, "State") == 0) rexp(1, inputs$r.HS1) else inputs$horizon + 1
}
years_till_healthy <- function(inputs) {
  if (get_attribute(env, "State") == 1) rexp(1, inputs$r.S1H) else inputs$horizon + 1
}
years_till_sick2 <- function(inputs) {
  state <- get_attribute(env, "State")
  onA   <- get_attribute(env, "onA")
  if (state == 1 && onA == 0) rexp(1, inputs$r.S1S2)        # untreated S1 can progress
  else if (state == 1 && onA == 1) inputs$horizon + 1       # treatment HALTS progression
  else inputs$horizon + 1
}
terminate_at_horizon <- function(inputs) inputs$horizon - now(env)

# ---------------------------------------------------------------------------
event_registry <- list(
  list(name="Terminate", attr="aTerminate", time_to_event=terminate_at_horizon,  func=terminate_simulation, reactive=FALSE),
  list(name="Death",     attr="aDeath",     time_to_event=years_till_death,       func=death,                reactive=TRUE),
  list(name="Sick1",     attr="aSick1",     time_to_event=years_till_sick1,       func=sick1,                reactive=TRUE),
  list(name="Healthy",   attr="aHealthy",   time_to_event=years_till_healthy,     func=healthy,              reactive=TRUE),
  list(name="Sick2",     attr="aSick2",     time_to_event=years_till_sick2,       func=sick2,                reactive=TRUE),
  list(name="GetA",      attr="aTreatA",    time_to_event=years_till_treatmentA,  func=get_treatmentA,       reactive=TRUE),
  list(name="EndA",      attr="aEndA",      time_to_event=years_till_endA,        func=end_treatmentA,       reactive=TRUE)
)

counters <- c("time_in_model","death","healthy","sick1","sick2")

inputs_base <- list(
  N        = 100,
  horizon  = 50,
  capacity = 2,                      # c : contended slots
  mu_A     = 0.5,                    # treatment service rate (mean course = 2 yrs)
  rate_admit_when_free = 365,        # ~1 day when a slot is free (near-instant admit)
  r.HS1  = 0.15, r.S1H = 0.5, r.S1S2 = 0.105,
  r.HD   = 0.02, hr.S1D = 3, hr.S2D = 10
)

# ---------------------------------------------------------------------------
des_run <- function(inputs) {
  reset_waitlist()
  env <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env |>
    create_counters(counters) |>
    add_resource("A", capacity = inputs$capacity, queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon=2) |>
    run(inputs$horizon + 1/365) |>
    wrap()

  arr <- get_mon_arrivals(env, per_resource = TRUE)
  res <- get_mon_resources(env)
  list(arrivals = arr, resources = res, env = env)
}

# --- instrumentation: extract waits, deaths, S2-time, A-time ---------------
summarise_run <- function(out, inputs) {
  arr <- out$arrivals
  # deaths
  n_death <- sum(arr$resource == "death")
  # time in S2 (sum over all S1->...): seize/release of 'sick2'
  s2 <- arr[arr$resource == "sick2", ]
  s2_time <- sum(pmax(0, s2$end_time - s2$start_time))
  # time on A
  a <- arr[arr$resource == "A", ]
  a_time  <- sum(pmax(0, a$end_time - a$start_time))
  n_treated <- nrow(a)
  # REALIZED WAIT: time from entering S1 (start of a 'sick1' seize) to seizing A.
  # We reconstruct per-patient: first sick1 start vs first A start.
  s1 <- arr[arr$resource == "sick1", ]
  waits <- numeric(0)
  if (nrow(a) > 0) {
    for (nm in unique(a$name)) {
      a_starts  <- sort(a$start_time[a$name == nm])
      s1_starts <- sort(s1$start_time[s1$name == nm])
      # pair each A admission with the most recent prior S1 entry
      for (ast in a_starts) {
        prior <- s1_starts[s1_starts <= ast + 1e-9]
        if (length(prior)) waits <- c(waits, ast - max(prior))
      }
    }
  }
  list(
    n_death   = n_death,
    s2_time   = s2_time,
    a_time    = a_time,
    n_treated = n_treated,
    mean_wait = if (length(waits)) mean(waits) else NA_real_,
    med_wait  = if (length(waits)) median(waits) else NA_real_,
    p_waited  = if (length(waits)) mean(waits > 0.01) else NA_real_,
    n_waits   = length(waits)
  )
}

# leak check: at end, server count of A should be >= 0 and <= capacity, and
# any still-held slots are end-of-horizon patients (acceptable). We check the
# resource monitor never went negative.
leak_check <- function(out) {
  res <- out$resources
  ra  <- res[res$resource == "A", ]
  if (nrow(ra) == 0) return(list(min_server=0, max_over_cap=FALSE))
  list(min_server = min(ra$server),
       max_over_cap = any(ra$server > ra$capacity),
       neg = any(ra$server < 0))
}

# ---------------------------------------------------------------------------
# RUN EXPERIMENTS
# ---------------------------------------------------------------------------
cat("=============================================================\n")
cat("PROBE A-endogenous-wait-event\n")
cat("=============================================================\n\n")

run_and_report <- function(label, inputs) {
  set.seed(42)
  out <- des_run(inputs)
  s   <- summarise_run(out, inputs)
  lk  <- leak_check(out)
  cat(sprintf("[%s]  N=%d c=%s mu=%.2f\n", label, inputs$N,
              ifelse(is.infinite(inputs$capacity),"Inf",as.character(inputs$capacity)), inputs$mu_A))
  cat(sprintf("   deaths=%d  treated=%d  meanWait=%.3f yr  medWait=%.3f  pWaited=%.2f  (n=%d)\n",
              s$n_death, s$n_treated, s$mean_wait, s$med_wait, s$p_waited, s$n_waits))
  cat(sprintf("   total S2-time=%.1f yr   total A-time=%.1f yr\n", s$s2_time, s$a_time))
  cat(sprintf("   leak: minServer=%s overCap=%s neg=%s\n\n",
              lk$min_server, lk$max_over_cap, ifelse(is.null(lk$neg),NA,lk$neg)))
  c(s, list(out=out))
}

# 1) FACE VALIDITY: tight capacity vs unconstrained
r_c2   <- run_and_report("c=2  (tight)",  modifyList(inputs_base, list(capacity = 2)))
r_cInf <- run_and_report("c=Inf (uncon)", modifyList(inputs_base, list(capacity = Inf)))

# 2) ENDOGENEITY in c: shrink c -> mean wait should rise
cat("--- ENDOGENEITY: vary capacity c (N=100 fixed) ---\n")
for (cc in c(1,2,4,8,20)) {
  rr <- run_and_report(sprintf("c=%d", cc), modifyList(inputs_base, list(capacity = cc)))
}

# 3) ENDOGENEITY in N: raise N at fixed c -> mean wait should rise
cat("--- ENDOGENEITY: vary N (c=2 fixed) ---\n")
for (nn in c(50,100,200)) {
  rr <- run_and_report(sprintf("N=%d", nn), modifyList(inputs_base, list(N = nn, capacity = 2)))
}

cat("\n=== SUMMARY (face validity, c=2 vs c=Inf) ===\n")
cat(sprintf("  deaths:   c=2 -> %d   c=Inf -> %d   (expect c=2 >= c=Inf)\n", r_c2$n_death, r_cInf$n_death))
cat(sprintf("  S2-time:  c=2 -> %.1f c=Inf -> %.1f (expect c=2 >= c=Inf)\n", r_c2$s2_time, r_cInf$s2_time))
cat(sprintf("  meanWait: c=2 -> %.3f c=Inf -> %.3f (expect c=2 >> c=Inf)\n", r_c2$mean_wait, r_cInf$mean_wait))
cat("\nDONE.\n")
