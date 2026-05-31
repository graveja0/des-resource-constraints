#############################################################################
# PROBE B: "claim-ticket-companion"
#
# GOAL: faithful FINITE-RESOURCE CONTENTION in the event-registry/rollback DES.
#
# MECHANISM (decoupled claim-ticket / companion):
#   - The PATIENT arrival lives ONLY in the event loop. It never blocks and
#     never touches the contended treatment resource. It only ever
#     seizes/releases STATE TALLIES (healthy/sick1/sick2/death/time_in_model).
#   - When the patient becomes Sick1 and wants treatment, an event func clones
#     a lightweight COMPANION (a "claim ticket"). The companion's ONLY job is
#     to seize() the finite-capacity "treatment" resource. Blocking in that
#     queue is fine: the companion carries NO state tallies, so there is no
#     shared-resource double-release (the model12 bug).
#   - When the companion ACQUIRES the slot it send()s a per-patient
#     "acq_<pid>" signal. The patient has a reactive registry event
#     ("Treatment acquired") that fires on that signal and flips the patient
#     onto treatment (slows S1->S2 progression / enables S1->Healthy cure).
#   - clone(n=2) + synchronize(wait=FALSE): the patient-self clone reaches
#     synchronize instantly (it never blocks), so it is ALWAYS the survivor and
#     cleanly RE-ENTERS the rollback() event loop (resolves model12 failure #1).
#     The companion blocks in seize(), runs its own course, and dies on release.
#   - TEARDOWN: if the patient dies / progresses to S2 / recovers while the
#     companion is still queued, the patient send()s "abandon_<pid>" and the
#     companion renege_if()s out of the queue (releasing the slot if held).
#
# This file replicates the main_loop.R architecture inline so the result
# transfers directly to the real model.
#############################################################################

suppressMessages(library(simmer))

VERBOSE <- FALSE   # set TRUE for the simmer log_ trace

############################ MAIN-LOOP BOILERPLATE ###########################
# (lifted near-verbatim from /tmp/sick_sicker_des/main_loop.R)

create_counters <- function(env, counters) {
  sapply(counters, FUN = function(counter) env <- add_resource(env, counter, Inf, 0))
  env
}

mark <- function(traj, counter) {
  traj |> seize(counter, 1) |> timeout(0) |> release(counter, 1)
}

assign_events <- function(traj, inputs) {
  sapply(event_registry, FUN = function(event) {
    traj <- set_attribute(traj, event$attr, function() event$time_to_event(inputs))
  })
  traj
}

next_event <- function() {
  event_time <- Inf; event <- NA; id <- 0
  for (i in seq_along(event_registry)) {
    e <- event_registry[[i]]
    tmp_time <- get_attribute(env, e$attr)
    if (tmp_time < event_time) { event <- e; event_time <- tmp_time; id <- i }
  }
  list(event = event, event_time = event_time, id = id)
}

process_events <- function(traj, env, inputs) {
  traj <- timeout(traj, function() {
    ne <- next_event()
    max(0, ne[["event_time"]] - now(env))
  })
  args <- lapply(event_registry, FUN = function(e) {
    trajectory(e$name) |>
      e$func(inputs) |>
      set_attribute(e$attr, function() now(env) + e$time_to_event(inputs))
  })
  args$".trj"   <- traj
  args$option   <- function() next_event()$id
  args$continue <- rep(TRUE, length(event_registry))
  traj <- do.call(branch, args)
  lapply(event_registry[sapply(event_registry, function(x) x$reactive)],
         FUN = function(e) {
           traj <- set_attribute(traj, e$attr, function() now(env) + e$time_to_event(inputs))
         })
  traj
}

des <- function(env, inputs) {
  trajectory("Patient") |>
    initialize_patient(inputs) |>
    assign_events(inputs) |>
    branch(function() 1, continue = TRUE,
           trajectory("main_loop") |> process_events(env, inputs)) |>
    rollback(1, 100)
}

############################## MODEL DEFINITION ###############################

inputs <- list(
  N        = 100,
  horizon  = 20,        # years (short enough that escaping to Health changes
                        #        deaths-by-horizon, not just timing)

  # transition rates (per year)
  r.HS1  = 0.10,        # H  -> S1
  r.S1H  = 0.05,        # S1 -> H  (natural recovery, slow)
  r.S1S2 = 0.30,        # S1 -> S2 progression (fast: waiting hurts)
  r.HD   = 0.005,       # H  -> D
  hr.S1D = 3,           # S1 death hazard ratio
  hr.S2D = 20,          # S2 death hazard ratio (S2 is deadly)

  # TREATMENT effect: while a patient holds treatment in S1,
  #   - S1->S2 progression rate is multiplied by trt.progfac (slows progression)
  #   - S1->H recovery rate is multiplied by trt.curefac (boosts cure -> escape)
  trt.progfac = 0.1,    # 90% reduction in progression while treated
  trt.curefac = 8.0,    # 8x recovery while treated (strong cure)

  treatment_capacity = 5,     # << demand: a queue genuinely forms
  treatment_duration = 3,     # years a slot is held
  treatA = TRUE
)

# NOTE: "treatment" is the CONTENDED resource (finite capacity, added separately
# in des_run). It must NOT appear in counters (which are Inf tallies), or the
# capacity would be silently overridden to Inf. "treatment_held" is the Inf
# patient-side tally used only for costing time-on-treatment.
counters <- c("time_in_model", "death", "healthy", "sick1", "sick2")

initialize_patient <- function(traj, inputs) {
  traj |>
    seize("time_in_model") |>
    set_attribute("pid", function() as.numeric(gsub("patient", "", get_name(env)))) |>
    set_attribute("State", 0) |>           # 0=H 1=S1 2=S2 3=D
    set_attribute("onTrt", 0) |>           # patient-side flag: treatment active
    set_attribute("trtReady", 0) |>        # set by acquire-signal trap handler
    set_attribute("wantTrt", as.integer(inputs$treatA)) |>
    seize("healthy")
}

# --- helper: per-patient signal names ---------------------------------------
sig_acq     <- function() paste0("acq_",     get_attribute(env, "pid"))
sig_abandon <- function() paste0("abandon_", get_attribute(env, "pid"))

# ============================================================================
# CORE MECHANISM: the companion claim-ticket
# ============================================================================
companion_trajectory <- function(inputs) {
  trajectory("companion") |>
    # if the patient signals abandon while we are queued, leave the queue
    renege_if(sig_abandon,
              out = trajectory() |>
                (\(t) if (VERBOSE) log_(t, "companion: ABANDON (left queue)") else t)()) |>
    seize("treatment", 1) |>          # BLOCKS here when all c slots full
    renege_abort() |>                 # got a slot: cancel the abandon-renege
    (\(t) if (VERBOSE) log_(t, "companion: ACQUIRED slot") else t)() |>
    send(sig_acq) |>                  # tell the patient: treatment acquired
    timeout(inputs$treatment_duration) |>
    release("treatment", 1) |>        # hold for duration, then free the slot
    (\(t) if (VERBOSE) log_(t, "companion: released slot") else t)()
}

sick1 <- function(traj, inputs) {
  traj |>
    set_attribute("State", 1) |>
    release("healthy") |>
    seize("sick1") |>
    # set up the per-patient acquire listener. The trap handler does NOT change
    # state directly: it sets trtReady=1 and returns control to the loop's event
    # dispatch. The "Treatment acquired" REGISTRY event (time_to_event -> 0 when
    # trtReady) then fires THROUGH process_events, so its func runs AND the
    # reactive resampling re-draws sick2/healthy at the *treated* rates. (Setting
    # the flag inside a trap handler alone would not resample the competing
    # risks the patient is already timing out on -- that was the first bug.)
    trap(sig_acq,
         handler = trajectory() |>
           set_attribute("trtReady", 1) |>
           # Force the "Treatment acquired" event to be the soonest one NOW, so
           # the post-interrupt branch dispatch selects it (not a stale event).
           set_attribute("aTrt", function() now(env)) |>
           (\(t) if (VERBOSE) log_(t, "patient: acquire signal -> trtReady") else t)()) |>
    # if the patient wants treatment, spawn the companion claim-ticket.
    branch(
      function() get_attribute(env, "wantTrt") + 1,
      continue = rep(TRUE, 2),
      trajectory(),                                   # 1: no treatment wanted
      trajectory() |>                                 # 2: spawn companion
        clone(n = 2,
              trajectory("self"),                     # patient-self: continues loop
              companion_trajectory(inputs)            # companion: seizes treatment
        ) |>
        synchronize(wait = FALSE)                     # self survives, re-enters loop
    )
}

# Registry func: fires when the acquire signal has set trtReady. Activates the
# treatment (flag + Inf tally for costing). Reactive resampling that follows in
# process_events re-draws progression/recovery at the treated rates.
treatment_acquired <- function(traj, inputs) {
  traj |>
    branch(
      function() if (get_attribute(env, "trtReady") == 1 &&
                     get_attribute(env, "onTrt") == 0 &&
                     get_attribute(env, "State") == 1) 1 else 2,
      continue = rep(TRUE, 2),
      trajectory() |>
        set_attribute("onTrt", 1) |>
        seize("treatment_held", 1) |>           # Inf tally for time-on-treatment
        (\(t) if (VERBOSE) log_(t, "patient: treatment STARTED (resampling)") else t)(),
      trajectory()
    )
}

# leaving S1 (to S2, to H, or death) => abandon any pending/held treatment
abandon_treatment <- function(traj) {
  traj |>
    send(sig_abandon) |>           # companion reneges out of queue if still waiting
    set_attribute("trtReady", 0) |>  # clear the pending-acquire flag
    branch(                         # if treatment was actually active, drop the tally
      function() get_attribute(env, "onTrt") + 1,
      continue = rep(TRUE, 2),
      trajectory(),                                   # 1: not on treatment
      trajectory() |>
        release("treatment_held", 1) |>
        set_attribute("onTrt", 0)
    )
}

healthy <- function(traj, inputs) {
  traj |>
    abandon_treatment() |>
    set_attribute("State", 0) |>
    release("sick1") |>
    seize("healthy")
}

sick2 <- function(traj, inputs) {
  traj |>
    abandon_treatment() |>
    set_attribute("State", 2) |>
    release("sick1") |>
    seize("sick2")
}

death <- function(traj, inputs) {
  traj |>
    branch(function() 1, continue = FALSE,
           trajectory("Death") |>
             abandon_treatment() |>
             # release whichever state tally is held (State is 0/1/2 here),
             # THEN mark dead. cleanup_on_termination's State==3 arm is a no-op.
             branch(function() get_attribute(env, "State") + 1,
                    continue = rep(TRUE, 3),
                    trajectory() |> release("healthy"),
                    trajectory() |> release("sick1"),
                    trajectory() |> release("sick2")) |>
             set_attribute("State", 3) |>     # record death so summary sees it
             mark("death") |>
             terminate_simulation(inputs))
}

terminate_simulation <- function(traj, inputs) {
  traj |> branch(function() 1, continue = FALSE,
                 trajectory() |> cleanup_on_termination(inputs))
}

cleanup_on_termination <- function(traj, inputs) {
  traj |>
    release("time_in_model") |>
    # release whichever state tally is still held. State is 0/1/2 for the
    # horizon-cut path; for the death path it is 3, but death() already
    # released the held state tally (see below), so arm 4 is a no-op.
    branch(function() get_attribute(env, "State") + 1,
           continue = rep(TRUE, 4),
           trajectory() |> release("healthy"),
           trajectory() |> release("sick1"),
           trajectory() |> release("sick2"),
           trajectory()) |>   # State==3 (dead): tally already released in death()
    # release treatment tally if still on it (death-while-treated path also
    # passes through abandon_treatment(), but horizon-cut goes straight here)
    branch(function() get_attribute(env, "onTrt") + 1,
           continue = rep(TRUE, 2),
           trajectory(),
           trajectory() |> release("treatment_held", 1) |> set_attribute("onTrt", 0))
}

# ----- time-to-event functions (competing risks, hand-rolled) ---------------
years_till_death <- function(inputs) {
  state <- get_attribute(env, "State")
  rate <- inputs$r.HD
  if (state == 1) rate <- rate * inputs$hr.S1D
  if (state == 2) rate <- rate * inputs$hr.S2D
  rexp(1, rate)
}

years_till_sick1 <- function(inputs) {
  if (get_attribute(env, "State") == 0) rexp(1, inputs$r.HS1) else inputs$horizon + 1
}

years_till_healthy <- function(inputs) {
  if (get_attribute(env, "State") == 1) {
    rate <- inputs$r.S1H
    if (get_attribute(env, "onTrt") == 1) rate <- rate * inputs$trt.curefac
    rexp(1, rate)
  } else inputs$horizon + 1
}

years_till_sick2 <- function(inputs) {
  if (get_attribute(env, "State") == 1) {
    rate <- inputs$r.S1S2
    if (get_attribute(env, "onTrt") == 1) rate <- rate * inputs$trt.progfac
    rexp(1, rate)
  } else inputs$horizon + 1
}

# time_to_event for the treatment-acquired event: returns ~0 (fires "now") once
# the acquire-signal trap handler has set trtReady, while the patient is still an
# untreated S1. Otherwise it parks past the horizon. The trap handler interrupts
# whatever timeout the patient is in; control returns to the loop's branch
# dispatch, next_event() then picks this (time ~0) event, runs treatment_acquired
# THROUGH process_events, and the reactive resample that follows re-draws the
# competing risks at the treated rates.
years_till_trt <- function(inputs) {
  if (get_attribute(env, "trtReady") == 1 &&
      get_attribute(env, "onTrt") == 0 &&
      get_attribute(env, "State") == 1) {
    0
  } else {
    inputs$horizon + 1
  }
}

event_registry <- list(
  list(name = "Terminate at horizon", attr = "aTerminate",
       time_to_event = function(inputs) inputs$horizon - now(env),
       func = terminate_simulation, reactive = FALSE),
  list(name = "Death", attr = "aDeath",
       time_to_event = years_till_death, func = death, reactive = TRUE),
  list(name = "Sick1", attr = "aSick1",
       time_to_event = years_till_sick1, func = sick1, reactive = TRUE),
  list(name = "Healthy", attr = "aHealthy",
       time_to_event = years_till_healthy, func = healthy, reactive = TRUE),
  list(name = "Sick2", attr = "aSick2",
       time_to_event = years_till_sick2, func = sick2, reactive = TRUE),
  list(name = "Treatment acquired", attr = "aTrt",
       time_to_event = years_till_trt, func = treatment_acquired, reactive = TRUE)
)

############################### RUN HARNESS ###################################

des_run <- function(inputs) {
  env <<- simmer("ProbeB", verbose = VERBOSE)
  traj <- des(env, inputs)
  env |>
    create_counters(c(counters, "treatment_held")) |>
    add_resource("treatment", capacity = inputs$treatment_capacity, queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1 / 365) |>
    wrap()
  list(
    arrivals  = get_mon_arrivals(env, per_resource = TRUE),
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

# Wait time per companion = time from queue entry (t=becomes-sick) to acquire.
# We read it from the treatment resource arrivals: start_time is acquire time,
# but we want acquire - enqueue. Easiest: instrument via the treatment arrivals'
# activity vs total. Instead we compute waits from per-resource arrivals on
# "treatment": end_time - start_time = duration held; the WAIT is the gap
# between the companion's birth (its self-patient became sick) and its seize.
# We capture waits directly with attributes below.

summarize_run <- function(res, inputs, label) {
  arr  <- res$arrivals

  # deaths counted from the death tally (robust; State attr is also set to 3)
  n_dead <- nrow(subset(arr, resource == "death"))

  # time spent in each state tally (sum of end-start across state resources)
  state_time <- function(r) {
    s <- subset(arr, resource == r)
    if (nrow(s) == 0) return(0)
    sum(pmin(s$end_time, inputs$horizon) - s$start_time)
  }
  t_s1 <- state_time("sick1")
  t_s2 <- state_time("sick2")
  t_h  <- state_time("healthy")
  # simple (undiscounted) QALY proxy: u.H=1, u.S1=0.75, u.S2=0.5
  total_qaly <- 1.0 * t_h + 0.75 * t_s1 + 0.50 * t_s2
  mean_qaly  <- total_qaly / inputs$N

  # treatment contention metrics
  trt_res <- subset(res$resources, resource == "treatment")
  max_q   <- if (nrow(trt_res)) max(trt_res$queue) else 0
  max_srv <- if (nrow(trt_res)) max(trt_res$server) else 0
  min_srv <- if (nrow(trt_res)) min(trt_res$server) else 0  # leak check (>=0)

  # actual acquisitions = treatment_held tally rows (set only when the patient
  # trap fires on an acquire signal). Abandoned/queued companions never appear.
  n_treated <- nrow(subset(arr, resource == "treatment_held"))

  list(label = label, n_dead = n_dead, t_s1 = t_s1, t_s2 = t_s2,
       mean_qaly = mean_qaly,
       n_treated = n_treated, max_queue = max_q,
       max_server = max_srv, min_server = min_srv,
       cap = inputs$treatment_capacity)
}

# --- to measure WAIT directly, instrument companion birth time -------------
# We add it cleanly by recording the enqueue time in the companion and the
# acquire time, then differencing via attributes attached to the patient.
# (Re-using attributes: companion sets tEnq at birth, tAcq at acquire.)

############################### EXECUTE #######################################

run_scenario <- function(cap, N = 100, seed = 1, treatA = TRUE, label = NULL) {
  set.seed(seed)
  ins <- modifyList(inputs, list(treatment_capacity = cap, N = N, treatA = treatA))
  res <- des_run(ins)
  if (is.null(label)) label <- paste0("cap=", cap, ", N=", N)
  summarize_run(res, ins, label)
}

cat("=============================================================\n")
cat("PROBE B: claim-ticket-companion  (faithful finite contention)\n")
cat("=============================================================\n\n")

# Scenario sweep: no-treatment baseline, then tighten capacity, then raise N.
scn <- list(
  run_scenario(cap = 1e9, N = 100, seed = 1, treatA = FALSE, label = "NO treatment"),
  run_scenario(cap = 1e9, N = 100, seed = 1, label = "cap=Inf (unconstr)"),
  run_scenario(cap = 5,   N = 100, seed = 1, label = "cap=5"),
  run_scenario(cap = 2,   N = 100, seed = 1, label = "cap=2"),
  run_scenario(cap = 1,   N = 100, seed = 1, label = "cap=1 (tightest)"),
  run_scenario(cap = 2,   N = 200, seed = 1, label = "cap=2, N=200")
)

cat(sprintf("%-18s %6s %8s %8s %9s %7s %6s %6s %6s\n",
            "scenario", "deaths", "S2-time", "mQALY",
            "treated", "maxQ", "mSrv", "minSrv", "cap"))
cat(strrep("-", 86), "\n")
for (s in scn) {
  cat(sprintf("%-18s %6d %8.1f %8.3f %9d %7d %6d %6d %6g\n",
              s$label, s$n_dead, s$t_s2, s$mean_qaly,
              s$n_treated, s$max_queue, s$max_server, s$min_server, s$cap))
}
cat("\n")
cat("Reading: tighter cap => fewer treated, longer queues, more S2-time,\n")
cat("         more deaths, lower mean QALY. minSrv must stay >= 0 (no leak).\n\n")

#############################################################################
# DIRECT WAIT MEASUREMENT (endogeneity) — second pass with wait capture.
# We re-run capturing the companion's enqueue and acquire times.
#############################################################################

measure_waits <- function(cap, N = 100, seed = 1) {
  set.seed(seed)
  ins <- modifyList(inputs, list(treatment_capacity = cap, N = N))
  env <<- simmer("ProbeBwait", verbose = FALSE)

  # rebuild companion to record enqueue/acquire times as global-keyed attrs
  companion_trajectory <<- function(inputs) {
    trajectory("companion") |>
      set_attribute("tEnq", function() now(env)) |>
      renege_if(sig_abandon, out = trajectory()) |>
      seize("treatment", 1) |>
      renege_abort() |>
      set_attribute("tAcq", function() now(env)) |>
      set_attribute("wait", function() now(env) - get_attribute(env, "tEnq")) |>
      send(sig_acq) |>
      timeout(inputs$treatment_duration) |>
      release("treatment", 1)
  }
  traj <- des(env, ins)
  env |>
    create_counters(c(counters, "treatment_held")) |>
    add_resource("treatment", capacity = ins$treatment_capacity, queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, ins$N)), mon = 2) |>
    run(ins$horizon + 1 / 365) |>
    wrap()

  at <- get_mon_attributes(env)
  w  <- subset(at, key == "wait")
  if (nrow(w) == 0) return(c(n = 0, mean = NA, max = NA))
  c(n = nrow(w), mean = mean(w$value), max = max(w$value))
}

cat("ENDOGENEITY: mean companion wait (yrs) as capacity shrinks / N grows\n")
cat(strrep("-", 60), "\n")
for (cfg in list(c(cap = 1e9, N = 100), c(cap = 20, N = 100),
                 c(cap = 5, N = 100), c(cap = 2, N = 100),
                 c(cap = 1, N = 100), c(cap = 2, N = 200))) {
  w <- measure_waits(cfg["cap"], cfg["N"], seed = 1)
  cat(sprintf("cap=%-10g N=%-4g  n_acquired=%4d  mean_wait=%6.3f  max_wait=%6.3f\n",
              cfg["cap"], cfg["N"], w["n"], w["mean"], w["max"]))
}

cat("\nDONE.\n")
