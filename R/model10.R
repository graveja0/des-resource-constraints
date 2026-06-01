###############################################################################
# model10.R — endogenous confirmation queue, approach A
#
# Replaces the exogenous (fixed-distribution) wait of model-9 with a
# genuinely finite confirmation resource of capacity n.confirm.cap.  The
# wait a patient experiences is ENDOGENOUS: it is read off the LIVE occupancy
# of the resource and the patient's FIFO position in a global waitlist.
# Disease keeps progressing during the wait; the patient NEVER blocks.
#
# Mechanism (approach A from methodology.md §4-A)
#   - add_resource("confirm", capacity = n.confirm.cap) — FINITE, contended.
#     NOT in the counters list (that list is for capacity-Inf tallies only).
#   - Global FIFO waitlist WL: a patient JOINs on receiving a positive screen
#     result (no blocking) and LEAVEs on every exit path.
#   - years_till_confirm(): reads live get_server_count("confirm") vs
#     get_capacity("confirm") and the patient's FIFO position.  If a slot is
#     (in expectation) available → near-instant admit; otherwise draw an
#     M/M/c-flavoured Exp wait.  Reactive → resamples after every event.
#   - get_confirm(): FIRE-TIME GUARD — seizes "confirm" only if a slot is
#     genuinely free; otherwise re-defers.  Capacity is enforced exactly.
#   - release_confirm(): releases the slot after a treatment course
#     (mean 1/mu.confirm years), freeing it for the next waiter.
#   - EVERY exit path removes the patient from WL and releases "confirm"
#     if held.  No leaks, no double-release.
#
# Honest caveat: the wait DISTRIBUTION is an analytic approximation (correct
# first moment; single-Exp).  For mean-based CEA endpoints this is benign.
# Use model11 (approach B) when the full queue distribution matters.
###############################################################################

library(simmer)

source('discount.R')
source('inputs2.R')
source('main_loop.R')
source('crn.R')        # per-patient common-random-number banks

source('event_death3.R')
source('event_sick1.R')
source('event_healthy.R')
source('event_sick2.R')
source('event_screen.R')   # provides years_till_screen; screen() overridden below

# ---------------------------------------------------------------------------
# Global FIFO waitlist (lives outside simmer; reset each run in des_run)
# ---------------------------------------------------------------------------
WL <- new.env(parent = emptyenv())
reset_waitlist <- function() WL$order <- character(0)
wl_join     <- function(nm) { if (!(nm %in% WL$order)) WL$order <- c(WL$order, nm) }
wl_leave    <- function(nm) WL$order <- setdiff(WL$order, nm)
wl_position <- function(nm) { p <- match(nm, WL$order); if (is.na(p)) 1L else p }

# ---------------------------------------------------------------------------
# Override screen(): positive patients join the FIFO waitlist (no blocking).
# ---------------------------------------------------------------------------
screen <- function(traj, inputs)
{
  traj |>
  set_attribute("Screened", 1) |>
  branch(
    function() {
      state <- get_attribute(env, "State")

      strat <- inputs$strategy
      cov  <- if (strat == 'mol') inputs$cov.mol   else inputs$cov.field
      sens <- if (strat == 'mol') inputs$sens.mol  else inputs$sens.field
      spec <- if (strat == 'mol') inputs$spec.mol  else inputs$spec.field
      u_cov <- draw_unif("screen"); u_res <- draw_unif("screen")   # CRN
      if (strat == 'noscreen') return(1L)
      if (state >= 2)          return(1L)
      if (u_cov >= cov)        return(1L)   # not reached

      if (state == 1L && u_res < sens)       return(2L)
      if (state == 0L && u_res < (1 - spec)) return(3L)
      return(4L)                                          # screened negative
    },
    continue = rep(TRUE, 4),

    trajectory(),   # not screened

    ## TP: join waitlist, record type so confirm knows to treat
    trajectory() |>
      set_attribute("ScreenCost",     function() screen_unit_cost(inputs)) |>
      set_attribute("ConfirmCost",    function() inputs$c.confirm) |>
      set_attribute("WaitingConfirm", 1)  |>
      set_attribute("IsTruePos",      1)  |>
      set_attribute("HasConfirm",     0)  |>
      set_attribute("joinWL", function() { wl_join(get_name(env)); 1L }),

    ## FP: join waitlist, pay costs, no treatment on confirmation
    trajectory() |>
      set_attribute("ScreenCost",     function() screen_unit_cost(inputs)) |>
      set_attribute("ConfirmCost",    function() inputs$c.confirm) |>
      set_attribute("WaitingConfirm", 1)  |>
      set_attribute("IsTruePos",      0)  |>
      set_attribute("HasConfirm",     0)  |>
      set_attribute("joinWL", function() { wl_join(get_name(env)); 1L }),

    ## branch 4: screened negative — test cost only (no workup, no queue)
    trajectory() |>
      set_attribute("ScreenCost",     function() screen_unit_cost(inputs))
  )
}

# ---------------------------------------------------------------------------
# Endogenous wait: read live occupancy + FIFO position.
# ---------------------------------------------------------------------------
years_till_confirm <- function(inputs)
{
  if (get_attribute(env, "WaitingConfirm") != 1) return(inputs$horizon + 1)
  if (get_attribute(env, "HasConfirm")     == 1) return(inputs$horizon + 1)

  cap  <- get_capacity(env, "confirm")
  busy <- get_server_count(env, "confirm")
  free <- cap - busy
  pos  <- wl_position(get_name(env))
  mu   <- inputs$mu.confirm

  if (free >= pos) {
    draw_exp("confirm", inputs$rate_admit_free)      # slot available now
  } else {
    n_ahead <- pos - free                            # patients ahead in queue
    draw_exp("confirm", cap * mu / n_ahead)          # M/M/c-flavoured wait
  }
}

# ---------------------------------------------------------------------------
# Fire-time guard: seize "confirm" only if a slot is genuinely free.
# ---------------------------------------------------------------------------
get_confirm <- function(traj, inputs)
{
  traj |>
  branch(
    function() {
      if (get_capacity(env, "confirm") > get_server_count(env, "confirm")) 1L else 2L
    },
    continue = c(TRUE, TRUE),
    ## slot free: seize and start confirmation/treatment
    trajectory() |>
      seize("confirm") |>
      set_attribute("HasConfirm",     1) |>
      set_attribute("WaitingConfirm", 0) |>
      set_attribute("wlLeave", function() { wl_leave(get_name(env)); 1L }) |>
      branch(
        function() if (isTRUE(get_attribute(env, "IsTruePos") == 1L) &&
                        get_attribute(env, "State") == 1L) 1L else 2L,
        continue = c(TRUE, TRUE),
        ## TP still in S1: start treatment
        trajectory() |>
          set_attribute("TreatA", 1) |>
      set_attribute("TreatCost", function() inputs$c.Trt.onetime) |>
          release("sick1")           |>
          seize("treated_s1"),
        ## FP or TP that already progressed: no treatment
        trajectory()
      ),
    ## no slot free: re-defer (reactive reschedule will retry)
    trajectory()
  )
}

# ---------------------------------------------------------------------------
# Release "confirm" slot after treatment course completes.
# ---------------------------------------------------------------------------
years_till_release_confirm <- function(inputs)
{
  if (get_attribute(env, "HasConfirm") != 1) return(inputs$horizon + 1)
  draw_exp("trtdur", inputs$mu.confirm)
}

release_confirm <- function(traj, inputs)
{
  traj |>
  set_attribute("HasConfirm", 0) |>
  release("confirm")
}

# ---------------------------------------------------------------------------
# Override sick2 / healthy / years_till_sick2
# ---------------------------------------------------------------------------
sick2 <- function(traj, inputs)
{
  traj |>
  set_attribute("State", 2) |>
  set_attribute("WaitingConfirm", 0) |>
  set_attribute("wlLeave", function() { wl_leave(get_name(env)); 1L }) |>
  branch(
    function() if (isTRUE(get_attribute(env, "TreatA") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("treated_s1"),
    trajectory() |> release("sick1")
  ) |>
  seize("sick2")
}

healthy <- function(traj, inputs)
{
  traj |>
  set_attribute("State", 0) |>
  set_attribute("WaitingConfirm", 0) |>
  set_attribute("wlLeave", function() { wl_leave(get_name(env)); 1L }) |>
  branch(
    function() if (isTRUE(get_attribute(env, "TreatA") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("treated_s1") |> set_attribute("TreatA", 0),
    trajectory() |> release("sick1")
  ) |>
  seize("healthy")
}

years_till_sick2 <- function(inputs)
{
  if (get_attribute(env, "State") != 1) return(inputs$horizon + 1)
  hr <- if (isTRUE(get_attribute(env, "TreatA") == 1L)) inputs$hr.TrtS1S2 else 1.0
  draw_exp("sick2", inputs$r.S1S2 * hr)
}

# ---------------------------------------------------------------------------
# Counters, initialisation, termination
# ---------------------------------------------------------------------------
counters <- c(
  "time_in_model", "death", "healthy", "sick1", "treated_s1", "sick2"
)

initialize_patient <- function(traj, inputs)
{
  traj                   |>
  set_attribute("crnInit", function() { crn_init(get_name(env)); 0 }) |>
  seize("time_in_model") |>
  set_attribute("AgeInitial",      function() 20 + floor(draw_unif("age") * 11)) |>
  set_attribute("tScreen", function() inputs$t.screen.start +
    (inputs$t.screen.end - inputs$t.screen.start) * draw_unif("screen_time")) |>
  set_attribute("State",           0) |>
  set_attribute("TreatA",          0) |>
  set_attribute("WaitingConfirm",  0) |>
  set_attribute("IsTruePos",       0) |>
  set_attribute("HasConfirm",      0) |>
  set_attribute("Screened",        0) |>
  seize("healthy")
}

cleanup_on_termination <- function(traj, inputs)
{
  traj |>
  release("time_in_model") |>
  set_attribute("wlLeave", function() { wl_leave(get_name(env)); 1L }) |>
  branch(
    function() if (isTRUE(get_attribute(env, "HasConfirm") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("confirm"),
    trajectory()
  ) |>
  branch(
    function() {
      state  <- get_attribute(env, "State")
      treatA <- get_attribute(env, "TreatA")
      if      (state == 0)                           1L
      else if (state == 1 && isTRUE(treatA == 1L))  3L
      else if (state == 1)                           2L
      else                                           4L
    },
    continue = rep(TRUE, 4),
    trajectory() |> release("healthy"),
    trajectory() |> release("sick1"),
    trajectory() |> release("treated_s1"),
    trajectory() |> release("sick2")
  )
}

terminate_simulation <- function(traj, inputs)
{
  traj |>
  branch(function() 1, continue = FALSE,
    trajectory() |> cleanup_on_termination(inputs)
  )
}

# ---------------------------------------------------------------------------
# Event registry
# ---------------------------------------------------------------------------
event_registry <- list(
  list(name          = "Terminate at time horizon",
       attr          = "aTerminate",
       time_to_event = function(inputs) inputs$horizon - now(env),
       func          = terminate_simulation,
       reactive      = FALSE),
  list(name          = "Death",
       attr          = "aDeath",
       time_to_event = years_till_death,
       func          = death,
       reactive      = TRUE),
  list(name          = "Sick1",
       attr          = "aSick1",
       time_to_event = years_till_sick1,
       func          = sick1,
       reactive      = TRUE),
  list(name          = "Healthy",
       attr          = "aHealthy",
       time_to_event = years_till_healthy,
       func          = healthy,
       reactive      = TRUE),
  list(name          = "Sick2",
       attr          = "aSick2",
       time_to_event = years_till_sick2,
       func          = sick2,
       reactive      = TRUE),
  list(name          = "Screen",
       attr          = "aScreen",
       time_to_event = years_till_screen,
       func          = screen,
       reactive      = FALSE),
  list(name          = "Get Confirm",
       attr          = "aGetConfirm",
       time_to_event = years_till_confirm,
       func          = get_confirm,
       reactive      = TRUE),
  list(name          = "Release Confirm",
       attr          = "aReleaseConfirm",
       time_to_event = years_till_release_confirm,
       func          = release_confirm,
       reactive      = TRUE)
)

# ---------------------------------------------------------------------------
# Costing / QALYs / helpers
# ---------------------------------------------------------------------------
cost_arrivals <- function(arrivals, inputs)
{
  arrivals$cost  <- 0
  arrivals$dcost <- 0

  selector <- arrivals$resource == 'healthy'
  arrivals$cost[selector]  <- inputs$c.H *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'sick1'
  arrivals$cost[selector]  <- inputs$c.S1 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S1,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'treated_s1'
  arrivals$cost[selector]  <- (inputs$c.S1 + inputs$c.TrtA) *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S1 + inputs$c.TrtA,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'sick2'
  arrivals$cost[selector]  <- inputs$c.S2 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S2,
    arrivals$start_time[selector], arrivals$end_time[selector])

  arrivals
}

qaly_arrivals <- function(arrivals, inputs)
{
  arrivals$qaly  <- 0
  arrivals$dqaly <- 0

  selector <- arrivals$resource == 'healthy'
  arrivals$qaly[selector]  <- inputs$u.H *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.H,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'sick1'
  arrivals$qaly[selector]  <- inputs$u.S1 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.S1,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'treated_s1'
  arrivals$qaly[selector]  <- inputs$u.TrtA *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.TrtA,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector <- arrivals$resource == 'sick2'
  arrivals$qaly[selector]  <- inputs$u.S2 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.S2,
    arrivals$start_time[selector], arrivals$end_time[selector])

  arrivals
}

add_attr_costs <- function(arrivals, inputs)
{
  attrs <- get_mon_attributes(env)
  oc    <- attrs[attrs$key %in% c("ScreenCost", "ConfirmCost", "TreatCost"), , drop = FALSE]
  if (nrow(oc) == 0) return(arrivals)

  undsum <- tapply(oc$value, oc$name, sum)
  dscsum <- tapply(
    discount_value(oc$value, oc$time, annual_rate = inputs$d.r),
    oc$name, sum)

  idx <- which(arrivals$resource == "time_in_model")
  nm  <- arrivals$name[idx]
  arrivals$cost[idx]  <- arrivals$cost[idx]  +
    ifelse(is.na(undsum[nm]), 0, undsum[nm])
  arrivals$dcost[idx] <- arrivals$dcost[idx] +
    ifelse(is.na(dscsum[nm]), 0, dscsum[nm])
  arrivals
}

des_run <- function(inputs, seed = 12345L)
{
  crn_reset(seed)         # arm per-patient CRN banks for this run
  set.seed(seed)
  reset_waitlist()
  # Confirmation capacity is specified per 1,000 patients; scale to absolute.
  n.cap <- max(1, round(inputs$cap.confirm.per1000 * inputs$N / 1000))
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env  |>
    create_counters(counters) |>
    add_resource("confirm", capacity = n.cap) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1/365) |>
    wrap()

  get_mon_arrivals(env, per_resource = TRUE) |>
    cost_arrivals(inputs) |>
    qaly_arrivals(inputs) |>
    add_attr_costs(inputs)
}

# ---------------------------------------------------------------------------
# Experiment: molecular vs field with endogenous confirmation queue
# Both arms share the SAME seed -> identical per-patient CRN banks (CRN).
# ---------------------------------------------------------------------------
summarise_run <- function(r) {
  n <- length(unique(r$name))
  data.frame(dcost = sum(r$dcost) / n, dqaly = sum(r$dqaly) / n)
}

# Experiment block runs only when this file is executed directly (Rscript),
# not when source()d into the manuscript or a harness.
if (sys.nframe() == 0L) {
  run_mol   <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'mol')),   seed = 42L)
  run_field <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'field')), seed = 42L)

  res_mol   <- summarise_run(run_mol)
  res_field <- summarise_run(run_field)

  delta_cost <- res_field$dcost - res_mol$dcost
  delta_qaly <- res_field$dqaly - res_mol$dqaly

  cat(sprintf("Molecular (endog queue, A): dcost = %8.0f  dQALY = %.3f\n",
              res_mol$dcost, res_mol$dqaly))
  cat(sprintf("Field     (endog queue, A): dcost = %8.0f  dQALY = %.3f\n",
              res_field$dcost, res_field$dqaly))
  cat(sprintf("Δcost = %.0f  ΔdQALY = %.3f\n", delta_cost, delta_qaly))
  quadrant <- if (delta_cost < 0 && delta_qaly > 0) "SE (dominant)" else
              if (delta_cost > 0 && delta_qaly > 0) "NE" else
              if (delta_cost < 0 && delta_qaly < 0) "SW" else "NW (dominated)"
  cat(sprintf("Quadrant: %s\n", quadrant))
}
