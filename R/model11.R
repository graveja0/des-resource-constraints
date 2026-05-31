###############################################################################
# model11.R — claim-ticket companion, approach B  (most defensible)
#
# Same finite confirmation resource as model10 (approach A), but the queue is
# now the EXACT emergent FIFO queue of a real blocking seize(), not an analytic
# approximation. The patient NEVER blocks: when screen-positive it spawns a
# lightweight COMPANION ("claim ticket") via clone(); the companion's only job
# is to seize() the finite "confirm" resource. Blocking in that queue is fine —
# the companion carries no state tallies, so there is no double-release.
#
# MECHANISM (faithful to validation/probe-B-claim-ticket.R)
#   - Per-patient signals are keyed on a numeric "pid" attribute that the clone
#     INHERITS, so the companion's send() names the right patient. (Keying on
#     get_name(env) fails: inside the companion that returns the companion's
#     own name.)
#   - On a positive screen the patient: arms trap(acq_<pid>), then
#     clone(n = 2, self, companion) + synchronize(wait = FALSE). The self-clone
#     reaches synchronize instantly and re-enters the rollback() event loop; the
#     companion blocks in seize("confirm") and is discarded at synchronize after
#     its course.
#   - When the companion ACQUIRES a slot it send()s acq_<pid>. The trap handler
#     does NOT treat directly — it sets confirmReady = 1 and forces the
#     "Confirm acquired" REGISTRY event to fire now (aConfirmAcq = now). That
#     event runs through process_events so the reactive resample re-draws
#     sick2/healthy at the *treated* rates. (Treating inside the trap handler
#     alone would not resample the competing risks — that was bug #3.)
#   - TEARDOWN: leaving S1 (recover / progress / die) send()s abandon_<pid>; a
#     still-queued companion renege_if()s out (renege_abort() after seize means
#     abandon is a no-op once a slot is held). This mirrors model10's wl_leave,
#     so A and B agree on the contended population.
#
# For mean CEA endpoints (deaths, S2-time, dQALY, dcost) approaches A and B
# agree within Monte-Carlo noise; they differ only on the wait DISTRIBUTION
# (B exact FCFS, A single-Exp analytic).
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
# Per-patient signal names, keyed on the inherited "pid" attribute.
# ---------------------------------------------------------------------------
sig_acq     <- function() paste0("acq_",     get_attribute(env, "pid"))
sig_abandon <- function() paste0("abandon_", get_attribute(env, "pid"))

# ---------------------------------------------------------------------------
# Companion claim-ticket: queues for the finite "confirm" resource, signals the
# patient on acquire, holds the slot for the workup, then frees it. Carries NO
# state tallies. Duration draw stays on the global RNG (queue-dynamics noise);
# the patient's own trajectory is fully on CRN banks, so this cannot desync it.
# ---------------------------------------------------------------------------
companion_trajectory <- function(inputs) {
  trajectory("companion") |>
    renege_if(sig_abandon, out = trajectory()) |>   # leave queue if patient abandons
    seize("confirm", 1) |>                           # BLOCKS here until a slot frees
    renege_abort() |>                                # got a slot: cancel the renege
    send(sig_acq) |>                                 # tell the patient: confirmed
    timeout(function() rexp(1, inputs$mu.confirm)) |># hold slot for the workup
    release("confirm", 1)
}

# ---------------------------------------------------------------------------
# Override screen(): on a positive result spawn the companion + arm the trap.
# ---------------------------------------------------------------------------
spawn_confirm_companion <- function(traj, inputs, is_tp) {
  traj |>
    set_attribute("ScreenCost",  function() screen_unit_cost(inputs)) |>
    set_attribute("ConfirmCost", function() inputs$c.confirm) |>
    set_attribute("IsTruePos",   is_tp) |>
    # arm the per-patient acquire listener BEFORE cloning the companion
    trap(sig_acq,
         handler = trajectory() |>
           set_attribute("confirmReady", 1) |>
           set_attribute("aConfirmAcq", function() now(env))) |>
    clone(n = 2,
          trajectory("self"),                      # patient-self: re-enters loop
          companion_trajectory(inputs)) |>         # companion: queues for confirm
    synchronize(wait = FALSE)                       # self survives instantly
}

screen <- function(traj, inputs) {
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

      if (state == 1L && u_res < sens)       return(2L)  # TP
      if (state == 0L && u_res < (1 - spec)) return(3L)  # FP
      return(4L)                                          # screened negative
    },
    continue = rep(TRUE, 4),

    trajectory(),                                           # not screened
    trajectory() |> spawn_confirm_companion(inputs, 1),     # TP -> companion
    trajectory() |> spawn_confirm_companion(inputs, 0),     # FP -> companion
    ## screened negative — test cost only
    trajectory() |>
      set_attribute("ScreenCost", function() screen_unit_cost(inputs))
  )
}

# ---------------------------------------------------------------------------
# "Confirm acquired" registry event. Fires (time ~0) once the acquire-signal
# trap has set confirmReady. Treats only a true positive still in S1.
# ---------------------------------------------------------------------------
years_till_confirm_acq <- function(inputs) {
  if (get_attribute(env, "confirmReady") == 1) 0 else inputs$horizon + 1
}

confirm_acquired <- function(traj, inputs) {
  traj |>
  branch(
    function() if (get_attribute(env, "confirmReady") == 1 &&
                   get_attribute(env, "IsTruePos")    == 1 &&
                   get_attribute(env, "State")        == 1 &&
                   get_attribute(env, "TreatA")       == 0) 1L else 2L,
    continue = c(TRUE, TRUE),
    ## TP still in S1: start treatment
    trajectory() |>
      set_attribute("confirmReady", 0) |>
      set_attribute("TreatA", 1) |>
      set_attribute("TreatCost", function() inputs$c.Trt.onetime) |>
      release("sick1") |>
      seize("treated_s1"),
    ## FP, or progressed/recovered before confirmation: just clear the flag
    trajectory() |>
      set_attribute("confirmReady", 0)
  )
}

# ---------------------------------------------------------------------------
# Leaving S1: abandon any queued companion (mirrors model10's wl_leave).
# ---------------------------------------------------------------------------
sick2 <- function(traj, inputs) {
  traj |>
  send(sig_abandon) |>
  set_attribute("State", 2) |>
  branch(
    function() if (isTRUE(get_attribute(env, "TreatA") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("treated_s1"),
    trajectory() |> release("sick1")
  ) |>
  seize("sick2")
}

healthy <- function(traj, inputs) {
  traj |>
  send(sig_abandon) |>
  set_attribute("State", 0) |>
  branch(
    function() if (isTRUE(get_attribute(env, "TreatA") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("treated_s1") |> set_attribute("TreatA", 0),
    trajectory() |> release("sick1")
  ) |>
  seize("healthy")
}

death <- function(traj, inputs) {
  traj |> branch(
    function() 1, continue = c(FALSE),
    trajectory("Death") |>
      send(sig_abandon) |>
      mark("death") |>
      terminate_simulation(inputs)
  )
}

years_till_sick2 <- function(inputs) {
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
  set_attribute("pid",  function() as.numeric(gsub("[^0-9]", "", get_name(env)))) |>
  set_attribute("AgeInitial",      function() 20 + floor(draw_unif("age") * 11)) |>
  set_attribute("tScreen", function() inputs$t.screen.start +
    (inputs$t.screen.end - inputs$t.screen.start) * draw_unif("screen_time")) |>
  set_attribute("State",           0) |>
  set_attribute("TreatA",          0) |>
  set_attribute("IsTruePos",       0) |>
  set_attribute("confirmReady",    0) |>
  set_attribute("Screened",        0) |>
  seize("healthy")
}

cleanup_on_termination <- function(traj, inputs)
{
  traj |>
  release("time_in_model") |>
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
    trajectory()                              # State 2 (sick2) handled below
  ) |>
  branch(
    function() if (get_attribute(env, "State") == 2) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("sick2"),
    trajectory()
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
# Event registry — adds "Confirm acquired"
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
  list(name          = "Confirm acquired",
       attr          = "aConfirmAcq",
       time_to_event = years_till_confirm_acq,
       func          = confirm_acquired,
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
  # Confirmation capacity is specified per 1,000 patients; scale to absolute.
  n.cap <- max(1, round(inputs$cap.confirm.per1000 * inputs$N / 1000))
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env  |>
    create_counters(counters) |>
    add_resource("confirm", capacity = n.cap, queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1/365) |>
    wrap()

  get_mon_arrivals(env, per_resource = TRUE) |>
    cost_arrivals(inputs) |>
    qaly_arrivals(inputs) |>
    add_attr_costs(inputs)
}

# ---------------------------------------------------------------------------
# Experiment: molecular vs field, approach B
# Headline check: A and B agree on mean CEA endpoints within MC noise.
# n is counted from time_in_model rows so companion clones don't dilute it.
# ---------------------------------------------------------------------------
summarise_run <- function(r) {
  n <- length(unique(r$name[r$resource == "time_in_model"]))
  data.frame(dcost = sum(r$dcost) / n, dqaly = sum(r$dqaly) / n)
}

run_mol   <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'mol')),   seed = 42L)
run_field <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'field')), seed = 42L)

res_mol   <- summarise_run(run_mol)
res_field <- summarise_run(run_field)

delta_cost <- res_field$dcost - res_mol$dcost
delta_qaly <- res_field$dqaly - res_mol$dqaly

cat(sprintf("Molecular (FIFO companion, B): dcost = %8.0f  dQALY = %.3f\n",
            res_mol$dcost, res_mol$dqaly))
cat(sprintf("Field     (FIFO companion, B): dcost = %8.0f  dQALY = %.3f\n",
            res_field$dcost, res_field$dqaly))
cat(sprintf("Δcost = %.0f  ΔdQALY = %.3f\n", delta_cost, delta_qaly))
quadrant <- if (delta_cost < 0 && delta_qaly > 0) "SE (dominant)" else
            if (delta_cost > 0 && delta_qaly > 0) "NE" else
            if (delta_cost < 0 && delta_qaly < 0) "SW" else "NW (dominated)"
cat(sprintf("Quadrant: %s\n", quadrant))
