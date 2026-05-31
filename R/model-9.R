###############################################################################
# model-9.R — exogenous confirmation queue
#
# Extends model-8 by inserting a confirmation wait between a positive screen
# result and the start of treatment.  The wait is drawn from a lognormal
# distribution (mean ~18 weeks, SD ~8 weeks — pilot data from the case study).
# During the wait the patient's disease keeps progressing: S1 can advance to
# S2 before confirmation arrives, losing the treatment benefit entirely.
#
# The wait is EXOGENOUS: it is a policy parameter drawn from a fixed
# distribution, independent of how many other patients are in the system.
# This is the "analyst specifies the queue from data" approach.
#
# New in this model vs model-8
#   attribute : TreatA_pending, tConfirm  (schedule delayed confirmation)
#   event     : Confirm  (reactive, fires at stored tConfirm time)
#   screen()  : overridden — TPs now pend rather than immediately treat
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
# Override screen(): TPs schedule a deferred confirmation; FPs pay costs only.
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
      u_tp <- draw_unif("screen"); u_fp <- draw_unif("screen")   # CRN
      if (state >= 2)          return(1L)
      if (strat == 'noscreen') return(1L)

      if (state == 1L && u_tp < cov * sens)       return(2L)  # TP
      if (state == 0L && u_fp < cov * (1 - spec)) return(3L)  # FP
      return(1L)
    },
    continue = rep(TRUE, 3),

    ## branch 1: no positive result
    trajectory(),

    ## branch 2: true positive — defer confirmation
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost",     function() inputs$c.confirm) |>
      set_attribute("TreatA_pending",  1) |>
      set_attribute("tConfirm", function()
        now(env) + qlnorm(draw_unif("confirm"),
                          inputs$confirm_wait_logmean, inputs$confirm_wait_logsd)),

    ## branch 3: false positive — pay costs, no treatment
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost", function() inputs$c.confirm)
  )
}

# ---------------------------------------------------------------------------
# Confirmation event: fires at the stored tConfirm time.
# Treats only if the patient is still in S1 when confirmed.
# ---------------------------------------------------------------------------
years_till_confirm <- function(inputs)
{
  pending <- get_attribute(env, "TreatA_pending")
  if (is.na(pending) || pending != 1) return(inputs$horizon + 1)
  t_conf <- get_attribute(env, "tConfirm")
  max(0, t_conf - now(env))
}

confirm <- function(traj, inputs)
{
  traj |>
  set_attribute("TreatA_pending", 0) |>
  branch(
    function() {
      # Only treat if the patient is still in S1 when confirmation arrives
      if (get_attribute(env, "State") == 1L) 1L else 2L
    },
    continue = c(TRUE, TRUE),
    ## branch 1: still in S1 — start treatment
    trajectory() |>
      set_attribute("TreatA", 1) |>
      release("sick1")           |>
      seize("treated_s1"),
    ## branch 2: already progressed or died — no benefit
    trajectory()
  )
}

# ---------------------------------------------------------------------------
# Override sick2 / healthy / years_till_sick2 (identical to model-7/8)
# ---------------------------------------------------------------------------
sick2 <- function(traj, inputs)
{
  traj |>
  set_attribute("State", 2) |>
  set_attribute("TreatA_pending", 0) |>    # cancel any pending confirm
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
  set_attribute("TreatA_pending", 0) |>
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
  set_attribute("TreatA_pending",  0) |>
  set_attribute("tConfirm",        0) |>
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
# Event registry — adds Confirm event
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
  list(name          = "Confirm",
       attr          = "aConfirm",
       time_to_event = years_till_confirm,
       func          = confirm,
       reactive      = TRUE)
)

# ---------------------------------------------------------------------------
# Costing / QALYs / helpers (identical to model-7/8)
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
  oc    <- attrs[attrs$key %in% c("ScreenCost", "ConfirmCost"), , drop = FALSE]
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
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env  |>
    create_counters(counters) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1/365) |>
    wrap()

  get_mon_arrivals(env, per_resource = TRUE) |>
    cost_arrivals(inputs) |>
    qaly_arrivals(inputs) |>
    add_attr_costs(inputs)
}

# ---------------------------------------------------------------------------
# Experiment: molecular vs field with exogenous confirmation queue
# Both arms share the SAME seed -> identical per-patient CRN banks (CRN).
# ---------------------------------------------------------------------------
summarise_run <- function(r) {
  n <- length(unique(r$name))
  data.frame(dcost = sum(r$dcost) / n, dqaly = sum(r$dqaly) / n)
}

run_mol   <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'mol')),   seed = 42L)
run_field <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'field')), seed = 42L)

res_mol   <- summarise_run(run_mol)
res_field <- summarise_run(run_field)

delta_cost <- res_field$dcost - res_mol$dcost
delta_qaly <- res_field$dqaly - res_mol$dqaly

cat(sprintf("Molecular (exog queue): dcost = %8.0f  dQALY = %.3f\n",
            res_mol$dcost, res_mol$dqaly))
cat(sprintf("Field     (exog queue): dcost = %8.0f  dQALY = %.3f\n",
            res_field$dcost, res_field$dqaly))
cat(sprintf("Δcost = %.0f  ΔdQALY = %.3f\n", delta_cost, delta_qaly))
quadrant <- if (delta_cost < 0 && delta_qaly > 0) "SE (dominant)" else
            if (delta_cost > 0 && delta_qaly > 0) "NE" else
            if (delta_cost < 0 && delta_qaly < 0) "SW" else "NW (dominated)"
cat(sprintf("Quadrant: %s\n", quadrant))
