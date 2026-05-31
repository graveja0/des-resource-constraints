###############################################################################
# model-8.R — field test comparison, capacity = ∞  (SE-quadrant result)
#
# Same structure as model-7 but compares two screening strategies:
#   'mol'   : molecular test  (12 % coverage, sens 0.95, spec 0.95, cost 500)
#   'field' : field test      (50 % coverage, sens 0.70, spec 0.80, cost 100)
#
# No confirmation queue yet — all screen-positives are confirmed and treated
# instantly.  At this stage the field test is dominant (SE quadrant): lower
# total discounted cost AND more discounted QALYs, because its 4× coverage
# expansion detects ~3× more true positives, and each treated patient avoids
# expensive S2 disease.  The disease cost savings outweigh the extra FP
# workup burden.
#
# Model-9 will introduce the confirmation queue and show how that erodes
# the SE-quadrant result.
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
source('event_screen.R')

# Identical overrides to model-7 -----------------------------------------

sick2 <- function(traj, inputs)
{
  traj |>
  set_attribute("State", 2) |>
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

counters <- c(
  "time_in_model", "death", "healthy", "sick1", "treated_s1", "sick2"
)

initialize_patient <- function(traj, inputs)
{
  traj                   |>
  set_attribute("crnInit", function() { crn_init(get_name(env)); 0 }) |>
  seize("time_in_model") |>
  set_attribute("AgeInitial", function() 20 + floor(draw_unif("age") * 11)) |>
  set_attribute("tScreen", function() inputs$t.screen.start +
    (inputs$t.screen.end - inputs$t.screen.start) * draw_unif("screen_time")) |>
  set_attribute("State",    0) |>
  set_attribute("TreatA",   0) |>
  set_attribute("Screened", 0) |>
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
       reactive      = FALSE)
)

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
# Experiment: molecular vs field test — expect SE quadrant (field dominant)
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

cat(sprintf("Molecular : dcost = %8.0f  dQALY = %.3f\n",
            res_mol$dcost,   res_mol$dqaly))
cat(sprintf("Field     : dcost = %8.0f  dQALY = %.3f\n",
            res_field$dcost, res_field$dqaly))
cat(sprintf("Δcost = %.0f  ΔdQALY = %.3f\n", delta_cost, delta_qaly))
if (delta_cost < 0 && delta_qaly > 0)
  cat("SE quadrant: field test is dominant (lower cost, more QALYs)\n")
