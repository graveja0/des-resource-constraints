###############################################################################
# model-7.R — molecular screening, capacity = ∞
#
# Adds a one-time population screen to the full Sick-Sicker natural history
# (model-6).  The molecular test reaches 12 % of the eligible population.
# True positives (screen-detected S1 patients) are confirmed and treated
# immediately — no queue yet.  Treatment reduces S1→S2 progression by 80 %
# and improves utility.
#
# New in this model
#   counter   : treated_s1  (replaces sick1 while on treatment)
#   event     : Screen       (one-time, reactive = FALSE, fires at t.screen)
#   parameters: inputs2.R   (cov.mol, sens.mol, spec.mol, c.screen.mol,
#                             c.confirm, hr.TrtS1S2, c.TrtA, u.TrtA)
#
# One-time screening and confirmation costs are stored as patient attributes
# ("ScreenCost", "ConfirmCost") and extracted in des_run().
###############################################################################

library(simmer)

source('discount.R')
source('inputs2.R')    # extends inputs.R with screening parameters
source('main_loop.R')
source('crn.R')        # per-patient common-random-number banks

source('event_death3.R')
source('event_sick1.R')
source('event_healthy.R')
source('event_sick2.R')
source('event_screen.R')

# ---------------------------------------------------------------------------
# Override sick2: release treated_s1 (not sick1) when a treated patient
# progresses to S2.
# ---------------------------------------------------------------------------
sick2 <- function(traj, inputs)
{
  traj |>
  set_attribute("State", 2) |>
  branch(
    function() if (isTRUE(get_attribute(env, "TreatA") == 1L)) 1L else 2L,
    continue = c(TRUE, TRUE),
    trajectory() |> release("treated_s1"),   # was on treatment
    trajectory() |> release("sick1")          # was untreated
  ) |>
  seize("sick2")
}

# Override healthy: same logic for recovering out of S1.
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

# Override years_till_sick2: apply 80 % hazard reduction for treated patients.
years_till_sick2 <- function(inputs)
{
  if (get_attribute(env, "State") != 1) return(inputs$horizon + 1)
  hr <- if (isTRUE(get_attribute(env, "TreatA") == 1L)) inputs$hr.TrtS1S2 else 1.0
  draw_exp("sick2", inputs$r.S1S2 * hr)
}

# ---------------------------------------------------------------------------
# Counters
# ---------------------------------------------------------------------------
counters <- c(
  "time_in_model",
  "death",
  "healthy",
  "sick1",       # untreated S1
  "treated_s1",  # S1 on treatment (replaces sick1 after screen detection)
  "sick2"
)

# ---------------------------------------------------------------------------
# Patient initialisation
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Termination: release whichever S1 resource the patient currently holds.
# ---------------------------------------------------------------------------
cleanup_on_termination <- function(traj, inputs)
{
  traj |>
  release("time_in_model") |>
  branch(
    function() {
      state  <- get_attribute(env, "State")
      treatA <- get_attribute(env, "TreatA")
      if      (state == 0)                           1L  # healthy
      else if (state == 1 && isTRUE(treatA == 1L))  3L  # treated S1
      else if (state == 1)                           2L  # untreated S1
      else                                           4L  # S2
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
       reactive      = FALSE)
)

# ---------------------------------------------------------------------------
# Costing
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

  # treated_s1: full annual cost = c.S1 + c.TrtA
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

# ---------------------------------------------------------------------------
# QALYs
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Helper: fold one-time attribute costs into the time_in_model rows.
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# DES run
# ---------------------------------------------------------------------------
des_run <- function(inputs, seed = 12345L)
{
  crn_reset(seed)         # arm per-patient CRN banks for this run
  set.seed(seed)          # global stream (deterministic; banks are private)
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
# Experiment: no-screen vs molecular screening
# Both arms share the SAME seed -> identical per-patient CRN banks (CRN).
# ---------------------------------------------------------------------------
summarise_run <- function(r) {
  n <- length(unique(r$name))
  data.frame(
    dcost = sum(r$dcost) / n,
    dqaly = sum(r$dqaly) / n
  )
}

# Experiment block runs only when this file is executed directly (Rscript),
# not when source()d into the manuscript or a harness.
if (sys.nframe() == 0L) {
  run_noscreen <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'noscreen')), seed = 42L)
  run_mol      <- des_run(modifyList(inputs, list(N = inputs$N, strategy = 'mol')),      seed = 42L)

  res_noscreen <- summarise_run(run_noscreen)
  res_mol      <- summarise_run(run_mol)

  icer <- (res_mol$dcost - res_noscreen$dcost) / (res_mol$dqaly - res_noscreen$dqaly)
  cat(sprintf("No screen : dcost = %8.0f  dQALY = %.3f\n",
              res_noscreen$dcost, res_noscreen$dqaly))
  cat(sprintf("Molecular : dcost = %8.0f  dQALY = %.3f\n",
              res_mol$dcost, res_mol$dqaly))
  cat(sprintf("ICER (mol vs no-screen) = %.0f per QALY\n", icer))
}
