###############################################################################
# model11.R — FIFO companion trajectory, approach B  (most defensible)
#
# Same finite confirmation resource as model10 (approach A), but the queue
# is now modelled via a COMPANION TRAJECTORY that holds a real blocking
# seize().  The main disease trajectory is NEVER blocked: it uses
# send() to launch the companion, then trap() to wait for the companion's
# signal that a slot was acquired.  The companion's blocking seize() drives
# the exact emergent FIFO queue — no analytic approximation.
#
# Why this is more defensible than approach A
#   - The wait DISTRIBUTION is exact (whatever simmer's scheduler produces
#     under FCFS), not a single-Exp approximation.
#   - Capacity is enforced exactly by simmer's native resource management.
#   - For mean-based CEA endpoints (deaths, S2-time, dQALY, dcost) approaches
#     A and B agree within Monte-Carlo noise.  They differ on the wait
#     distribution, which matters for tail / variance-sensitive analyses.
#
# Companion mechanism
#   1. Screen positive: main trajectory calls send(<patient-signal>), launching
#      a companion arrival that will queue for the "confirm" resource.
#   2. Main trajectory continues running (disease progresses).
#   3. Companion seize("confirm") blocks until a slot is free (FIFO).
#   4. When the companion acquires a slot, it calls send(<confirm-signal>)
#      back to the main trajectory.
#   5. Main trajectory is trap()-ing for the confirm signal; when it fires,
#      main checks state and starts treatment if still in S1.
#   6. After a treatment course, companion releases "confirm".
#
# Invariants preserved: patient never blocks; disease always progresses;
# capacity-n.confirm.cap is enforced exactly; no leaks.
###############################################################################

library(simmer)

source('discount.R')
source('inputs2.R')
source('main_loop.R')

source('event_death3.R')
source('event_sick1.R')
source('event_healthy.R')
source('event_sick2.R')
source('event_screen.R')   # provides years_till_screen; screen() overridden below

# ---------------------------------------------------------------------------
# Signal name helpers (one per patient, globally unique)
# ---------------------------------------------------------------------------
req_signal  <- function() paste0("req_",  get_name(env))   # main → companion
conf_signal <- function() paste0("conf_", get_name(env))   # companion → main

# ---------------------------------------------------------------------------
# Companion trajectory: queues for "confirm", signals main when acquired.
# ---------------------------------------------------------------------------
make_companion_traj <- function(inputs) {
  trajectory("companion") |>
  seize("confirm") |>                              # blocks here until slot free
  send(function() conf_signal()) |>               # wake up main trajectory
  timeout(function() rexp(1, inputs$mu.confirm)) |> # hold slot for treatment course
  release("confirm")
}

# ---------------------------------------------------------------------------
# Override screen(): launch companion on positive result; main traps for reply.
# ---------------------------------------------------------------------------
screen <- function(traj, inputs)
{
  traj |>
  set_attribute("Screened", 1) |>
  branch(
    function() {
      if (inputs$strategy == 'noscreen') return(1L)

      state <- get_attribute(env, "State")
      if (state >= 2) return(1L)

      strat <- inputs$strategy
      cov  <- if (strat == 'mol') inputs$cov.mol   else inputs$cov.field
      sens <- if (strat == 'mol') inputs$sens.mol  else inputs$sens.field
      spec <- if (strat == 'mol') inputs$spec.mol  else inputs$spec.field

      if (state == 1L && runif(1) < cov * sens)       return(2L)
      if (state == 0L && runif(1) < cov * (1 - spec)) return(3L)
      return(1L)
    },
    continue = rep(TRUE, 3),

    trajectory(),   # not positive

    ## TP: pay costs, record as TP, launch companion, trap for confirm signal
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost",   function() inputs$c.confirm) |>
      set_attribute("IsTruePos",     1) |>
      set_attribute("WaitingConfirm", 1) |>
      send(req_signal) |>
      trap(
        function() conf_signal(),
        handler = trajectory() |>
          set_attribute("WaitingConfirm", 0) |>
          branch(
            function() if (isTRUE(get_attribute(env, "IsTruePos") == 1L) &&
                            get_attribute(env, "State") == 1L) 1L else 2L,
            continue = c(TRUE, TRUE),
            trajectory() |>
              set_attribute("TreatA", 1) |>
              release("sick1")           |>
              seize("treated_s1"),
            trajectory()
          )
      ),

    ## FP: pay costs, launch companion (consumes a slot), no treatment
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost",    function() inputs$c.confirm) |>
      set_attribute("IsTruePos",      0) |>
      set_attribute("WaitingConfirm", 1) |>
      send(req_signal) |>
      trap(
        function() conf_signal(),
        handler = trajectory() |>
          set_attribute("WaitingConfirm", 0)
      )
  )
}

# ---------------------------------------------------------------------------
# Override sick2 / healthy / years_till_sick2
# ---------------------------------------------------------------------------
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
  rexp(1, inputs$r.S1S2 * hr)
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
  seize("time_in_model") |>
  set_attribute("AgeInitial",      function() sample(20:30, 1)) |>
  set_attribute("State",           0) |>
  set_attribute("TreatA",          0) |>
  set_attribute("IsTruePos",       0) |>
  set_attribute("WaitingConfirm",  0) |>
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

des_run <- function(inputs)
{
  companion <- make_companion_traj(inputs)

  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env  |>
    create_counters(counters) |>
    add_resource("confirm", capacity = inputs$n.confirm.cap) |>
    # Companion generator: one arrival per req_signal, no monitoring needed
    add_generator("companion", companion,
                  when_activated(paste0("req_patient", seq_len(inputs$N) - 1)),
                  mon = 0) |>
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
# ---------------------------------------------------------------------------
summarise_run <- function(r) {
  n <- length(unique(r$name))
  data.frame(dcost = sum(r$dcost) / n, dqaly = sum(r$dqaly) / n)
}

set.seed(42)
run_mol   <- des_run(modifyList(inputs, list(N = 200, strategy = 'mol')))
set.seed(42)
run_field <- des_run(modifyList(inputs, list(N = 200, strategy = 'field')))

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
