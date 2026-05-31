###############################################################################
# event_screen.R — one-time population screen at t.screen years
#
# Fires once per patient (reactive = FALSE).  Checks the patient's state at
# the moment the screen offer arrives and applies the test's coverage,
# sensitivity, and specificity:
#
#   State = 0 (H):  false positive with prob  cov * (1 - spec)
#   State = 1 (S1): true positive  with prob  cov * sens
#   State = 2 (S2): already symptomatic — no screening benefit
#
# On a POSITIVE result (TP or FP):
#   - One-time screening cost and confirmation cost stored as patient
#     attributes ("ScreenCost", "ConfirmCost"); des_run() reads these via
#     get_mon_attributes() and adds them to each patient's total cost.
#
# On a TRUE POSITIVE (TP, still in S1):
#   - "sick1" counter released; "treated_s1" counter seized.
#   - Attribute TreatA = 1 set so years_till_sick2 applies hr.TrtS1S2.
#
# Models 9–11 override this screen() function to insert a confirmation wait
# before treatment is applied.
###############################################################################

if (!exists("CRN")) source("crn.R")   # CRN draw helpers (inert unless armed)

years_till_screen <- function(inputs)
{
  # After firing, "Screened" is set to 1 and this returns horizon+1 so
  # the main_loop's post-event reschedule pushes the event past the horizon.
  # The per-patient screen time "tScreen" is drawn once at initialisation
  # (staggered over the rollout window), so the queue never sees a burst.
  screened <- get_attribute(env, "Screened")
  if (!is.na(screened) && screened == 1) return(inputs$horizon + 1)
  max(0, get_attribute(env, "tScreen") - now(env))
}

# Cost of one screening test for the current strategy.
screen_unit_cost <- function(inputs)
  if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field

screen <- function(traj, inputs)
{
  traj |>
  set_attribute("Screened", 1) |>    # guard: prevent re-firing at same time
  branch(
    function() {
      state <- get_attribute(env, "State")

      strat <- inputs$strategy
      cov  <- if (strat == 'mol') inputs$cov.mol   else inputs$cov.field
      sens <- if (strat == 'mol') inputs$sens.mol  else inputs$sens.field
      spec <- if (strat == 'mol') inputs$spec.mol  else inputs$spec.field

      # COVERAGE is now separate from RESULT: u_cov decides whether the program
      # reaches (and tests) this person; u_res is the test outcome. Drawing both
      # first keeps the per-patient "screen" stream aligned across arms (CRN).
      u_cov <- draw_unif("screen")   # reached by the program?
      u_res <- draw_unif("screen")   # test result

      if (strat == 'noscreen') return(1L)   # no program
      if (state >= 2)          return(1L)   # symptomatic — outside screening
      if (u_cov >= cov)        return(1L)   # not reached — no test, no cost

      # Reached & asymptomatic -> a test is performed (pays c.screen below):
      if (state == 1L && u_res < sens)       return(2L)  # true positive
      if (state == 0L && u_res < (1 - spec)) return(3L)  # false positive
      return(4L)                                          # screened negative
    },
    continue = rep(TRUE, 4),

    ## branch 1: not screened (no program / symptomatic / not reached) — no cost
    trajectory(),

    ## branch 2: true positive — test + confirmatory workup, then treat
    trajectory() |>
      set_attribute("ScreenCost",  function() screen_unit_cost(inputs)) |>
      set_attribute("ConfirmCost", function() inputs$c.confirm) |>
      set_attribute("TreatA", 1) |>
      release("sick1")           |>
      seize("treated_s1"),

    ## branch 3: false positive — test + confirmatory workup, no treatment
    trajectory() |>
      set_attribute("ScreenCost",  function() screen_unit_cost(inputs)) |>
      set_attribute("ConfirmCost", function() inputs$c.confirm),

    ## branch 4: screened negative — test cost only (the cost previously omitted)
    trajectory() |>
      set_attribute("ScreenCost",  function() screen_unit_cost(inputs))
  )
}
