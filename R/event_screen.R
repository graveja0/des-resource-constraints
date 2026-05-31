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

years_till_screen <- function(inputs)
{
  # After firing, "Screened" is set to 1 and this returns horizon+1 so
  # the main_loop's post-event reschedule pushes the event past the horizon.
  screened <- get_attribute(env, "Screened")
  if (!is.na(screened) && screened == 1) return(inputs$horizon + 1)
  max(0, inputs$t.screen - now(env))
}

screen <- function(traj, inputs)
{
  traj |>
  set_attribute("Screened", 1) |>    # guard: prevent re-firing at same time
  branch(
    function() {
      if (inputs$strategy == 'noscreen') return(1L)   # no program — skip all

      state <- get_attribute(env, "State")
      if (state >= 2) return(1L)   # S2 or dead — no screen benefit

      strat <- inputs$strategy
      cov  <- if (strat == 'mol') inputs$cov.mol   else inputs$cov.field
      sens <- if (strat == 'mol') inputs$sens.mol  else inputs$sens.field
      spec <- if (strat == 'mol') inputs$spec.mol  else inputs$spec.field

      if (state == 1L && runif(1) < cov * sens)       return(2L)  # true positive
      if (state == 0L && runif(1) < cov * (1 - spec)) return(3L)  # false positive
      return(1L)                                                    # not screened positive
    },
    continue = rep(TRUE, 3),

    ## branch 1: no positive result — nothing happens
    trajectory(),

    ## branch 2: true positive (in S1, detected and immediately confirmed)
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost", function() inputs$c.confirm) |>
      set_attribute("TreatA", 1) |>
      release("sick1")           |>
      seize("treated_s1"),

    ## branch 3: false positive (in H, workup cost but no treatment)
    trajectory() |>
      set_attribute("ScreenCost", function() {
        if (inputs$strategy == 'mol') inputs$c.screen.mol else inputs$c.screen.field
      }) |>
      set_attribute("ConfirmCost", function() inputs$c.confirm)
  )
}
