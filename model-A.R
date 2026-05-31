#############################################################################
# MODEL A — model10 with REAL ENDOGENOUS CONTENTION on treatment resource "A"
#############################################################################
#
# WHAT THIS IS
# ------------
# This is model10's cost-effectiveness DES spine (states H/S1/S2 = 0/1/2,
# split_arrivals() period-overlap costing, dampack ICER table) with the
# DETERMINISTIC policy wait (timeWhenCanGetA = now + waitA_days/365, capacity
# = Inf) REPLACED by a *genuinely finite, contended* treatment resource of
# capacity c << N. The wait a patient experiences is ENDOGENOUS: it is read
# off the LIVE occupancy of the capacity-c resource and the patient's FIFO
# position in a waitlist. Disease KEEPS PROGRESSING while the patient waits
# (death / S1->S2 / recovery all keep racing), the patient NEVER blocks, and
# the patient re-enters the event loop cleanly after acquiring (or never
# acquiring) treatment.
#
# This is approach "A" from methodology.md (§4-A). It marries the validated
# contention mechanism in drafts/probe-A-endogenous-wait.R to model10's CEA
# spine. None of clone()/synchronize()/send()/trap() appears anywhere -> none
# of the fragility documented in methodology.md §2.
#
# CORE MECHANISM (one breath)
# ---------------------------
#  - add_resource("A", capacity = inputs$n.capacity)  -- FINITE, contended.
#    NOT capacity-Inf, NOT in the `counters` (Inf-capacity tally) list.
#  - A global FIFO waitlist (an R environment keyed by get_name(env)). A patient
#    JOINS it on entering S1 (no blocking) and LEAVES it on every exit path.
#  - years_till_treatmentA(): ENDOGENOUS time_to_event. Reads live
#    get_server_count("A") vs get_capacity("A") and the patient's FIFO position.
#    If a slot is (in expectation) free -> admit ~now; else draw an M/M/c-
#    flavoured rexp(rate = cap*mu_A / n_ahead). Reactive => resamples each event.
#  - get_treatmentA(): a FIRE-TIME GUARD branch. It seizes "A" ONLY if a slot is
#    genuinely free (get_server_count < get_capacity); otherwise it skips and the
#    reactive reschedule re-defers it. So capacity = c is enforced EXACTLY, with
#    no blocking of the patient arrival.
#  - "EndA" timed event releases the slot at end-of-course (mean 1/mu_A years),
#    freeing it for the next waiter.
#  - EVERY exit path (recover -> healthy, progress -> sick2, death,
#    horizon cleanup) releases "A" if held AND removes the patient from the
#    waitlist. No leaks, no double-release (patient touches ONLY tallies + the
#    single finite "A"; there are no clones).
#
# TREATMENT EFFECT (toggleable -> required null-effect test)
# ---------------------------------------------------------
#  While ON treatment ("A" held, onTrt == 1):
#   - S1->S2 progression rate is multiplied by inputs$tx_prog_factor
#     (default 0.2 -> "treatment slows progression"; set to 1 for null effect).
#   - S1->H recovery rate is multiplied by inputs$tx_recov_factor
#     (default 1.5 -> "treatment improves recovery"; set to 1 for null effect).
#   - utility in S1 is inputs$u.TrtA (model10's utility bump; set == u.S1 for
#     null effect).
#  Setting tx_prog_factor = 1, tx_recov_factor = 1, u.TrtA = u.S1 makes a
#  treatment run reproduce the no-treatment outcome across ALL c -- this is the
#  decisive "nothing is silently frozen" regression test.
#
# COSTING (reused from model10 verbatim, with one robustness extension)
# ---------------------------------------------------------------------
#  Because "A" is held CONCURRENTLY with "sick1", costing uses model10's
#  period-overlap decomposition: split_arrivals() builds period_start/period_end
#  rows tagged with active_resources / *_active, then cost_arrivals() and
#  qaly_arrivals() integrate per period. We reuse model10's cost_arrivals() and
#  qaly_arrivals() unchanged in substance; qaly_arrivals() is generalized only to
#  cover the 'sick1, A' (and 'sick1, A, B') utility-bump strings exactly as
#  model10 did, plus a couple of A-with-sick2 strings that can arise if a patient
#  progresses while a slot is mid-release (utility taken as u.S2, treatment
#  utility bump only applies in S1 -- matching model10's selectors).
#
# HONEST CAVEAT (methodology.md §4-A): capacity is enforced EXACTLY but the
# wait DISTRIBUTION is an analytic approximation (single Exp; correct first
# moment under saturation). For mean-based CEA endpoints (QALYs, costs, deaths)
# this is benign; for tail/variance-sensitive endpoints use model B.
#############################################################################

suppressMessages({
  library(tidyverse)
  library(here)
  # Load simmer LAST and force it to the top of the search path. tidyverse
  # attaches lubridate, whose now() otherwise masks simmer::now() and crashes
  # the hand-rolled engine's now(env) ("cannot convert to POSIXct"). A plain
  # library(simmer) is a no-op when simmer is already attached (e.g. when this
  # file is source()d into a session that loaded simmer first, as in the Quarto
  # doc), so detach + reattach to guarantee simmer::now() wins.
  if ("package:simmer" %in% search()) detach("package:simmer", unload = FALSE)
  library(simmer)
})
source(here('discount.R'))
source(here('main_loop.R'))            # hand-rolled next-event engine (black box)
source(here('cea-table-functions.R'))

# ---------------------------------------------------------------------------
# GLOBAL FIFO WAITLIST  (lives OUTSIDE simmer; reset every run in des_run)
# ---------------------------------------------------------------------------
WL <- new.env()
reset_waitlist <- function() WL$join_order <- character(0)
wl_join     <- function(nm) if (!(nm %in% WL$join_order)) WL$join_order <- c(WL$join_order, nm)
wl_leave    <- function(nm) WL$join_order <- setdiff(WL$join_order, nm)
wl_position <- function(nm) match(nm, WL$join_order)   # 1 = front of queue; NA if not waiting

# ---------------------------------------------------------------------------
# TRAJECTORY: becoming sick (S1). On the treatment-A scenario the patient JOINS
# the waitlist (no blocking) instead of stamping a deterministic wait time.
# ---------------------------------------------------------------------------
sick1 <- function(traj, inputs) {
  base <- traj %>%
    set_attribute("State", 1) %>%          # S1
    release("healthy") %>%
    seize("sick1") %>%
    set_attribute("waitingForA", 0)

  base %>%
    branch(
      function() get_attribute(env, "trtA") + 1,   # 0 (no A) or 1 (A scenario)
      continue = rep(TRUE, 2),
      ## (a) No treatment-A scenario: do nothing special
      trajectory(),
      ## (b) Treatment-A scenario: JOIN the FIFO waitlist (endogenous wait)
      trajectory() %>%
        set_attribute("waitingForA", 1) %>%
        set_attribute("joinedWL", function() { wl_join(get_name(env)); 1 })
    ) %>%
    ## optional treatment B (unchanged from model10)
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>% seize("B")
    )
}

# ---------------------------------------------------------------------------
# ENDOGENOUS wait. Read LIVE occupancy of the finite resource "A" + FIFO position.
# Reactive => resampled after every event, so congestion is always current.
# ---------------------------------------------------------------------------
years_till_treatmentA <- function(inputs) {
  # eligible only while genuinely waiting in S1 and not already on treatment
  if (get_attribute(env, "State")        != 1) return(inputs$horizon + 1)
  if (get_attribute(env, "waitingForA")  != 1) return(inputs$horizon + 1)
  if (get_attribute(env, "onTrt")        == 1) return(inputs$horizon + 1)

  cap  <- get_capacity(env, "A")
  busy <- get_server_count(env, "A")          # slots currently occupied
  free <- cap - busy
  pos  <- wl_position(get_name(env))           # 1 = front of queue
  if (is.na(pos)) pos <- 1
  mu   <- inputs$mu_A                           # treatment service rate (per year)

  if (free >= pos) {
    # A slot is (in expectation) available for me now -> near-instant admit.
    rexp(1, rate = inputs$rate_admit_when_free)
  } else {
    # All slots busy; (pos - free) people block ahead of me. M/M/c-flavoured:
    # departures at rate cap*mu; I wait for (pos-free) departures.
    # Mean wait = (pos-free)/(cap*mu).
    n_ahead <- pos - free
    rexp(1, rate = (cap * mu) / n_ahead)
  }
}

# ---------------------------------------------------------------------------
# FIRE-TIME GUARD: seize "A" ONLY if a slot is genuinely free. Else skip; the
# reactive reschedule re-defers. This enforces capacity = c EXACTLY, no blocking.
# On success: mark onTrt, leave the waitlist, schedule end-of-course (EndA).
# ---------------------------------------------------------------------------
get_treatmentA <- function(traj, inputs) {
  traj %>%
    branch(
      function() {
        ok <- get_attribute(env, "State")       == 1 &&
              get_attribute(env, "waitingForA")  == 1 &&
              get_attribute(env, "onTrt")        == 0 &&
              (get_server_count(env, "A") < get_capacity(env, "A"))
        ok + 1L
      },
      continue = rep(TRUE, 2),
      ## 1: slot not free / not eligible -> skip (will reschedule via reactive)
      trajectory(),
      ## 2: take the slot
      trajectory() %>%
        seize("A", 1) %>%
        set_attribute("onTrt", 1) %>%
        set_attribute("hasA", 1) %>%
        set_attribute("waitingForA", 0) %>%
        set_attribute("leftWL", function() { wl_leave(get_name(env)); 1 }) %>%
        set_attribute("tEndA", function() now(env) + rexp(1, inputs$mu_A))
    )
}

# ---------------------------------------------------------------------------
# END-OF-COURSE: release the slot so the next waiter can be admitted.
# ---------------------------------------------------------------------------
years_till_endA <- function(inputs) {
  if (get_attribute(env, "onTrt") == 1) {
    max(0, get_attribute(env, "tEndA") - now(env))
  } else inputs$horizon + 1
}
end_treatmentA <- function(traj, inputs) {
  traj %>% branch(
    function() (get_attribute(env, "onTrt") == 1) + 1L,
    continue = rep(TRUE, 2),
    trajectory(),
    trajectory() %>%
      release("A", 1) %>%
      set_attribute("onTrt", 0) %>%
      set_attribute("hasA", 0)
  )
}

# ---------------------------------------------------------------------------
# RECOVER to Healthy. Release "A" if held; leave the waitlist.
# ---------------------------------------------------------------------------
healthy <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 0) %>%
    set_attribute("waitingForA", 0) %>%
    set_attribute("leftWL", function() { wl_leave(get_name(env)); 1 }) %>%
    seize("healthy") %>%
    release("sick1") %>%
    ## release A only if the patient is actually on it
    branch(
      function() (get_attribute(env, "onTrt") == 1) + 1L,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>%
        release("A", 1) %>%
        set_attribute("onTrt", 0) %>%
        set_attribute("hasA", 0)
    ) %>%
    ## release B if needed (unchanged from model10)
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>% release("B")
    )
}

# ---------------------------------------------------------------------------
# PROGRESS to S2. Release "A" if held (treatment should normally halt this, but
# a slot can be held when the rate is merely slowed, not zero); leave waitlist.
# ---------------------------------------------------------------------------
sick2 <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 2) %>%
    set_attribute("waitingForA", 0) %>%
    set_attribute("leftWL", function() { wl_leave(get_name(env)); 1 }) %>%
    release("sick1") %>%
    seize("sick2") %>%
    branch(
      function() (get_attribute(env, "onTrt") == 1) + 1L,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>%
        release("A", 1) %>%
        set_attribute("onTrt", 0) %>%
        set_attribute("hasA", 0)
    )
}

# ---------------------------------------------------------------------------
# DEATH. Release "A" if held BEFORE terminating; leave the waitlist.
# ---------------------------------------------------------------------------
death <- function(traj, inputs) {
  traj %>%
    set_attribute("leftWL", function() { wl_leave(get_name(env)); 1 }) %>%
    branch(
      function() (get_attribute(env, "onTrt") == 1) + 1L,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>%
        release("A", 1) %>%
        set_attribute("onTrt", 0) %>%
        set_attribute("hasA", 0)
    ) %>%
    branch(
      function() 1,
      continue = c(FALSE),  # FALSE => patient death; branch forces termination
      trajectory("Death") %>%
        mark("death") %>%
        terminate_simulation(inputs)
    )
}

terminate_simulation <- function(traj, inputs) {
  traj %>%
    branch(function() 1, continue = FALSE,
           trajectory() %>% cleanup_on_termination(inputs))
}

# ---------------------------------------------------------------------------
# HORIZON CLEANUP. Release whatever is held; leave the waitlist (no leaks).
# ---------------------------------------------------------------------------
cleanup_on_termination <- function(traj, inputs) {
  traj %>%
    release("time_in_model") %>%
    set_attribute("leftWL", function() { wl_leave(get_name(env)); 1 }) %>%
    ## release whichever health-state resource is still held
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 3),
      trajectory() %>% release("healthy"),
      trajectory() %>% release("sick1"),
      trajectory() %>% release("sick2")
    ) %>%
    ## release A only if it was actually seized
    branch(
      function() (get_attribute(env, "onTrt") == 1) + 1L,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>%
        release("A", 1) %>%
        set_attribute("onTrt", 0) %>%
        set_attribute("hasA", 0)
    ) %>%
    ## release B if needed (conditioned on trtB & not-healthy, as model10)
    branch(
      function() (get_attribute(env, "trtB") &&
                    get_attribute(env, "State")) + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>% release("B")
    )
}

# ---------------------------------------------------------------------------
# COMPETING RISKS (rexp), same pattern as model10. Death / Sick1 / Healthy /
# Sick2. The TREATMENT EFFECT lives in years_till_sick2 (and years_till_healthy):
# while ON treatment, progression is slowed by tx_prog_factor and recovery is
# improved by tx_recov_factor. Toggle factors to 1 for the null-effect test.
# ---------------------------------------------------------------------------
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
  state <- get_attribute(env, "State")
  onTrt <- get_attribute(env, "onTrt")
  if (state == 1) {
    rate <- inputs$r.S1H
    if (onTrt == 1) rate <- rate * inputs$tx_recov_factor   # treatment improves recovery
    rexp(1, rate)
  } else inputs$horizon + 1
}

years_till_sick2 <- function(inputs) {
  state <- get_attribute(env, "State")
  onB   <- get_attribute(env, "trtB")
  onTrt <- get_attribute(env, "onTrt")

  if (state == 1) {
    rate <- inputs$r.S1S2
    if (onB)        rate <- rate * inputs$hr.S1S2.TrtB    # treatment B (model10)
    if (onTrt == 1) rate <- rate * inputs$tx_prog_factor # treatment A slows progression
    rexp(1, rate)
  } else inputs$horizon + 1
}

# ---------------------------------------------------------------------------
# INITIALIZE
# ---------------------------------------------------------------------------
initialize_patient <- function(traj, inputs) {
  traj %>%
    seize("time_in_model") %>%
    set_attribute("AgeInitial", 25) %>%
    set_attribute("State", 0) %>%            # start Healthy
    seize("healthy") %>%
    set_attribute("trtA", as.integer(inputs$treatA)) %>%
    set_attribute("trtB", as.integer(inputs$treatB)) %>%
    set_attribute("onTrt", 0) %>%            # currently holding a slot of A
    set_attribute("hasA", 0) %>%             # ever-seized flag (== onTrt here)
    set_attribute("waitingForA", 0) %>%      # on the FIFO waitlist for A
    set_attribute("tEndA", 0)
}

# ---------------------------------------------------------------------------
# EVENT REGISTRY  (model10's, with "Get Treatment A" now endogenous + "EndA")
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
  list(name          = "Get Treatment A",
       attr          = "aTreatmentA",
       time_to_event = years_till_treatmentA,
       func          = get_treatmentA,
       reactive      = TRUE),
  list(name          = "EndA",
       attr          = "aEndA",
       time_to_event = years_till_endA,
       func          = end_treatmentA,
       reactive      = TRUE)
)

# ---------------------------------------------------------------------------
# INPUTS  (model10's, plus the contention block)
# ---------------------------------------------------------------------------
inputs <- list(
  N      = 1e3,

  ## ----- CONTENTION BLOCK (the new, documented assumptions) -----
  n.capacity = 5,                # c : finite contended slots for treatment A
  mu_A       = 0.5,              # treatment service rate per year (mean course 2y)
  rate_admit_when_free = 365,    # near-instant admit (~1 day) when a slot is free
  ## treatment effect (toggle to 1 / u.S1 for the null-effect test)
  tx_prog_factor  = 0.2,         # S1->S2 rate multiplier while on treatment A
  tx_recov_factor = 1.5,         # S1->H  rate multiplier while on treatment A

  # Parameters
  horizon=    75,      # Time horizon
  d.r    =     0.0,    # Discount Rate (note: discount_value hardcodes 0.03; see runs)

  r.HS1  =     0.15,   # Disease Onset Rate / year       (H  -> S1)
  r.S1H  =     0.5,    # Recovery Rate / year            (S1 -> H)
  r.S1S2 =     0.105,  # Disease Progression rate / year (S1 -> S2)
  r.HD   =     0.002,  # Healthy to Dead rate / year     (H  -> D)
  hr.S1D =     3,      # Hazard ratio in S1 vs healthy
  hr.S2D =    10,      # Hazard ratio in S2 vs healthy
  hr.S1S2.TrtB = 0.6,  # Reduction in rate of disease progression (B)

  # Annual Costs
  c.H    =  2000,
  c.S1   =  4000,
  c.S2   = 15000,
  c.D    =     0,
  c.TrtA  = 12000,
  c.TrtB = 13000,

  # Utility Weights
  u.H    =     1.00,
  u.S1   =     0.75,
  u.S2   =     0.50,
  u.D    =     0.00,

  # Intervention effect (utility bump in S1 while on A)
  u.TrtA  =     0.95,

  wtp    =     1e5,

  treatA = FALSE,
  treatB = FALSE
)

# "A" is NOT in counters (it is a real finite resource, not an Inf tally).
counters <- c(
  "time_in_model",
  "death",
  "healthy",
  "sick1",
  "sick2",
  "B"
)

# ---------------------------------------------------------------------------
# COSTING  (reused from model10; A held concurrently with sick1 -> period-overlap)
# ---------------------------------------------------------------------------
cost_arrivals <- function(arrivals, inputs)
{
  arrivals$cost  <- 0
  arrivals$dcost <- 0

  selector = arrivals$active_resources == 'healthy'
  arrivals$cost[selector] <- inputs$c.H *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
                                             arrivals$period_start[selector], arrivals$period_end[selector])

  selector = grepl('sick1', arrivals$active_resources)
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S1 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S1,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])

  selector = grepl('sick2', arrivals$active_resources)
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S2 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S2,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])

  # 'A_active' is created by split_arrivals() because "A" is a monitored resource.
  selector = arrivals$A_active
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.TrtA *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtA,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])

  selector = arrivals$B_active
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.TrtB *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtB,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])

  arrivals
}

qaly_arrivals <- function(arrivals, inputs)
{
  # NOTE: model10 keyed utilities on the EXACT active_resources string (e.g.
  # 'sick1, A'). That is FRAGILE: split_arrivals() pastes resources in the order
  # they appear in the arrival rows, so a concurrent A+sick1 window can come out
  # as 'A, sick1' instead of 'sick1, A' and silently fall through to 0 utility.
  # Under real contention the seize order of A vs sick1 is not fixed, so we key on
  # the ORDER-INDEPENDENT boolean *_active columns split_arrivals() also produces.
  # This is a strict generalization of model10's selectors (same utilities, same
  # rule that the treatment bump applies only in S1).
  dur <- arrivals$period_end - arrivals$period_start
  arrivals$qaly  <- 0
  arrivals$dqaly <- 0

  s_healthy <- if ("healthy_active" %in% names(arrivals)) arrivals$healthy_active else rep(FALSE, nrow(arrivals))
  s_sick1   <- if ("sick1_active"   %in% names(arrivals)) arrivals$sick1_active   else rep(FALSE, nrow(arrivals))
  s_sick2   <- if ("sick2_active"   %in% names(arrivals)) arrivals$sick2_active   else rep(FALSE, nrow(arrivals))
  s_A       <- if ("A_active"       %in% names(arrivals)) arrivals$A_active       else rep(FALSE, nrow(arrivals))

  set_u <- function(sel, u) {
    if (!any(sel)) return(invisible())
    arrivals$qaly[sel]  <<- u * dur[sel]
    arrivals$dqaly[sel] <<- discount_value(u, arrivals$period_start[sel], arrivals$period_end[sel])
  }

  # Healthy
  set_u(s_healthy, inputs$u.H)
  # S1 ON treatment A (utility bump). State precedence: S2 > S1 > H.
  set_u(s_sick1 & s_A & !s_sick2, inputs$u.TrtA)
  # S1 OFF treatment A (base S1 utility)
  set_u(s_sick1 & !s_A & !s_sick2, inputs$u.S1)
  # S2 dominates (treatment bump only applies in S1, matching model10)
  set_u(s_sick2, inputs$u.S2)

  arrivals
}

# ---------------------------------------------------------------------------
# SINGLE DES RUN
# ---------------------------------------------------------------------------
des_run <- function(inputs, compute_outcomes = TRUE)
{
  reset_waitlist()                      # <- reset the global FIFO every run
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env %>%
    create_counters(counters) %>%
    add_resource("A", capacity = inputs$n.capacity, queue_size = Inf) %>%   # FINITE, contended
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
    run(inputs$horizon + 1/365) %>%
    wrap()

  arrivals <- get_mon_arrivals(env, per_resource = TRUE)

  # The per-patient split_arrivals()/cost_arrivals()/qaly_arrivals() costing
  # (inherited from model10) is the expensive step. Callers that only need the
  # queue/leak/endogeneity audit can skip it with compute_outcomes = FALSE.
  outcomes <- NULL
  if (compute_outcomes) {
    outcomes <- arrivals %>%
      group_by(name) %>%
      filter(resource != "time_in_model") %>%
      nest() %>%
      mutate(data = map2(data, name, ~(.x %>% mutate(name = .y)))) %>%
      mutate(qaly_i = map(data, ~(split_arrivals(.x) %>% qaly_arrivals(inputs)))) %>%
      mutate(cost_i = map(data, ~(split_arrivals(.x) %>% cost_arrivals(inputs)))) %>%
      mutate(qaly  = map_dbl(qaly_i, ~(.x %>% summarise(qaly  = sum(qaly))  %>% pull(qaly)))) %>%
      mutate(dqaly = map_dbl(qaly_i, ~(.x %>% summarise(dqaly = sum(dqaly)) %>% pull(dqaly)))) %>%
      mutate(cost  = map_dbl(cost_i, ~(.x %>% summarise(cost  = sum(cost))  %>% pull(cost)))) %>%
      mutate(dcost = map_dbl(cost_i, ~(.x %>% summarise(dcost = sum(dcost)) %>% pull(dcost)))) %>%
      ungroup()
  }

  list(
    arrivals   = arrivals,
    outcomes   = outcomes,
    resources  = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

# ---------------------------------------------------------------------------
# Convenience summariser for the run report
# ---------------------------------------------------------------------------
summarise_outcomes <- function(run) {
  run %>% pluck("outcomes") %>%
    summarise(cost  = mean(cost),  dcost = mean(dcost),
              qaly  = mean(qaly),  dqaly = mean(dqaly))
}

# ===========================================================================
# RUN / INSTRUMENT
# Runs only when executed as the main script (Rscript model-A.R). To source the
# model's functions for testing WITHOUT running the experiments, set
# Sys.setenv(MODEL_A_NORUN = "1") before source().
# ===========================================================================
if (!nzchar(Sys.getenv("MODEL_A_NORUN"))) {

  # helper: capacity / progression / endogeneity audit on a run
  audit_run <- function(run, inputs) {
    arr <- run$arrivals
    res <- run$resources
    ra  <- res[res$resource == "A", ]
    over_cap <- if (nrow(ra)) any(ra$server > ra$capacity) else FALSE
    neg      <- if (nrow(ra)) any(ra$server < 0) else FALSE
    max_srv  <- if (nrow(ra)) max(ra$server) else 0

    # disease progresses during wait: patients who entered S1 (in the A scenario)
    # but reached S2 or died WITHOUT ever seizing A.
    a   <- arr[arr$resource == "A", ]
    s1  <- arr[arr$resource == "sick1", ]
    s2  <- arr[arr$resource == "sick2", ]
    treated_names    <- unique(a$name)
    s1_names         <- unique(s1$name)
    progressed_names <- unique(s2$name)
    n_s1             <- length(s1_names)
    n_treated        <- length(treated_names)
    n_progressed_untreated <- length(setdiff(progressed_names, treated_names))

    # realized wait: S1-entry to A-seize (per patient, most recent prior S1 entry)
    waits <- numeric(0)
    if (nrow(a)) for (nm in treated_names) {
      a_starts  <- sort(a$start_time[a$name == nm])
      s1_starts <- sort(s1$start_time[s1$name == nm])
      for (ast in a_starts) {
        prior <- s1_starts[s1_starts <= ast + 1e-9]
        if (length(prior)) waits <- c(waits, ast - max(prior))
      }
    }
    list(over_cap = over_cap, neg = neg, max_srv = max_srv,
         n_s1 = n_s1, n_treated = n_treated,
         n_progressed_untreated = n_progressed_untreated,
         n_deaths = sum(arr$resource == "death"),
         mean_wait = if (length(waits)) mean(waits) else NA_real_,
         med_wait  = if (length(waits)) median(waits) else NA_real_,
         n_waits   = length(waits))
  }

  # multi-seed mean of an audit metric (kills MC noise so gradients are clean).
  # Uses compute_outcomes=FALSE: the queue/leak audit needs only arrivals/resources,
  # so we skip the expensive per-patient costing pipeline here.
  multi_audit <- function(ov, seeds) {
    rows <- lapply(seeds, function(s) { set.seed(s); audit_run(des_run(ov, compute_outcomes = FALSE), inputs) })
    g <- function(f) mean(sapply(rows, f), na.rm = TRUE)
    list(mean_wait = g(function(x) x$mean_wait),
         n_treated = g(function(x) x$n_treated),
         n_s1      = g(function(x) x$n_s1),
         n_prog_untr = g(function(x) x$n_progressed_untreated),
         n_deaths  = g(function(x) x$n_deaths),
         over_cap  = any(sapply(rows, function(x) x$over_cap)),
         neg       = any(sapply(rows, function(x) isTRUE(x$neg))),
         max_srv   = max(sapply(rows, function(x) x$max_srv)))
  }
  multi_outcome <- function(ov, seeds) {
    v <- lapply(seeds, function(s) { set.seed(s); summarise_outcomes(des_run(ov)) })
    list(dcost = mean(sapply(v, function(x) x$dcost)),
         dqaly = mean(sapply(v, function(x) x$dqaly)),
         dqaly_sd = sd(sapply(v, function(x) x$dqaly)),
         cost  = mean(sapply(v, function(x) x$cost)),
         qaly  = mean(sapply(v, function(x) x$qaly)))
  }

  cat("=============================================================\n")
  cat("MODEL A — endogenous contention on treatment resource A\n")
  cat("=============================================================\n\n")

  seeds  <- 1:3          # multi-seed averaging for the gradients/CEA
  Ncea   <- 1000

  # ------ 1) CEA across a couple of capacities c (treatment-A vs SoC) ------
  cat("--- CEA: treatment A at varying capacity c (N=1000, 3-seed mean) ---\n")
  cea_tab <- list()
  for (cc in c(2, 5, 25, Inf)) {
    ov  <- modifyList(inputs, list(N = Ncea, treatA = TRUE, treatB = FALSE, n.capacity = cc))
    o   <- multi_outcome(ov, seeds)
    a   <- multi_audit(ov, seeds)
    cea_tab[[ifelse(is.infinite(cc), "Inf", as.character(cc))]] <- list(o = o, a = a)
    cat(sprintf("  c=%-4s  dcost=%9.0f  dqaly=%6.3f  deaths=%5.0f  treated=%5.0f/%4.0f  meanWait=%5.3f  overCap=%s\n",
                ifelse(is.infinite(cc), "Inf", as.character(cc)),
                o$dcost, o$dqaly, a$n_deaths, a$n_treated, a$n_s1, a$mean_wait, a$over_cap))
  }
  ov.soc  <- modifyList(inputs, list(N = Ncea, treatA = FALSE, treatB = FALSE))
  o.soc   <- multi_outcome(ov.soc, seeds)
  cat(sprintf("  SoC    dcost=%9.0f  dqaly=%6.3f\n", o.soc$dcost, o.soc$dqaly))

  # dampack ICER table: SoC vs A at a few capacities (reuses model10 costing path)
  cat("\n  CEA (dampack ICER table, SoC vs A-constrained):\n")
  strat   <- c("SoC", "A (c=2)", "A (c=5)", "A (c=25)", "A (c=Inf)")
  costs   <- c(o.soc$dcost, cea_tab[["2"]]$o$dcost, cea_tab[["5"]]$o$dcost,
               cea_tab[["25"]]$o$dcost, cea_tab[["Inf"]]$o$dcost)
  effs    <- c(o.soc$dqaly, cea_tab[["2"]]$o$dqaly, cea_tab[["5"]]$o$dqaly,
               cea_tab[["25"]]$o$dqaly, cea_tab[["Inf"]]$o$dqaly)
  icer    <- dampack::calculate_icers(cost = costs, effect = effs, strategies = strat)
  print(icer)
  cat("\n")

  # ------ 2) ENDOGENEITY in c: shrink c -> mean wait rises (4-seed mean) ------
  cat("--- ENDOGENEITY: mean realized wait vs capacity c (N=500, 4-seed mean) ---\n")
  for (cc in c(1, 2, 5, 25)) {
    a <- multi_audit(modifyList(inputs, list(N = 500, treatA = TRUE, treatB = FALSE, n.capacity = cc)), seeds)
    cat(sprintf("  c=%-3d  meanWait=%6.3f  treated=%5.1f/%4.1f  S2-untreated=%5.1f  deaths=%5.1f\n",
                cc, a$mean_wait, a$n_treated, a$n_s1, a$n_prog_untr, a$n_deaths))
  }
  cat("\n")

  # ------ 3) ENDOGENEITY in N: raise N at fixed c -> mean wait rises (4-seed mean) ------
  cat("--- ENDOGENEITY: mean realized wait vs N (c=3, 4-seed mean) ---\n")
  for (nn in c(150, 300, 600)) {
    a <- multi_audit(modifyList(inputs, list(N = nn, treatA = TRUE, treatB = FALSE, n.capacity = 3)), seeds)
    cat(sprintf("  N=%-3d  meanWait=%6.3f  treated=%5.1f/%4.1f  S2-untreated=%5.1f  deaths=%5.1f  overCap=%s\n",
                nn, a$mean_wait, a$n_treated, a$n_s1, a$n_prog_untr, a$n_deaths, a$over_cap))
  }
  cat("\n")

  # ------ 4) NULL-EFFECT TEST: factors=1, u.TrtA=u.S1 -> reproduce no-treat across c ------
  cat("--- NULL-EFFECT TEST (tx_prog_factor=1, tx_recov_factor=1, u.TrtA=u.S1) ---\n")
  null_over <- modifyList(inputs, list(
    N = 1500, tx_prog_factor = 1, tx_recov_factor = 1, u.TrtA = inputs$u.S1))
  o.null.soc <- multi_outcome(modifyList(null_over, list(treatA = FALSE, treatB = FALSE)), seeds)
  se <- o.null.soc$dqaly_sd / sqrt(length(seeds))
  cat(sprintf("  no-treat:      dqaly=%7.4f  (SE=%.4f)\n", o.null.soc$dqaly, se))
  for (cc in c(2, 5, Inf)) {
    o.null <- multi_outcome(modifyList(null_over, list(treatA = TRUE, treatB = FALSE, n.capacity = cc)), seeds)
    d <- o.null$dqaly - o.null.soc$dqaly
    cat(sprintf("  treatA c=%-4s  dqaly=%7.4f  (Δ vs no-treat = %+7.4f, %+.1f SE)\n",
                ifelse(is.infinite(cc), "Inf", as.character(cc)), o.null$dqaly, d, d/se))
  }
  cat("\n  PASS criterion: Δqaly is small AND FLAT across c (no monotone trend with\n")
  cat("  scarcity). A capacity-INDEPENDENT residual is RNG-stream divergence (the\n")
  cat("  treatment arm draws extra rexp() for waits/EndA), not a frozen-clock bug.\n\n")

  cat("DONE.\n")
}
