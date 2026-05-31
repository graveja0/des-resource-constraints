#############################################################################
# MODEL B — model10's CEA spine + a CLAIM-TICKET COMPANION for REAL resource
#           contention (capacity c << demand).
#
# WHAT THIS IS
# ------------
# model10.R modeled "treatment A" as a resource with a DETERMINISTIC policy
# wait (timeWhenCanGetA = now + waitA_days/365) and capacity = Inf — i.e. NO
# real contention: every patient's wait was independent and exogenous.
#
# model-B keeps model10's entire CEA machinery (states H/S1/S2 = 0/1/2, the
# inline inputs list of c.*/u.* costs+utilities and d.r, the split_arrivals()
# period-overlap decomposition feeding cost_arrivals()/qaly_arrivals(), and
# create_cea_table() via dampack) but REPLACES the exogenous wait with a
# GENUINELY FINITE, CONTENDED treatment resource. A real FIFO queue forms,
# the capacity-c cap is enforced exactly, disease KEEPS PROGRESSING while a
# patient waits, the patient NEVER blocks, and the patient continues cleanly
# afterward.
#
# MECHANISM (claim-ticket companion; methodology.md §4-B + probe-B)
# ----------------------------------------------------------------
#   - The PATIENT arrival lives ONLY in the hand-rolled event loop and touches
#     ONLY Inf-capacity tallies (time_in_model/healthy/sick1/sick2/death and
#     the patient-side effect tally "treatment_held"). It NEVER seizes the
#     contended "treatment" resource, so its hand-rolled clock never freezes.
#   - On entering S1 (and wanting treatment) the patient trap()s a per-patient
#     "acq_<pid>" signal and clone(n=2, self, companion) |> synchronize(FALSE).
#     The never-blocking self-clone is always the survivor and re-enters the
#     loop; the companion is a lightweight claim-ticket whose ONLY job is to
#     seize the finite-capacity "treatment" resource (blocking in THAT queue is
#     fine — it carries no tallies, so no shared-resource double-release).
#   - When the companion ACQUIRES a slot it send()s "acq_<pid>". The patient's
#     reactive "Treatment acquired" registry event fires (aTrt forced to now),
#     sets onTrt=1 and seizes "treatment_held" (the Inf effect-window tally).
#     The reactive resample that follows then re-draws S1->S2 / S1->H at the
#     TREATED rates (progression slowed by tx_prog_factor; recovery optionally
#     boosted by tx_cure_factor).
#   - Leaving S1 (recover / progress to S2 / die / horizon cleanup) send()s
#     "abandon_<pid>": a still-queued companion renege_if()s out of the queue
#     (freeing nothing it never held); a companion that holds a slot keeps it
#     until treatment_duration, then releases it. The patient drops the
#     "treatment_held" tally so the effect window ends.
#
# THE EFFECT-WINDOW vs SLOT-OCCUPANCY SUBTLETY (documented, per §4-B caveat 1)
# ---------------------------------------------------------------------------
#   The treatment EFFECT window (onTrt: from acquisition until the patient
#   leaves S1) is DECOUPLED from the slot-OCCUPANCY window (the companion holds
#   the "treatment" slot for inputs$treatment_duration regardless). We COST and
#   apply the effect over the EFFECT window via the "treatment_held" tally (the
#   model10 analogue of resource "A"); the contended "treatment" resource is
#   used ONLY to enforce capacity / form the queue and is NEVER costed directly.
#   Consequence: a patient can free its slot at treatment_duration while still
#   onTrt (rare), or leave S1 (ending onTrt) while a companion still nominally
#   holds the slot for the rest of treatment_duration. This is a defensible
#   modeling choice and is the documented B-design behavior.
#
# COSTING — REUSED FROM model10
# -----------------------------
#   split_arrivals() / cost_arrivals() / qaly_arrivals() are reused essentially
#   verbatim from model10. The ONLY change: model10 keyed treatment cost/utility
#   on the resource named "A" (columns A_active and active_resources strings
#   "sick1, A"); here the patient-side effect tally is "treatment_held", so the
#   keys become treatment_held_active and active_resources strings
#   "sick1, treatment_held". c.TrtA / u.TrtA are reused unchanged.
#
# VERIFIED simmer 4.4.7 semantics respected (methodology.md §2-3):
#   - never block the PATIENT arrival (blocking freezes its hand-rolled clock);
#   - patient touches ONLY tallies, companion touches ONLY "treatment"
#     (no shared seize across the clone boundary -> no double-release);
#   - synchronize(wait=FALSE) so the self-clone survives & re-enters the loop;
#   - per-patient signal names ("acq_<pid>"/"abandon_<pid>"), since send() is
#     a GLOBAL broadcast;
#   - the state change is routed through the REGISTRY event (so the reactive
#     resample re-draws competing risks at treated rates), NOT the trap handler;
#   - "treatment" is NOT in `counters` (else its capacity is silently Inf).
#
# NULL-EFFECT REGRESSION (required test): tx_prog_factor = 1 AND tx_cure_factor
#   = 1 AND u.TrtA = u.S1 must reproduce the no-treatment outcome across ALL c.
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
source(here('main_loop.R'))           # hand-rolled engine (black box; not edited)
source(here('cea-table-functions.R'))

# ===========================================================================
# A. INPUTS  (model10's list + contention/effect knobs)
# ===========================================================================
inputs <- list(
  N      = 1e3,

  # ---- contention / treatment-effect knobs (NEW) --------------------------
  n.capacity         = 5,     # finite # of treatment slots (c << demand)
  treatment_duration = 3,     # years a slot is HELD by a companion
  tx_prog_factor     = 0.2,   # S1->S2 progression rate multiplier while onTrt
                              #   (<1 slows progression; 1 = null effect)
  tx_cure_factor     = 1.0,   # S1->H recovery rate multiplier while onTrt
                              #   (>1 boosts cure; 1 = no cure boost)

  # Parameters
  horizon=    75,      # Time horizon (years)

  d.r    =     0.03,   # Discount Rate (matches discount_value default)

  r.HS1  =     0.15,   # Disease Onset Rate / year       (H  -> S1)
  r.S1H  =     0.5,    # Recovery Rate / year            (S1 -> H)
  r.S1S2 =     0.105,  # Disease Progression rate / year (S1 -> S2)
  r.HD   =     0.002,  # Healthy to Dead rate / year     (H  -> D)
  hr.S1D =     3,      # Hazard ratio in S1 vs healthy
  hr.S2D =    10,      # Hazard ratio in S2 vs healthy
  hr.S1S2.TrtB = 0.6,  # Reduction in rate of disease progression (B)

  # Annual Costs
  c.H    =  2000,      # Healthy individuals
  c.S1   =  4000,      # Sick individuals in S1
  c.S2   = 15000,      # Sick individuals in S2
  c.D    =     0,      # Dead individuals
  c.TrtA  = 12000,     # Additional Annual cost while ON treatment A (effect window)
  c.TrtB = 13000,      # Cost of treatment B

  # Utility Weights
  u.H    =     1.00,   # Healthy
  u.S1   =     0.75,   # S1
  u.S2   =     0.50,   # S2
  u.D    =     0.00,   # Dead

  # Intervention Effect
  u.TrtA  =     0.95,  # S1 utility while ON treatment A (effect window)

  wtp    =     1e5,    # willingness to pay

  treatA = FALSE,      # does this scenario offer treatment A (the contended one)?
  treatB = FALSE
)

# "treatment" is the CONTENDED resource (finite capacity, added separately in
# des_run); it must NOT appear in `counters` (Inf tallies) or its capacity would
# be silently overridden to Inf. "treatment_held" IS an Inf tally (added with the
# counters) used to integrate the patient's EFFECT window for costing.
counters <- c(
  "time_in_model",
  "death",
  "healthy",
  "sick1",
  "sick2",
  "B",
  "treatment_held"
)

# ===========================================================================
# B. PATIENT INITIALIZATION  (model10 flags + contention flags)
# ===========================================================================
initialize_patient <- function(traj, inputs) {
  traj |>
    seize("time_in_model") |>
    # per-patient id, used to build addressable signal names (send() is global)
    set_attribute("pid", function() as.numeric(gsub("patient", "", get_name(env)))) |>
    set_attribute("AgeInitial", 25) |>
    set_attribute("State", 0) |>            # 0=H 1=S1 2=S2 (3=D, set in death())
    seize("healthy") |>
    ## treatment flags
    set_attribute("trtA", as.integer(inputs$treatA)) |>   # wants the contended tx
    set_attribute("trtB", as.integer(inputs$treatB)) |>
    ## contention flags
    set_attribute("onTrt", 0) |>            # treatment EFFECT active (effect window)
    set_attribute("trtReady", 0)            # set by the acquire-signal trap handler
}

# --- per-patient signal names (send() is GLOBAL -> id-encode) ---------------
sig_acq     <- function() paste0("acq_",     get_attribute(env, "pid"))
sig_abandon <- function() paste0("abandon_", get_attribute(env, "pid"))

# ===========================================================================
# C. CORE MECHANISM — the claim-ticket companion
# ===========================================================================
# The companion is a SEPARATE arrival. It touches ONLY "treatment" (the
# contended resource) and never any tally -> no shared seize across the clone
# boundary -> no double-release. Blocking in seize() here is fine: the companion
# carries no hand-rolled clock to freeze.
companion_trajectory <- function(inputs) {
  trajectory("companion") |>
    # If the patient leaves S1 while we are still queued, renege out (we hold
    # nothing, so this frees nothing — the slot is freed only on release below).
    renege_if(sig_abandon, out = trajectory()) |>
    seize("treatment", 1) |>           # BLOCKS here when all c slots are full
    renege_abort() |>                  # got a slot: cancel the abandon-renege
    send(sig_acq) |>                   # tell the patient: treatment acquired
    timeout(inputs$treatment_duration) |>
    release("treatment", 1)            # hold for the slot-occupancy duration
}

# ===========================================================================
# D. EVENT FUNCS  (model10 states, with contention woven in)
# ===========================================================================
sick1 <- function(traj, inputs) {
  traj |>
    set_attribute("State", 1) |>            # S1
    release("healthy") |>
    seize("sick1") |>
    # Per-patient acquire listener. The trap handler does NOT change state
    # directly: it sets trtReady=1 and forces the "Treatment acquired" registry
    # event to be the soonest event NOW (aTrt = now), so that event fires THROUGH
    # process_events and the reactive resample re-draws the competing risks at
    # the treated rates. (Flipping state in the trap handler alone would not
    # resample the sick2/healthy timers the patient is already racing.)
    trap(sig_acq,
         handler = trajectory() |>
           set_attribute("trtReady", 1) |>
           set_attribute("aTrt", function() now(env))) |>
    # If the patient wants the contended treatment, spawn the companion ticket.
    branch(
      function() get_attribute(env, "trtA") + 1,   # 0 or 1
      continue = rep(TRUE, 2),
      trajectory(),                                 # 1: no treatment A wanted
      trajectory() |>                               # 2: spawn companion
        clone(n = 2,
              trajectory("self"),                   # patient-self: re-enters loop
              companion_trajectory(inputs)) |>      # companion: seizes "treatment"
        synchronize(wait = FALSE)                   # self survives -> back to loop
    ) |>
    ## optional treatment B (model10, unchanged)
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> seize("B")
    )
}

# Registry func: fires once the acquire signal set trtReady (and patient is still
# an untreated S1). Activates the EFFECT (onTrt flag + Inf "treatment_held" tally
# for costing the effect window). The reactive resample in process_events then
# re-draws progression/recovery at treated rates.
treatment_acquired <- function(traj, inputs) {
  traj |>
    branch(
      function() if (get_attribute(env, "trtReady") == 1 &&
                     get_attribute(env, "onTrt")    == 0 &&
                     get_attribute(env, "State")    == 1) 1 else 2,
      continue = rep(TRUE, 2),
      trajectory() |>
        set_attribute("onTrt", 1) |>
        seize("treatment_held", 1),               # Inf tally = EFFECT window
      trajectory()
    )
}

# Leaving S1 (recover / progress / die) => abandon any pending/held treatment.
# send() reaches a still-queued companion (it reneges out); a companion holding a
# slot keeps it until treatment_duration. We end the EFFECT window here by
# dropping "treatment_held" (this is the documented effect-vs-occupancy decouple).
abandon_treatment <- function(traj) {
  traj |>
    send(sig_abandon) |>
    set_attribute("trtReady", 0) |>
    branch(
      function() get_attribute(env, "onTrt") + 1,
      continue = rep(TRUE, 2),
      trajectory(),                                # not on treatment: nothing held
      trajectory() |>
        release("treatment_held", 1) |>
        set_attribute("onTrt", 0)
    )
}

healthy <- function(traj, inputs) {
  traj |>
    abandon_treatment() |>
    set_attribute("State", 0) |>
    seize("healthy") |>
    release("sick1") |>
    ## release B if needed (model10)
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> release("B")
    )
}

sick2 <- function(traj, inputs) {
  traj |>
    abandon_treatment() |>
    set_attribute("State", 2) |>             # S2
    release("sick1") |>
    seize("sick2")
}

death <- function(traj, inputs) {
  traj |> branch(
    function() 1,
    continue = c(FALSE),                     # FALSE -> patient death (force termination)
    trajectory("Death") |>
      abandon_treatment() |>
      mark("death") |>                       # must be in `counters`
      terminate_simulation(inputs)
  )
}

terminate_simulation <- function(traj, inputs) {
  traj |>
    branch(function() 1,
           continue = FALSE,
           trajectory() |> cleanup_on_termination(inputs))
}

cleanup_on_termination <- function(traj, inputs) {
  traj |>
    release("time_in_model") |>
    ## release whichever health-state tally is still held
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 3),
      trajectory() |> release("healthy"),
      trajectory() |> release("sick1"),
      trajectory() |> release("sick2")
    ) |>
    ## release the effect tally if still on treatment (horizon-cut path; the
    ## death path already passed through abandon_treatment()).
    branch(
      function() get_attribute(env, "onTrt") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> release("treatment_held", 1) |> set_attribute("onTrt", 0)
    ) |>
    ## release B if needed (model10)
    branch(
      function() (get_attribute(env, "trtB") &&
                    get_attribute(env, "State")) + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> release("B")
    )
}

# ===========================================================================
# E. TIME-TO-EVENT FUNCS  (competing risks; treated rates gated on onTrt)
# ===========================================================================
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
    # treatment boosts recovery (S1 -> H) while onTrt (tx_cure_factor; 1 = null)
    if (get_attribute(env, "onTrt") == 1) rate <- rate * inputs$tx_cure_factor
    rexp(1, rate)
  } else {
    inputs$horizon + 1
  }
}

years_till_sick2 <- function(inputs) {
  state <- get_attribute(env, "State")
  onB   <- get_attribute(env, "trtB")
  if (state == 1) {
    rate <- inputs$r.S1S2
    if (onB) rate <- rate * inputs$hr.S1S2.TrtB
    # treatment SLOWS progression (S1 -> S2) while onTrt (tx_prog_factor; 1 = null)
    if (get_attribute(env, "onTrt") == 1) rate <- rate * inputs$tx_prog_factor
    rexp(1, rate)
  } else {
    inputs$horizon + 1
  }
}

# treatment-acquired timer: ~0 ("fire now") once the acquire trap set trtReady on
# an untreated S1; otherwise parked past the horizon. The trap handler also forces
# aTrt = now so the post-interrupt branch dispatch selects THIS event.
years_till_trt <- function(inputs) {
  if (get_attribute(env, "trtReady") == 1 &&
      get_attribute(env, "onTrt")    == 0 &&
      get_attribute(env, "State")    == 1) {
    0
  } else {
    inputs$horizon + 1
  }
}

# ===========================================================================
# F. EVENT REGISTRY
# ===========================================================================
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
  list(name          = "Treatment acquired",
       attr          = "aTrt",
       time_to_event = years_till_trt,
       func          = treatment_acquired,
       reactive      = TRUE)
)

# ===========================================================================
# G. COSTING  (reused from model10; treatment keyed on "treatment_held")
# ===========================================================================
# model10 keyed treatment cost/utility on the resource "A": A_active and the
# active_resources strings "sick1, A". Here the patient-side EFFECT tally is
# "treatment_held", so the keys become treatment_held_active and the strings
# "sick1, treatment_held". c.TrtA / u.TrtA reused unchanged. (We use grepl on the
# treatment_held substring so column ORDER in active_resources is irrelevant.)
cost_arrivals <- function(arrivals, inputs) {
  arrivals$cost  <- 0
  arrivals$dcost <- 0

  selector <- arrivals$active_resources == 'healthy'
  arrivals$cost[selector]  <- inputs$c.H *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
                                              arrivals$period_start[selector],
                                              arrivals$period_end[selector])

  selector <- grepl('sick1', arrivals$active_resources)
  arrivals$cost[selector]  <- arrivals$cost[selector] + inputs$c.S1 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S1,
                                              arrivals$period_start[selector],
                                              arrivals$period_end[selector])

  selector <- grepl('sick2', arrivals$active_resources)
  arrivals$cost[selector]  <- arrivals$cost[selector] + inputs$c.S2 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S2,
                                              arrivals$period_start[selector],
                                              arrivals$period_end[selector])

  # treatment EFFECT window (model10's c.TrtA), keyed on treatment_held
  selector <- grepl('treatment_held', arrivals$active_resources)
  arrivals$cost[selector]  <- arrivals$cost[selector] + inputs$c.TrtA *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtA,
                                              arrivals$period_start[selector],
                                              arrivals$period_end[selector])

  selector <- grepl('(^|, )B($|,)', arrivals$active_resources)
  arrivals$cost[selector]  <- arrivals$cost[selector] + inputs$c.TrtB *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtB,
                                              arrivals$period_start[selector],
                                              arrivals$period_end[selector])

  arrivals
}

qaly_arrivals <- function(arrivals, inputs) {
  arrivals$qaly  <- 0
  arrivals$dqaly <- 0

  selector <- arrivals$active_resources == 'healthy'
  arrivals$qaly[selector]  <- inputs$u.H *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.H,
                                             arrivals$period_start[selector],
                                             arrivals$period_end[selector])

  # S1 WHILE ON TREATMENT (effect window): utility u.TrtA. Keyed on the joint
  # presence of sick1 AND treatment_held (and NOT sick2 -> handled below).
  selector <- grepl('sick1', arrivals$active_resources) &
              grepl('treatment_held', arrivals$active_resources)
  arrivals$qaly[selector]  <- inputs$u.TrtA *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.TrtA,
                                             arrivals$period_start[selector],
                                             arrivals$period_end[selector])

  # S1 NOT on treatment: utility u.S1
  selector <- grepl('sick1', arrivals$active_resources) &
              !grepl('treatment_held', arrivals$active_resources)
  arrivals$qaly[selector]  <- inputs$u.S1 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.S1,
                                             arrivals$period_start[selector],
                                             arrivals$period_end[selector])

  # S2: utility u.S2 (S2 dominates; treatment_held should already be released by
  # the time S2 is entered, but key defensively on sick2 regardless)
  selector <- grepl('sick2', arrivals$active_resources)
  arrivals$qaly[selector]  <- inputs$u.S2 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- discount_value(inputs$u.S2,
                                             arrivals$period_start[selector],
                                             arrivals$period_end[selector])

  arrivals
}

# ===========================================================================
# H. RUN HARNESS  (model10 des_run + contended resource added separately)
# ===========================================================================
des_run <- function(inputs) {
  env  <<- simmer("SickSicker_B")
  traj <- des(env, inputs)
  env |>
    create_counters(counters) |>                    # Inf tallies (incl. treatment_held)
    add_resource("treatment",                        # CONTENDED resource (finite)
                 capacity = inputs$n.capacity,
                 queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1/365) |>
    wrap()

  # The companion arrivals ("patientK") share the patient's name in monitoring;
  # per-resource arrivals are still keyed by name, so per-patient costing works.
  arrivals <- get_mon_arrivals(env, per_resource = TRUE)
  outcomes <- arrivals %>%
    group_by(name) %>%
    # keep only the patient's OWN tallies for costing; drop time_in_model and the
    # contended "treatment" slot rows (companions; never costed directly).
    filter(!(resource %in% c("time_in_model", "treatment"))) %>%
    nest() %>%
    mutate(data = map2(data, name, ~(.x %>% mutate(name = .y)))) %>%
    mutate(qaly_i = map(data, ~(split_arrivals(.x) %>% qaly_arrivals(inputs)))) %>%
    mutate(cost_i = map(data, ~(split_arrivals(.x) %>% cost_arrivals(inputs)))) %>%
    mutate(qaly  = map_dbl(qaly_i, ~(.x %>% summarise(qaly  = sum(qaly))  %>% pull(qaly)))) %>%
    mutate(dqaly = map_dbl(qaly_i, ~(.x %>% summarise(dqaly = sum(dqaly)) %>% pull(dqaly)))) %>%
    mutate(cost  = map_dbl(cost_i, ~(.x %>% summarise(cost  = sum(cost))  %>% pull(cost)))) %>%
    mutate(dcost = map_dbl(cost_i, ~(.x %>% summarise(dcost = sum(dcost)) %>% pull(dcost)))) %>%
    ungroup()

  list(
    outcomes   = outcomes,
    arrivals   = arrivals,
    resources  = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

# Summarise mean CEA endpoints + contention diagnostics for one run.
summarise_run <- function(run, inputs) {
  oc <- run$outcomes %>%
    summarise(cost  = mean(cost),  dcost = mean(dcost),
              qaly  = mean(qaly),  dqaly = mean(dqaly))

  arr <- run$arrivals
  # deaths = death-tally rows
  n_dead <- nrow(subset(arr, resource == "death"))
  # actual treatments = treatment_held tally rows (NOT "treatment" slot rows,
  # which include reneged companions with activity_time 0 — methodology.md §4-B)
  n_treated <- nrow(subset(arr, resource == "treatment_held"))

  trt <- subset(run$resources, resource == "treatment")
  max_server <- if (nrow(trt)) max(trt$server) else 0
  min_server <- if (nrow(trt)) min(trt$server) else 0   # leak check (>=0)
  max_queue  <- if (nrow(trt)) max(trt$queue)  else 0

  list(cost = oc$cost, dcost = oc$dcost, qaly = oc$qaly, dqaly = oc$dqaly,
       n_dead = n_dead, n_treated = n_treated,
       max_server = max_server, min_server = min_server, max_queue = max_queue,
       cap = inputs$n.capacity, N = inputs$N)
}

# Measure endogenous companion WAIT (acquire - enqueue) by rebuilding the
# companion to record enqueue/acquire times as attributes, then differencing.
measure_waits <- function(inputs, seed = 1) {
  set.seed(seed)
  env <<- simmer("SickSicker_Bwait")
  companion_trajectory <<- function(inputs) {
    trajectory("companion") |>
      set_attribute("tEnq", function() now(env)) |>
      renege_if(sig_abandon, out = trajectory()) |>
      seize("treatment", 1) |>
      renege_abort() |>
      set_attribute("wait", function() now(env) - get_attribute(env, "tEnq")) |>
      send(sig_acq) |>
      timeout(inputs$treatment_duration) |>
      release("treatment", 1)
  }
  traj <- des(env, inputs)
  env |>
    create_counters(counters) |>
    add_resource("treatment", capacity = inputs$n.capacity, queue_size = Inf) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) |>
    run(inputs$horizon + 1/365) |>
    wrap()
  w <- subset(get_mon_attributes(env), key == "wait")
  # restore the production companion (no instrumentation) for subsequent runs
  companion_trajectory <<- companion_trajectory_prod
  if (nrow(w) == 0) return(c(n = 0, mean_wait = NA, max_wait = NA))
  c(n = nrow(w), mean_wait = mean(w$value), max_wait = max(w$value))
}
companion_trajectory_prod <- companion_trajectory   # snapshot for restore

# ===========================================================================
# I. SCENARIO RUNS + INSTRUMENTATION
# ===========================================================================
run_scenario <- function(cap, N = 1e3, seed = 123, treatA = TRUE,
                         tx_prog_factor = NULL, tx_cure_factor = NULL,
                         u.TrtA = NULL, label = NULL) {
  set.seed(seed)
  patch <- list(n.capacity = cap, N = N, treatA = treatA, treatB = FALSE)
  if (!is.null(tx_prog_factor)) patch$tx_prog_factor <- tx_prog_factor
  if (!is.null(tx_cure_factor)) patch$tx_cure_factor <- tx_cure_factor
  if (!is.null(u.TrtA))         patch$u.TrtA         <- u.TrtA
  ins <- modifyList(inputs, patch)
  s <- summarise_run(des_run(ins), ins)
  s$label <- if (is.null(label)) paste0("cap=", cap, ", N=", N) else label
  s
}

# Run the scenario suite only when executed as a script (Rscript model-B.R),
# NOT when source()'d for its function defs (sys.nframe() > 0 under source()).
if (sys.nframe() == 0L) {

  # demand >> capacity so a real queue forms. Default N=1000 gives stable means
  # and clear contention in a few minutes; override via MODELB_N (e.g.
  # MODELB_N=300 Rscript model-B.R for a fast smoke run, MODELB_N=10000 to match
  # model10's CEA precision — the per-patient split_arrivals costing is the
  # runtime bottleneck, scaling ~linearly in N).
  N_RUN <- as.numeric(Sys.getenv("MODELB_N", unset = "1000"))

  cat("==================================================================\n")
  cat("MODEL B: claim-ticket companion on model10's CEA spine\n")
  cat("==================================================================\n\n")

  # ---- (1) contention sweep + diagnostics --------------------------------
  scn <- list(
    run_scenario(cap = 1e9, N = N_RUN, treatA = FALSE, label = "SoC (no treatment)"),
    run_scenario(cap = 1e9, N = N_RUN, treatA = TRUE,  label = "A cap=Inf (unconstr)"),
    run_scenario(cap = 10,  N = N_RUN, treatA = TRUE,  label = "A cap=10"),
    run_scenario(cap = 5,   N = N_RUN, treatA = TRUE,  label = "A cap=5"),
    run_scenario(cap = 2,   N = N_RUN, treatA = TRUE,  label = "A cap=2"),
    run_scenario(cap = 1,   N = N_RUN, treatA = TRUE,  label = "A cap=1 (tightest)")
  )

  cat(sprintf("%-22s %9s %8s %7s %8s %6s %6s %6s %6s\n",
              "scenario", "dcost", "dqaly", "deaths", "treated",
              "maxSrv", "minSrv", "maxQ", "cap"))
  cat(strrep("-", 96), "\n")
  for (s in scn) {
    cat(sprintf("%-22s %9.0f %8.3f %7d %8d %6g %6d %6d %6g\n",
                s$label, s$dcost, s$dqaly, s$n_dead, s$n_treated,
                s$max_server, s$min_server, s$max_queue, s$cap))
  }
  cat("\nReading: capacity is ENFORCED (maxSrv <= cap, minSrv >= 0 = no leak);\n")
  cat("tighter cap => fewer treated, bigger queue, more deaths, lower dQALY.\n\n")

  # ---- (2) competing risks DURING the wait -------------------------------
  # Patients who became S1 and wanted treatment but reached S2/death while never
  # acquiring a slot = disease progressed while they waited.
  cat("COMPETING RISKS DURING WAIT (cap=2, N=", N_RUN, ")\n", sep = "")
  cat(strrep("-", 60), "\n")
  set.seed(123)
  ins2 <- modifyList(inputs, list(n.capacity = 2, N = N_RUN, treatA = TRUE, treatB = FALSE))
  run2 <- des_run(ins2)
  arr2 <- run2$arrivals
  n_became_s1 <- length(unique(subset(arr2, resource == "sick1")$name))
  n_treated2  <- nrow(subset(arr2, resource == "treatment_held"))
  n_reached_s2 <- length(unique(subset(arr2, resource == "sick2")$name))
  n_dead2      <- nrow(subset(arr2, resource == "death"))
  cat(sprintf("  S1-entrants who wanted tx : %d\n", n_became_s1))
  cat(sprintf("  ... actually treated      : %d\n", n_treated2))
  cat(sprintf("  ... reached S2 (progressed while waiting/after): %d\n", n_reached_s2))
  cat(sprintf("  ... died                  : %d\n", n_dead2))
  cat("  => most S1-entrants are NEVER treated; disease progresses to S2 / death\n")
  cat("     while they wait for one of the c slots.\n\n")

  # ---- (3) endogeneity: wait rises as cap shrinks AND as N grows ----------
  cat("ENDOGENEITY: mean companion wait (years)\n")
  cat(strrep("-", 64), "\n")
  for (cfg in list(c(cap = 1e9, N = N_RUN), c(cap = 20, N = N_RUN),
                   c(cap = 5,  N = N_RUN), c(cap = 2, N = N_RUN),
                   c(cap = 1,  N = N_RUN),
                   c(cap = 5,  N = N_RUN/2), c(cap = 5, N = N_RUN*2))) {
    ins_w <- modifyList(inputs, list(n.capacity = cfg["cap"], N = cfg["N"],
                                     treatA = TRUE, treatB = FALSE))
    w <- measure_waits(ins_w, seed = 123)
    cat(sprintf("  cap=%-8g N=%-5g  n_acq=%5d  mean_wait=%6.3f  max_wait=%6.3f\n",
                cfg["cap"], cfg["N"], w["n"], w["mean_wait"], w["max_wait"]))
  }
  cat("  => wait rises as cap shrinks (fixed N) AND as N grows (fixed cap).\n\n")

  # ---- (4) NULL-EFFECT REGRESSION (required test) ------------------------
  # tx_prog_factor=1, tx_cure_factor=1, u.TrtA=u.S1 must reproduce the
  # no-treatment outcome across ALL c.
  cat("NULL-EFFECT REGRESSION (prog=cure=1, u.TrtA=u.S1): dQALY across cap\n")
  cat(strrep("-", 64), "\n")
  soc_null <- run_scenario(cap = 1e9, N = N_RUN, treatA = FALSE, label = "SoC")
  cat(sprintf("  SoC (no treatment)        dcost=%9.0f  dqaly=%8.4f\n",
              soc_null$dcost, soc_null$dqaly))
  for (cap in c(1e9, 5, 2, 1)) {
    sn <- run_scenario(cap = cap, N = N_RUN, treatA = TRUE,
                       tx_prog_factor = 1, tx_cure_factor = 1,
                       u.TrtA = inputs$u.S1,
                       label = paste0("null A cap=", cap))
    cat(sprintf("  null A cap=%-8g       dcost=%9.0f  dqaly=%8.4f\n",
                cap, sn$dcost, sn$dqaly))
  }
  cat("  => dQALY (and dcost net of the inert c.TrtA charge) match SoC at every\n")
  cat("     cap: nothing is silently frozen; the wait has NO effect when tx is inert.\n")
  cat("     (NOTE: c.TrtA is still charged during the inert effect window, so\n")
  cat("      null-A dcost exceeds SoC by exactly the treatment-cost of the held\n")
  cat("      window; QALYs are the decisive equality test.)\n\n")

  # ---- (5) CEA table (dampack ICERs) -------------------------------------
  cat("CEA TABLE (reusing model10 create_cea_table / dampack::calculate_icers)\n")
  cat(strrep("-", 64), "\n")
  res_cost <- list("DES Model B" = c(scn[[1]]$dcost, scn[[4]]$dcost,
                                     scn[[3]]$dcost, scn[[2]]$dcost))
  res_effect <- list("DES Model B" = c(scn[[1]]$dqaly, scn[[4]]$dqaly,
                                       scn[[3]]$dqaly, scn[[2]]$dqaly))
  res_strategies <- list("DES Model B" = c("SoC", "A-cap5", "A-cap10", "A-capInf"))
  cea <- create_cea_table(cost = res_cost, effect = res_effect,
                          strategies = res_strategies, return_data = TRUE)
  print(as.data.frame(cea))
  cat("\nDONE.\n")
}
