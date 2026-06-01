###############################################################################
# inputs2.R — extends inputs.R with cancer screening program parameters
#
# Used by model-7 onward. Adds one-time screening, confirmation, and
# treatment parameters for the LMIC screening arc.
#
# CALIBRATION TARGET (the manuscript's headline): molecular test = standard of
# care; field test = cheaper, lower-sensitivity alternative.
#   - WITHOUT resource contention, field is in the SE quadrant (cheaper AND more
#     effective) — its coverage expansion dominates its sensitivity loss.
#   - WITH a saturated confirmatory bottleneck, the ICER drifts into the SW
#     quadrant (still cheaper, now LESS effective): field's volume overwhelms a
#     fixed capacity, true positives progress untreated to deadly disease.
# The cost axis is PROGRAM-DRIVEN (field's cheap test at scale), so it stays
# negative across regimes; the effect axis flips sign as the queue bites.
###############################################################################

source('inputs.R')

inputs <- modifyList(inputs, list(

  N = 1000,     # default for screening models; use 5000+ for production

  # NOTE: the natural-history and disease-state-cost parameters (r.S1H, hr.S2D,
  # c.H, c.S1, c.S2, utilities, ...) are INHERITED from inputs.R unchanged, so
  # models 1-6 and 7-11 share identical values. inputs2.R only ADDS the
  # screening / treatment / queue parameters below.

  # --- one-time screen, staggered over a program rollout window --------------
  # Each patient is screened ONCE, at a time drawn ~Uniform(start, end). This
  # spreads confirmatory-queue arrivals over the rollout (clinically realistic:
  # a program reaches women over years, not all at once) and keeps the queue
  # near steady state so approach A's M/M/c wait matches approach B's exact FCFS.
  t.screen.start    =  1.0,    # program rollout begins (years)
  t.screen.end      = 11.0,    # rollout complete (10-year window)

  # molecular test  (strategy = 'mol') — the STANDARD OF CARE
  cov.mol           =  0.12,   # population coverage (low)
  sens.mol          =  0.95,   # sensitivity (high: detects pre-clinical S1)
  spec.mol          =  0.95,   # specificity (high: few false positives)
  c.screen.mol      = 1200,    # cost per screen (cold chain, cartridges, lab)

  # field test  (strategy = 'field') — the cheaper alternative
  cov.field         =  0.50,   # coverage (primary-care deliverable)
  sens.field        =  0.70,   # sensitivity (misses some pre-clinical cases)
  spec.field        =  0.70,   # specificity (heavier false-positive burden)
  c.screen.field    =  100,    # cost per screen (~1/12 of molecular, at scale)

  # confirmatory workup — same cost regardless of which test triggered it
  c.confirm         =   50,    # cost per screen-positive (colposcopy / biopsy)

  # treatment for confirmed true positives
  hr.TrtS1S2        =  0.20,   # S1->S2 hazard ratio under treatment (80% reduction)
  c.Trt.onetime     =  300,    # ONE-TIME treatment cost (early-lesion ablation,
                               #   e.g. cryotherapy), charged when treatment starts
  c.TrtA            =    0,    # no additional ANNUAL on-treatment cost
  u.TrtA            =  0.90,   # utility in treated S1  (vs u.S1 = 0.80 untreated)

  # exogenous queue distribution parameters (model-9)
  # lognormal with mean ~18 weeks (0.346 yr), SD ~8 weeks (0.154 yr)
  confirm_wait_logmean = -1.152,
  confirm_wait_logsd   =  0.425,

  # finite confirmation capacity (model-10 and model-11)
  # Parameterised PER 1,000 PATIENTS so it scales with N. des_run() converts it
  # to absolute slots: round(cap.confirm.per1000 * N / 1000). At 2/1000, mu=2:
  # throughput = 4 confirmations/yr per 1,000. Field generates ~18 positives/yr
  # per 1,000 (ρ ≈ 4.5 — the bottleneck saturates and true positives progress
  # untreated); molecular ~5/yr (served). This is what drives SE -> SW.
  cap.confirm.per1000 =  2,    # confirmation slots per 1,000 simulated patients
  mu.confirm        =    2,    # service rate: ~2 completions per slot per year
  rate_admit_free   = 1000,    # near-instant admit when a slot is genuinely free

  strategy = 'mol'             # 'noscreen', 'mol', or 'field'
))
