###############################################################################
# inputs2.R — extends inputs.R with cancer screening program parameters
#
# Used by model-7 onward. Adds one-time screening, confirmation, and
# treatment parameters for the LMIC screening arc.
###############################################################################

source('inputs.R')

inputs <- modifyList(inputs, list(

  # Pre-clinical cancer rarely resolves spontaneously — override the base
  # r.S1H = 0.70 (appropriate for the generic Sick-Sicker spine) with a
  # much smaller value.  This makes S1 a near-irreversible state and gives
  # treatment a large enough benefit to be visible at practical N.
  r.S1H = 0.05,

  # --- one-time screen (fires at t.screen years after cohort entry) ----------
  t.screen          =  2.0,    # year the screening program reaches each patient

  # molecular test  (strategy = 'mol')
  cov.mol           =  0.12,   # population coverage
  sens.mol          =  0.95,   # sensitivity (high: detects pre-clinical S1)
  spec.mol          =  0.95,   # specificity (high: few false positives)
  c.screen.mol      =  500,    # cost per screen (cold chain, lab technician)

  # field test  (strategy = 'field')
  cov.field         =  0.50,   # coverage (primary-care deliverable)
  sens.field        =  0.70,   # sensitivity (lower: misses some pre-clinical cases)
  spec.field        =  0.80,   # specificity (lower: more false positives)
  c.screen.field    =  100,    # cost per screen (1/5 of molecular)

  # confirmatory workup — same cost regardless of which test triggered it
  c.confirm         =  150,    # cost per screen-positive (colposcopy / biopsy)

  # treatment for confirmed true positives
  hr.TrtS1S2        =  0.20,   # S1->S2 hazard ratio under treatment (80% reduction)
  c.TrtA            =   500,   # additional annual cost for treated S1 (generic drug)
  u.TrtA            =  0.90,   # utility in treated S1  (vs u.S1 = 0.80 untreated)

  # exogenous queue distribution parameters (model-9)
  # lognormal with mean ~18 weeks (0.346 yr), SD ~8 weeks (0.154 yr)
  confirm_wait_logmean = -1.152,
  confirm_wait_logsd   =  0.425,

  # finite confirmation capacity (model-10 and model-11)
  n.confirm.cap     =   15,    # concurrent confirmation slots (per simulation unit)
  mu.confirm        =    2,    # service rate: ~2 completions per slot per year
  rate_admit_free   = 1000,    # near-instant admit when a slot is genuinely free

  strategy = 'mol'             # 'noscreen', 'mol', or 'field'
))
