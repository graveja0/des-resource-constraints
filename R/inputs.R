  #############################################################################
 #
#
# Copyright 2015, 2025 Shawn Garbett, Vanderbilt University Medical Center
#
# Permission to use, copy, modify, distribute, and sell this software and
# its documentation for any purpose is hereby granted without fee,
# provided that the above copyright notice appear in all copies and that
# both that copyright notice and this permission notice appear in
# supporting documentation. No representations are made about the
# suitability of this software for any purpose.  It is provided "as is"
# without express or implied warranty.
#
###############################################################################


# Default parameters for the reference Sick-Sicker model.
#
# These values are the SINGLE source of truth for every model in the document,
# so the explainer builds incrementally: a parameter introduced in an early
# model (e.g. c.S2 in model-6) carries the SAME value the screening models
# (7-11) later rely on. The natural-history and cost values below are therefore
# the cancer-screening calibration; inputs2.R only ADDS screening/treatment/
# queue parameters on top (it no longer overrides these).

inputs <- list(
    N      = 5,

    # Parameters
    horizon=    30,      # Time horizon

    # Cycle => 1 year
    d.r    =     0.03,   # Discount Rate

    r.HS1  =     0.15,   # Disease Onset Rate / year       (H  -> S1)
    r.S1H  =     0.05,   # Recovery Rate / year            (S1 -> H); pre-clinical
                         #   cancer rarely resolves spontaneously
    r.S1S2 =     0.10,   # Disease Progression rate / year (S1 -> S2)
    r.HD   =     0.005,  # Healthy to Dead rate / year     (H  -> D)
    hr.S1D =     3,      # Hazard ratio in S1 vs healthy
    hr.S2D =    60,      # Hazard ratio in S2 vs healthy; advanced disease is
                         #   deadly (~3-yr survival)

    # Annual disease-state costs. Deliberately MODEST so the cost axis of the
    # screening comparison is set by the program (the test), not by disease:
    c.H    =     0,      # Healthy: no disease-attributable cost
    c.S1   =   300,      # Pre-clinical disease: asymptomatic -> low cost
    c.S2   =  1000,      # Advanced disease: cheap palliative care (LMIC)
    c.D    =     0,      # Dead individuals

    # Utility Weights
    u.H    =     1.00,   # Healthy
    u.S1   =     0.80,   # S1
    u.S2   =     0.60,   # S2
    u.D    =     0.00,   # Dead

    wtp    =     1e5,    # 100k willingness to pay

    strategy = 'notreat' # Default strategy is no treatment
  )
