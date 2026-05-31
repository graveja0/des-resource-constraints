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


# Default parameters for a reference Sick Sicker Model

inputs <- list(
    N      = 20,
  
    # Parameters
    horizon=    30,      # Time horizon
    
    # Cycle => 1 year
    d.r    =     0.03,   # Discount Rate
    
    r.HS1  =     0.15,   # Disease Onset Rate / year       (H  -> S1)
    r.S1H  =     0.7,    # Recovery Rate / year            (S1 -> H)
    r.S1S2 =     0.10,   # Disease Progression rate / year (S1 -> S2)
    r.HD   =     0.005,  # Healthy to Dead rate / year     (H  -> D)
    hr.S1D =     3,      # Hazard ratio in S1 vs healthy 
    hr.S2D =    10,      # Hazard ratio in S2 vs healthy
 
    # Annual Costs
    c.H    =  2000,      # Healthy individuals 
    c.S1   =  4000,      # Sick individuals in S1
    c.S2   = 15000,      # Sick individuals in S2
    c.D    =     0,      # Dead individuals
    c.Trt  = 12000,      # Additional Annual cost for S1 and S2
    
    # Utility Weights
    u.H    =     1.00,   # Healthy
    u.S1   =     0.80,   # S1
    u.S2   =     0.60,   # S2
    u.D    =     0.00,   # Dead
    
    # Intervention Effect
    u.Trt  =     0.95,   # S1 Utility for treatment in S1
    
    wtp    =     1e5,    # 100k willingness to pay
    
    strategy = 'treat',  # Default strategy is  treatment
    
    # Resource Constraint
    n.capacity = 2       # Number of individuals who can receive treatment
  )
