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

years_till_sick1 <- function(inputs)
{
  state <- get_attribute(env, "State") 
  if(state == 0) # 0 => Healthy
  {
    rexp(1,inputs$r.HS1)
  } else
  {
    inputs$horizon+1 # Past end of simulation time
  }
}

sick1 <- function(traj, inputs)
{
  traj                      |> 
  set_attribute("State", 1) |> # 1 => Sick 1 (S1)
  release('healthy')        |> # Track state change for tally later
  seize('sick1')            |>
  branch( 
    function() get_attribute(env, "Treat")+1,
    continue = rep(TRUE, 2),
    trajectory(),  # No Treatment
    trajectory() |> seize('treat')
  )
}