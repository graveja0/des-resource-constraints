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

years_till_sick2 <- function(inputs)
{
  state <- get_attribute(env, "State") 
  if(state == 1) # 1 => Sick1
  {
    rexp(1,inputs$r.S1S2)
  } else
  {
    inputs$horizon+1 # Past end of simulation time
  }
}

sick2 <- function(traj, inputs)
{
  traj                      |> 
  set_attribute("State", 2) |> # 2 => Sick 2 (S2)
  release('sick1')          |> # Track state change for tally later
  seize('sick2')
}