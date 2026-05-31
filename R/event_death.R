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

# Given attributes of a patient (trajectory), it returns in years 
# how long till the patient would die a secular death.
#
# This is a terrible mortality model, but is the reference
# from sicker as a simple exponential draw
years_till_death <- function(inputs)
{
  rexp(1, inputs$r.HD)
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
death <- function(traj, inputs)
{
  traj |> branch(
    function() 1,
    continue=c(FALSE), # False is patient death, had to use a branch to force termination
    trajectory("Death") |>
      mark("death")     |> # Must be in 'counters'
      terminate_simulation()
  )
}