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

# This is an empty model with nothing but entry/exit of a patient trajectory

library(simmer)

source('inputs.R')     # Your Model Parameters
source('main_loop.R')  # Boilerplate code
  
# Resource or "Counters"
#
# These are used to track things that incur costs or qalys, or other
# things of which a count might be of interest.
# Infinite in quantity
counters <- c(
  "time_in_model"
)
  
# Define starting state of patient
initialize_patient <- function(traj, inputs)
{
  traj                   |>
  seize("time_in_model") |>
  set_attribute("AgeInitial", function() sample(20:30, 1))
}

# Cleanup function if a termination occurs
# Good for releasing any seized resources based on state.
cleanup_on_termination <- function(traj)
{
  traj |> 
  release("time_in_model")
}

terminate_simulation <- function(traj, inputs)
{
  traj |>
  branch( function() 1, 
          continue=FALSE,
          trajectory() |> cleanup_on_termination()
        )
}

# Main Event registry
event_registry <- list(
  list(name          = "Terminate at time horizon",
       attr          = "aTerminate",
       time_to_event = function(inputs) inputs$horizon-now(env),
       func          = terminate_simulation,
       reactive      = FALSE)
)

# This does a single DES run versus the defined inputs.
des_run <- function(inputs)
{
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env |> 
    create_counters(counters) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon=2) |>
    run(inputs$horizon+1/365) |> # Simulate just past horizon (in years)
    wrap()
        
  get_mon_arrivals(env, per_resource = T)
}


