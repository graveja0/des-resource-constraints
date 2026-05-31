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

source('discount.R')
source('inputs2.R')     # Your Model Parameters
source('main_loop.R')  # Boilerplate code
  
# Events (State Transitions)
source('event_death3.R')
source('event_healthy-v2.R')
source('event_sick1-v2.R')
source('event_sick2.R')
  
# Resource or "Counters"
#
# These are used to track things that incur costs or qalys, or other
# things of which a count might be of interest.
# Infinite in quantity
counters <- c(
  "time_in_model",
  "death",
  "healthy",
  "sick1",
  "sick2"
)


sick1 <- function(traj, inputs)
{
  traj                                      |> 
  set_attribute("State", 1)                 |> # 1 => Sick 1 (S1)
  release('healthy')                        |> # Track state change for tally later
  seize('sick1')                            |>
  set_attribute("sS1", function() now(env)) |> 
  branch(
    function() (inputs$strategy == "treat")+1,
    continue = rep(TRUE, 2),
    trajectory(),  # No Treatment Strategy
    trajectory() |>
    clone(
      n = 2,
      
      # This clone "escapes" and continues health state progression
      trajectory() |>
      trap('sieze_treat', 
            trajectory() |> 
            set_attribute("Treat", 1) |>
            untrap('sieze_treat') 
      ),
  
      # This clone waits patiently for treatment
      trajectory() |>
      seize('treat') |>
      trap('release_treat', 
           trajectory() |> 
           release('treat') |>
           untrap('release_treat') |>
           branch(function() 1, continue=FALSE, trajectory())
      ) |>
      send('sieze_treat') |>
      log_(function() {paste("Waited:", now(env) - get_attribute(env,"sS1"))}) |>
      timeout(inputs$horizon) # Make sure last arrival
      
    ) |>
    synchronize(wait=FALSE)
  )
}

# A helper function to deal with releasing treatment via
# a signal if needed to release
release_treat <- function(.traj) 
{
  .traj |> 
    branch(
      function() get_attribute(env, "Treat") +1,
      continue = rep(TRUE, 2),
      trajectory(),  # No Treatment
      trajectory() |> 
        send('release_treat') |>
        set_attribute("Treat", 0)
    ) 
}

healthy <- function(traj, inputs)
{
  traj                      |> 
    set_attribute("State", 0) |> # 0 => Healthy (H)
    seize('healthy')          |>
    release('sick1')          |>
    release_treat()
}

death <- function(traj, inputs)
{
  traj |>
    release_treat() |>
    branch(
      function() 1,
      continue=c(FALSE), # False is patient death, had to use a branch to force termination
      trajectory("Death") |>
        mark("death")     |> # Must be in 'counters'
        terminate_simulation(inputs)
    )
}
  
# Define starting state of patient
initialize_patient <- function(traj, inputs)
{
  traj                   |>
  seize("time_in_model") |>
  set_attribute("AgeInitial", function() sample(20:30, 1)) |>
  set_attribute("State", 0) |> # Patients start healthy
  set_attribute("Treat", 0)   |> # No treatment to start
  seize("healthy")       
}

# Cleanup function if a termination occurs
# Good for releasing any seized resources based on state.
cleanup_on_termination <- function(traj, inputs)
{
  traj |>
  release("time_in_model") |>
  branch( 
    function() get_attribute(env, "State")+1,
      continue = rep(TRUE, 3),
      trajectory() |> release("healthy"),
      trajectory() |> release("sick1"),
      trajectory() |> release("sick2") 
  ) |>
  branch(
    function() get_attribute(env, "Treat") +1,
    continue = rep(TRUE, 2),
    trajectory(),  # No Treatment
    trajectory() |> send('release_treat')
  )
}

terminate_simulation <- function(traj, inputs)
{
  traj |>
  branch( function() 1, 
          continue=FALSE,
          trajectory() |> cleanup_on_termination(inputs)
        )
}

# Main Event registry
event_registry <- list(
  list(name          = "Terminate at time horizon",
       attr          = "aTerminate",
       time_to_event = function(inputs) inputs$horizon-now(env),
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
       reactive      = TRUE)
)

cost_arrivals <- function(arrivals, inputs)
{
  arrivals$cost  <- 0  # No costs yet
  arrivals$dcost <- 0  # No discounted costs either
  
  selector = arrivals$resource == 'healthy'
  arrivals$cost[selector] <- inputs$c.H *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
    arrivals$start_time[selector], arrivals$end_time[selector])
  
  selector = arrivals$resource == 'sick1'
  arrivals$cost[selector] <- inputs$c.S1 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S1,
    arrivals$start_time[selector], arrivals$end_time[selector])
  
  selector = arrivals$resource == 'sick2'
  arrivals$cost[selector] <- inputs$c.S2 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S2,
    arrivals$start_time[selector], arrivals$end_time[selector])
 
  selector = arrivals$resource == 'treat'
  arrivals$cost[selector] <- inputs$c.Trt *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.Trt,
    arrivals$start_time[selector], arrivals$end_time[selector])
 
  arrivals
}

qaly_arrivals <- function(arrivals, inputs)
{
  arrivals$qaly  <- 0  # No qaly yet
  arrivals$dqaly <- 0  # No discounted qaly either
  
  selector <- arrivals$resource == 'healthy'
  arrivals$qaly[selector] <-
    inputs$u.H*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(inputs$u.H, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])
  
  selector <- arrivals$resource == 'sick1'
  uS1 <- if(inputs$strategy == 'treat') inputs$u.Trt else inputs$u.S1
  arrivals$qaly[selector] <-
    uS1*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(uS1, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])
  
  selector <- arrivals$resource == 'sick2'
  arrivals$qaly[selector] <-
    inputs$u.S2*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(inputs$u.S2, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])
  
  arrivals
}

# This does a single DES run versus the defined inputs.
des_run <- function(inputs)
{
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env |> 
    create_counters(counters) |>
    add_resource("treat", inputs$n.capacity) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon=2) |>
    run(inputs$horizon+1/365) |> # Simulate just past horizon (in years)
    wrap()
        
  arrivals <- get_mon_arrivals(env, per_resource = T) |>
    cost_arrivals(inputs) |> 
    qaly_arrivals(inputs) 
  
  return(list(
    arrivals = arrivals,
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  ))
}


