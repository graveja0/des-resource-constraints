#############################################################################
# DISEASE MODEL WITH DIRECT WAIT TIME SIMULATION - NO CLONING NEEDED
#############################################################################
#
# MODEL DESCRIPTION:
# This model simulates disease progression with treatment delays by directly
# scheduling treatment availability at predetermined future times, rather than
# using resource contention within the simmer environment.
#
# KEY ASSUMPTIONS:
# - Treatment wait times are predetermined and deterministic (e.g., 365 days)
# - Wait times represent policy/system delays, not resource competition
# - Individual patients don't compete for treatment slots
# - Disease progression continues normally during predetermined wait periods
# - Treatment availability is scheduled as a future event, not resource-limited
#
# WHY NO CLONING IS NEEDED:
# 1. DETERMINISTIC SCHEDULING: Wait times are known in advance
#    - Patient becomes sick → calculates exact future treatment availability
#    - No uncertainty about when treatment will become available
#    - timeWhenCanGetA = now(env) + wait_days/365 (predetermined)
#
# 2. EVENT-DRIVEN WAIT MANAGEMENT: Wait is handled through event timing
#    - "Get Treatment A" event scheduled for specific future time
#    - Event system naturally handles competing risks during wait period
#    - No need for parallel processes - events compete sequentially
#
# 3. FLAG-BASED STATE TRACKING: Simple boolean logic manages waiting status
#    - waitingForA flag indicates patient is in waiting period
#    - Event fires when predetermined time arrives
#    - Competing events (death, progression) can interrupt waiting naturally
#
# 4. NO RESOURCE CONTENTION: Treatment availability is not capacity-limited
#    - Each patient gets their own predetermined wait time
#    - No queue competition between patients
#    - Wait time represents external constraints, not internal capacity limits
#
# TECHNICAL APPROACH:
# - Direct time calculation: timeWhenCanGetA = now(env) + predetermined_delay
# - Event registry includes "Get Treatment A" with time_to_event function
# - Competing events during wait period handled by standard event timing
# - Boolean flags (waitingForA, hasA) track treatment status simply
#
# ADVANTAGES OF THIS APPROACH:
# - Simple and computationally efficient
# - Easy to model policy scenarios (1-year wait vs instant access)
# - Clear separation between wait time policy and disease progression
# - Deterministic results for policy comparison
# - No complex inter-process communication needed
#
# USE CASES:
# - Policy analysis with known wait time targets (e.g., "treatment within 1 year")
# - Comparing deterministic delay scenarios (instant vs 6 months vs 1 year)
# - Cost-effectiveness analysis where wait times are policy parameters
# - System-level analysis where individual resource competition is not modeled
# - Sensitivity analysis across different predetermined wait time policies
#
# WHEN TO USE THIS VS CLONING APPROACH:
# Use this approach when:
# - Wait times are policy parameters, not resource constraints
# - Individual patient competition for resources is not the focus
# - You want deterministic, comparable scenarios
# - System-level policy analysis is the goal
#
# Use cloning approach when:
# - Resource capacity limits create uncertain wait times
# - Patient competition for limited treatment slots is key
# - Queue dynamics and patient interactions are important
# - Realistic resource utilization modeling is needed
#
# KEY INSIGHTS THIS MODEL REVEALS:
# - Impact of predetermined wait time policies on population health outcomes
# - Cost-effectiveness of different wait time targets
# - Disease progression burden during systematic delays
# - Policy trade-offs between access speed and resource allocation
# - Population-level effects of standardized wait time policies
#
#############################################################################
library(tidyverse)
library(simmer)
library(here)
library(flextable)
source(here('discount.R'))
source(here('main_loop.R'))  # Boilerplate code
source(here('cea-table-functions.R'))

sick1 <- function(traj, inputs) {
  
  wait_days <- inputs$waitA_days        # 0 for "instant A", else positive
  
  ## ------------- main clinical path -----------------------------------------
  base <- traj %>%
    set_attribute("State", 1) %>%           # S1
    release("healthy") %>%                  # left H
    seize("sick1") %>%
    set_attribute("waitingForA", 0)         # initialize waiting flag
  
  ## branch on whether the scenario allows A
  base %>%
    branch(
      function() get_attribute(env, "trtA") + 1,   # 0 or 1
      continue = rep(TRUE, 2),
      
      ## ---- (a) No treatment A scenario ---------------------------------------
      trajectory(),
      
      ## ---- (b) Treatment A scenario - schedule delayed A  ---------------
      trajectory() %>%
        # Set both attributes in the correct order, ensuring timeWhenCanGetA is set first
        set_attribute("timeWhenCanGetA", function() now(env) + wait_days/365) %>%  # when A becomes available
        set_attribute("waitingForA", 1)      # flag that we're waiting (set after time is calculated)
    ) %>%
    
    ## ── branch for optional treatment B (unchanged) ──
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>% seize("B")
    )
}


years_till_treatmentA <- function(inputs)
{
  waitingForA <- get_attribute(env, "waitingForA")
  timeWhenCanGetA <- get_attribute(env, "timeWhenCanGetA")
  
  if(waitingForA == 1 && get_attribute(env, "State") == 1 ) # waiting and still in S1
  {
    time_remaining <- timeWhenCanGetA - now(env)
    max(0, time_remaining)  # can't be negative
  } else
  {
    inputs$horizon + 1 # Past end of simulation time
  }
}

get_treatmentA <- function(traj, inputs) {
  traj %>%
    seize("A") %>%                      # seize treatment A
    set_attribute("hasA", 1) %>%        # flag that A was successfully obtained
    set_attribute("waitingForA", 0)     # no longer waiting
}

healthy <- function(traj, inputs) {
  traj |>
    set_attribute("State", 0) |>
    set_attribute("waitingForA", 0) |>   # cancel any waiting for A
    seize("healthy") |>
    release("sick1") |>
    
    ## release A only if the patient is actually on it
    branch(
      function() get_attribute(env, "hasA") + 1,   # 0 or 1
      continue = rep(TRUE, 2),
      
      trajectory(),                                # 1: nothing to do
      
      trajectory() |>                              # 2: release A
        release("A") |>
        set_attribute("hasA", 0)                   # reset flag
    ) |>
    
    ## release B if needed
    branch(
      function() get_attribute(env, "trtB") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> release("B")
    )
}

death <- function(traj, inputs)
{
  traj |> branch(
    function() 1,
    continue=c(FALSE), # False is patient death, had to use a branch to force termination
    trajectory("Death") |>
      mark("death") |>                       # Must be in 'counters'
      terminate_simulation(inputs)
  )
}

sick2 <- function(traj, inputs)
{
  traj |> 
    set_attribute("State", 2) |>             # 2 => Sick 2 (S2)
    set_attribute("waitingForA", 0) |>       # cancel any waiting for A
    release('sick1') |>                      # Track state change for tally later
    seize('sick2')
}

terminate_simulation <- function(traj, inputs)
{
  traj |>
    branch( function() 1, 
            continue=FALSE,
            trajectory() |> cleanup_on_termination(inputs)
    )
}

inputs <- list(
  N      = 1e3,
  
  waitA_days = 1e-5,
  
  # Parameters
  horizon=    75,      # Time horizon
  
  # Cycle => 1 year
  d.r    =     0.0,   # Discount Rate
  
  r.HS1  =     0.15,   # Disease Onset Rate / year       (H  -> S1)
  r.S1H  =     0.5,    # Recovery Rate / year            (S1 -> H)
  r.S1S2 =     0.105,   # Disease Progression rate / year (S1 -> S2)
  r.HD   =     0.002,  # Healthy to Dead rate / year     (H  -> D)
  hr.S1D =     3,      # Hazard ratio in S1 vs healthy 
  hr.S2D =    10,      # Hazard ratio in S2 vs healthy
  hr.S1S2.TrtB = 0.6,  # REduction in rate of disease progression
  
  # Annual Costs
  c.H    =  2000,      # Healthy individuals 
  c.S1   =  4000,      # Sick individuals in S1
  c.S2   = 15000,      # Sick individuals in S2
  c.D    =     0,      # Dead individuals
  c.TrtA  = 12000,     # Additional Annual cost for S1 and S2
  c.TrtB = 13000,      # Cost of treatment B
  
  # Utility Weights
  u.H    =     1.00,   # Healthy
  u.S1   =     0.75,   # S1
  u.S2   =     0.50,   # S2
  u.D    =     0.00,   # Dead
  
  # Intervention Effect
  u.TrtA  =     0.95,   # S1 Utility for treatment in S1
  
  wtp    =     1e5,    # 100k willingness to pay
  
  treatA = FALSE,
  treatB = FALSE
)

counters <- c(
  "time_in_model",
  "death",
  "healthy",
  "sick1",
  "sick2",
  "B",
  "A"
)

initialize_patient <- function(traj, inputs) {
  traj |>
    seize("time_in_model") |>
    set_attribute("AgeInitial", 25) |>
    set_attribute("State", 0) |>           # start Healthy
    seize("healthy") |>
    ## treatment flags
    set_attribute("trtA", as.integer(inputs$treatA)) |>
    set_attribute("trtB", as.integer(inputs$treatB)) |>
    ## NEW: flag that records a *successful* seize of A
    set_attribute("hasA", 0) |>
    ## NEW: flags for waiting for treatment A
    set_attribute("waitingForA", 0) |>
    set_attribute("timeWhenCanGetA", 0)
}

cleanup_on_termination <- function(traj, inputs) {
  traj |>
    release("time_in_model") |>
    
    ## release whichever health-state resource is still held
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 3),
      trajectory() |> release("healthy"),
      trajectory() |> release("sick1"),
      trajectory() |> release("sick2")
    ) |>
    
    ## release A *only* if it was actually seized earlier
    branch(
      function() get_attribute(env, "hasA") + 1,
      continue = rep(TRUE, 2),
      trajectory(),               # hasA == 0  → nothing to release
      trajectory() |> release("A")# hasA == 1  → free resource A
    ) |>
    
    ## original branch for B, still conditioned on trtB & not-healthy
    branch(
      function() (get_attribute(env, "trtB") &&
                    get_attribute(env, "State")) + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() |> release("B")
    )
}

years_till_death <- function(inputs)
{
  state <- get_attribute(env, "State")
  rate <- inputs$r.HD
  if(state == 1) rate <- rate * inputs$hr.S1D # Deal with Sick1 Hazard Ratio
  if(state == 2) rate <- rate * inputs$hr.S2D
  rexp(1, rate)
}

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

years_till_healthy <- function(inputs)
{
  state <- get_attribute(env, "State") 
  if(state == 1) # 1 => Sick 1
  {
    rexp(1,inputs$r.S1H)
  } else
  {
    inputs$horizon+1 # Past end of simulation time
  }
}

years_till_sick2 <- function(inputs)
{
  state <- get_attribute(env, "State") 
  onB <- get_attribute(env, "trtB")
  
  if(state == 1 && !(onB)) # 1 => Sick1
  {
    rexp(1,inputs$r.S1S2)
  } else if (state == 1 && onB) 
  {
    rexp(1,inputs$r.S1S2 * inputs$hr.S1S2.TrtB)
  } else
  {
    inputs$horizon+1 # Past end of simulation time
  }
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
       reactive      = TRUE),
  list(name          = "Get Treatment A",
       attr          = "aTreatmentA",
       time_to_event = years_till_treatmentA,
       func          = get_treatmentA,
       reactive      = TRUE)
)

cost_arrivals <- function(arrivals, inputs)
{
  
  arrivals$cost  <- 0  # No costs yet
  arrivals$dcost <- 0  # No discounted costs either
  
  selector = arrivals$active_resources == 'healthy'
  arrivals$cost[selector] <- inputs$c.H *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = grepl('sick1',arrivals$active_resources) 
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S1 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector]  + discount_value(inputs$c.S1,
                                                                         arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = grepl('sick2',arrivals$active_resources) 
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S2 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S2,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = arrivals$A_active
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.TrtA *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtA,
                                                                        arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = arrivals$B_active
  arrivals$cost[selector] <- arrivals$cost[selector]  + inputs$c.TrtB *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector]  + discount_value(inputs$c.TrtB,
                                                                         arrivals$period_start[selector], arrivals$period_end[selector])
  
  arrivals
}
qaly_arrivals <- function(arrivals, inputs)
{
  arrivals$qaly  <- 0  # No qaly yet
  arrivals$dqaly <- 0  # No discounted qaly either
  
  selector <- arrivals$active_resources == 'healthy'
  arrivals$qaly[selector] <-
    inputs$u.H*
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- 
    discount_value(inputs$u.H, 
                   arrivals$period_start[selector],
                   arrivals$period_end[selector])
  
  selector <- (arrivals$active_resources == 'sick1, A' | arrivals$active_resources == 'sick1, A, B')
  uS1 <- inputs$u.TrtA  
  arrivals$qaly[selector] <-
    uS1*
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- 
    discount_value(uS1, 
                   arrivals$period_start[selector],
                   arrivals$period_end[selector])
  
  selector <- (arrivals$active_resources == 'sick1' | arrivals$active_resources == 'sick1, B')
  uS1 <- inputs$u.S1 
  arrivals$qaly[selector] <-
    uS1*
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- 
    discount_value(uS1, 
                   arrivals$period_start[selector],
                   arrivals$period_end[selector])
  
  selector <- (arrivals$active_resources == 'sick2' | arrivals$active_resources == 'sick2, A' | arrivals$active_resources == 'sick2, B' | arrivals$active_resources == 'sick2, A, B')
  arrivals$qaly[selector] <-
    inputs$u.S2*
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dqaly[selector] <- 
    discount_value(inputs$u.S2, 
                   arrivals$period_start[selector],
                   arrivals$period_end[selector])
  
  arrivals
}
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
  
  arrivals <- get_mon_arrivals(env, per_resource = T) 
  outcomes <- arrivals %>% 
    group_by(name) %>% 
    filter(resource != "time_in_model") %>% 
    nest() %>% 
    mutate(data = map2(data,name,~(.x %>% mutate(name = .y)))) %>% 
    mutate(qaly_i = map(data,~(split_arrivals(.x) %>% qaly_arrivals(inputs)))) %>% 
    mutate(cost_i = map(data,~(split_arrivals(.x) %>% cost_arrivals(inputs)))) %>% 
    mutate(qaly = map_dbl(qaly_i,~(.x %>% summarise(qaly = sum(qaly)) %>% pull(qaly)))) %>% 
    mutate(dqaly = map_dbl(qaly_i,~(.x %>% summarise(dqaly = sum(dqaly)) %>% pull(dqaly)))) %>% 
    mutate(cost = map_dbl(cost_i,~(.x %>% summarise(cost = sum(cost)) %>% pull(cost)))) %>% 
    mutate(dcost = map_dbl(cost_i,~(.x %>% summarise(dcost = sum(dcost)) %>% pull(dcost)))) %>% 
    ungroup() 
  arrivals <- get_mon_arrivals(env, per_resource = T) |>
    cost_arrivals(inputs) |> 
    qaly_arrivals(inputs) 
  
  return(list(
    arrivals = arrivals,
    outcomes = outcomes,
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  ))
}

# debug <- des_run_debug(inputs, pp = "patient0", model = "10")


set.seed(123)
run.SoC <- des_run(modifyList(inputs, list(N = 1e4, treatA = FALSE, treatB = FALSE)))
result.SoC <- 
  run.SoC %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
            )
set.seed(123)
run.A.unconstrained <- des_run(modifyList(inputs, list(N = 1e4, treatA = TRUE, treatB = FALSE, waitA_days = 0)))
result.A.unconstrained <- 
  run.A.unconstrained %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
  )

set.seed(123)
run.B.unconstrained <- des_run(modifyList(inputs, list(N = 1e4, treatA = FALSE, treatB = TRUE, waitA_days = 0)))
result.B.unconstrained <- 
  run.B.unconstrained %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
  )

set.seed(123)
run.AB.unconstrained <- des_run(modifyList(inputs, list(N = 1e4, treatA = TRUE, treatB = TRUE, waitA_days = 0)))
result.AB.unconstrained <-
  run.AB.unconstrained %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
  )

set.seed(123)
run.A.fully.constrained <- des_run(modifyList(inputs, list(N = 1e4, treatA = TRUE, treatB = FALSE, waitA_days = 1e6)))
result.A.fully.constrained <- 
  run.A.fully.constrained %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
  )

set.seed(123)
run.A.constrained <- des_run(modifyList(inputs, list(N = 1e4, treatA = TRUE, treatB = FALSE, waitA_days = 365)))
result.A.constrained <- 
  run.A.constrained %>% 
  pluck("outcomes") %>% 
  summarise(cost = mean(cost),
            dcost = mean(dcost),
            qaly = mean(qaly),
            dqaly = mean(dqaly)
  )

res_cost <- list(
  "DTMC" = c(147036.6 , 271907.9 ,243650.1, 353744.0),
  "DES (M=10k)" = c(result.SoC$dcost, result.A.fully.constrained$dcost, result.A.unconstrained$dcost, result.A.constrained$dcost,result.B.unconstrained$dcost, result.AB.unconstrained$dcost)
  
)

res_effect <- list(
  "DTMC" = c(21.02288,  21.73058 ,22.56671, 23.42227),
  "DES (M=10k)"  = c(result.SoC$dqaly, result.A.fully.constrained$dqaly, result.A.unconstrained$dqaly, result.A.constrained$dqaly, result.B.unconstrained$dqaly, result.AB.unconstrained$dqaly)
)

res_strategies <- list(
  "DTMC" = c("SoC","A", "B", "AB"),
  "DES (M=10k)"  = c("SoC", "A-FULL","A-NONE", "A-1YR","B", "AB")

)

create_cea_table(cost = res_cost, effect = res_effect, strategies = res_strategies)

