library(simmer)

#############################################################################
# DISEASE MODEL WITHOUT CLONING - IMMEDIATE TREATMENT ACCESS
#############################################################################
#
# MODEL DESCRIPTION:
# This model simulates a disease with three states (Healthy → Sick → Sicker → Dead)
# where treatment is readily available with minimal wait times.
#
# KEY ASSUMPTIONS:
# - Treatment capacity is adequate relative to demand
# - Patients get treatment immediately or after very short waits
# - Queue waiting time is negligible compared to disease progression timescales
# - Treatment decisions are immediate (no complex decision processes)
#
# WHY NO CLONING IS NEEDED:
# 1. SEQUENTIAL COMPETING RISKS: Events compete but only one "wins" at each timepoint
#    - Become sick → immediately seek treatment → get treatment quickly
#    - While on treatment: death vs. treatment completion compete
#    - Simple event-driven logic with renege_if() handles interruptions
#
# 2. IMMEDIATE RESOURCE ACCESS: Treatment queue is effectively instantaneous
#    - seize("treatment_center") → brief/no wait → timeout(treatment_duration)
#    - No need to model parallel "waiting" and "disease progression" processes
#
# 3. CLEAN STATE TRANSITIONS: Patient states are mutually exclusive
#    - Either healthy, sick, sicker, or dead at any given time
#    - Either on treatment or not on treatment
#    - Built-in simmer queue + renege_if() handles everything cleanly
#
# TECHNICAL APPROACH:
# - Event registry drives state transitions
# - renege_if() handles treatment interruptions (death, progression)
# - Simple seize/timeout/release for treatment process
# - Competing risks resolved through event timing (earliest event wins)
#
# USE CASES:
# - Well-resourced healthcare systems with adequate capacity
# - Rapid treatment protocols (emergency care, acute interventions)
# - Modeling treatment effectiveness rather than access issues
# - Focus on clinical outcomes rather than healthcare delivery constraints
#
#############################################################################

# Parameters
inputs <- list(
  N = 20,                     # Number of patients
  horizon = 5,                # Time horizon (years)
  d.r = 0.03,                # Discount Rate
  
  # Disease progression rates (per year)
  r.HS = 0.15,               # Healthy to Sick rate
  r.SH = 0.1,                # Sick to Healthy (natural recovery) rate  
  r.SS2 = 0.20,              # Sick to Sicker rate
  r.HD = 0.005,              # Healthy to Death rate
  r.SD = 0.02,               # Sick to Death rate (higher than healthy)
  r.S2D = 0.10,              # Sicker to Death rate (much higher)
  
  # Treatment parameters
  treatment_duration = 2,     # Treatment duration (years)
  treatment_capacity = 1,     # Number of treatment slots
  treatment_success_rate = 0.95, # Probability treatment returns patient to healthy
  treatment_protection_factor = 0.3 # Reduces progression risk by 70% during treatment
)

#############################################################################
# SIMPLIFIED APPROACH: Use simmer's built-in queue
#############################################################################

# Event: Healthy to Sick progression
years_till_sick <- function(inputs) {
  state <- get_attribute(env, "State")
  if(state == 0) { # 0 = Healthy
    rexp(1, inputs$r.HS)
  } else {
    inputs$horizon + 1 # Past simulation time
  }
}

become_sick <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 1) %>% # 1 = Sick
    release('healthy') %>%
    seize('sick') %>%
    log_("Became sick - will seek treatment immediately") %>%
    
    # Immediately try to get treatment using simmer's built-in queue
    # Use renege_if to handle competing risks while waiting
    renege_if(
      "died_or_progressed",
      out = trajectory() %>%
        log_("Competing event occurred - leaving treatment queue")
    ) %>%
    
    seize("treatment_center", 1) %>%
    renege_abort() %>%
    log_("Started treatment") %>%
    
    # During treatment, patient faces reduced (but not zero) risk of progression and death
    set_attribute("on_treatment", 1) %>%
    
    # Use renege_if during treatment to handle death or disease progression
    renege_if(
      "died_or_progressed",
      out = trajectory() %>%
        log_("Competing event during treatment - treatment interrupted") %>%
        set_attribute("on_treatment", 0) %>%
        release("treatment_center")
    ) %>%
    
    timeout(inputs$treatment_duration) %>%
    renege_abort() %>%
    
    # Treatment completion (only if no competing events occurred)
    log_("Treatment completed successfully") %>%
    branch(
      function() if(runif(1) < inputs$treatment_success_rate) 1 else 2,
      continue = c(TRUE, TRUE),
      
      # Successful treatment - return to healthy
      trajectory() %>%
        log_("Treatment successful - returning to healthy") %>%
        set_attribute("State", 0) %>%
        release("sick") %>%
        seize("healthy"),
      
      # Treatment failed - remain sick
      trajectory() %>%
        log_("Treatment failed - remaining sick")
    ) %>%
    
    set_attribute("on_treatment", 0) %>%
    release("treatment_center")
}

# Event: Sick to Healthy (natural recovery) - blocked during treatment
years_till_healthy <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  
  # Natural recovery is blocked during treatment (treatment handles the cure)
  if(state == 1 && on_treatment == 0) { # Sick and not on treatment
    rexp(1, inputs$r.SH)
  } else {
    inputs$horizon + 1
  }
}

recover_naturally <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 0) %>% # 0 = Healthy
    release('sick') %>%
    seize('healthy') %>%
    log_("Recovered naturally to healthy state") %>%
    send("died_or_progressed") # Signal to interrupt treatment seeking
}

# Event: Sick to Sicker progression - reduced rate during treatment  
years_till_sicker <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  
  if(state == 1) { # Patient is sick
    if(on_treatment == 1) {
      # Treatment reduces progression risk but doesn't eliminate it
      reduced_rate <- inputs$r.SS2 * inputs$treatment_protection_factor
      rexp(1, reduced_rate)
    } else {
      # Normal progression rate when not on treatment
      rexp(1, inputs$r.SS2)
    }
  } else {
    inputs$horizon + 1
  }
}

progress_to_sicker <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 2) %>% # 2 = Sicker
    release('sick') %>%
    seize('sicker') %>%
    
    # Send signal first to trigger renege_if for anyone on treatment
    send("died_or_progressed") %>%
    
    # Small delay to let renege_if process
    timeout(0.001) %>%
    
    # If this patient was on treatment, the renege_if should have handled cleanup
    # Just log the progression
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      
      # This shouldn't happen if renege_if worked correctly
      trajectory() %>%
        log_("Progressed to sicker - was on treatment (cleanup should have happened)") %>%
        set_attribute("on_treatment", 0),
      
      # Normal case - wasn't on treatment
      trajectory() %>%
        log_("Progressed to sicker state")
    )
}

# Event: Death (state-dependent) - ALWAYS possible, even during treatment
years_till_death <- function(inputs) {
  state <- get_attribute(env, "State")
  
  # Death can occur regardless of treatment status
  rate <- switch(state + 1, # +1 because states are 0,1,2
                 inputs$r.HD,   # Healthy
                 inputs$r.SD,   # Sick  
                 inputs$r.S2D)  # Sicker
  
  rexp(1, rate)
}

death <- function(traj, inputs) {
  traj %>%
    branch(
      function() 1,
      continue = FALSE,
      trajectory("Death") %>%
        set_attribute("State", 3) %>% # 3 = Dead
        log_("Patient died") %>%
        
        # Send signal first - this will trigger renege_if for patients on treatment
        send("died_or_progressed") %>%
        
        # Small delay to let renege_if process, then check if we need to clean up
        timeout(0.001) %>%
        
        # Only release if somehow still holding treatment resource
        # (shouldn't happen with renege_if, but safety check)
        branch(
          function() get_attribute(env, "on_treatment") == 1,
          continue = rep(TRUE, 2),
          trajectory() %>%
            log_("Emergency cleanup - releasing treatment center") %>%
            set_attribute("on_treatment", 0) %>%
            release("treatment_center"),
          trajectory()
        ) %>%
        
        mark("death") %>%
        terminate_simulation(inputs)
    )
}

#############################################################################
# INFRASTRUCTURE FUNCTIONS (unchanged)
#############################################################################

create_counters <- function(env, counters) {
  sapply(counters, FUN=function(counter) {
    env <- add_resource(env, counter, Inf, 0)
  })
  env
}

mark <- function(traj, counter) {
  traj %>%
    seize(counter, 1) %>%
    timeout(0) %>%
    release(counter, 1)
}

assign_events <- function(traj, inputs) {
  sapply(event_registry, FUN=function(event) {
    traj <- set_attribute(traj, event$attr, function() {
      event$time_to_event(inputs)
    })
  })
  traj
}

next_event <- function() {
  event_time <- Inf
  event <- NA
  id <- 0
  
  for(i in 1:length(event_registry)) {
    e <- event_registry[[i]]
    tmp_time <- get_attribute(env, e$attr)
    if(tmp_time < event_time) {
      event <- e
      event_time <- tmp_time
      id <- i
    }
  }
  
  return(list(event=event, event_time=event_time, id=id))
}

process_events <- function(traj, env, inputs) {
  traj <- timeout(traj, function() {
    ne <- next_event()
    event_time <- ne[['event_time']]
    max(0, event_time - now(env))
  })
  
  # Create handlers for each event
  args <- lapply(event_registry, FUN=function(e) {
    trajectory(e$name) %>%
      e$func(inputs) %>%
      set_attribute(e$attr, function() {now(env) + e$time_to_event(inputs)})
  })
  
  args$".trj" <- traj
  args$option <- function() next_event()$id
  args$continue <- rep(TRUE, length(event_registry))
  traj <- do.call(branch, args)
  
  # Apply reactive events
  lapply(event_registry[sapply(event_registry, function(x) x$reactive)], 
         FUN=function(e) {
           traj <- set_attribute(traj, e$attr, function() {now(env) + e$time_to_event(inputs)})
         })
  
  traj
}

des <- function(env, inputs) {
  trajectory("Patient") %>%
    initialize_patient(inputs) %>%
    assign_events(inputs) %>%
    branch(
      function() 1,
      continue = TRUE,
      trajectory("main_loop") %>% process_events(env, inputs)
    ) %>%
    rollback(1, 100)
}

initialize_patient <- function(traj, inputs) {
  traj %>%
    seize("time_in_model") %>%
    set_attribute("AgeInitial", function() sample(20:30, 1)) %>%
    set_attribute("State", 0) %>% # Start healthy
    set_attribute("on_treatment", 0) %>%
    seize("healthy")
}

cleanup_on_termination <- function(traj, inputs) {
  traj %>%
    release("time_in_model") %>%
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 4),
      trajectory() %>% release("healthy"),
      trajectory() %>% release("sick"), 
      trajectory() %>% release("sicker"),
      trajectory() # Dead - nothing to release
    ) %>%
    
    # Also release treatment center if on treatment
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      trajectory() %>% release("treatment_center"),
      trajectory() # Not on treatment
    )
}

terminate_simulation <- function(traj, inputs) {
  traj %>%
    branch(
      function() 1,
      continue = FALSE,
      trajectory() %>% cleanup_on_termination(inputs)
    )
}

#############################################################################
# EVENT REGISTRY (simplified)
#############################################################################

event_registry <- list(
  list(name = "Terminate at horizon",
       attr = "aTerminate", 
       time_to_event = function(inputs) inputs$horizon - now(env),
       func = terminate_simulation,
       reactive = FALSE),
  
  list(name = "Death",
       attr = "aDeath",
       time_to_event = years_till_death,
       func = death, 
       reactive = TRUE),
  
  list(name = "Become Sick",
       attr = "aSick",
       time_to_event = years_till_sick,
       func = become_sick,
       reactive = TRUE),
  
  list(name = "Recover to Healthy", 
       attr = "aHealthy",
       time_to_event = years_till_healthy,
       func = recover_naturally,
       reactive = TRUE),
  
  list(name = "Progress to Sicker",
       attr = "aSicker", 
       time_to_event = years_till_sicker,
       func = progress_to_sicker,
       reactive = TRUE)
)

#############################################################################
# SIMULATION SETUP
#############################################################################

counters <- c(
  "time_in_model",
  "death", 
  "healthy",
  "sick",
  "sicker"
)

des_run <- function(inputs) {
  env <<- simmer("DiseaseModel")
  traj <- des(env, inputs)
  
  env %>%
    create_counters(counters) %>%
    add_resource("treatment_center", capacity = inputs$treatment_capacity, queue_size = Inf) %>%
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
    run(inputs$horizon + 1/365) %>%
    wrap()
  
  list(
    arrivals = get_mon_arrivals(env, per_resource = TRUE),
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

#############################################################################
# RUN SIMULATION
#############################################################################

cat("Running fixed disease model simulation...\n")
cat("Treatment duration:", inputs$treatment_duration, "years\n")
cat("Treatment capacity:", inputs$treatment_capacity, "\n")
cat("Number of patients:", inputs$N, "\n\n")

results <- des_run(inputs)

cat("=== Simulation Results ===\n")

# Show final states
final_states <- results$attributes %>%
  subset(key == "State") %>%
  aggregate(value ~ name, data = ., FUN = tail, n = 1)

state_names <- c("Healthy", "Sick", "Sicker", "Dead")
final_states$state_name <- state_names[final_states$value + 1]

cat("Final patient states:\n")
print(table(final_states$state_name))

# Analyze treatment outcomes
treatment_events <- subset(results$arrivals, resource == "treatment_center")
if(nrow(treatment_events) > 0) {
  cat("\nTreatment Analysis:\n")
  cat("Total treatment episodes:", nrow(treatment_events), "\n")
  
  # Calculate treatment completion rate
  completed_treatments <- subset(treatment_events, 
                                 abs((end_time - start_time) - inputs$treatment_duration) < 0.01)
  interrupted_treatments <- subset(treatment_events, 
                                   (end_time - start_time) < inputs$treatment_duration * 0.95)
  
  cat("Completed treatments:", nrow(completed_treatments), "\n")
  cat("Interrupted treatments:", nrow(interrupted_treatments), "\n")
  cat("Treatment completion rate:", 
      round(nrow(completed_treatments) / nrow(treatment_events) * 100, 1), "%\n")
  
  if(nrow(interrupted_treatments) > 0) {
    cat("Average time before interruption:", 
        round(mean(interrupted_treatments$end_time - interrupted_treatments$start_time), 2), "years\n")
  }
}

# Check treatment center usage
treatment_usage <- subset(results$resources, resource == "treatment_center")
if(nrow(treatment_usage) > 0) {
  cat("\nTreatment center usage:\n")
  cat("Max server count:", max(treatment_usage$server), "\n")
  cat("Max queue length:", max(treatment_usage$queue), "\n")
  cat("Final server count:", tail(treatment_usage$server, 1), "\n")
  cat("Final queue length:", tail(treatment_usage$queue, 1), "\n")
  
  # Show treatment events
  treatment_events <- subset(results$arrivals, resource == "treatment_center")
  if(nrow(treatment_events) > 0) {
    cat("\nTreatment events:\n")
    cat("Number of treatments started:", nrow(treatment_events), "\n")
    cat("Average treatment time:", mean(treatment_events$end_time - treatment_events$start_time), "\n")
    
    # Show some examples
    cat("\nSample treatment events:\n")
    sample_treatments <- head(treatment_events[, c("name", "start_time", "end_time")], 5)
    print(sample_treatments)
  }
}

# Show queue dynamics over time
if(nrow(treatment_usage) > 0) {
  cat("\nQueue dynamics over time:\n")
  queue_summary <- treatment_usage %>%
    transform(
      time_rounded = round(time, 2),
      total_in_system = server + queue
    ) %>%
    subset(select = c(time_rounded, server, queue, total_in_system))
  
  print(head(queue_summary, 10))
  
  if(max(treatment_usage$queue) > 0) {
    cat("\nMax queue length was:", max(treatment_usage$queue), "\n")
    cat("Queue was non-empty for", 
        sum(treatment_usage$queue > 0) / nrow(treatment_usage) * 100, 
        "% of the time\n")
  }
}