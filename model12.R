#############################################################################
# DISEASE MODEL WITH CLONING - LONG UNCERTAIN TREATMENT WAITS
#############################################################################
#
# MODEL DESCRIPTION:
# This model simulates the same disease progression but with severely limited
# treatment capacity, creating long and uncertain wait times for treatment.
# Patients' disease continues to evolve while they wait for care.
#
# KEY ASSUMPTIONS:
# - Treatment capacity is severely limited (e.g., 1 slot for 50 patients)
# - Treatment wait times are long (months to years) and uncertain
# - Disease progression continues normally while waiting for treatment
# - Patients may give up waiting, die, or become ineligible while in queue
#
# WHY CLONING IS ESSENTIAL:
# 1. PARALLEL COMPETING PROCESSES: Two simultaneous processes must be modeled
#    Clone 1: Treatment-seeking pathway (waits in queue for uncertain duration)
#    Clone 2: Natural disease progression (continues independently of queue status)
#
# 2. UNCERTAIN QUEUE DYNAMICS: Wait time is not deterministic
#    - Patient may wait months/years before getting treatment
#    - During this time, disease progression can't be "paused"
#    - Need to model interaction between wait time and disease evolution
#
# 3. COMPLEX SYNCHRONIZATION: Multiple exit conditions from waiting state
#    - Treatment becomes available → start treatment (Clone 1 wins)
#    - Disease progresses to ineligible state → leave queue (Clone 2 wins)
#    - Patient dies while waiting → leave queue (Clone 2 wins)
#    - Patient gives up waiting → continue natural progression (Clone 2 wins)
#
# 4. REALISTIC HEALTHCARE ACCESS: Models real-world resource constraints
#    - Queue position doesn't guarantee eventual treatment
#    - Disease severity may change eligibility while waiting
#    - Patient preferences and patience affect treatment seeking
#
# TECHNICAL APPROACH:
# - Clone 1: seize("treatment_center") with trap() to listen for Clone 2 events
# - Clone 2: continues event-driven progression, sends signals to Clone 1
# - synchronize() determines which pathway continues based on what happened
# - Extensive use of trap()/send() for inter-clone communication
#
# USE CASES:
# - Resource-constrained healthcare systems
# - Expensive/specialized treatments with limited capacity
# - Modeling healthcare access disparities and wait time impacts
# - Policy analysis of treatment capacity vs. population health outcomes
# - Studying patient behavior under uncertainty (giving up, seeking alternatives)
#
# KEY INSIGHTS THIS MODEL REVEALS:
# - Impact of wait times on clinical outcomes
# - Treatment access rates vs. clinical need
# - Disease progression while waiting for care
# - Resource utilization efficiency under constraints
# - Equity implications of limited treatment capacity
#
#############################################################################

library(simmer)

# Parameters - modeling a scenario with very limited treatment capacity
inputs <- list(
  N = 100,                     # Number of patients
  horizon = 100,               # Time horizon (years)
  d.r = 0.03,                # Discount Rate
  
  # Disease progression rates (per year)
  r.HS = 0.2,                # Healthy to Sick rate (higher for demonstration)
  r.SH = 1e-6,               # Sick to Healthy (natural recovery) rate  
  r.SS2 = 0.3,               # Sick to Sicker rate (higher - disease progresses while waiting)
  r.HD = 0.005,              # Healthy to Death rate
  r.SD = 0.03,               # Sick to Death rate (higher than healthy)
  r.S2D = 0.15,              # Sicker to Death rate (much higher)
  
  # Treatment parameters - VERY LIMITED CAPACITY
  treatment_duration = 3,     # Treatment duration (years) - long treatment
  treatment_capacity = 1,     # Only 1 treatment slot for 50 patients!
  treatment_success_rate = 0.95, # High success rate when you can get it
  treatment_protection_factor = 0.1, # Treatment reduces progression by 90%
  
  # Queue eligibility - patients may become too sick for treatment
  max_wait_time = 5,          # Maximum time willing to wait (years)
  sicker_eligible_for_treatment = FALSE # Sicker patients can't get treatment
)

######################
# EVENT DEFINITIONS 
#####################

# Event: Healthy to Sick progression
years_till_sick <- function(inputs) {
  state <- get_attribute(env, "State")
  if(state == 0) { # 0 = Healthy
    rexp(1, inputs$r.HS)
  } else {
    inputs$horizon + 1 # Past simulation time
  }
}

source(here::here("become-sick-cloned.R"))

# Modified progression events that interact with clones
years_till_healthy <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  seeking_treatment <- get_attribute(env, "seeking_treatment")
  
  # Can only recover naturally if sick, not on treatment, and either:
  # 1. Not seeking treatment, OR
  # 2. Gave up waiting for treatment
  if(state == 1 && on_treatment == 0) {
    rexp(1, inputs$r.SH)
  } else {
    inputs$horizon + 1
  }
}

recover_naturally <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 0) %>%
    release('sick') %>%
    seize('healthy') %>%
    log_("Recovered naturally to healthy state") %>%
    send(function() paste0("patient", get_attribute(env, "patient_id"), "_recovered"))
}

years_till_sicker <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  
  if(state == 1) { # Patient is sick
    if(on_treatment == 1) {
      # Reduced progression rate during treatment
      reduced_rate <- inputs$r.SS2 * inputs$treatment_protection_factor
      rexp(1, reduced_rate)
    } else {
      # Normal progression rate
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
    log_("Progressed to sicker state") %>%
    
    # Send patient-specific signals based on treatment status
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      
      trajectory() %>%
        log_("Progressed to sicker during treatment") %>%
        set_attribute("on_treatment", 0) %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed_during_tx")),
      
      trajectory() %>%
        log_("Progressed to sicker while waiting or not seeking treatment") %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed"))
    )
}

years_till_death <- function(inputs) {
  state <- get_attribute(env, "State")
  
  rate <- switch(state + 1,
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
        set_attribute("State", 3) %>%
        log_("Patient died") %>%
        
        # Send appropriate death signals
        branch(
          function() get_attribute(env, "on_treatment"),
          continue = rep(TRUE, 2),
          
          trajectory() %>%
            send(function() paste0("patient", get_attribute(env, "patient_id"), "_died_during_tx")),
          
          trajectory() %>%
            send(function() paste0("patient", get_attribute(env, "patient_id"), "_died"))
        ) %>%
        
        mark("death") %>%
        terminate_simulation(inputs)
    )
}
#############################################################################
# INFRASTRUCTURE FUNCTIONS (same as before)
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
  
  args <- lapply(event_registry, FUN=function(e) {
    trajectory(e$name) %>%
      e$func(inputs) %>%
      set_attribute(e$attr, function() {now(env) + e$time_to_event(inputs)})
  })
  
  args$".trj" <- traj
  args$option <- function() next_event()$id
  args$continue <- rep(TRUE, length(event_registry))
  traj <- do.call(branch, args)
  
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
    set_attribute("State", 0) %>%
    set_attribute("on_treatment", 0) %>%
    set_attribute("seeking_treatment", 0) %>%
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
      trajectory()
    ) %>%
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      trajectory() %>% release("treatment_center"),
      trajectory()
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
# EVENT REGISTRY
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
# SIMULATION SETUP AND EXECUTION
#############################################################################

counters <- c(
  "time_in_model",
  "death", 
  "healthy",
  "sick",
  "sicker"
)

des_run <- function(inputs) {
  env <<- simmer("DiseaseModelWithCloning")
  traj <- des(env, inputs)
  
  env %>%
    create_counters(counters) %>%
    add_resource("treatment_center", capacity = inputs$treatment_capacity, queue_size = Inf) %>%
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
    run(inputs$horizon + 1/365) %>%
    wrap()
  
  list(
    arrivals = get_mon_arrivals(env, per_resource = TRUE),
    resources = get_mon_resources(env) %>% filter(resource == "treatment_center"),
    attributes = get_mon_attributes(env)
  )
}

#############################################################################
# RUN SIMULATION AND ANALYZE RESULTS
#############################################################################

cat("Running disease model with cloning (long uncertain treatment waits)...\n")
cat("Treatment duration:", inputs$treatment_duration, "years\n")
cat("Treatment capacity:", inputs$treatment_capacity, "slots for", inputs$N, "patients\n")
cat("Max waiting time:", inputs$max_wait_time, "years\n")
cat("Disease progression rate:", inputs$r.SS2, "per year\n\n")

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

# Analyze treatment center usage
treatment_usage <- subset(results$resources, resource == "treatment_center")
if(nrow(treatment_usage) > 0) {
  cat("\nTreatment Center Analysis:\n")
  cat("Max server count:", max(treatment_usage$server), "\n")
  cat("Max queue length:", max(treatment_usage$queue), "\n")
  cat("Final queue length:", tail(treatment_usage$queue, 1), "\n")
  cat("Average queue length:", round(mean(treatment_usage$queue), 2), "\n")
  
  # Calculate queue waiting times
  if(max(treatment_usage$queue) > 0) {
    cat("Queue was non-empty", 
        round(sum(treatment_usage$queue > 0) / nrow(treatment_usage) * 100, 1), 
        "% of simulation time\n")
  }
}

# Analyze treatment outcomes
treatment_events <- subset(results$arrivals, resource == "treatment_center")
if(nrow(treatment_events) > 0) {
  cat("\nTreatment Outcomes:\n")
  cat("Total patients who got treatment:", nrow(treatment_events), "\n")
  
  # Identify completed vs interrupted treatments
  completed_treatments <- subset(treatment_events, 
                                 abs((end_time - start_time) - inputs$treatment_duration) < 0.1)
  
  cat("Treatments completed:", nrow(completed_treatments), "\n")
  cat("Treatments interrupted:", nrow(treatment_events) - nrow(completed_treatments), "\n")
  
  if(nrow(treatment_events) > nrow(completed_treatments)) {
    interrupted <- subset(treatment_events, 
                          (end_time - start_time) < inputs$treatment_duration * 0.9)
    cat("Average time before interruption:", 
        round(mean(interrupted$end_time - interrupted$start_time), 2), "years\n")
  }
  
  # Show waiting times for those who got treatment
  cat("Average wait time for treatment:", 
      round(mean(treatment_events$start_time), 2), "years\n")
}

# Count patients who likely gave up waiting
sick_at_end <- subset(final_states, state_name == "Sick")
sicker_at_end <- subset(final_states, state_name == "Sicker")
patients_who_needed_treatment <- nrow(sick_at_end) + nrow(sicker_at_end)

cat("\nAccess to Care Analysis:\n")
cat("Patients who got treatment:", nrow(treatment_events), "out of", inputs$N, "\n")
cat("Patients still sick/sicker at end:", patients_who_needed_treatment, "\n")
cat("Treatment access rate:", 
    round(nrow(treatment_events) / inputs$N * 100, 1), "%\n")

# Show some example patient trajectories
cat("\nSample Patient Journeys:\n")
sample_patients <- head(unique(results$arrivals$name), 5)
for(patient in sample_patients) {
  cat("\n", patient, ":\n")
  patient_events <- subset(results$arrivals, name == patient)
  for(i in 1:nrow(patient_events)) {
    cat("  ", patient_events$resource[i], ": ", 
        round(patient_events$start_time[i], 2), " to ", 
        round(patient_events$end_time[i], 2), " years\n")
  }
}