become_sick <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 1) %>% # 1 = Sick
    release('healthy') %>%
    # set_attribute("sick_time", now(env)) %>% 
    log_("Became sick - creating clones for treatment vs natural progression") %>%
    
    # Extract patient ID for signaling
    set_attribute("patient_id", function() {
      name <- get_name(env)
      as.numeric(gsub("patient", "", name))
    }) %>%
    
    clone(
      n = 2,
      
      # Clone 1: Treatment-seeking pathway with comprehensive event listening
      trajectory("Treatment Seeking") %>%
        log_("Clone 1: Seeking treatment") %>%
        set_attribute("seeking_treatment", 1) %>%
        
        # Listen for ALL possible events that could interrupt treatment-seeking
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_died")) %>%
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed")) %>%
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_recovered")) %>%
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_failed")) %>%
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_simulation_ending")) %>%
        
        # Try to get treatment - this could take a long time due to queue
        seize("treatment_center", 1) %>%
        log_("Got treatment slot!") %>%
        
        # Signal treatment started
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_started")) %>%
        
        # During treatment, still vulnerable to death/progression
        set_attribute("on_treatment", 1) %>%
        
        # Treatment with interruption possibilities
        renege_if(function() paste0("patient", get_attribute(env, "patient_id"), "_died_during_tx"),
                  out = trajectory() %>%
                    log_("Died during treatment") %>%
                    set_attribute("on_treatment", 0) %>%
                    release("treatment_center")) %>%
        
        renege_if(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed_during_tx"), 
                  out = trajectory() %>%
                    log_("Disease progressed during treatment - treatment stopped") %>%
                    set_attribute("on_treatment", 0) %>%
                    release("treatment_center")) %>%
        
        timeout(inputs$treatment_duration) %>%
        renege_abort() %>%
        
        # Treatment completion with success/failure
        log_("Treatment completed!") %>%
        
        branch(
          function() if(runif(1) < inputs$treatment_success_rate) 1 else 2,
          continue = c(TRUE, TRUE),
          
          # Successful treatment
          trajectory() %>%
            log_("Treatment successful - returning to healthy!") %>%
            send(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_successful")) %>%
            set_attribute("State", 0) %>%
            release("sick") %>%
            seize("healthy"),
          
          # Treatment failed  
          trajectory() %>%
            log_("Treatment failed - remaining sick") %>%
            send(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_failed"))
        ) %>%
        
        # Final cleanup
        set_attribute("on_treatment", 0) %>%
        set_attribute("seeking_treatment", 0) %>%
        release("treatment_center") %>%
        
        # Signal treatment pathway completed (success or failure)
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_completed")),
      
      # Clone 2: Natural disease progression monitoring with comprehensive event listening
      trajectory("Natural History") %>%
        log_("Clone 2: Waiting for natural disease events") %>%
        
        # Listen for ALL possible outcomes from Clone 1
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_started"),
             handler = trajectory() %>%
               log_("Treatment started - monitoring for completion")) %>%
        
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_successful"),
             handler = trajectory() %>%
               log_("Treatment successful - Clone 2 terminating")) %>%
        
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_failed"),
             handler = trajectory() %>%
               log_("Treatment failed - Clone 2 terminating")) %>%
        
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_treatment_completed"),
             handler = trajectory() %>%
               log_("Treatment completed - Clone 2 terminating")) %>%
        
        # Listen for external termination events
        trap(function() paste0("patient", get_attribute(env, "patient_id"), "_simulation_ending"),
             handler = trajectory() %>%
               log_("Simulation ending - Clone 2 terminating")) %>%
        
        # Small delay to allow immediate treatment seizing, then wait for events
        timeout(0.001) %>%
        
        # Check if treatment started immediately during the small delay
        branch(
          function() get_attribute(env, "on_treatment"),
          continue = c(TRUE, TRUE),
          
          # Treatment started immediately - just wait for completion signals
          trajectory() %>%
            log_("Treatment started immediately - waiting for completion") %>%
            wait(),
          
          # No immediate treatment - wait for natural events or eventual treatment
          trajectory() %>%
            log_("No immediate treatment - waiting for natural events or eventual treatment") %>%
            wait()
        )
    ) %>%
    
    # Synchronize - whoever finishes first determines the outcome
    synchronize(wait = FALSE, mon_all = FALSE) %>% 
    log_("Treatment vs natural history resolved - continuing with outcome") %>% 
    synchronize(wait = FALSE, mon_all = FALSE) %>%
    log_("Treatment vs natural history resolved - continuing with outcome") %>%
    
    # the single surviving clone now reacquires its true state resource
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 4),
      trajectory() %>% seize("healthy"),   # 0 = Healthy
      trajectory() %>% seize("sick"),      # 1 = Sick
      trajectory() %>% seize("sicker"),    # 2 = Sicker
      trajectory()                         # 3 = Dead → no resource
    )
}
