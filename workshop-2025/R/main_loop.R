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


# This file is ~10 years old with little to no modification.
# I.e. it works and shouldn't require much change.
#
# It constitutes a main loop of using DES for health policy simulations.
# It utilizes an event registry that has functions to return time to event and
# it picks the nearest event and executes it's trajectory modification function.

# Create the counters, takes a list
create_counters <- function(env, counters)
{
  sapply(counters, FUN=function(counter)
  {
    env <- add_resource(env, counter, Inf, 0)
  })
  
  env
}

# Mark a counter
mark <- function(traj, counter)
{
  traj               |>
    seize(counter,1)   |>
    timeout(0)         |>
    release(counter,1)
}

  ##############################################
 ##
## Helper functions for managing events
##
## Hopefully, no modification required.
##
assign_events <- function(traj, inputs)
{
  sapply(event_registry, FUN=function(event)
  {
    traj <- set_attribute(traj, event$attr, function()
    {
      event$time_to_event(inputs)
    })
  })
  traj
}

# Find the next event based on time
next_event <- function()
{
  event_time <- Inf
  event      <- NA
  id         <- 0
  for(i in 1:length(event_registry))
  {
    e <- event_registry[[i]]
    tmp_time   <- get_attribute(env,e$attr)
    if(tmp_time < event_time)
    {
      event      <- e
      event_time <- tmp_time
      id         <- i
    }
  }
  
  return(list(event=event, event_time=event_time, id=id))
}

# Process events in main loop
process_events <- function(traj, env, inputs)
{
  # Find the next event from possible events, and timeout (wait) till that moment
  traj <- timeout(traj, function()
  {
    # Determine next up
    ne <- next_event()
    event      <- ne[['event']]
    event_time <- ne[['event_time']]
    
    #cat(" Next up => ",event$name,"\n")
    #cat("            waiting", event_time-now(env), '\n')
    
    # Wait the clock time for the nearest event, minus now()
    event_time - now(env)
  })
  
  # Age them by clock
  traj <- set_attribute(traj,'aAge',function() get_attribute(env,"aAgeInitial")+(now(env)/365.0))
  
  # Create a handler for every possible event, using their
  # list position as the branch number
  # This will determine the id of the next event
  # Call it's modification function
  # and then update it's next time to event
  args <- lapply(event_registry,FUN=function(e) {
    #print(e$name)   # Good for debugging event loading
    trajectory(e$name) |>
      e$func(inputs) |>
      set_attribute(e$attr, function() {now(env)+e$time_to_event(inputs)})
  })
  args$".trj"    <- traj
  args$option    <- function() next_event()$id
  args$continue  <- rep(TRUE,length(event_registry))
  
  traj <- do.call(branch, args)
  
  # Apply reactive events
  lapply(event_registry[sapply(event_registry, function(x) x$reactive)], FUN=function(e){
    traj <- set_attribute(traj, e$attr, function() {now(env)+e$time_to_event(inputs)})
  })
  
  traj
}

  ##############################################
 ##
## MAIN LOOP
##
## This should not require modification
## This creates a patient simulation (trajectory)
## 
## It uses a branch in a manner to prevent the
## rollback from looking further up the stack
## of the event loop. 
des <- function(env, inputs)
{
  trajectory("Patient")            |>
    initialize_patient(inputs)     |>
    assign_events(inputs)          |>
    branch( # Used branch, to prevent rollback from looking inside event loop function
      function() 1,
      continue=TRUE,
      trajectory("main_loop") |> process_events(env, inputs)
    ) |> 
    # note: this function has been updated, occasionally causing errors 
    # The rollback() function in simmer uses the argument target (not target =) and times (not times =), and in 
    # newer versions, the signature is rollback(.trj, target, times = Inf, check = NULL, ..., tag) 
    # The orignal code was using the old syntax with named arguments target = 1, times = 100, but the 
    # function expects positional arguments. the simple fix below resolves this
    rollback(1, 100) # Process up to 100 events per person
}

split_arrivals <- function(patient_data) {
  library(dplyr)
  
  # Get all unique time points and resource types
  time_points <- sort(unique(c(patient_data$start_time, patient_data$end_time)))
  all_resources <- unique(patient_data$resource)
  
  # Create time periods
  periods <- data.frame(
    period_start = time_points[-length(time_points)],
    period_end = time_points[-1]
  ) %>%
    mutate(
      period_duration = period_end - period_start,
      period_id = row_number()
    )
  
  # For each period, determine which events are active
  result_list <- lapply(1:nrow(periods), function(i) {
    period_start <- periods$period_start[i]
    period_end <- periods$period_end[i]
    
    # Find active events
    active_rows <- which(
      patient_data$start_time < period_end & 
        patient_data$end_time > period_start
    )
    
    # Create base period info
    period_info <- data.frame(
      period_id = i,
      period_start = period_start,
      period_end = period_end,
      period_duration = period_end - period_start,
      n_active_events = length(active_rows)
    )
    
    if (length(active_rows) > 0) {
      active_data <- patient_data[active_rows, ]
      period_info$active_resources <- paste(active_data$resource, collapse = ", ")
      period_info$active_row_ids <- paste(active_rows, collapse = ", ")
      
      # Create binary indicators for each resource type
      for (resource in all_resources) {
        col_name <- paste0(resource, "_active")
        period_info[[col_name]] <- resource %in% active_data$resource
      }
    } else {
      period_info$active_resources <- ""
      period_info$active_row_ids <- ""
      
      # Set all resource indicators to FALSE
      for (resource in all_resources) {
        col_name <- paste0(resource, "_active")
        period_info[[col_name]] <- FALSE
      }
    }
    
    return(period_info)
  })
  
  # Combine results
  result <- do.call(rbind, result_list)
  result$patient_name <- unique(patient_data$name)[1]
  
  return(result)
}


plot_patient_trajectory <- function(patient_data) {
  # Load required libraries
  library(ggplot2)
  library(dplyr)
  
  # Prepare data for plotting
  plot_data <- patient_data %>%
    mutate(
      resource = as.factor(resource),
      row_id = row_number()  # Add row identifier for y-axis positioning
    )
  
  # Create the plot
  p <- ggplot(plot_data, aes(y = row_id)) +
    geom_segment(
      aes(x = start_time, xend = end_time, 
          color = resource, size = 1.5),
      lineend = "round"
    ) +
    scale_color_viridis_d(name = "Health State") +  # Use viridis color palette
    scale_size_identity() +
    labs(
      title = "Patient Health State Trajectory Over Time",
      x = "Time",
      y = "Event Sequence",
      subtitle = paste("Patient:", unique(plot_data$name))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  return(p)
}

# Alternative version that groups overlapping events by resource type
plot_patient_trajectory_grouped <- function(patient_data) {
  library(ggplot2)
  library(dplyr)
  
  # Get unique resources and assign y-positions
  resources <- unique(patient_data$resource)
  resource_positions <- data.frame(
    resource = resources,
    y_pos = seq_along(resources)
  )
  
  # Prepare data for plotting
  plot_data <- patient_data %>%
    left_join(resource_positions, by = "resource") %>%
    mutate(resource = as.factor(resource))
  
  # Create the plot
  p <- ggplot(plot_data, aes(y = y_pos)) +
    geom_segment(
      aes(x = start_time, xend = end_time, 
          color = resource),
      size = 4, lineend = "round", alpha = 0.8
    ) +
    scale_color_viridis_d(name = "Health State") +
    scale_y_continuous(
      breaks = resource_positions$y_pos,
      labels = resource_positions$resource
    ) +
    labs(
      title = "Patient Health State Trajectory Over Time",
      x = "Time",
      y = "Health State",
      subtitle = paste("Patient:", unique(plot_data$name))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  return(p)
}

