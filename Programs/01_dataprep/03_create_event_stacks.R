# ECON 899 REPLICATION PACKAGE
# 03_create_event_stacks.R
#
# Purpose:
# Combine the country-year panel with the event-specific research design and construct the stacked DiD dataset.
#
# Inputs:
#   Data/data_for_analysis/country_panel.rds
#   Data/data_for_analysis/event_design.rds
#
# Output:
#   Data/data_for_analysis/stacked_data.rds
# ============================================================


library(tidyverse)


# 1. Input files
# ------------------------------------------------------------

country_panel_file <- file.path(
  ANALYSIS_DATA_DIR,
  "country_panel.rds"
)

event_design_file <- file.path(
  ANALYSIS_DATA_DIR,
  "event_design.rds"
)


if (!file.exists(country_panel_file)) {
  stop("country_panel.rds was not found.")
}

if (!file.exists(event_design_file)) {
  stop("event_design.rds was not found.")
}


# 2. Load data
# ------------------------------------------------------------

panel <- readRDS(
  country_panel_file
)

event_design <- readRDS(
  event_design_file
)


event_info <- event_design$event_info
event_treated <- event_design$event_treated
event_controls <- event_design$event_controls


# 3. Construct one dataset for each event stack
# ------------------------------------------------------------

stack_list <- lapply(
  event_info$stack_id,
  
  function(current_stack) {
    
    current_info <- event_info %>%
      filter(
        stack_id == current_stack
      )
    
    
    treated_countries <-
      event_treated[[current_stack]]
    
    comparison_countries <-
      event_controls[[current_stack]]
    
    stack_countries <- unique(
      c(
        treated_countries,
        comparison_countries
      )
    )
    
    
    panel %>%
      filter(
        iso3c %in% stack_countries
      ) %>%
      mutate(
        
        stack_id =
          current_stack,
        
        stack_event_name =
          current_info$event_name,
        
        stack_event_year =
          as.integer(
            current_info$event_year
          ),
        
        stack_announcement_year =
          as.integer(
            current_info$announcement_year
          ),
        
        stack_event_type =
          current_info$event_type,
        
        stack_umi =
          as.integer(
            current_info$stack_umi
          ),
        
        treated =
          as.integer(
            iso3c %in%
              treated_countries
          ),
        
        post =
          as.integer(
            year >=
              stack_event_year
          ),
        
        treat_post =
          treated *
          post,
        
        rel_time =
          year -
          stack_event_year,
        
        rel_time_binned =
          case_when(
            
            rel_time <= -6 ~ -6,
            
            rel_time >= 6 ~ 6,
            
            TRUE ~
              as.numeric(
                rel_time
              )
          )
      )
  }
)


# 4. Combine event stacks
# ------------------------------------------------------------

stacked_data_all <- bind_rows(
  stack_list
)


# The analysis requires a defined tourism outcome.
# Missing tourist-arrival years are therefore removed here.

stacked_data <- stacked_data_all %>%
  filter(
    !is.na(log_arrivals)
  ) %>%
  arrange(
    stack_id,
    iso3c,
    year
  )


#5. Validate stacked structure
# ------------------------------------------------------------

if (
  n_distinct(stacked_data$stack_id) != 11
) {
  stop(
    "The stacked dataset should contain 11 event stacks."
  )
}


if (
  n_distinct(
    stacked_data$iso3c[
      stacked_data$treated == 1
    ]
  ) != 12
) {
  stop(
    "The stacked dataset should contain 12 treated host countries."
  )
}


duplicate_stack_rows <- stacked_data %>%
  count(
    stack_id,
    iso3c,
    year,
    name =
      "number_of_rows"
  ) %>%
  filter(
    number_of_rows > 1
  )


if (nrow(duplicate_stack_rows) > 0) {
  
  print(
    duplicate_stack_rows
  )
  
  stop(
    "Duplicate stack-country-year observations were found."
  )
}


# 6. Summary
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STACKED DATA SUMMARY\n")
cat("============================================\n")

cat(
  "Rows before removing missing outcomes:",
  nrow(stacked_data_all),
  "\n"
)

cat(
  "Rows after removing missing outcomes:",
  nrow(stacked_data),
  "\n"
)

cat(
  "Event stacks:",
  n_distinct(
    stacked_data$stack_id
  ),
  "\n"
)

cat(
  "Treated host countries:",
  n_distinct(
    stacked_data$iso3c[
      stacked_data$treated == 1
    ]
  ),
  "\n"
)


# 7. Save stacked dataset
# ------------------------------------------------------------

stacked_data_file <- file.path(
  ANALYSIS_DATA_DIR,
  "stacked_data.rds"
)


saveRDS(
  stacked_data,
  stacked_data_file
)


message(
  "Stacked dataset saved to: ",
  stacked_data_file
)

message(
  "03_create_event_stacks.R completed successfully."
)