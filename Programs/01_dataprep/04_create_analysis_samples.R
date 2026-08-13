# ============================================================
# ECON 899 REPLICATION PACKAGE
# 04_create_analysis_samples.R
#
# Purpose:
# Construct the final baseline and robustness-analysis samples.
#
# Inputs:
#   Data/data_for_analysis/stacked_data.rds
#   Data/data_for_analysis/event_design.rds
#
# Output:
#   Data/data_for_analysis/analysis_samples.rds
# ============================================================


library(tidyverse)


# ============================================================
# 1. LOAD STACKED DATA
# ============================================================

stacked_data_file <- file.path(
  ANALYSIS_DATA_DIR,
  "stacked_data.rds"
)

if (!file.exists(stacked_data_file)) {
  stop(
    paste(
      "Required file was not found:",
      stacked_data_file
    )
  )
}

stacked_data <- readRDS(
  stacked_data_file
)


# ============================================================
# 2. PREFERRED COMPLETE-CASE ANALYSIS SAMPLE
# ============================================================

analysis_data <- stacked_data %>%
  filter(
    complete.cases(
      log_arrivals,
      log_gdp_pc,
      gdp_growth,
      reer
    )
  ) %>%
  arrange(
    stack_id,
    iso3c,
    year
  )


# ============================================================
# 3. RESTRICTED EVENT WINDOW: -5 THROUGH +5
# ============================================================

target_event_times <- -5:5

analysis_window_5 <- analysis_data %>%
  filter(
    rel_time >= -5,
    rel_time <= 5
  )


# ============================================================
# 4. IDENTIFY EVENT STACKS WITH FULL -5/+5 SUPPORT
# ============================================================
#
# A stack is retained if both:
#
#   1. treated observations exist at every relative year -5:+5
#   2. comparison observations exist at every relative year -5:+5
#
# This reproduces the stack-level balance check used in the
# original analysis.
# ============================================================

window_stack_support <- analysis_window_5 %>%
  
  group_by(
    stack_id,
    stack_event_name,
    stack_event_year
  ) %>%
  
  summarise(
    
    observations =
      n(),
    
    countries =
      n_distinct(
        iso3c
      ),
    
    treated_countries =
      n_distinct(
        iso3c[
          treated == 1
        ]
      ),
    
    control_countries =
      n_distinct(
        iso3c[
          treated == 0
        ]
      ),
    
    minimum_event_time =
      min(
        rel_time
      ),
    
    maximum_event_time =
      max(
        rel_time
      ),
    
    treated_relative_periods =
      n_distinct(
        rel_time[
          treated == 1
        ]
      ),
    
    control_relative_periods =
      n_distinct(
        rel_time[
          treated == 0
        ]
      ),
    
    full_treated_support =
      all(
        target_event_times %in%
          rel_time[
            treated == 1
          ]
      ),
    
    full_control_support =
      all(
        target_event_times %in%
          rel_time[
            treated == 0
          ]
      ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    full_minus5_plus5_support =
      full_treated_support &
      full_control_support
  )


# ============================================================
# 5. IDENTIFY FULLY SUPPORTED EVENT STACKS
# ============================================================

balanced_stack_ids <- window_stack_support %>%
  
  filter(
    full_minus5_plus5_support
  ) %>%
  
  pull(
    stack_id
  )


cat("\n============================================\n")
cat("FULL -5/+5 EVENT STACKS\n")
cat("============================================\n")

cat(
  "Number of fully supported stacks:",
  length(
    balanced_stack_ids
  ),
  "\n"
)

cat(
  "Stack IDs:",
  paste(
    balanced_stack_ids,
    collapse = ", "
  ),
  "\n"
)


# ============================================================
# 6. CREATE STACK-BALANCED SAMPLE
# ============================================================

analysis_window_5_balanced <- analysis_window_5 %>%
  
  filter(
    stack_id %in%
      balanced_stack_ids
  )


# ============================================================
# 7. CHECK COUNTRY-STACK BALANCE
# ============================================================
#
# A stack can have overall -5/+5 support even if an individual
# country-stack unit is missing one relative year.
#
# We therefore require every retained country-stack unit to
# contain all eleven relative years from -5 through +5.
# ============================================================

country_stack_window_support <-
  analysis_window_5_balanced %>%
  
  group_by(
    stack_id,
    iso3c,
    treated
  ) %>%
  
  summarise(
    
    observations =
      n(),
    
    relative_periods =
      n_distinct(
        rel_time
      ),
    
    minimum_event_time =
      min(
        rel_time
      ),
    
    maximum_event_time =
      max(
        rel_time
      ),
    
    full_country_stack_window =
      all(
        target_event_times %in%
          rel_time
      ),
    
    .groups = "drop"
  )


incomplete_country_stacks <-
  country_stack_window_support %>%
  
  filter(
    !full_country_stack_window
  )


cat("\n============================================\n")
cat("INCOMPLETE COUNTRY-STACK WINDOWS\n")
cat("============================================\n")

if (nrow(incomplete_country_stacks) == 0) {
  
  cat(
    "All country-stack units have complete -5/+5 support.\n"
  )
  
} else {
  
  print(
    incomplete_country_stacks
  )
}


# ============================================================
# 8. CREATE STRICT COUNTRY-STACK-BALANCED SAMPLE
# ============================================================

complete_country_stack_units <-
  country_stack_window_support %>%
  
  filter(
    full_country_stack_window
  ) %>%
  
  select(
    stack_id,
    iso3c
  )


analysis_window_5_strict <-
  analysis_window_5_balanced %>%
  
  semi_join(
    complete_country_stack_units,
    by = c(
      "stack_id",
      "iso3c"
    )
  )


# ============================================================
# 9. LOAD EVENT DESIGN
# ============================================================

event_design_file <- file.path(
  ANALYSIS_DATA_DIR,
  "event_design.rds"
)

if (!file.exists(event_design_file)) {
  stop(
    paste(
      "Required file was not found:",
      event_design_file
    )
  )
}


event_design <- readRDS(
  event_design_file
)

event_info <-
  event_design$event_info

event_treated <-
  event_design$event_treated


# ============================================================
# 10. CONSTRUCT HOST-EVENT HISTORY
# ============================================================

host_event_history <-
  map_dfr(
    
    event_info$stack_id,
    
    function(current_stack) {
      
      current_info <-
        event_info %>%
        filter(
          stack_id ==
            current_stack
        )
      
      tibble(
        
        iso3c =
          event_treated[[current_stack]],
        
        hosting_stack =
          current_stack,
        
        hosting_year =
          as.integer(
            current_info$event_year
          )
      )
    }
  )


# ============================================================
# 11. FUNCTION TO IDENTIFY CONTAMINATED CONTROLS
# ============================================================
#
# A comparison country is contaminated if it hosts another
# selected mega-event within five years of the event defining
# its current comparison stack.
# ============================================================

identify_contaminated_controls <- function(data) {
  
  control_units <-
    data %>%
    
    filter(
      treated == 0
    ) %>%
    
    distinct(
      stack_id,
      stack_event_year,
      iso3c
    )
  
  
  contaminated_units <-
    control_units %>%
    
    left_join(
      host_event_history,
      by = "iso3c"
    ) %>%
    
    filter(
      !is.na(
        hosting_year
      ),
      hosting_stack !=
        stack_id,
      abs(
        hosting_year -
          stack_event_year
      ) <= 5
    ) %>%
    
    distinct(
      stack_id,
      iso3c
    ) %>%
    
    as_tibble()
  
  
  contaminated_units
}


# ============================================================
# 12. IDENTIFY CONTAMINATION IN BOTH WINDOWS
# ============================================================

contaminated_keys_all <-
  identify_contaminated_controls(
    analysis_window_5
  )


contaminated_keys_strict <-
  identify_contaminated_controls(
    analysis_window_5_strict
  )


cat("\n============================================\n")
cat("CONTAMINATED KEYS: ALL -5/+5 WINDOW\n")
cat("============================================\n")

print(
  contaminated_keys_all
)


cat("\n============================================\n")
cat("CONTAMINATED KEYS: STRICT WINDOW\n")
cat("============================================\n")

print(
  contaminated_keys_strict
)


# ============================================================
# 13. CREATE CLEAN-CONTROL SAMPLES
# ============================================================

analysis_window_5_clean_controls <-
  analysis_window_5 %>%
  
  anti_join(
    contaminated_keys_all,
    by = c(
      "stack_id",
      "iso3c"
    )
  )


analysis_window_5_strict_clean_controls <-
  analysis_window_5_strict %>%
  
  anti_join(
    contaminated_keys_strict,
    by = c(
      "stack_id",
      "iso3c"
    )
  )


# ============================================================
# 14. ADDITIONAL ROBUSTNESS SAMPLES
# ============================================================

data_no2020 <- analysis_data %>%
  
  filter(
    year <= 2019
  )


data_no_russia <- analysis_data %>%
  
  filter(
    stack_id != "RUS"
  )


# ============================================================
# 15. SAMPLE SUMMARY
# ============================================================

sample_summary <- tibble(
  
  sample = c(
    "Baseline complete case",
    "Restricted -5/+5",
    "Restricted -5/+5 clean controls",
    "Stack balanced -5/+5",
    "Strict balanced -5/+5",
    "Strict balanced -5/+5 clean controls",
    "Exclude 2020",
    "Exclude Russia"
  ),
  
  observations = c(
    nrow(
      analysis_data
    ),
    nrow(
      analysis_window_5
    ),
    nrow(
      analysis_window_5_clean_controls
    ),
    nrow(
      analysis_window_5_balanced
    ),
    nrow(
      analysis_window_5_strict
    ),
    nrow(
      analysis_window_5_strict_clean_controls
    ),
    nrow(
      data_no2020
    ),
    nrow(
      data_no_russia
    )
  )
)


cat("\n============================================\n")
cat("ANALYSIS SAMPLE SUMMARY\n")
cat("============================================\n")

print(
  sample_summary,
  n = Inf
)


# ============================================================
# 16. BASELINE SAMPLE CHARACTERISTICS
# ============================================================

cat(
  "\nBaseline countries:",
  n_distinct(
    analysis_data$iso3c
  ),
  "\n"
)

cat(
  "Baseline country-event units:",
  n_distinct(
    interaction(
      analysis_data$iso3c,
      analysis_data$stack_id,
      drop = TRUE
    )
  ),
  "\n"
)

cat(
  "Event stacks:",
  n_distinct(
    analysis_data$stack_id
  ),
  "\n"
)

cat(
  "Treated host countries:",
  n_distinct(
    analysis_data$iso3c[
      analysis_data$treated == 1
    ]
  ),
  "\n"
)


# ============================================================
# 17. EXPECTED REPLICATION CHECKPOINTS
# ============================================================

expected_counts <- c(
  
  baseline =
    1085,
  
  restricted =
    418,
  
  restricted_clean =
    400,
  
  stack_balanced =
    274,
  
  strict =
    264,
  
  strict_clean =
    253,
  
  exclude_2020 =
    1047,
  
  exclude_russia =
    929
)


actual_counts <- c(
  
  baseline =
    nrow(
      analysis_data
    ),
  
  restricted =
    nrow(
      analysis_window_5
    ),
  
  restricted_clean =
    nrow(
      analysis_window_5_clean_controls
    ),
  
  stack_balanced =
    nrow(
      analysis_window_5_balanced
    ),
  
  strict =
    nrow(
      analysis_window_5_strict
    ),
  
  strict_clean =
    nrow(
      analysis_window_5_strict_clean_controls
    ),
  
  exclude_2020 =
    nrow(
      data_no2020
    ),
  
  exclude_russia =
    nrow(
      data_no_russia
    )
)


if (any(actual_counts != expected_counts)) {
  
  warning(
    paste(
      "One or more reconstructed sample counts",
      "differ from the original analysis."
    )
  )
  
} else {
  
  message(
    "All reconstructed sample counts match the original analysis."
  )
}


# ============================================================
# 18. SAVE ALL ANALYSIS SAMPLES
# ============================================================

analysis_samples <- list(
  
  analysis_data =
    analysis_data,
  
  analysis_window_5 =
    analysis_window_5,
  
  analysis_window_5_balanced =
    analysis_window_5_balanced,
  
  analysis_window_5_strict =
    analysis_window_5_strict,
  
  analysis_window_5_clean_controls =
    analysis_window_5_clean_controls,
  
  analysis_window_5_strict_clean_controls =
    analysis_window_5_strict_clean_controls,
  
  data_no2020 =
    data_no2020,
  
  data_no_russia =
    data_no_russia,
  
  window_stack_support =
    window_stack_support,
  
  country_stack_window_support =
    country_stack_window_support,
  
  incomplete_country_stacks =
    incomplete_country_stacks,
  
  contaminated_keys_all =
    contaminated_keys_all,
  
  contaminated_keys_strict =
    contaminated_keys_strict,
  
  sample_summary =
    sample_summary
)


analysis_samples_file <- file.path(
  ANALYSIS_DATA_DIR,
  "analysis_samples.rds"
)


saveRDS(
  analysis_samples,
  analysis_samples_file
)


# ============================================================
# 19. VERIFY SAVED FILE
# ============================================================

if (!file.exists(analysis_samples_file)) {
  stop(
    "analysis_samples.rds was not created."
  )
}


analysis_samples_test <- readRDS(
  analysis_samples_file
)


if (
  nrow(
    analysis_samples_test$analysis_data
  ) !=
  nrow(
    analysis_data
  )
) {
  
  stop(
    "Saved analysis data does not contain the expected number of rows."
  )
}


message(
  "Analysis samples saved to: ",
  analysis_samples_file
)

message(
  "04_create_analysis_samples.R completed successfully."
)