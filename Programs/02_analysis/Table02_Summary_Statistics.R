# ============================================================
# ECON 899 REPLICATION PACKAGE
# Table02_Summary_Statistics.R
#
# Purpose:
# Reproduce Table 2 of the paper:
# "Summary Statistics: Main Estimation Sample"
#
# Output:
#   Results/Table02_Summary_Statistics.csv
# ============================================================


library(tidyverse)


# ============================================================
# 1. LOAD CONFIGURATION IF NECESSARY
# ============================================================

if (!exists("RESULTS_DIR")) {
  
  config_candidates <- c(
    "config.R",
    file.path("Programs", "config.R"),
    file.path("..", "config.R")
  )
  
  config_file <- config_candidates[
    file.exists(config_candidates)
  ]
  
  if (length(config_file) == 0) {
    
    stop(
      paste(
        "Could not find config.R.",
        "Run this script from the project root,",
        "Programs folder, or 02_analysis folder."
      )
    )
  }
  
  source(
    config_file[1]
  )
}


# ============================================================
# 2. LOAD ANALYSIS SAMPLE
# ============================================================

analysis_samples_file <- file.path(
  ANALYSIS_DATA_DIR,
  "analysis_samples.rds"
)


if (!file.exists(analysis_samples_file)) {
  
  stop(
    paste(
      "Required file was not found:",
      analysis_samples_file
    )
  )
}


analysis_samples <- readRDS(
  analysis_samples_file
)


analysis_data <-
  analysis_samples$analysis_data


# ============================================================
# 3. REMOVE FIXED-EFFECT SINGLETONS
# ============================================================
#
# The baseline regression contains:
#
#   country x event-stack fixed effects
#   year x event-stack fixed effects
#
# fixest removes singleton fixed-effect observations.
#
# We reproduce that removal explicitly so Table 2 uses
# exactly the same 1,084 observations as the baseline model.
# ============================================================

remove_fe_singletons <- function(data) {
  
  cleaned_data <- data
  
  repeat {
    
    n_before <-
      nrow(
        cleaned_data
      )
    
    
    cleaned_data <-
      cleaned_data %>%
      
      add_count(
        iso3c,
        stack_id,
        name = "n_country_stack"
      ) %>%
      
      add_count(
        year,
        stack_id,
        name = "n_year_stack"
      ) %>%
      
      filter(
        n_country_stack > 1,
        n_year_stack > 1
      ) %>%
      
      select(
        -n_country_stack,
        -n_year_stack
      )
    
    
    n_after <-
      nrow(
        cleaned_data
      )
    
    
    if (n_after == n_before) {
      break
    }
  }
  
  
  cleaned_data
}


table02_sample <-
  remove_fe_singletons(
    analysis_data
  )


# ============================================================
# 4. VERIFY ESTIMATION SAMPLE
# ============================================================

if (nrow(table02_sample) != 1084) {
  
  stop(
    paste(
      "Table 2 estimation sample should contain",
      "1,084 observations.",
      "Found:",
      nrow(table02_sample)
    )
  )
}


if (
  n_distinct(
    table02_sample$iso3c
  ) != 24
) {
  
  stop(
    "Table 2 sample should contain 24 countries."
  )
}


number_country_stack_units <-
  n_distinct(
    interaction(
      table02_sample$iso3c,
      table02_sample$stack_id,
      drop = TRUE
    )
  )


if (number_country_stack_units != 43) {
  
  stop(
    paste(
      "Table 2 sample should contain",
      "43 country-stack units.",
      "Found:",
      number_country_stack_units
    )
  )
}


if (
  n_distinct(
    table02_sample$stack_id
  ) != 11
) {
  
  stop(
    "Table 2 sample should contain 11 event stacks."
  )
}


# ============================================================
# 5. CREATE TABLE VARIABLES
# ============================================================

table02_sample <-
  table02_sample %>%
  
  mutate(
    
    arrivals_millions =
      arrivals / 1000000,
    
    host_indicator =
      treated,
    
    host_post_indicator =
      treat_post,
    
    umi_event_stack =
      stack_umi
  )


# ============================================================
# 6. DEFINE VARIABLES AND LABELS
# ============================================================

table02_variables <- tribble(
  
  ~variable, ~label,
  
  "arrivals_millions",
  "International tourist arrivals (millions)",
  
  "log_arrivals",
  "Log international tourist arrivals",
  
  "host_indicator",
  "Winning-host indicator",
  
  "host_post_indicator",
  "Host x post",
  
  "umi_event_stack",
  "Upper-middle-income host stack",
  
  "log_gdp_pc",
  "Log real GDP per capita",
  
  "gdp_growth",
  "GDP growth (%)",
  
  "reer",
  "Real effective exchange rate"
)


# ============================================================
# 7. HELPER FUNCTION FOR SUMMARY STATISTICS
# ============================================================

summarise_table_variable <- function(
    variable_name,
    variable_label
) {
  
  values <-
    table02_sample[[variable_name]]
  
  
  tibble(
    
    Variable =
      variable_label,
    
    N =
      sum(
        !is.na(values)
      ),
    
    Mean =
      mean(
        values,
        na.rm = TRUE
      ),
    
    `Std. Dev.` =
      sd(
        values,
        na.rm = TRUE
      ),
    
    Min =
      min(
        values,
        na.rm = TRUE
      ),
    
    Max =
      max(
        values,
        na.rm = TRUE
      )
  )
}


# ============================================================
# 8. CALCULATE SUMMARY STATISTICS
# ============================================================

table02 <-
  map2_dfr(
    
    table02_variables$variable,
    
    table02_variables$label,
    
    summarise_table_variable
  )


# ============================================================
# 9. DISPLAY TABLE
# ============================================================

cat("\n============================================\n")
cat("TABLE 2: SUMMARY STATISTICS\n")
cat("============================================\n")


print(
  table02,
  n = Inf,
  width = Inf
)


cat(
  "\nEstimation-sample observations:",
  nrow(table02_sample),
  "\n"
)


cat(
  "Countries:",
  n_distinct(
    table02_sample$iso3c
  ),
  "\n"
)


cat(
  "Country-stack units:",
  number_country_stack_units,
  "\n"
)


cat(
  "Event stacks:",
  n_distinct(
    table02_sample$stack_id
  ),
  "\n"
)


# ============================================================
# 10. CHECK MEANS AGAINST FINAL PAPER
# ============================================================

expected_means <- c(
  42.2,
  16.8,
  0.275,
  0.156,
  0.351,
  9.77,
  2.43,
  98.6
)


paper_rounding_digits <- c(
  1,
  1,
  3,
  3,
  3,
  2,
  2,
  1
)


actual_rounded_means <-
  mapply(
    round,
    table02$Mean,
    paper_rounding_digits
  )


expected_rounded_means <-
  mapply(
    round,
    expected_means,
    paper_rounding_digits
  )


if (
  any(
    actual_rounded_means !=
    expected_rounded_means
  )
) {
  
  warning(
    paste(
      "One or more Table 2 means differ",
      "from the values reported in the final paper."
    )
  )
  
} else {
  
  message(
    "Table 2 means match the final paper."
  )
}


# ============================================================
# 11. WRITE TABLE TO RESULTS FOLDER
# ============================================================

table02_file <- file.path(
  RESULTS_DIR,
  "Table02_Summary_Statistics.csv"
)


write.csv(
  table02,
  table02_file,
  row.names = FALSE
)


# ============================================================
# 12. VERIFY OUTPUT FILE
# ============================================================

if (!file.exists(table02_file)) {
  
  stop(
    "Table02_Summary_Statistics.csv was not created."
  )
}


table02_test <- read.csv(
  table02_file,
  stringsAsFactors = FALSE
)


if (nrow(table02_test) != 8) {
  
  stop(
    "Saved Table 2 should contain eight variables."
  )
}


if (!all(table02_test$N == 1084)) {
  
  stop(
    "One or more saved Table 2 variables do not have 1,084 observations."
  )
}


message(
  "Table 2 saved to: ",
  table02_file
)


message(
  "Table02_Summary_Statistics.R completed successfully."
)