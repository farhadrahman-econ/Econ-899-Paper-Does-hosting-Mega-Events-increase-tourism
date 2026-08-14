# ECON 899 REPLICATION PACKAGE
# Table04_Parameters_Definitions.R
#
# Purpose:
# Reproduce Table 4 of the paper:
# "Parameters and Definitions for the Empirical Model"
#
# Output:
#   Results/Table04_Parameters_Definitions.csv
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
# 2. DEFINE TABLE 4 CONTENT
# ============================================================

table04 <- tribble(
  
  ~`Parameter / variable`,
  ~Definition,
  
  "Y_ist",
  paste(
    "Natural logarithm of international tourist arrivals",
    "for country i, event stack s, in year t."
  ),
  
  "Host_is",
  paste(
    "Indicator equal to 1 if country i is the winning host",
    "in event stack s, and 0 for countries in the",
    "event-specific comparison group."
  ),
  
  "Post_st",
  paste(
    "Indicator equal to 1 in the event year and all",
    "subsequent observed years for stack s, and 0 before",
    "the event."
  ),
  
  "D_ist = Host_is x Post_st",
  paste(
    "Treatment indicator; equal to 1 for a winning host",
    "in the event year and post-event years, and 0 otherwise."
  ),
  
  "beta",
  paste(
    "Average change in log tourist arrivals associated with",
    "winning and hosting, relative to the event-specific",
    "comparison countries used in stack s."
  ),
  
  "X_it",
  paste(
    "Time-varying controls: log real GDP per capita,",
    "annual real GDP growth, and the real effective",
    "exchange rate."
  ),
  
  "gamma",
  "Vector of coefficients on the time-varying controls.",
  
  "alpha_is",
  "Country-by-event-stack fixed effects.",
  
  "lambda_st",
  "Year-by-event-stack fixed effects.",
  
  "UMI_s",
  paste(
    "Indicator equal to 1 for event stacks coded as",
    "upper-middle-income and 0 for high-income host stacks."
  ),
  
  "delta",
  paste(
    "Upper-middle-income minus high-income difference",
    "in the hosting effect in the heterogeneity specification."
  ),
  
  "tau_st",
  paste(
    "Event time, defined as calendar year minus the",
    "hosting year for stack s."
  ),
  
  "epsilon_ist",
  "Regression error term."
)


# ============================================================
# 3. DISPLAY TABLE
# ============================================================

cat("\n============================================\n")
cat("TABLE 4: PARAMETERS AND DEFINITIONS\n")
cat("============================================\n")


print(
  table04,
  n = Inf,
  width = Inf
)


# ============================================================
# 4. VALIDATE NUMBER OF DEFINITIONS
# ============================================================

if (nrow(table04) != 13) {
  
  stop(
    paste(
      "Table 4 should contain 13 definitions.",
      "Found:",
      nrow(table04)
    )
  )
}


# ============================================================
# 5. VALIDATE PARAMETER LIST
# ============================================================

expected_parameters <- c(
  "Y_ist",
  "Host_is",
  "Post_st",
  "D_ist = Host_is x Post_st",
  "beta",
  "X_it",
  "gamma",
  "alpha_is",
  "lambda_st",
  "UMI_s",
  "delta",
  "tau_st",
  "epsilon_ist"
)


if (
  any(
    table04$`Parameter / variable` !=
    expected_parameters
  )
) {
  
  stop(
    "Table 4 parameter list does not match the final paper."
  )
  
} else {
  
  message(
    "Table 4 parameter list matches the final paper."
  )
}


# ============================================================
# 6. CHECK FOR MISSING DEFINITIONS
# ============================================================

if (
  any(
    is.na(
      table04$Definition
    )
  ) ||
  any(
    table04$Definition == ""
  )
) {
  
  stop(
    "One or more Table 4 definitions are missing."
  )
}


# ============================================================
# 7. WRITE TABLE TO RESULTS FOLDER
# ============================================================

table04_file <- file.path(
  RESULTS_DIR,
  "Table04_Parameters_Definitions.csv"
)


write.csv(
  table04,
  table04_file,
  row.names = FALSE
)


# ============================================================
# 8. VERIFY OUTPUT FILE
# ============================================================

if (!file.exists(table04_file)) {
  
  stop(
    "Table04_Parameters_Definitions.csv was not created."
  )
}


table04_test <- read.csv(
  table04_file,
  stringsAsFactors = FALSE
)


if (nrow(table04_test) != 13) {
  
  stop(
    "Saved Table 4 should contain 13 definitions."
  )
}


if (ncol(table04_test) != 2) {
  
  stop(
    "Saved Table 4 should contain two columns."
  )
}


message(
  "Table 4 saved to: ",
  table04_file
)


message(
  "Table04_Parameters_Definitions.R completed successfully."
)