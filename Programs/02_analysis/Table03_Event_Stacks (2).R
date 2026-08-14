# ============================================================
# ECON 899 REPLICATION PACKAGE
# Table03_Event_Stacks.R
#
# Purpose:
# Reproduce Table 3 of the paper:
# "Event Stacks and Analysis Comparison Groups"
#
# Input:
#   Data/data_for_analysis/event_design.rds
#
# Output:
#   Results/Table03_Event_Stacks.csv
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
# 2. LOAD EVENT DESIGN
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

event_controls <-
  event_design$event_controls


# ============================================================
# 3. COUNTRY DISPLAY NAMES
# ============================================================
#
# These labels are used only for presentation in Table 3.
# ISO3 codes remain the identifiers used in the empirical
# analysis.
# ============================================================

country_names <- c(
  
  ARG = "Argentina",
  AUS = "Australia",
  BEL = "Belgium",
  BRA = "Brazil",
  CAN = "Canada",
  CHE = "Switzerland",
  CHN = "China",
  COL = "Colombia",
  DEU = "Germany",
  EGY = "Egypt",
  ESP = "Spain",
  FRA = "France",
  GBR = "United Kingdom",
  GRC = "Greece",
  ITA = "Italy",
  JPN = "Japan",
  KOR = "South Korea",
  MAR = "Morocco",
  MEX = "Mexico",
  NLD = "Netherlands",
  PRT = "Portugal",
  RUS = "Russia",
  SWE = "Sweden",
  TUN = "Tunisia",
  TUR = "Turkey",
  USA = "United States",
  ZAF = "South Africa"
)


# ============================================================
# 4. EVENT DISPLAY NAMES
# ============================================================

event_display_names <- c(
  
  USA =
    "Atlanta Olympics",
  
  FRA =
    "France World Cup",
  
  AUS =
    "Sydney Olympics",
  
  KOR_JPN =
    "Korea/Japan World Cup",
  
  GRC =
    "Athens Olympics",
  
  DEU =
    "Germany World Cup",
  
  CHN =
    "Beijing Olympics",
  
  ZAF =
    "South Africa World Cup",
  
  GBR =
    "London Olympics",
  
  BRA =
    "Brazil World Cup",
  
  RUS =
    "Russia World Cup"
)


# ============================================================
# 5. HELPER: CONVERT ISO3 CODES TO COUNTRY NAMES
# ============================================================

format_country_names <- function(country_codes) {
  
  names_found <-
    unname(
      country_names[
        country_codes
      ]
    )
  
  
  if (any(is.na(names_found))) {
    
    missing_codes <-
      country_codes[
        is.na(names_found)
      ]
    
    
    stop(
      paste(
        "Country display names are missing for:",
        paste(
          missing_codes,
          collapse = ", "
        )
      )
    )
  }
  
  
  paste(
    names_found,
    collapse = ", "
  )
}


# ============================================================
# 6. CONSTRUCT TABLE 3
# ============================================================

table03 <-
  map_dfr(
    
    event_info$stack_id,
    
    function(current_stack) {
      
      current_info <-
        event_info %>%
        
        filter(
          stack_id ==
            current_stack
        )
      
      
      if (nrow(current_info) != 1) {
        
        stop(
          paste(
            "Expected exactly one event-information row for stack:",
            current_stack
          )
        )
      }
      
      
      tibble(
        
        Event =
          unname(
            event_display_names[
              current_stack
            ]
          ),
        
        Year =
          as.integer(
            current_info$event_year
          ),
        
        `Winning host(s)` =
          format_country_names(
            event_treated[[current_stack]]
          ),
        
        `Analysis comparison countries` =
          format_country_names(
            event_controls[[current_stack]]
          ),
        
        `Host income group` =
          if_else(
            current_info$stack_umi == 1,
            "Upper-middle",
            "High"
          )
      )
    }
  )


# ============================================================
# 7. DISPLAY TABLE
# ============================================================

cat("\n============================================\n")
cat("TABLE 3: EVENT STACKS AND ANALYSIS COMPARISON GROUPS\n")
cat("============================================\n")


print(
  table03,
  n = Inf,
  width = Inf
)


# ============================================================
# 8. VALIDATE NUMBER OF EVENT STACKS
# ============================================================

if (nrow(table03) != 11) {
  
  stop(
    paste(
      "Table 3 should contain 11 event stacks.",
      "Found:",
      nrow(table03)
    )
  )
}


# ============================================================
# 9. VALIDATE NUMBER OF TREATED HOST COUNTRIES
# ============================================================

number_treated_hosts <-
  sum(
    lengths(
      event_treated
    )
  )


if (number_treated_hosts != 12) {
  
  stop(
    paste(
      "The event design should contain 12 treated host countries.",
      "Found:",
      number_treated_hosts
    )
  )
}


# ============================================================
# 10. VALIDATE INCOME-GROUP COMPOSITION
# ============================================================

number_umi_stacks <-
  sum(
    table03$`Host income group` ==
      "Upper-middle"
  )


number_high_income_stacks <-
  sum(
    table03$`Host income group` ==
      "High"
  )


if (number_umi_stacks != 4) {
  
  stop(
    paste(
      "Table 3 should contain four upper-middle-income host stacks.",
      "Found:",
      number_umi_stacks
    )
  )
}


if (number_high_income_stacks != 7) {
  
  stop(
    paste(
      "Table 3 should contain seven high-income host stacks.",
      "Found:",
      number_high_income_stacks
    )
  )
}


# ============================================================
# 11. VALIDATE EVENT YEARS
# ============================================================

expected_years <- c(
  1996,
  1998,
  2000,
  2002,
  2004,
  2006,
  2008,
  2010,
  2012,
  2014,
  2018
)


if (
  any(
    table03$Year !=
    expected_years
  )
) {
  
  stop(
    "Table 3 event years do not match the final paper."
  )
  
} else {
  
  message(
    "Table 3 event years match the final paper."
  )
}


# ============================================================
# 12. VALIDATE EVENT NAMES
# ============================================================

expected_events <- c(
  "Atlanta Olympics",
  "France World Cup",
  "Sydney Olympics",
  "Korea/Japan World Cup",
  "Athens Olympics",
  "Germany World Cup",
  "Beijing Olympics",
  "South Africa World Cup",
  "London Olympics",
  "Brazil World Cup",
  "Russia World Cup"
)


if (
  any(
    table03$Event !=
    expected_events
  )
) {
  
  stop(
    "Table 3 event names do not match the final paper."
  )
  
} else {
  
  message(
    "Table 3 event names match the final paper."
  )
}


# ============================================================
# 13. WRITE TABLE TO RESULTS FOLDER
# ============================================================

table03_file <- file.path(
  RESULTS_DIR,
  "Table03_Event_Stacks.csv"
)


write.csv(
  table03,
  table03_file,
  row.names = FALSE
)


# ============================================================
# 14. VERIFY OUTPUT FILE
# ============================================================

if (!file.exists(table03_file)) {
  
  stop(
    "Table03_Event_Stacks.csv was not created."
  )
}


table03_test <- read.csv(
  table03_file,
  stringsAsFactors = FALSE
)


if (nrow(table03_test) != 11) {
  
  stop(
    "Saved Table 3 should contain 11 event stacks."
  )
}


if (ncol(table03_test) != 5) {
  
  stop(
    "Saved Table 3 should contain five columns."
  )
}


message(
  "Table 3 saved to: ",
  table03_file
)


message(
  "Table03_Event_Stacks.R completed successfully."
)