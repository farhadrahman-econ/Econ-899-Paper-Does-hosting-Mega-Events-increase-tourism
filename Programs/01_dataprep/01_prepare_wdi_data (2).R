# ECON 899 REPLICATION PACKAGE
# 01_prepare_wdi_data.R
#
# Purpose:
# Read and clean the raw World Bank WDI data used in the empirical analysis.
#
# Input:
#   Data/raw_data/world_bank_wdi_1995_2020.csv
#
# Output:
#   Data/data_for_analysis/country_panel.rds
# ============================================================


# 1. Required package
# ------------------------------------------------------------

library(tidyverse)



# 2. Define input and output files
# ------------------------------------------------------------

raw_wdi_file <- file.path(
  RAW_DATA_DIR,
  "world_bank_wdi_1995_2020.csv"
)

country_panel_file <- file.path(
  ANALYSIS_DATA_DIR,
  "country_panel.rds"
)



# 3. Check raw-data file
# ------------------------------------------------------------

if (!file.exists(raw_wdi_file)) {
  stop(
    paste(
      "Raw World Bank file not found:",
      raw_wdi_file
    )
  )
}



# 4. Read raw World Bank data
# ------------------------------------------------------------

wdi_raw <- read.csv(
  raw_wdi_file,
  stringsAsFactors = FALSE
)



# 5. Check required variables
# ------------------------------------------------------------

required_variables <- c(
  "country",
  "iso3c",
  "year",
  "arrivals",
  "gdp_pc",
  "gdp_growth",
  "reer",
  "population"
)

missing_variables <- setdiff(
  required_variables,
  names(wdi_raw)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "The following required variables are missing:",
      paste(missing_variables, collapse = ", ")
    )
  )
}



# 6. Construct cleaned country-year panel
# ------------------------------------------------------------

country_panel <- wdi_raw %>%
  transmute(
    country_name = as.character(country),
    iso3c = as.character(iso3c),
    year = as.integer(year),
    
    arrivals = as.numeric(arrivals),
    gdp_pc = as.numeric(gdp_pc),
    gdp_growth = as.numeric(gdp_growth),
    reer = as.numeric(reer),
    population = as.numeric(population),
    
    log_arrivals = if_else(
      !is.na(arrivals) & arrivals > 0,
      log(arrivals),
      NA_real_
    ),
    
    log_gdp_pc = if_else(
      !is.na(gdp_pc) & gdp_pc > 0,
      log(gdp_pc),
      NA_real_
    ),
    
    log_population = if_else(
      !is.na(population) & population > 0,
      log(population),
      NA_real_
    )
  ) %>%
  arrange(
    iso3c,
    year
  )



# 7. Validate country-year structure
# ------------------------------------------------------------

duplicate_rows <- country_panel %>%
  count(
    iso3c,
    year,
    name = "number_of_rows"
  ) %>%
  filter(
    number_of_rows > 1
  )

if (nrow(duplicate_rows) > 0) {
  print(duplicate_rows)
  stop("Duplicate country-year observations were found.")
}


expected_countries <- 27
expected_years <- 1995:2020
expected_rows <- expected_countries * length(expected_years)

if (nrow(country_panel) != expected_rows) {
  stop(
    paste(
      "Unexpected number of country-year observations.",
      "Expected:",
      expected_rows,
      "Found:",
      nrow(country_panel)
    )
  )
}

if (n_distinct(country_panel$iso3c) != expected_countries) {
  stop("The cleaned panel does not contain 27 countries.")
}

if (
  min(country_panel$year) != min(expected_years) ||
  max(country_panel$year) != max(expected_years)
) {
  stop("The cleaned panel does not cover 1995-2020.")
}


# 8. Missing-data summary
# ------------------------------------------------------------

missing_summary <- country_panel %>%
  summarise(
    missing_arrivals = sum(is.na(arrivals)),
    missing_gdp_pc = sum(is.na(gdp_pc)),
    missing_gdp_growth = sum(is.na(gdp_growth)),
    missing_reer = sum(is.na(reer)),
    missing_population = sum(is.na(population))
  )

cat("\n============================================\n")
cat("COUNTRY PANEL SUMMARY\n")
cat("============================================\n")

cat("Observations:", nrow(country_panel), "\n")
cat("Countries:", n_distinct(country_panel$iso3c), "\n")
cat(
  "Years:",
  min(country_panel$year),
  "to",
  max(country_panel$year),
  "\n"
)

cat("\nMissing values:\n")
print(missing_summary)



# 9. Save cleaned country panel
# ------------------------------------------------------------

saveRDS(
  country_panel,
  country_panel_file
)



# 10. Verify saved file
# ------------------------------------------------------------

if (!file.exists(country_panel_file)) {
  stop("country_panel.rds was not created.")
}

country_panel_test <- readRDS(
  country_panel_file
)

if (nrow(country_panel_test) != nrow(country_panel)) {
  stop("Saved country panel does not have the expected rows.")
}


message(
  "Clean country-year panel saved to: ",
  country_panel_file
)

message(
  "01_prepare_wdi_data.R completed successfully."
)