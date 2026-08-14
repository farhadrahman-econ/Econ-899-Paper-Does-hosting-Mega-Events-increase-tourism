# ============================================================
# ECON 899 REPLICATION PACKAGE
# Table01_Data_Sources.R
#
# Purpose:
# Reproduce Table 1 of the paper:
# "Data Sources"
#
# Output:
#   Results/Table01_Data_Sources.csv
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
# 2. DEFINE TABLE CONTENT
# ============================================================

table01 <- tibble(
  
  Dataset = c(
    "International tourist arrivals",
    "GDP per capita (constant 2015 US$)",
    "GDP growth (annual %)",
    "Real effective exchange rate"
  ),
  
  Indicator = c(
    "ST.INT.ARVL",
    "NY.GDP.PCAP.KD",
    "NY.GDP.MKTP.KD.ZG",
    "PX.REX.REER"
  ),
  
  Source = c(
    "World Bank WDI",
    "World Bank WDI",
    "World Bank WDI",
    "World Bank WDI"
  ),
  
  Period = c(
    "1995-2020",
    "1995-2020",
    "1995-2020",
    "1995-2020"
  )
)


# ============================================================
# 3. DISPLAY TABLE
# ============================================================

cat("\n============================================\n")
cat("TABLE 1: DATA SOURCES\n")
cat("============================================\n")

print(
  table01,
  n = Inf,
  width = Inf
)


# ============================================================
# 4. VALIDATE TABLE
# ============================================================

if (nrow(table01) != 4) {
  stop(
    "Table 1 should contain exactly four data series."
  )
}


expected_indicators <- c(
  "ST.INT.ARVL",
  "NY.GDP.PCAP.KD",
  "NY.GDP.MKTP.KD.ZG",
  "PX.REX.REER"
)


if (!identical(
  table01$Indicator,
  expected_indicators
)) {
  
  stop(
    "Table 1 indicator codes do not match the final paper."
  )
}


if (!all(
  table01$Source == "World Bank WDI"
)) {
  
  stop(
    "Unexpected source found in Table 1."
  )
}


if (!all(
  table01$Period == "1995-2020"
)) {
  
  stop(
    "Unexpected period found in Table 1."
  )
}


# ============================================================
# 5. WRITE TABLE TO RESULTS FOLDER
# ============================================================

table01_file <- file.path(
  RESULTS_DIR,
  "Table01_Data_Sources.csv"
)


write.csv(
  table01,
  table01_file,
  row.names = FALSE
)


# ============================================================
# 6. VERIFY OUTPUT FILE
# ============================================================

if (!file.exists(table01_file)) {
  stop(
    "Table01_Data_Sources.csv was not created."
  )
}


table01_test <- read.csv(
  table01_file,
  stringsAsFactors = FALSE
)


if (nrow(table01_test) != 4) {
  stop(
    "Saved Table 1 does not contain four rows."
  )
}


message(
  "Table 1 saved to: ",
  table01_file
)

message(
  "Table01_Data_Sources.R completed successfully."
)