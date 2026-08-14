# ============================================================
# ECON 899 REPLICATION PACKAGE
# Programs/01_dataprep/main.R
#
# Runs all data-preparation programs in the required order.
# ============================================================


# ------------------------------------------------------------
# Load configuration if it has not already been loaded
# ------------------------------------------------------------

if (!exists("DATAPREP_DIR")) {
  
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
        "Run this program from the repository root,",
        "the Programs folder, or the 01_dataprep folder."
      )
    )
  }
  
  source(config_file[1])
}


# ------------------------------------------------------------
# 01. Prepare World Bank data
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("01. PREPARE WORLD BANK DATA")
message("--------------------------------------------")

source(
  file.path(
    DATAPREP_DIR,
    "01_prepare_wdi_data.R"
  )
)


# ------------------------------------------------------------
# 02. Define event information
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("02. DEFINE EVENT INFORMATION")
message("--------------------------------------------")

source(
  file.path(
    DATAPREP_DIR,
    "02_event_information.R"
  )
)


# ------------------------------------------------------------
# 03. Create event stacks
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("03. CREATE EVENT STACKS")
message("--------------------------------------------")

source(
  file.path(
    DATAPREP_DIR,
    "03_create_event_stacks.R"
  )
)


# ------------------------------------------------------------
# 04. Create analysis samples
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("04. CREATE ANALYSIS SAMPLES")
message("--------------------------------------------")

source(
  file.path(
    DATAPREP_DIR,
    "04_create_analysis_samples.R"
  )
)


# ------------------------------------------------------------
# Complete
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("DATA PREPARATION COMPLETED")
message("--------------------------------------------")