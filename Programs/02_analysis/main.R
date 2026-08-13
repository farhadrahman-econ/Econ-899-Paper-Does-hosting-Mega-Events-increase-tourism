# ECON 899 REPLICATION PACKAGE
# Programs/02_analysis/main.R
#
# Runs all main-paper analysis programs in the required order.

# Load configuration if necessary
# ------------------------------------------------------------

if (!exists("ANALYSIS_DIR")) {
  
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
        "Run this program from the project root,",
        "Programs folder, or 02_analysis folder."
      )
    )
  }
  
  source(
    config_file[1]
  )
}


# ------------------------------------------------------------
# Estimate core models
# ------------------------------------------------------------

message("\n--------------------------------------------")
message("ESTIMATING CORE MODELS")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "00_models.R"
  )
)


message("\n--------------------------------------------")
message("MAIN ANALYSIS MODEL STAGE COMPLETED")
message("--------------------------------------------")