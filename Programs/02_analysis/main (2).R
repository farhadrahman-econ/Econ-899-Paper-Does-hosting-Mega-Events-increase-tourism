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
message("CREATING TABLE 1: DATA SOURCES")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Table01_Data_Sources.R"
  )
)

message("\n--------------------------------------------")
message("CREATING TABLE 2: SUMMARY STATISTICS")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Table02_Summary_Statistics.R"
  )
)
message("\n--------------------------------------------")
message("CREATING TABLE 3: EVENT STACKS")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Table03_Event_Stacks.R"
  )
)
message("\n--------------------------------------------")
message("CREATING TABLE 4: PARAMETERS AND DEFINITIONS")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Table04_Parameters_Definitions.R"
  )
)
message("\n--------------------------------------------")
message("CREATING TABLE 5: MAIN REGRESSION RESULTS")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Table05_Main_Results.R"
  )
)
message("\n--------------------------------------------")
message("CREATING MAIN-PAPER FIGURES 1-4")
message("--------------------------------------------")

source(
  file.path(
    ANALYSIS_DIR,
    "Figures01_04_Main_Paper.R"
  )
)
message("\n--------------------------------------------")
message("MAIN ANALYSIS MODEL STAGE COMPLETED")
message("--------------------------------------------")