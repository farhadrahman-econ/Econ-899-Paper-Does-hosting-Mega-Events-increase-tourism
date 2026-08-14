# ============================================================
# ECON 899 REPLICATION PACKAGE
# 01_main.R
#
# Purpose:
# Run the complete replication package in the required order.
#
# Before running this file:
#   1. Run 00_setup.R once to install required packages.
#   2. Edit config.R if the project location is different.
# ============================================================


# ------------------------------------------------------------
# 1. Load configuration
# ------------------------------------------------------------

config_candidates <- c(
  "config.R",
  file.path("Programs", "config.R")
)

config_file <- config_candidates[
  file.exists(config_candidates)
]

if (length(config_file) == 0) {
  stop(
    "Could not find config.R. Run 01_main.R from either the project root or the Programs folder."
  )
}

source(config_file[1])


# ------------------------------------------------------------
# 2. Run data preparation
# ------------------------------------------------------------

message("\n============================================")
message("RUNNING DATA PREPARATION")
message("============================================")

source(
  file.path(
    DATAPREP_DIR,
    "main.R"
  )
)


# ------------------------------------------------------------
# 3. Run main-paper analysis
# ------------------------------------------------------------

message("\n============================================")
message("RUNNING MAIN ANALYSIS")
message("============================================")

source(
  file.path(
    ANALYSIS_DIR,
    "main.R"
  )
)


# ------------------------------------------------------------
# 4. Run appendix analysis
# ------------------------------------------------------------

message("\n============================================")
message("RUNNING APPENDIX ANALYSIS")
message("============================================")

source(
  file.path(
    APPENDIX_DIR,
    "main.R"
  )
)


# ------------------------------------------------------------
# 5. Replication complete
# ------------------------------------------------------------

message("\n============================================")
message("ECON 899 REPLICATION COMPLETED SUCCESSFULLY")
message("============================================")