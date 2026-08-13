# ECON 899 REPLICATION PACKAGE
# config.R
#
# This is the only file a replication user may need to edit.
#
#
# 1. Project root
# ---------------

PROJECT_ROOT <- normalizePath(
  "C:/Users/sylfa/Desktop/econ 899 github/Econ-899-Paper-Does-hosting-Mega-Events-increase-tourism",
  winslash = "/",
  mustWork = TRUE
)



# 2. Data directories
# --------------------

RAW_DATA_DIR <- file.path(
  PROJECT_ROOT,
  "Data",
  "raw_data"
)

ANALYSIS_DATA_DIR <- file.path(
  PROJECT_ROOT,
  "Data",
  "data_for_analysis"
)



# 3. Program directories
# -------------------------

PROGRAMS_DIR <- file.path(
  PROJECT_ROOT,
  "Programs"
)

DATAPREP_DIR <- file.path(
  PROGRAMS_DIR,
  "01_dataprep"
)

ANALYSIS_DIR <- file.path(
  PROGRAMS_DIR,
  "02_analysis"
)

APPENDIX_DIR <- file.path(
  PROGRAMS_DIR,
  "03_appendix"
)



# 4. Results directory
# ---------------------

RESULTS_DIR <- file.path(
  PROJECT_ROOT,
  "Results"
)



# 5. Check required directories
# -------------------------------

required_directories <- c(
  RAW_DATA_DIR,
  ANALYSIS_DATA_DIR,
  DATAPREP_DIR,
  ANALYSIS_DIR,
  APPENDIX_DIR,
  RESULTS_DIR
)

missing_directories <- required_directories[
  !dir.exists(required_directories)
]

if (length(missing_directories) > 0) {
  
  stop(
    paste(
      "The following required directories are missing:",
      paste(
        missing_directories,
        collapse = "\n"
      )
    )
  )
}



# 6. Configuration complete
# -------------------------

message("Project root: ", PROJECT_ROOT)
message("Configuration loaded successfully.")