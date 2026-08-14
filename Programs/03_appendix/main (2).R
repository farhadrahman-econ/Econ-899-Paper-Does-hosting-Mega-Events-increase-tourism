# ============================================================
# ECON 899 REPLICATION PACKAGE
# Programs/03_appendix/main.R
#
# Runs all appendix analysis.
# ============================================================


if (!exists("APPENDIX_DIR")) {
  
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
        "Programs folder, or 03_appendix folder."
      )
    )
  }
  
  source(config_file[1])
}


message("\n--------------------------------------------")
message("CREATING APPENDIX TABLES A1-A5")
message("--------------------------------------------")

source(
  file.path(
    APPENDIX_DIR,
    "Appendix_Tables_A1_A5.R"
  )
)


message("\n--------------------------------------------")
message("APPENDIX TABLE STAGE COMPLETED")
message("--------------------------------------------")
message("\n--------------------------------------------")
message("CREATING APPENDIX FIGURE A1")
message("--------------------------------------------")

source(
  file.path(
    APPENDIX_DIR,
    "FigureA01_PreTreatment_Trend_Gaps.R"
  )
)


message("\n--------------------------------------------")
message("APPENDIX ANALYSIS COMPLETED")
message("--------------------------------------------")