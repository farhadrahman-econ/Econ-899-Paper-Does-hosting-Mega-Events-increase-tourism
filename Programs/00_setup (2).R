# ECON 899 REPLICATION PACKAGE
# 00_setup.R


required_packages <- c(
  "tidyverse",
  "fixest",
  "WDI"
)



installed_packages <- rownames(installed.packages())

packages_to_install <- setdiff(
  required_packages,
  installed_packages
)



if (length(packages_to_install) > 0) {
  
  install.packages(
    packages_to_install,
    repos = "https://cloud.r-project.org"
  )
  
} else {
  
  message("All required R packages are already installed.")
  
}



installation_check <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

if (!all(installation_check)) {
  
  stop(
    paste(
      "The following packages could not be loaded:",
      paste(
        required_packages[!installation_check],
        collapse = ", "
      )
    )
  )
}




message("Required R packages are available.")

# 6. DOWNLOAD RAW WORLD BANK DATA
# =================================

# Load WDI package
library(WDI)


# Countries used in the final analysis
country_codes <- c(
  "ARG", "AUS", "BEL", "BRA", "CAN", "CHE", "CHN",
  "COL", "DEU", "EGY", "ESP", "FRA", "GBR", "GRC",
  "ITA", "JPN", "KOR", "MAR", "MEX", "NLD", "PRT",
  "RUS", "SWE", "TUN", "TUR", "USA", "ZAF"
)


# World Bank indicators
indicators <- c(
  arrivals   = "ST.INT.ARVL",
  gdp_pc     = "NY.GDP.PCAP.KD",
  gdp_growth = "NY.GDP.MKTP.KD.ZG",
  reer       = "PX.REX.REER",
  population = "SP.POP.TOTL"
)



# Locate project root
# --------------------

setup_wd <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

if (basename(setup_wd) == "Programs") {
  
  setup_project_root <- dirname(setup_wd)
  
} else if (dir.exists(file.path(setup_wd, "Programs"))) {
  
  setup_project_root <- setup_wd
  
} else {
  
  stop(
    "Run 00_setup.R from either the project root or the Programs folder."
  )
}


raw_data_directory <- file.path(
  setup_project_root,
  "Data",
  "raw_data"
)

raw_wdi_file <- file.path(
  raw_data_directory,
  "world_bank_wdi_1995_2020.csv"
)



# Download data only if raw file does not already exist
# ------------------------------------------------------------

if (!file.exists(raw_wdi_file)) {
  
  message("Downloading World Bank WDI data...")
  
  wdi_raw <- WDI(
    country = country_codes,
    indicator = indicators,
    start = 1995,
    end = 2020,
    extra = FALSE
  )
  
  write.csv(
    wdi_raw,
    raw_wdi_file,
    row.names = FALSE
  )
  
  message(
    "Raw World Bank data saved to: ",
    raw_wdi_file
  )
  
} else {
  
  message(
    "Raw World Bank data already exists: ",
    raw_wdi_file
  )
  
}



# Verify raw file
# -------------------

if (!file.exists(raw_wdi_file)) {
  stop("Raw World Bank data file was not created.")
}

message("Raw-data setup completed successfully.")
message("ECON 899 replication-package setup completed successfully.")
