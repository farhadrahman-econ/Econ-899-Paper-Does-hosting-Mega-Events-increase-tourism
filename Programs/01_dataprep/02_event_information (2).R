# ECON 899 REPLICATION PACKAGE
# 02_event_information.R
#
# Purpose:
# Define the event-specific research design used in the paper: event dates, announcement dates, treated host countries,
# comparison countries, event type, and host-income group.
#
# Output:
#   Data/data_for_analysis/event_design.rds
#
# Event assignments were compiled from IOC candidature materials and FIFA host-selection records, as documented
# in the paper and replication README.



library(tidyverse)


# 1. Event-level information
# ------------------------------------------------------------

event_info <- tibble(
  stack_id = c(
    "USA",
    "FRA",
    "AUS",
    "KOR_JPN",
    "GRC",
    "DEU",
    "CHN",
    "ZAF",
    "GBR",
    "BRA",
    "RUS"
  ),
  
  event_name = c(
    "Atlanta 1996 Olympics",
    "France 1998 World Cup",
    "Sydney 2000 Olympics",
    "Korea/Japan 2002 World Cup",
    "Athens 2004 Olympics",
    "Germany 2006 World Cup",
    "Beijing 2008 Olympics",
    "South Africa 2010 World Cup",
    "London 2012 Olympics",
    "Brazil 2014 World Cup",
    "Russia 2018 World Cup"
  ),
  
  event_year = c(
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
  ),
  
  announcement_year = c(
    1990,
    1992,
    1993,
    1996,
    1997,
    2000,
    2001,
    2004,
    2005,
    2007,
    2010
  ),
  
  event_type = c(
    "Olympics",
    "World Cup",
    "Olympics",
    "World Cup",
    "Olympics",
    "World Cup",
    "Olympics",
    "World Cup",
    "Olympics",
    "World Cup",
    "World Cup"
  ),
  
  stack_umi = c(
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    0,
    1,
    1
  )
)


# 2. Winning host countries
# ------------------------------------------------------------

event_treated <- list(
  
  USA = c("USA"),
  
  FRA = c("FRA"),
  
  AUS = c("AUS"),
  
  KOR_JPN = c(
    "KOR",
    "JPN"
  ),
  
  GRC = c("GRC"),
  
  DEU = c("DEU"),
  
  CHN = c("CHN"),
  
  ZAF = c("ZAF"),
  
  GBR = c("GBR"),
  
  BRA = c("BRA"),
  
  RUS = c("RUS")
)


# 3. Event-specific comparison countries
# ------------------------------------------------------------

event_controls <- list(
  
  USA = c(
    "AUS",
    "CAN",
    "GBR",
    "MEX"
  ),
  
  FRA = c(
    "MAR",
    "CHE"
  ),
  
  AUS = c(
    "CHN",
    "GBR",
    "DEU",
    "TUR"
  ),
  
  KOR_JPN = c(
    "MEX"
  ),
  
  GRC = c(
    "ITA",
    "ZAF",
    "ARG",
    "SWE"
  ),
  
  DEU = c(
    "ZAF",
    "MAR",
    "BRA"
  ),
  
  CHN = c(
    "CAN",
    "FRA",
    "TUR",
    "JPN"
  ),
  
  ZAF = c(
    "EGY",
    "MAR",
    "TUN"
  ),
  
  GBR = c(
    "FRA",
    "USA",
    "RUS",
    "ESP"
  ),
  
  BRA = c(
    "COL"
  ),
  
  RUS = c(
    "GBR",
    "ESP",
    "PRT",
    "NLD",
    "BEL"
  )
)

# 4. Validation
# ------------------------------------------------------------

if (nrow(event_info) != 11) {
  stop("event_info should contain exactly 11 event stacks.")
}

if (!identical(
  sort(event_info$stack_id),
  sort(names(event_treated))
)) {
  stop("event_treated does not match the event stack IDs.")
}

if (!identical(
  sort(event_info$stack_id),
  sort(names(event_controls))
)) {
  stop("event_controls does not match the event stack IDs.")
}


number_treated_countries <- sum(
  lengths(event_treated)
)

if (number_treated_countries != 12) {
  stop(
    paste(
      "Expected 12 treated host countries across the 11 stacks.",
      "Found:",
      number_treated_countries
    )
  )
}


# 5. Save event design
# ------------------------------------------------------------

event_design <- list(
  event_info = event_info,
  event_treated = event_treated,
  event_controls = event_controls
)


event_design_file <- file.path(
  ANALYSIS_DATA_DIR,
  "event_design.rds"
)


saveRDS(
  event_design,
  event_design_file
)


message(
  "Event design saved to: ",
  event_design_file
)

message(
  "02_event_information.R completed successfully."
)