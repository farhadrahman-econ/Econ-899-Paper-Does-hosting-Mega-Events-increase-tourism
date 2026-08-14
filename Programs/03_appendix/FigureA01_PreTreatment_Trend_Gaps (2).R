# ============================================================
# ECON 899 REPLICATION PACKAGE
# FigureA01_PreTreatment_Trend_Gaps.R
#
# Purpose:
# Reproduce Appendix Figure A1:
# "Pre-Treatment Tourism-Trend Differences Between Winning
# Hosts and Event-Specific Comparison Countries"
#
# Outputs:
#   Results/FigureA01_PreTreatment_Trend_Gaps.png
#   Results/FigureA01_PreTreatment_Trend_Gaps_Data.csv
# ============================================================


library(tidyverse)


# ============================================================
# 1. LOAD CONFIGURATION IF NECESSARY
# ============================================================

if (
  !exists("RESULTS_DIR") ||
  !exists("ANALYSIS_DIR") ||
  !exists("ANALYSIS_DATA_DIR")
) {
  
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
        "Programs folder, or 03_appendix folder."
      )
    )
  }
  
  source(
    config_file[1]
  )
}


# ============================================================
# 2. LOAD ANALYSIS SAMPLE IF NECESSARY
# ============================================================

if (
  !exists(
    "analysis_window_5_strict_clean_controls"
  )
) {
  
  message(
    "Core analysis objects are not loaded. Running 00_models.R."
  )
  
  source(
    file.path(
      ANALYSIS_DIR,
      "00_models.R"
    )
  )
}


# ============================================================
# 3. CHECK REQUIRED VARIABLES
# ============================================================

required_variables <- c(
  "stack_id",
  "stack_event_name",
  "iso3c",
  "treated",
  "rel_time",
  "log_arrivals"
)


missing_variables <- setdiff(
  required_variables,
  names(
    analysis_window_5_strict_clean_controls
  )
)


if (length(missing_variables) > 0) {
  
  stop(
    paste(
      "The following variables required for Figure A1 are missing:",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 4. KEEP COMMON PRE-TREATMENT PERIOD
# ============================================================
#
# Figure A1 uses the seven strict-balanced clean-control
# event stacks.
#
# For every country-stack unit, use:
#
#     t = -5, -4, -3, -2, -1
#
# ============================================================

pretrend_data <-
  analysis_window_5_strict_clean_controls %>%
  
  filter(
    rel_time >= -5,
    rel_time <= -1
  ) %>%
  
  arrange(
    stack_id,
    iso3c,
    rel_time
  )


# ============================================================
# 5. VERIFY PRE-TREATMENT SAMPLE
# ============================================================

if (
  nrow(
    pretrend_data
  ) != 115
) {
  
  stop(
    paste(
      "Figure A1 pre-treatment sample should contain",
      "115 observations. Found:",
      nrow(pretrend_data)
    )
  )
}


if (
  n_distinct(
    pretrend_data$stack_id
  ) != 7
) {
  
  stop(
    "Figure A1 should contain seven event stacks."
  )
}


if (
  n_distinct(
    pretrend_data$iso3c
  ) != 17
) {
  
  stop(
    paste(
      "Figure A1 sample should contain 17 distinct",
      "country clusters."
    )
  )
}


# ============================================================
# 6. VERIFY FIVE PRE-EVENT YEARS PER COUNTRY-STACK
# ============================================================

pretrend_support_check <-
  pretrend_data %>%
  
  count(
    stack_id,
    iso3c,
    treated,
    name = "pre_observations"
  )


if (
  !all(
    pretrend_support_check$
    pre_observations == 5
  )
) {
  
  stop(
    paste(
      "At least one country-stack unit does not",
      "contain exactly five pre-event observations."
    )
  )
}


if (
  nrow(
    pretrend_support_check
  ) != 23
) {
  
  stop(
    paste(
      "Figure A1 should contain 23",
      "country-stack units. Found:",
      nrow(pretrend_support_check)
    )
  )
}


message(
  "Figure A1 pre-treatment sample verified."
)


# ============================================================
# 7. ESTIMATE PRE-EVENT TOURISM TREND FOR EACH COUNTRY-STACK
# ============================================================
#
# For every country within every event stack:
#
# log(arrivals) = a + b * relative event time
#
# The slope b measures the annual pre-event tourism trend.
# ============================================================

country_pretrends <-
  pretrend_data %>%
  
  group_by(
    stack_id,
    stack_event_name,
    iso3c,
    treated
  ) %>%
  
  summarise(
    
    pre_observations =
      n(),
    
    pre_tourism_trend =
      unname(
        coef(
          lm(
            log_arrivals ~
              rel_time
          )
        )[
          "rel_time"
        ]
      ),
    
    .groups =
      "drop"
  )


# ============================================================
# 8. VERIFY COUNTRY-STACK TREND ESTIMATES
# ============================================================

if (
  nrow(
    country_pretrends
  ) != 23
) {
  
  stop(
    paste(
      "Expected 23 country-stack",
      "pre-treatment trend estimates."
    )
  )
}


if (
  any(
    is.na(
      country_pretrends$
      pre_tourism_trend
    )
  )
) {
  
  stop(
    "At least one pre-treatment tourism trend is missing."
  )
}


message(
  "All country-stack pre-treatment trends estimated successfully."
)


# ============================================================
# 9. CALCULATE HOST AND COMPARISON-COUNTRY TRENDS
# ============================================================
#
# Within each event stack:
#
# treated_mean
#     = mean slope for winning host(s)
#
# comparison_mean
#     = mean slope for comparison countries
#
# Korea/Japan has two treated host countries, so their slopes
# are averaged before calculating the host trend.
# ============================================================

figure_a1_data <-
  country_pretrends %>%
  
  group_by(
    stack_id,
    stack_event_name
  ) %>%
  
  summarise(
    
    treated_units =
      sum(
        treated == 1
      ),
    
    comparison_units =
      sum(
        treated == 0
      ),
    
    treated_mean =
      mean(
        pre_tourism_trend[
          treated == 1
        ]
      ),
    
    comparison_mean =
      mean(
        pre_tourism_trend[
          treated == 0
        ]
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    host_trend_percent =
      100 *
      (
        exp(
          treated_mean
        ) - 1
      ),
    
    comparison_trend_percent =
      100 *
      (
        exp(
          comparison_mean
        ) - 1
      ),
    
    trend_gap_percentage_points =
      host_trend_percent -
      comparison_trend_percent
  )


# ============================================================
# 10. PUT EVENTS IN CHRONOLOGICAL ORDER
# ============================================================

expected_stack_order <- c(
  "AUS",
  "KOR_JPN",
  "DEU",
  "CHN",
  "ZAF",
  "GBR",
  "BRA"
)


figure_a1_data <-
  figure_a1_data %>%
  
  mutate(
    
    event_order =
      match(
        stack_id,
        expected_stack_order
      )
    
  ) %>%
  
  arrange(
    event_order
  ) %>%
  
  mutate(
    
    event_label =
      factor(
        stack_event_name,
        levels =
          stack_event_name
      ),
    
    value_label =
      sprintf(
        "%+.1f",
        trend_gap_percentage_points
      )
  )


# ============================================================
# 11. DISPLAY FIGURE DATA
# ============================================================

cat("\n============================================\n")
cat("FIGURE A1 DATA\n")
cat("============================================\n")


print(
  
  figure_a1_data %>%
    
    select(
      stack_id,
      stack_event_name,
      treated_units,
      comparison_units,
      host_trend_percent,
      comparison_trend_percent,
      trend_gap_percentage_points
    ),
  
  n = Inf,
  width = Inf
)


# ============================================================
# 12. VALIDATE EVENT STACKS
# ============================================================

if (
  !identical(
    figure_a1_data$stack_id,
    expected_stack_order
  )
) {
  
  stop(
    "Figure A1 event-stack order is incorrect."
  )
}


# ============================================================
# 13. VALIDATE TREND GAPS
# ============================================================
#
# Values from the original analysis, in chronological order:
#
# Sydney        -2.02
# Korea/Japan   +3.33
# Germany       -1.34
# Beijing       +6.62
# South Africa  -0.84
# London        -0.18
# Brazil        +1.67
# ============================================================

expected_trend_gaps <- c(
  -2.02,
  3.33,
  -1.34,
  6.62,
  -0.844,
  -0.177,
  1.67
)


if (
  any(
    abs(
      figure_a1_data$
      trend_gap_percentage_points -
      expected_trend_gaps
    ) > 0.015
  )
) {
  
  warning(
    paste(
      "One or more Figure A1 trend gaps differ",
      "from the original analysis."
    )
  )
  
} else {
  
  message(
    "Figure A1 trend gaps match the original analysis."
  )
}


# ============================================================
# 14. SAVE FIGURE DATA
# ============================================================

figure_a1_data_file <- file.path(
  RESULTS_DIR,
  "FigureA01_PreTreatment_Trend_Gaps_Data.csv"
)


write.csv(
  
  figure_a1_data %>%
    
    select(
      stack_id,
      stack_event_name,
      treated_units,
      comparison_units,
      host_trend_percent,
      comparison_trend_percent,
      trend_gap_percentage_points
    ),
  
  figure_a1_data_file,
  
  row.names = FALSE
)


# ============================================================
# 15. CREATE FIGURE
# ============================================================

figure_a1 <-
  ggplot(
    figure_a1_data,
    aes(
      x =
        trend_gap_percentage_points,
      y =
        event_label
    )
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  
  geom_segment(
    aes(
      x = 0,
      xend =
        trend_gap_percentage_points,
      yend =
        event_label
    ),
    linewidth = 0.6
  ) +
  
  geom_point(
    size = 2.6
  ) +
  
  geom_text(
    aes(
      label =
        value_label
    ),
    hjust =
      ifelse(
        figure_a1_data$
          trend_gap_percentage_points >= 0,
        -0.35,
        1.35
      ),
    size = 3.3
  ) +
  
  scale_x_continuous(
    breaks =
      seq(
        -3,
        7,
        by = 1
      ),
    expand =
      expansion(
        mult =
          c(
            0.08,
            0.12
          )
      )
  ) +
  
  labs(
    x =
      paste0(
        "Host minus comparison-country pre-event tourism trend\n",
        "(percentage points per year)"
      ),
    y =
      NULL
  ) +
  
  theme_classic(
    base_size = 11
  ) +
  
  theme(
    
    axis.title.x =
      element_text(
        size = 11,
        margin =
          margin(
            t = 8
          )
      ),
    
    axis.text.x =
      element_text(
        size = 10
      ),
    
    axis.text.y =
      element_text(
        size = 10
      ),
    
    plot.margin =
      margin(
        t = 8,
        r = 24,
        b = 8,
        l = 8
      )
  )


# ============================================================
# 16. DISPLAY FIGURE
# ============================================================

print(
  figure_a1
)


# ============================================================
# 17. SAVE FIGURE
# ============================================================

figure_a1_file <- file.path(
  RESULTS_DIR,
  "FigureA01_PreTreatment_Trend_Gaps.png"
)


ggsave(
  filename =
    figure_a1_file,
  
  plot =
    figure_a1,
  
  width =
    7.2,
  
  height =
    4.8,
  
  units =
    "in",
  
  dpi =
    300
)


# ============================================================
# 18. VERIFY OUTPUT FILES
# ============================================================

if (
  !file.exists(
    figure_a1_file
  )
) {
  
  stop(
    "FigureA01_PreTreatment_Trend_Gaps.png was not created."
  )
}


if (
  !file.exists(
    figure_a1_data_file
  )
) {
  
  stop(
    "FigureA01_PreTreatment_Trend_Gaps_Data.csv was not created."
  )
}


message(
  "Figure A1 saved to: ",
  figure_a1_file
)


message(
  "Figure A1 data saved to: ",
  figure_a1_data_file
)


message(
  "FigureA01_PreTreatment_Trend_Gaps.R completed successfully."
)