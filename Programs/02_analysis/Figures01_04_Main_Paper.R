# ============================================================
# ECON 899 REPLICATION PACKAGE
# Figures01_04_Main_Paper.R
#
# Purpose:
# Reproduce all four figures reported in the main paper.
#
# Outputs:
#   Results/Figure01_Hosts_vs_Comparison_Countries.png
#   Results/Figure02_Dynamic_Event_Study.png
#   Results/Figure03_Income_Group_Heterogeneity.png
#   Results/Figure04_Robustness_Pooled_Effect.png
# ============================================================


library(tidyverse)
library(fixest)


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
        "Programs folder, or 02_analysis folder."
      )
    )
  }
  
  source(
    config_file[1]
  )
}


# ============================================================
# 2. LOAD CORE MODELS AND ANALYSIS SAMPLES IF NECESSARY
# ============================================================

required_objects <- c(
  "analysis_data",
  "analysis_window_5_clean_controls",
  "analysis_window_5_strict_clean_controls",
  "model_with_gdp",
  "model_interaction",
  "figure3_model_restricted",
  "figure3_model_strict",
  "figure4_model_restricted",
  "figure4_model_strict"
)


objects_available <- vapply(
  required_objects,
  exists,
  logical(1),
  envir = .GlobalEnv
)


if (!all(objects_available)) {
  
  message(
    "Core models are not loaded. Running 00_models.R."
  )
  
  source(
    file.path(
      ANALYSIS_DIR,
      "00_models.R"
    )
  )
}


# ============================================================
# 3. COMMON HELPERS
# ============================================================


# ------------------------------------------------------------
# Percentage-axis formatter
# ------------------------------------------------------------

percent_axis <- function(x) {
  
  paste0(
    format(
      round(
        x,
        0
      ),
      trim = TRUE
    ),
    "%"
  )
}


# ------------------------------------------------------------
# Common figure theme
# ------------------------------------------------------------

paper_theme <- theme_minimal(
  base_size = 11
) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(
      size = 11
    ),
    axis.text = element_text(
      size = 9.5
    )
  )


# ------------------------------------------------------------
# Remove fixed-effect singletons recursively
# ------------------------------------------------------------

remove_fe_singletons <- function(data) {
  
  cleaned_data <- data
  
  repeat {
    
    n_before <- nrow(
      cleaned_data
    )
    
    cleaned_data <- cleaned_data %>%
      
      add_count(
        iso3c,
        stack_id,
        name = "n_country_stack"
      ) %>%
      
      add_count(
        year,
        stack_id,
        name = "n_year_stack"
      ) %>%
      
      filter(
        n_country_stack > 1,
        n_year_stack > 1
      ) %>%
      
      select(
        -n_country_stack,
        -n_year_stack
      )
    
    n_after <- nrow(
      cleaned_data
    )
    
    if (n_after == n_before) {
      break
    }
  }
  
  cleaned_data
}


# ------------------------------------------------------------
# Standard pooled model
# ------------------------------------------------------------

fit_pooled_model <- function(data) {
  
  feols(
    
    log_arrivals ~
      treat_post +
      log_gdp_pc +
      gdp_growth +
      reer |
      
      iso3c^stack_id +
      year^stack_id,
    
    data = data,
    
    cluster = ~iso3c
  )
}


# ============================================================
# FIGURE 1
# WINNING HOSTS VS EVENT-SPECIFIC COMPARISON COUNTRIES
# ============================================================


message("\n============================================")
message("CREATING FIGURE 1")
message("============================================")


figure1_sample <-
  analysis_window_5_strict_clean_controls


# ------------------------------------------------------------
# 1A. Verify sample
# ------------------------------------------------------------

if (
  n_distinct(
    figure1_sample$stack_id
  ) != 7
) {
  
  stop(
    "Figure 1 should contain exactly seven event stacks."
  )
}


if (
  min(
    figure1_sample$rel_time
  ) != -5 ||
  max(
    figure1_sample$rel_time
  ) != 5
) {
  
  stop(
    "Figure 1 sample should cover event time -5 through +5."
  )
}


# ------------------------------------------------------------
# 1B. Normalize each country-stack to t = -1
# ------------------------------------------------------------

figure1_country_data <-
  figure1_sample %>%
  
  group_by(
    stack_id,
    iso3c
  ) %>%
  
  mutate(
    
    reference_log_arrivals =
      log_arrivals[
        rel_time == -1
      ][1],
    
    normalized_log_arrivals =
      log_arrivals -
      reference_log_arrivals,
    
    normalized_percent =
      100 *
      (
        exp(
          normalized_log_arrivals
        ) - 1
      )
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# 1C. Validate reference period
# ------------------------------------------------------------

figure1_reference <-
  figure1_country_data %>%
  
  filter(
    rel_time == -1
  )


if (
  any(
    abs(
      figure1_reference$
      normalized_percent
    ) > 1e-10
  )
) {
  
  stop(
    "Figure 1 normalization failed."
  )
}


# ------------------------------------------------------------
# 1D. Average within each event first
# ------------------------------------------------------------

figure1_event_data <-
  figure1_country_data %>%
  
  group_by(
    stack_id,
    rel_time,
    treated
  ) %>%
  
  summarise(
    
    event_mean_percent =
      mean(
        normalized_percent,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 1E. Equal-event average
# ------------------------------------------------------------

figure1_plot_data <-
  figure1_event_data %>%
  
  group_by(
    rel_time,
    treated
  ) %>%
  
  summarise(
    
    mean_percent_change =
      mean(
        event_mean_percent,
        na.rm = TRUE
      ),
    
    event_stacks =
      n_distinct(
        stack_id
      ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    group =
      if_else(
        treated == 1,
        "Winning hosts",
        "Comparison countries"
      )
  )


cat("\nFIGURE 1 PLOT DATA\n")

print(
  figure1_plot_data,
  n = Inf,
  width = Inf
)


if (
  any(
    figure1_plot_data$
    event_stacks != 7
  )
) {
  
  stop(
    "Figure 1 does not give all seven events equal representation."
  )
}


# ------------------------------------------------------------
# 1F. Check selected original values
# ------------------------------------------------------------

figure1_check <-
  figure1_plot_data %>%
  
  filter(
    rel_time %in%
      c(0, 5)
  ) %>%
  
  arrange(
    rel_time,
    treated
  )


expected_figure1 <- c(
  4.22,
  7.98,
  20.7,
  26.8
)


if (
  any(
    abs(
      figure1_check$mean_percent_change -
      expected_figure1
    ) > 0.1
  )
) {
  
  warning(
    "Selected Figure 1 values differ from the original analysis."
  )
  
} else {
  
  message(
    "Figure 1 values match the original analysis."
  )
}


# ------------------------------------------------------------
# 1G. Plot
# ------------------------------------------------------------

figure01 <-
  ggplot(
    figure1_plot_data,
    aes(
      x = rel_time,
      y = mean_percent_change,
      group = group,
      linetype = group,
      shape = group
    )
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey55"
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = "dashed",
    colour = "grey55"
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 2.3
  ) +
  
  scale_x_continuous(
    breaks = -5:5
  ) +
  
  scale_y_continuous(
    labels = percent_axis
  ) +
  
  labs(
    x = "Years relative to hosting",
    y =
      paste(
        "Average change in international tourist arrivals",
        "relative to year -1",
        sep = "\n"
      ),
    linetype = NULL,
    shape = NULL
  ) +
  
  paper_theme


figure01_file <- file.path(
  RESULTS_DIR,
  "Figure01_Hosts_vs_Comparison_Countries.png"
)


ggsave(
  filename = figure01_file,
  plot = figure01,
  width = 6.5,
  height = 4.3,
  units = "in",
  dpi = 600
)


message(
  "Figure 1 saved successfully."
)


# ============================================================
# FIGURE 2
# DYNAMIC EVENT-STUDY ESTIMATES
# ============================================================


message("\n============================================")
message("CREATING FIGURE 2")
message("============================================")


# ------------------------------------------------------------
# 2A. Estimate baseline event-study model
# ------------------------------------------------------------

event_study_model <-
  feols(
    
    log_arrivals ~
      
      i(
        rel_time_binned,
        treated,
        ref = -1
      ) +
      
      log_gdp_pc +
      gdp_growth +
      reer |
      
      iso3c^stack_id +
      year^stack_id,
    
    data =
      analysis_data,
    
    cluster =
      ~iso3c
  )


if (
  nobs(
    event_study_model
  ) != 1084
) {
  
  stop(
    paste(
      "Figure 2 event-study model should contain",
      "1,084 observations.",
      "Found:",
      nobs(event_study_model)
    )
  )
}


# ------------------------------------------------------------
# 2B. Extract dynamic coefficients
# ------------------------------------------------------------

event_coefficient_names <-
  grep(
    "^rel_time_binned::",
    names(
      coef(
        event_study_model
      )
    ),
    value = TRUE
  )


event_study_df <-
  fixest::degrees_freedom(
    event_study_model,
    type = "t"
  )


event_study_critical <-
  qt(
    0.975,
    df = event_study_df
  )


figure2_plot_data <-
  tibble(
    
    term =
      event_coefficient_names,
    
    relative_time =
      as.integer(
        sub(
          "rel_time_binned::(-?[0-9]+):treated",
          "\\1",
          event_coefficient_names
        )
      ),
    
    estimate_log =
      unname(
        coef(
          event_study_model
        )[
          event_coefficient_names
        ]
      ),
    
    standard_error =
      unname(
        se(
          event_study_model
        )[
          event_coefficient_names
        ]
      )
  ) %>%
  
  mutate(
    
    ci_lower_log =
      estimate_log -
      event_study_critical *
      standard_error,
    
    ci_upper_log =
      estimate_log +
      event_study_critical *
      standard_error,
    
    estimate_percent =
      100 *
      (
        exp(
          estimate_log
        ) - 1
      ),
    
    ci_lower_percent =
      100 *
      (
        exp(
          ci_lower_log
        ) - 1
      ),
    
    ci_upper_percent =
      100 *
      (
        exp(
          ci_upper_log
        ) - 1
      )
  )


# Add omitted year -1

figure2_reference <-
  tibble(
    
    term =
      "Reference period",
    
    relative_time =
      -1L,
    
    estimate_log =
      0,
    
    standard_error =
      NA_real_,
    
    ci_lower_log =
      NA_real_,
    
    ci_upper_log =
      NA_real_,
    
    estimate_percent =
      0,
    
    ci_lower_percent =
      NA_real_,
    
    ci_upper_percent =
      NA_real_
  )


figure2_plot_data <-
  bind_rows(
    figure2_plot_data,
    figure2_reference
  ) %>%
  
  arrange(
    relative_time
  )


cat("\nFIGURE 2 EVENT-STUDY DATA\n")

print(
  figure2_plot_data,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 2C. Validate coefficients against final paper
# ------------------------------------------------------------

figure2_nonreference <-
  figure2_plot_data %>%
  
  filter(
    relative_time != -1
  )


expected_figure2_coefficients <- c(
  0.1806,
  0.0898,
  0.1087,
  0.0666,
  0.0400,
  -0.0396,
  -0.0143,
  -0.0300,
  0.0373,
  0.0129,
  0.0230,
  0.2668
)


if (
  any(
    abs(
      figure2_nonreference$estimate_log -
      expected_figure2_coefficients
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Figure 2 coefficients differ from the final paper."
  )
  
} else {
  
  message(
    "Figure 2 coefficients match the final paper."
  )
}


# ------------------------------------------------------------
# 2D. Joint pre-treatment test
# ------------------------------------------------------------

pretrend_terms <- paste0(
  "rel_time_binned::",
  c(
    -6,
    -5,
    -4,
    -3,
    -2
  ),
  ":treated"
)


pretrend_beta <-
  coef(
    event_study_model
  )[
    pretrend_terms
  ]


pretrend_vcov <-
  vcov(
    event_study_model
  )[
    pretrend_terms,
    pretrend_terms,
    drop = FALSE
  ]


pretrend_q <-
  length(
    pretrend_terms
  )


pretrend_f <-
  as.numeric(
    t(pretrend_beta) %*%
      solve(
        pretrend_vcov
      ) %*%
      pretrend_beta /
      pretrend_q
  )


pretrend_p <-
  pf(
    pretrend_f,
    df1 = pretrend_q,
    df2 = event_study_df,
    lower.tail = FALSE
  )


cat(
  "\nFigure 2 joint pre-treatment test:",
  "F(",
  pretrend_q,
  ", ",
  event_study_df,
  ") = ",
  round(
    pretrend_f,
    3
  ),
  ", p = ",
  round(
    pretrend_p,
    3
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------
# 2E. Plot
# ------------------------------------------------------------

figure02 <-
  ggplot(
    figure2_plot_data,
    aes(
      x = relative_time,
      y = estimate_percent
    )
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey50"
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = "dashed",
    colour = "grey50"
  ) +
  
  geom_errorbar(
    data =
      figure2_plot_data %>%
      filter(
        !is.na(
          standard_error
        )
      ),
    aes(
      ymin = ci_lower_percent,
      ymax = ci_upper_percent
    ),
    width = 0.14,
    linewidth = 0.6
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks = -6:6,
    labels = c(
      "≤-6",
      "-5",
      "-4",
      "-3",
      "-2",
      "-1",
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "≥6"
    )
  ) +
  
  scale_y_continuous(
    labels = percent_axis
  ) +
  
  labs(
    x = "Years relative to hosting",
    y =
      "Estimated change in international tourist arrivals"
  ) +
  
  paper_theme


figure02_file <- file.path(
  RESULTS_DIR,
  "Figure02_Dynamic_Event_Study.png"
)


ggsave(
  filename = figure02_file,
  plot = figure02,
  width = 6.5,
  height = 4.5,
  units = "in",
  dpi = 600
)


message(
  "Figure 2 saved successfully."
)


# ============================================================
# FIGURE 3
# HOSTING EFFECTS BY HOST INCOME GROUP
# ============================================================


message("\n============================================")
message("CREATING FIGURE 3")
message("============================================")


# ------------------------------------------------------------
# 3A. Helper to extract high-income and implied UMI effects
# ------------------------------------------------------------

extract_income_effects <- function(
    model,
    specification_name
) {
  
  interaction_term <-
    grep(
      "treat_post:stack_umi|stack_umi:treat_post",
      names(
        coef(model)
      ),
      value = TRUE
    )
  
  
  if (length(interaction_term) != 1) {
    
    stop(
      paste(
        "Could not identify income interaction term for:",
        specification_name
      )
    )
  }
  
  
  model_coef <-
    coef(
      model
    )
  
  
  model_vcov <-
    vcov(
      model
    )
  
  
  high_estimate <-
    unname(
      model_coef[
        "treat_post"
      ]
    )
  
  
  high_se <-
    unname(
      se(model)[
        "treat_post"
      ]
    )
  
  
  interaction_estimate <-
    unname(
      model_coef[
        interaction_term
      ]
    )
  
  
  umi_estimate <-
    high_estimate +
    interaction_estimate
  
  
  umi_variance <-
    
    model_vcov[
      "treat_post",
      "treat_post"
    ] +
    
    model_vcov[
      interaction_term,
      interaction_term
    ] +
    
    2 *
    model_vcov[
      "treat_post",
      interaction_term
    ]
  
  
  umi_se <-
    sqrt(
      umi_variance
    )
  
  
  model_df <-
    fixest::degrees_freedom(
      model,
      type = "t"
    )
  
  
  critical_value <-
    qt(
      0.975,
      df = model_df
    )
  
  
  tibble(
    
    specification =
      specification_name,
    
    income_group =
      c(
        "High-income hosts",
        "Upper-middle-income hosts"
      ),
    
    estimate_log =
      c(
        high_estimate,
        umi_estimate
      ),
    
    standard_error =
      c(
        high_se,
        umi_se
      ),
    
    degrees_freedom =
      model_df
  ) %>%
    
    mutate(
      
      ci_lower_log =
        estimate_log -
        critical_value *
        standard_error,
      
      ci_upper_log =
        estimate_log +
        critical_value *
        standard_error,
      
      estimate_percent =
        100 *
        (
          exp(
            estimate_log
          ) - 1
        ),
      
      ci_lower_percent =
        100 *
        (
          exp(
            ci_lower_log
          ) - 1
        ),
      
      ci_upper_percent =
        100 *
        (
          exp(
            ci_upper_log
          ) - 1
        )
    )
}


# ------------------------------------------------------------
# 3B. Construct plot data
# ------------------------------------------------------------

figure3_plot_data <-
  bind_rows(
    
    extract_income_effects(
      model_interaction,
      "Baseline full sample"
    ),
    
    extract_income_effects(
      figure3_model_restricted,
      "Restricted -5/+5, clean controls"
    ),
    
    extract_income_effects(
      figure3_model_strict,
      "Strict balanced -5/+5, clean controls"
    )
  ) %>%
  
  mutate(
    
    y_base =
      case_when(
        
        specification ==
          "Baseline full sample" ~ 3,
        
        specification ==
          "Restricted -5/+5, clean controls" ~ 2,
        
        specification ==
          "Strict balanced -5/+5, clean controls" ~ 1
      ),
    
    y_position =
      y_base +
      if_else(
        income_group ==
          "High-income hosts",
        0.12,
        -0.12
      )
  )


cat("\nFIGURE 3 PLOT DATA\n")

print(
  figure3_plot_data,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 3C. Validate point estimates
# ------------------------------------------------------------

expected_figure3 <- c(
  0.0963,
  -0.1257,
  0.0148,
  -0.0970,
  0.0369,
  0.0346
)


if (
  any(
    abs(
      figure3_plot_data$estimate_log -
      expected_figure3
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Figure 3 estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Figure 3 estimates match the final paper."
  )
}


# ------------------------------------------------------------
# 3D. Plot
# ------------------------------------------------------------

figure03 <-
  ggplot(
    figure3_plot_data
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    colour = "grey50"
  ) +
  
  geom_segment(
    aes(
      x = ci_lower_percent,
      xend = ci_upper_percent,
      y = y_position,
      yend = y_position,
      linetype = income_group
    ),
    linewidth = 0.7
  ) +
  
  geom_segment(
    aes(
      x = ci_lower_percent,
      xend = ci_lower_percent,
      y = y_position - 0.055,
      yend = y_position + 0.055
    ),
    linewidth = 0.6
  ) +
  
  geom_segment(
    aes(
      x = ci_upper_percent,
      xend = ci_upper_percent,
      y = y_position - 0.055,
      yend = y_position + 0.055
    ),
    linewidth = 0.6
  ) +
  
  geom_point(
    aes(
      x = estimate_percent,
      y = y_position,
      shape = income_group
    ),
    size = 2.8
  ) +
  
  scale_y_continuous(
    breaks = c(
      1,
      2,
      3
    ),
    labels = c(
      "Strict balanced -5 to +5,\nclean controls",
      "Restricted -5 to +5,\nclean controls",
      "Baseline full sample"
    ),
    limits = c(
      0.5,
      3.5
    )
  ) +
  
  scale_x_continuous(
    labels = percent_axis
  ) +
  
  labs(
    x =
      "Estimated effect on international tourist arrivals",
    y = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  
  paper_theme +
  
  theme(
    legend.position = "top",
    axis.text.y =
      element_text(
        hjust = 1
      )
  )


figure03_file <- file.path(
  RESULTS_DIR,
  "Figure03_Income_Group_Heterogeneity.png"
)


ggsave(
  filename = figure03_file,
  plot = figure03,
  width = 6.5,
  height = 4.3,
  units = "in",
  dpi = 600
)


message(
  "Figure 3 saved successfully."
)


# ============================================================
# FIGURE 4
# ROBUSTNESS OF THE AVERAGE HOSTING EFFECT
# ============================================================


message("\n============================================")
message("CREATING FIGURE 4")
message("============================================")


# ------------------------------------------------------------
# 4A. Exclude 2020 model
# ------------------------------------------------------------

figure4_data_no2020 <-
  analysis_data %>%
  
  filter(
    year <= 2019
  )


figure4_model_no2020 <-
  fit_pooled_model(
    figure4_data_no2020
  )


# ------------------------------------------------------------
# 4B. Exclude Russia model
# ------------------------------------------------------------

figure4_data_no_russia <-
  analysis_data %>%
  
  filter(
    stack_id != "RUS"
  )


figure4_model_no_russia <-
  fit_pooled_model(
    figure4_data_no_russia
  )


# ------------------------------------------------------------
# 4C. Equal-event weighting
# ------------------------------------------------------------
#
# Remove the baseline singleton before constructing weights.
# Each event stack then receives exactly the same total
# regression weight.
# ------------------------------------------------------------

figure4_equal_event_data <-
  remove_fe_singletons(
    analysis_data
  ) %>%
  
  group_by(
    stack_id
  ) %>%
  
  mutate(
    
    stack_observations =
      n(),
    
    event_weight_raw =
      1 /
      stack_observations
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    event_weight =
      event_weight_raw /
      mean(
        event_weight_raw
      )
  )


figure4_weight_check <-
  figure4_equal_event_data %>%
  
  group_by(
    stack_id
  ) %>%
  
  summarise(
    
    total_stack_weight =
      sum(
        event_weight
      ),
    
    .groups = "drop"
  )


if (
  diff(
    range(
      figure4_weight_check$
      total_stack_weight
    )
  ) > 1e-10
) {
  
  stop(
    "Equal-event weights are not equal across event stacks."
  )
}


figure4_model_equal_event <-
  feols(
    
    log_arrivals ~
      treat_post +
      log_gdp_pc +
      gdp_growth +
      reer |
      
      iso3c^stack_id +
      year^stack_id,
    
    data =
      figure4_equal_event_data,
    
    weights =
      ~event_weight,
    
    cluster =
      ~iso3c,
    
    fixef.rm =
      "none"
  )


# ------------------------------------------------------------
# 4D. Helper: extract pooled effect and CI
# ------------------------------------------------------------

extract_pooled_effect <- function(
    model,
    specification_name
) {
  
  estimate <-
    unname(
      coef(model)[
        "treat_post"
      ]
    )
  
  
  standard_error <-
    unname(
      se(model)[
        "treat_post"
      ]
    )
  
  
  model_df <-
    fixest::degrees_freedom(
      model,
      type = "t"
    )
  
  
  critical_value <-
    qt(
      0.975,
      df = model_df
    )
  
  
  ci_lower_log <-
    estimate -
    critical_value *
    standard_error
  
  
  ci_upper_log <-
    estimate +
    critical_value *
    standard_error
  
  
  tibble(
    
    specification =
      specification_name,
    
    estimate_log =
      estimate,
    
    standard_error =
      standard_error,
    
    degrees_freedom =
      model_df,
    
    observations =
      nobs(model),
    
    estimate_percent =
      100 *
      (
        exp(
          estimate
        ) - 1
      ),
    
    ci_lower_percent =
      100 *
      (
        exp(
          ci_lower_log
        ) - 1
      ),
    
    ci_upper_percent =
      100 *
      (
        exp(
          ci_upper_log
        ) - 1
      )
  )
}


# ------------------------------------------------------------
# 4E. Build robustness dataset
# ------------------------------------------------------------

figure4_plot_data <-
  bind_rows(
    
    extract_pooled_effect(
      model_with_gdp,
      "Baseline full sample"
    ),
    
    extract_pooled_effect(
      figure4_model_no2020,
      "Exclude 2020"
    ),
    
    extract_pooled_effect(
      figure4_model_no_russia,
      "Exclude Russia"
    ),
    
    extract_pooled_effect(
      figure4_model_equal_event,
      "Equal-event weighting"
    ),
    
    extract_pooled_effect(
      figure4_model_restricted,
      "Restricted -5/+5, clean controls"
    ),
    
    extract_pooled_effect(
      figure4_model_strict,
      "Strict balanced -5/+5, clean controls"
    )
  ) %>%
  
  mutate(
    
    y_position =
      case_when(
        
        specification ==
          "Baseline full sample" ~ 6,
        
        specification ==
          "Exclude 2020" ~ 5,
        
        specification ==
          "Exclude Russia" ~ 4,
        
        specification ==
          "Equal-event weighting" ~ 3,
        
        specification ==
          "Restricted -5/+5, clean controls" ~ 2,
        
        specification ==
          "Strict balanced -5/+5, clean controls" ~ 1
      )
  )


cat("\nFIGURE 4 PLOT DATA\n")

print(
  figure4_plot_data,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 4F. Validate estimates and observation counts
# ------------------------------------------------------------

expected_figure4_estimates <- c(
  0.0152,
  0.0302,
  0.0472,
  0.0080,
  -0.0292,
  0.0360
)


expected_figure4_observations <- c(
  1084,
  1047,
  928,
  1084,
  400,
  253
)


if (
  any(
    abs(
      figure4_plot_data$estimate_log -
      expected_figure4_estimates
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Figure 4 estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Figure 4 estimates match the final paper."
  )
}


if (
  any(
    figure4_plot_data$observations !=
    expected_figure4_observations
  )
) {
  
  stop(
    "Figure 4 observation counts do not match the final paper."
  )
}


message(
  "Figure 4 observation counts match the final paper."
)


# ------------------------------------------------------------
# 4G. Plot
# ------------------------------------------------------------

figure04 <-
  ggplot(
    figure4_plot_data
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    colour = "grey50"
  ) +
  
  geom_segment(
    aes(
      x = ci_lower_percent,
      xend = ci_upper_percent,
      y = y_position,
      yend = y_position
    ),
    linewidth = 0.7
  ) +
  
  geom_segment(
    aes(
      x = ci_lower_percent,
      xend = ci_lower_percent,
      y = y_position - 0.07,
      yend = y_position + 0.07
    ),
    linewidth = 0.6
  ) +
  
  geom_segment(
    aes(
      x = ci_upper_percent,
      xend = ci_upper_percent,
      y = y_position - 0.07,
      yend = y_position + 0.07
    ),
    linewidth = 0.6
  ) +
  
  geom_point(
    aes(
      x = estimate_percent,
      y = y_position
    ),
    size = 2.8
  ) +
  
  scale_y_continuous(
    breaks = 1:6,
    labels = c(
      "Strict balanced -5 to +5,\nclean controls",
      "Restricted -5 to +5,\nclean controls",
      "Equal-event weighting",
      "Exclude Russia 2018 stack",
      "Exclude 2020",
      "Baseline full sample"
    ),
    limits = c(
      0.5,
      6.5
    )
  ) +
  
  scale_x_continuous(
    labels = percent_axis
  ) +
  
  labs(
    x =
      "Estimated effect on international tourist arrivals",
    y = NULL
  ) +
  
  paper_theme +
  
  theme(
    legend.position = "none",
    axis.text.y =
      element_text(
        hjust = 1
      )
  )


figure04_file <- file.path(
  RESULTS_DIR,
  "Figure04_Robustness_Pooled_Effect.png"
)


ggsave(
  filename = figure04_file,
  plot = figure04,
  width = 6.5,
  height = 4.5,
  units = "in",
  dpi = 600
)


message(
  "Figure 4 saved successfully."
)


# ============================================================
# FINAL OUTPUT CHECK
# ============================================================

main_figure_files <- c(
  figure01_file,
  figure02_file,
  figure03_file,
  figure04_file
)


missing_figure_files <-
  main_figure_files[
    !file.exists(
      main_figure_files
    )
  ]


if (
  length(
    missing_figure_files
  ) > 0
) {
  
  stop(
    paste(
      "The following main-paper figures were not created:",
      paste(
        missing_figure_files,
        collapse = "\n"
      )
    )
  )
}


message("\n============================================")
message("ALL MAIN-PAPER FIGURES COMPLETED SUCCESSFULLY")
message("============================================")

message(
  "Figure 1: ",
  figure01_file
)

message(
  "Figure 2: ",
  figure02_file
)

message(
  "Figure 3: ",
  figure03_file
)

message(
  "Figure 4: ",
  figure04_file
)