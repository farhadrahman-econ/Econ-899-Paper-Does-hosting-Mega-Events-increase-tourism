# ============================================================
# ECON 899 REPLICATION PACKAGE
# Appendix_Tables_A1_A5.R
#
# Purpose:
# Reproduce all appendix tables reported in the paper.
#
# Outputs:
#   Results/TableA01_Robustness.csv
#   Results/TableA02_Dynamic_Event_Study.csv
#   Results/TableA03_Finite_Cluster_Inference.csv
#   Results/TableA04_Event_Specific_Estimates.csv
#   Results/TableA05_Exploratory_Extensions.csv
# ============================================================


library(tidyverse)
library(fixest)


# ============================================================
# 1. LOAD CONFIGURATION
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
  
  source(config_file[1])
}


# ============================================================
# 2. LOAD CORE DATA AND MODELS
# ============================================================

required_objects <- c(
  "analysis_data",
  "analysis_window_5",
  "analysis_window_5_clean_controls",
  "analysis_window_5_strict",
  "analysis_window_5_strict_clean_controls",
  "data_no2020",
  "data_no_russia",
  "model_with_gdp",
  "model_interaction"
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
# 3. COMMON HELPER FUNCTIONS
# ============================================================


# ------------------------------------------------------------
# Standard pooled model
# ------------------------------------------------------------

fit_pooled <- function(data) {
  
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


# ------------------------------------------------------------
# GDP-controls-only model
# ------------------------------------------------------------

fit_pooled_gdp_only <- function(data) {
  
  feols(
    log_arrivals ~
      treat_post +
      log_gdp_pc +
      gdp_growth |
      iso3c^stack_id +
      year^stack_id,
    data = data,
    cluster = ~iso3c
  )
}


# ------------------------------------------------------------
# No-time-varying-controls model
# ------------------------------------------------------------

fit_pooled_no_controls <- function(data) {
  
  feols(
    log_arrivals ~
      treat_post |
      iso3c^stack_id +
      year^stack_id,
    data = data,
    cluster = ~iso3c
  )
}


# ------------------------------------------------------------
# Interaction model
# ------------------------------------------------------------

fit_income_interaction <- function(data) {
  
  feols(
    log_arrivals ~
      treat_post +
      treat_post:stack_umi +
      log_gdp_pc +
      gdp_growth +
      reer |
      iso3c^stack_id +
      year^stack_id,
    data = data,
    cluster = ~iso3c
  )
}


# ------------------------------------------------------------
# Event-study model
# ------------------------------------------------------------

fit_event_study <- function(data) {
  
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
    data = data,
    cluster = ~iso3c
  )
}


# ------------------------------------------------------------
# Extract one regression coefficient
# ------------------------------------------------------------

extract_term <- function(
    model,
    term,
    specification,
    panel
) {
  
  tibble(
    Panel = panel,
    Specification = specification,
    Estimate = unname(
      coef(model)[term]
    ),
    `Std. Error` = unname(
      se(model)[term]
    ),
    `p-value` = unname(
      pvalue(model)[term]
    ),
    Observations = nobs(model)
  )
}


# ------------------------------------------------------------
# Identify interaction term
# ------------------------------------------------------------

find_interaction_term <- function(model) {
  
  interaction_term <- grep(
    "treat_post:stack_umi|stack_umi:treat_post",
    names(coef(model)),
    value = TRUE
  )
  
  if (length(interaction_term) != 1) {
    stop(
      "Could not uniquely identify the income interaction term."
    )
  }
  
  interaction_term
}


# ------------------------------------------------------------
# Remove FE singletons recursively
# ------------------------------------------------------------

remove_fe_singletons <- function(data) {
  
  cleaned_data <- data
  
  repeat {
    
    n_before <- nrow(cleaned_data)
    
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
    
    n_after <- nrow(cleaned_data)
    
    if (n_after == n_before) {
      break
    }
  }
  
  cleaned_data
}


# ============================================================
# TABLE A1
# ROBUSTNESS OF THE AVERAGE HOSTING EFFECT
# ============================================================


message("\n============================================")
message("CREATING TABLE A1")
message("============================================")


# ------------------------------------------------------------
# A1.1 Full-sample robustness models
# ------------------------------------------------------------

model_no2020 <- fit_pooled(
  data_no2020
)

model_no_russia <- fit_pooled(
  data_no_russia
)


# ------------------------------------------------------------
# A1.2 Equal-event weighting
#
# Construct weights after singleton removal.
# ------------------------------------------------------------

equal_event_data <- remove_fe_singletons(
  analysis_data
) %>%
  
  group_by(
    stack_id
  ) %>%
  
  mutate(
    stack_observations = n(),
    event_weight_raw =
      1 / stack_observations
  ) %>%
  
  ungroup() %>%
  
  mutate(
    event_weight =
      event_weight_raw /
      mean(event_weight_raw)
  )


equal_event_weight_check <- equal_event_data %>%
  
  group_by(
    stack_id
  ) %>%
  
  summarise(
    total_stack_weight =
      sum(event_weight),
    .groups = "drop"
  )


if (
  diff(
    range(
      equal_event_weight_check$total_stack_weight
    )
  ) > 1e-10
) {
  
  stop(
    "Equal-event weights are not equal across stacks."
  )
}


equal_event_data <- equal_event_data %>%
  
  mutate(
    country_stack_fe =
      interaction(
        iso3c,
        stack_id,
        drop = TRUE
      ),
    
    year_stack_fe =
      interaction(
        year,
        stack_id,
        drop = TRUE
      )
  )


model_equal_event <- feols(
  
  log_arrivals ~
    treat_post +
    log_gdp_pc +
    gdp_growth +
    reer |
    country_stack_fe +
    year_stack_fe,
  
  data = equal_event_data,
  
  weights = ~event_weight,
  
  cluster = ~iso3c,
  
  fixef.rm = "none"
)


# ------------------------------------------------------------
# A1.3 Event-window models
# ------------------------------------------------------------

model_window_original <- fit_pooled(
  analysis_window_5
)

model_window_clean <- fit_pooled(
  analysis_window_5_clean_controls
)

model_strict_original <- fit_pooled(
  analysis_window_5_strict
)

model_strict_clean <- fit_pooled(
  analysis_window_5_strict_clean_controls
)


# ------------------------------------------------------------
# A1.4 Covariate-sensitivity models
# ------------------------------------------------------------

model_window_gdp_only <- fit_pooled_gdp_only(
  analysis_window_5_clean_controls
)

model_window_none <- fit_pooled_no_controls(
  analysis_window_5_clean_controls
)

model_strict_gdp_only <- fit_pooled_gdp_only(
  analysis_window_5_strict_clean_controls
)

model_strict_none <- fit_pooled_no_controls(
  analysis_window_5_strict_clean_controls
)


# ------------------------------------------------------------
# A1.5 Assemble Table A1
# ------------------------------------------------------------

table_a1 <- bind_rows(
  
  extract_term(
    model_with_gdp,
    "treat_post",
    "Baseline full sample",
    "A. Full-sample and weighting sensitivity"
  ),
  
  extract_term(
    model_no2020,
    "treat_post",
    "Exclude 2020",
    "A. Full-sample and weighting sensitivity"
  ),
  
  extract_term(
    model_no_russia,
    "treat_post",
    "Exclude Russia 2018 stack",
    "A. Full-sample and weighting sensitivity"
  ),
  
  extract_term(
    model_equal_event,
    "treat_post",
    "Equal-event weighting",
    "A. Full-sample and weighting sensitivity"
  ),
  
  extract_term(
    model_window_original,
    "treat_post",
    "Restricted [-5,+5], original controls",
    "B. Event-window and control-group sensitivity"
  ),
  
  extract_term(
    model_window_clean,
    "treat_post",
    "Restricted [-5,+5], clean controls",
    "B. Event-window and control-group sensitivity"
  ),
  
  extract_term(
    model_strict_original,
    "treat_post",
    "Strict balanced [-5,+5], original controls",
    "B. Event-window and control-group sensitivity"
  ),
  
  extract_term(
    model_strict_clean,
    "treat_post",
    "Strict balanced [-5,+5], clean controls",
    "B. Event-window and control-group sensitivity"
  ),
  
  extract_term(
    model_window_gdp_only,
    "treat_post",
    "Restricted [-5,+5], GDP controls only",
    "C. Covariate sensitivity in clean samples"
  ),
  
  extract_term(
    model_window_none,
    "treat_post",
    "Restricted [-5,+5], no time-varying covariates",
    "C. Covariate sensitivity in clean samples"
  ),
  
  extract_term(
    model_strict_gdp_only,
    "treat_post",
    "Strict balanced [-5,+5], GDP controls only",
    "C. Covariate sensitivity in clean samples"
  ),
  
  extract_term(
    model_strict_none,
    "treat_post",
    "Strict balanced [-5,+5], no time-varying covariates",
    "C. Covariate sensitivity in clean samples"
  )
)


cat("\nTABLE A1\n")

print(
  table_a1,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# A1.6 Validation
# ------------------------------------------------------------

expected_a1_estimates <- c(
  0.0152,
  0.0302,
  0.0472,
  0.0080,
  -0.0319,
  -0.0292,
  0.0328,
  0.0360,
  -0.0241,
  0.0053,
  0.0419,
  0.0675
)


expected_a1_observations <- c(
  1084,
  1047,
  928,
  1084,
  418,
  400,
  264,
  253,
  400,
  400,
  253,
  253
)


if (
  any(
    abs(
      table_a1$Estimate -
      expected_a1_estimates
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Table A1 estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Table A1 estimates match the final paper."
  )
}


if (
  any(
    table_a1$Observations !=
    expected_a1_observations
  )
) {
  
  stop(
    "Table A1 observation counts do not match the final paper."
  )
}


# ------------------------------------------------------------
# A1.7 Save
# ------------------------------------------------------------

table_a1_file <- file.path(
  RESULTS_DIR,
  "TableA01_Robustness.csv"
)


write.csv(
  table_a1,
  table_a1_file,
  row.names = FALSE
)


message(
  "Table A1 saved successfully."
)


# ============================================================
# TABLE A2
# DYNAMIC EVENT-STUDY ESTIMATES AND PRE-TREATMENT TESTS
# ============================================================


message("\n============================================")
message("CREATING TABLE A2")
message("============================================")


# ------------------------------------------------------------
# A2.1 Estimate event-study models
# ------------------------------------------------------------

es_baseline <- fit_event_study(
  analysis_data
)

es_no2020 <- fit_event_study(
  data_no2020
)

es_no_russia <- fit_event_study(
  data_no_russia
)


# ------------------------------------------------------------
# A2.2 Extract baseline dynamic coefficients
# ------------------------------------------------------------

es_terms <- grep(
  "^rel_time_binned::",
  names(coef(es_baseline)),
  value = TRUE
)


es_df <- fixest::degrees_freedom(
  es_baseline,
  type = "t"
)


es_critical <- qt(
  0.975,
  df = es_df
)


table_a2_dynamic <- tibble(
  
  Panel =
    "A. Baseline event-study estimates",
  
  Event_Time =
    as.integer(
      sub(
        "rel_time_binned::(-?[0-9]+):treated",
        "\\1",
        es_terms
      )
    ),
  
  Estimate =
    unname(
      coef(es_baseline)[
        es_terms
      ]
    ),
  
  `Std. Error` =
    unname(
      se(es_baseline)[
        es_terms
      ]
    ),
  
  `p-value` =
    unname(
      pvalue(es_baseline)[
        es_terms
      ]
    )
) %>%
  
  mutate(
    CI_Lower =
      Estimate -
      es_critical *
      `Std. Error`,
    
    CI_Upper =
      Estimate +
      es_critical *
      `Std. Error`
  )


# Add omitted reference period

table_a2_reference <- tibble(
  
  Panel =
    "A. Baseline event-study estimates",
  
  Event_Time = -1L,
  
  Estimate = 0,
  
  `Std. Error` = NA_real_,
  
  `p-value` = NA_real_,
  
  CI_Lower = NA_real_,
  
  CI_Upper = NA_real_
)


table_a2_dynamic <- bind_rows(
  table_a2_dynamic,
  table_a2_reference
) %>%
  
  arrange(
    Event_Time
  )


# ------------------------------------------------------------
# A2.3 Joint pre-treatment test function
# ------------------------------------------------------------

joint_pretrend_test <- function(
    model,
    specification
) {
  
  pre_terms <- paste0(
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
  
  
  beta <- coef(model)[
    pre_terms
  ]
  
  
  V <- vcov(model)[
    pre_terms,
    pre_terms,
    drop = FALSE
  ]
  
  
  q <- length(
    pre_terms
  )
  
  
  df2 <- fixest::degrees_freedom(
    model,
    type = "t"
  )
  
  
  F_stat <- as.numeric(
    t(beta) %*%
      solve(V) %*%
      beta /
      q
  )
  
  
  p_val <- pf(
    F_stat,
    df1 = q,
    df2 = df2,
    lower.tail = FALSE
  )
  
  
  tibble(
    Panel =
      "B. Joint tests of pre-treatment coefficients",
    
    Specification =
      specification,
    
    F_Statistic =
      F_stat,
    
    Numerator_df =
      q,
    
    Denominator_df =
      df2,
    
    `p-value` =
      p_val
  )
}


table_a2_tests <- bind_rows(
  
  joint_pretrend_test(
    es_baseline,
    "Baseline through 2020"
  ),
  
  joint_pretrend_test(
    es_no2020,
    "Exclude 2020"
  ),
  
  joint_pretrend_test(
    es_no_russia,
    "Exclude Russia 2018 stack"
  )
)


cat("\nTABLE A2 - PANEL A\n")

print(
  table_a2_dynamic,
  n = Inf,
  width = Inf
)


cat("\nTABLE A2 - PANEL B\n")

print(
  table_a2_tests,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# A2.4 Validate
# ------------------------------------------------------------

expected_es_coefficients <- c(
  0.1806,
  0.0898,
  0.1087,
  0.0666,
  0.0400,
  0,
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
      table_a2_dynamic$Estimate -
      expected_es_coefficients
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Table A2 dynamic estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Table A2 dynamic estimates match the final paper."
  )
}


expected_pretrend_f <- c(
  1.376,
  1.465,
  0.907
)


if (
  any(
    abs(
      table_a2_tests$F_Statistic -
      expected_pretrend_f
    ) > 0.002
  )
) {
  
  warning(
    "One or more Table A2 pre-treatment F statistics differ from the final paper."
  )
  
} else {
  
  message(
    "Table A2 pre-treatment tests match the final paper."
  )
}


# ------------------------------------------------------------
# A2.5 Create one CSV for the complete table
# ------------------------------------------------------------

table_a2_dynamic_output <- table_a2_dynamic %>%
  
  transmute(
    Panel,
    Row =
      case_when(
        Event_Time == -6 ~ "<= -6",
        Event_Time == 6 ~ ">= +6",
        Event_Time > 0 ~ paste0("+", Event_Time),
        TRUE ~ as.character(Event_Time)
      ),
    Estimate,
    `Std. Error`,
    `p-value`,
    CI_Lower,
    CI_Upper,
    F_Statistic = NA_real_,
    Numerator_df = NA_real_,
    Denominator_df = NA_real_
  )


table_a2_tests_output <- table_a2_tests %>%
  
  transmute(
    Panel,
    Row = Specification,
    Estimate = NA_real_,
    `Std. Error` = NA_real_,
    `p-value`,
    CI_Lower = NA_real_,
    CI_Upper = NA_real_,
    F_Statistic,
    Numerator_df,
    Denominator_df
  )


table_a2 <- bind_rows(
  table_a2_dynamic_output,
  table_a2_tests_output
)


table_a2_file <- file.path(
  RESULTS_DIR,
  "TableA02_Dynamic_Event_Study.csv"
)


write.csv(
  table_a2,
  table_a2_file,
  row.names = FALSE
)


message(
  "Table A2 saved successfully."
)


# ============================================================
# TABLE A3
# FINITE-CLUSTER INFERENCE
# ============================================================


message("\n============================================")
message("CREATING TABLE A3")
message("============================================")


# ------------------------------------------------------------
# A3.1 Prepare singleton-free jackknife sample
# ------------------------------------------------------------

prepare_jackknife_data <- function(data) {
  
  remove_fe_singletons(data) %>%
    
    mutate(
      country_stack_fe =
        interaction(
          iso3c,
          stack_id,
          drop = TRUE
        ),
      
      year_stack_fe =
        interaction(
          year,
          stack_id,
          drop = TRUE
        )
    )
}


# ------------------------------------------------------------
# A3.2 CV3 calculation
#
# CV3 variance:
#
# (G - 1)/G * sum[(beta(-g) - beta_full)^2]
# ------------------------------------------------------------

calculate_cv3 <- function(
    full_estimate,
    leave_one_out_estimates,
    clusters
) {
  
  factor <-
    (clusters - 1) /
    clusters
  
  
  variance <-
    factor *
    sum(
      (
        leave_one_out_estimates -
          full_estimate
      )^2
    )
  
  
  sqrt(
    variance
  )
}


# ------------------------------------------------------------
# A3.3 Pooled CV3 function
# ------------------------------------------------------------

pooled_cv3_results <- function(
    data,
    full_model,
    specification
) {
  
  jack_data <- prepare_jackknife_data(
    data
  )
  
  
  countries <- sort(
    unique(
      jack_data$iso3c
    )
  )
  
  
  G <- length(
    countries
  )
  
  
  loo_results <- map_dfr(
    
    countries,
    
    function(country_removed) {
      
      deleted_data <- jack_data %>%
        
        filter(
          iso3c != country_removed
        ) %>%
        
        droplevels()
      
      
      deleted_model <- feols(
        
        log_arrivals ~
          treat_post +
          log_gdp_pc +
          gdp_growth +
          reer |
          country_stack_fe +
          year_stack_fe,
        
        data = deleted_data,
        
        cluster = ~iso3c,
        
        fixef.rm = "singleton"
      )
      
      
      tibble(
        omitted_country =
          country_removed,
        
        estimate =
          unname(
            coef(deleted_model)[
              "treat_post"
            ]
          )
      )
    }
  )
  
  
  full_estimate <-
    unname(
      coef(full_model)[
        "treat_post"
      ]
    )
  
  
  crv1_se <-
    unname(
      se(full_model)[
        "treat_post"
      ]
    )
  
  
  cv3_se <- calculate_cv3(
    full_estimate,
    loo_results$estimate,
    G
  )
  
  
  df <- G - 1
  
  
  tibble(
    
    Panel =
      specification,
    
    Effect =
      "Pooled hosting effect",
    
    Method =
      c(
        "CRV1",
        "CV3"
      ),
    
    Estimate =
      full_estimate,
    
    `Std. Error` =
      c(
        crv1_se,
        cv3_se
      ),
    
    `p-value` =
      2 *
      pt(
        abs(
          full_estimate /
            c(
              crv1_se,
              cv3_se
            )
        ),
        df = df,
        lower.tail = FALSE
      ),
    
    Clusters =
      G
  )
}


# ------------------------------------------------------------
# A3.4 Interaction CV3 function
# ------------------------------------------------------------

interaction_cv3_results <- function(
    data,
    full_model,
    specification
) {
  
  jack_data <- prepare_jackknife_data(
    data
  )
  
  
  countries <- sort(
    unique(
      jack_data$iso3c
    )
  )
  
  
  G <- length(
    countries
  )
  
  
  interaction_name <-
    find_interaction_term(
      full_model
    )
  
  
  full_coef <- coef(
    full_model
  )
  
  
  full_vcov <- vcov(
    full_model
  )
  
  
  high_full <-
    unname(
      full_coef[
        "treat_post"
      ]
    )
  
  
  difference_full <-
    unname(
      full_coef[
        interaction_name
      ]
    )
  
  
  umi_full <-
    high_full +
    difference_full
  
  
  high_crv1_se <-
    unname(
      se(full_model)[
        "treat_post"
      ]
    )
  
  
  difference_crv1_se <-
    unname(
      se(full_model)[
        interaction_name
      ]
    )
  
  
  umi_crv1_variance <-
    
    full_vcov[
      "treat_post",
      "treat_post"
    ] +
    
    full_vcov[
      interaction_name,
      interaction_name
    ] +
    
    2 *
    full_vcov[
      "treat_post",
      interaction_name
    ]
  
  
  umi_crv1_se <-
    sqrt(
      umi_crv1_variance
    )
  
  
  loo_results <- map_dfr(
    
    countries,
    
    function(country_removed) {
      
      deleted_data <- jack_data %>%
        
        filter(
          iso3c != country_removed
        ) %>%
        
        droplevels()
      
      
      deleted_model <- feols(
        
        log_arrivals ~
          treat_post +
          treat_post:stack_umi +
          log_gdp_pc +
          gdp_growth +
          reer |
          country_stack_fe +
          year_stack_fe,
        
        data = deleted_data,
        
        cluster = ~iso3c,
        
        fixef.rm = "singleton"
      )
      
      
      deleted_interaction <-
        find_interaction_term(
          deleted_model
        )
      
      
      deleted_coef <- coef(
        deleted_model
      )
      
      
      high <-
        unname(
          deleted_coef[
            "treat_post"
          ]
        )
      
      
      difference <-
        unname(
          deleted_coef[
            deleted_interaction
          ]
        )
      
      
      tibble(
        omitted_country =
          country_removed,
        
        high =
          high,
        
        difference =
          difference,
        
        umi =
          high +
          difference
      )
    }
  )
  
  
  high_cv3_se <- calculate_cv3(
    high_full,
    loo_results$high,
    G
  )
  
  
  difference_cv3_se <- calculate_cv3(
    difference_full,
    loo_results$difference,
    G
  )
  
  
  umi_cv3_se <- calculate_cv3(
    umi_full,
    loo_results$umi,
    G
  )
  
  
  df <- G - 1
  
  
  effect_names <- c(
    "High-income host effect",
    "UMI - high-income difference",
    "Implied UMI host effect"
  )
  
  
  estimates <- c(
    high_full,
    difference_full,
    umi_full
  )
  
  
  crv1_se <- c(
    high_crv1_se,
    difference_crv1_se,
    umi_crv1_se
  )
  
  
  cv3_se <- c(
    high_cv3_se,
    difference_cv3_se,
    umi_cv3_se
  )
  
  
  bind_rows(
    
    tibble(
      Panel = specification,
      Effect = effect_names,
      Method = "CRV1",
      Estimate = estimates,
      `Std. Error` = crv1_se
    ),
    
    tibble(
      Panel = specification,
      Effect = effect_names,
      Method = "CV3",
      Estimate = estimates,
      `Std. Error` = cv3_se
    )
  ) %>%
    
    mutate(
      
      `p-value` =
        2 *
        pt(
          abs(
            Estimate /
              `Std. Error`
          ),
          df = df,
          lower.tail = FALSE
        ),
      
      Clusters =
        G
    ) %>%
    
    arrange(
      Effect,
      Method
    )
}


# ------------------------------------------------------------
# A3.5 Calculate final Table A3
# ------------------------------------------------------------

a3_pooled_baseline <- pooled_cv3_results(
  analysis_data,
  model_with_gdp,
  "A. Pooled hosting effect: baseline"
)


a3_pooled_strict <- pooled_cv3_results(
  analysis_window_5_strict_clean_controls,
  model_strict_clean,
  "A. Pooled hosting effect: strict balanced clean"
)


a3_interaction_baseline <- interaction_cv3_results(
  analysis_data,
  model_interaction,
  "B. Income-group heterogeneity: baseline"
)


a3_interaction_strict <- interaction_cv3_results(
  analysis_window_5_strict_clean_controls,
  figure3_model_strict,
  "C. Income-group heterogeneity: strict balanced clean"
)


table_a3 <- bind_rows(
  a3_pooled_baseline,
  a3_pooled_strict,
  a3_interaction_baseline,
  a3_interaction_strict
)


cat("\nTABLE A3\n")

print(
  table_a3,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# A3.6 Validation
# ------------------------------------------------------------

a3_cv3_check <- table_a3 %>%
  
  filter(
    Method == "CV3"
  )


expected_cv3_se <- c(
  0.1320,
  0.0853,
  0.1530,
  0.4090,
  0.3560,
  0.0555,
  0.3086,
  0.2960
)


# Order explicitly for validation

a3_cv3_validation <- bind_rows(
  
  a3_pooled_baseline %>%
    filter(Method == "CV3"),
  
  a3_pooled_strict %>%
    filter(Method == "CV3"),
  
  a3_interaction_baseline %>%
    filter(Method == "CV3") %>%
    slice(
      match(
        c(
          "High-income host effect",
          "UMI - high-income difference",
          "Implied UMI host effect"
        ),
        Effect
      )
    ),
  
  a3_interaction_strict %>%
    filter(Method == "CV3") %>%
    slice(
      match(
        c(
          "High-income host effect",
          "UMI - high-income difference",
          "Implied UMI host effect"
        ),
        Effect
      )
    )
)


if (
  any(
    abs(
      a3_cv3_validation$`Std. Error` -
      expected_cv3_se
    ) > 0.0015
  )
) {
  
  warning(
    "One or more Table A3 CV3 standard errors differ from the final paper."
  )
  
} else {
  
  message(
    "Table A3 CV3 results match the final paper."
  )
}


# ------------------------------------------------------------
# A3.7 Save
# ------------------------------------------------------------

table_a3_file <- file.path(
  RESULTS_DIR,
  "TableA03_Finite_Cluster_Inference.csv"
)


write.csv(
  table_a3,
  table_a3_file,
  row.names = FALSE
)


message(
  "Table A3 saved successfully."
)


# ============================================================
# TABLE A4
# EVENT-SPECIFIC HOSTING ESTIMATES
# ============================================================


message("\n============================================")
message("CREATING TABLE A4")
message("============================================")


# ------------------------------------------------------------
# A4.1 Event display names
# ------------------------------------------------------------

event_display_names <- c(
  
  USA = "Atlanta 1996 Olympics",
  FRA = "France 1998 World Cup",
  AUS = "Sydney 2000 Olympics",
  KOR_JPN = "Korea/Japan 2002 World Cup",
  GRC = "Athens 2004 Olympics",
  DEU = "Germany 2006 World Cup",
  CHN = "Beijing 2008 Olympics",
  ZAF = "South Africa 2010 World Cup",
  GBR = "London 2012 Olympics",
  BRA = "Brazil 2014 World Cup",
  RUS = "Russia 2018 World Cup"
)


event_order <- c(
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
)


# ------------------------------------------------------------
# A4.2 Estimate each event stack separately
# ------------------------------------------------------------

table_a4 <- map_dfr(
  
  event_order,
  
  function(current_stack) {
    
    event_data <- analysis_data %>%
      
      filter(
        stack_id ==
          current_stack
      )
    
    
    event_model <- feols(
      
      log_arrivals ~
        treat_post +
        log_gdp_pc +
        gdp_growth +
        reer |
        iso3c +
        year,
      
      data =
        event_data,
      
      cluster =
        ~iso3c
    )
    
    
    estimate <-
      unname(
        coef(event_model)[
          "treat_post"
        ]
      )
    
    
    raw_se <-
      unname(
        se(event_model)[
          "treat_post"
        ]
      )
    
    
    clusters <-
      n_distinct(
        event_data$iso3c
      )
    
    
    income_group <-
      if_else(
        first(
          event_data$stack_umi
        ) == 1,
        "UMI",
        "High"
      )
    
    
    tibble(
      
      Event =
        unname(
          event_display_names[
            current_stack
          ]
        ),
      
      `Income group` =
        income_group,
      
      Estimate =
        estimate,
      
      `Approx. %` =
        100 *
        (
          exp(estimate) - 1
        ),
      
      `Std. Error` =
        if_else(
          clusters >= 4,
          raw_se,
          NA_real_
        ),
      
      Observations =
        nobs(
          event_model
        ),
      
      Clusters =
        clusters
    )
  }
)


cat("\nTABLE A4\n")

print(
  table_a4,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# A4.3 Validate
# ------------------------------------------------------------

expected_a4_estimates <- c(
  0.156,
  -0.133,
  -0.026,
  0.797,
  0.109,
  0.085,
  0.110,
  0.339,
  -0.155,
  0.345,
  -0.480
)


expected_a4_observations <- c(
  129,
  69,
  104,
  78,
  98,
  103,
  96,
  78,
  123,
  50,
  156
)


if (
  any(
    abs(
      table_a4$Estimate -
      expected_a4_estimates
    ) > 0.002
  )
) {
  
  warning(
    "One or more Table A4 estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Table A4 estimates match the final paper."
  )
}


if (
  any(
    table_a4$Observations !=
    expected_a4_observations
  )
) {
  
  stop(
    "Table A4 observation counts do not match the final paper."
  )
}


# ------------------------------------------------------------
# A4.4 Save
# ------------------------------------------------------------

table_a4_file <- file.path(
  RESULTS_DIR,
  "TableA04_Event_Specific_Estimates.csv"
)


write.csv(
  table_a4,
  table_a4_file,
  row.names = FALSE
)


message(
  "Table A4 saved successfully."
)


# ============================================================
# TABLE A5
# EXPLORATORY EXTENSIONS
# ============================================================


message("\n============================================")
message("CREATING TABLE A5")
message("============================================")


# ============================================================
# A5 PANEL A
# ANNOUNCEMENT-YEAR SPECIFICATION
# ============================================================

announcement_data <- analysis_data %>%
  
  mutate(
    
    rel_time_announcement =
      year -
      stack_announcement_year,
    
    post_announcement =
      as.integer(
        year >=
          stack_announcement_year
      ),
    
    treat_announcement =
      treated *
      post_announcement
  )


model_announcement <- feols(
  
  log_arrivals ~
    treat_announcement +
    log_gdp_pc +
    gdp_growth +
    reer |
    iso3c^stack_id +
    year^stack_id,
  
  data =
    announcement_data,
  
  cluster =
    ~iso3c
)


table_a5_announcement <- tibble(
  
  Panel =
    "A. Announcement-year specification",
  
  Specification =
    "Baseline full sample",
  
  Result =
    "Host x post-announcement",
  
  Estimate =
    unname(
      coef(model_announcement)[
        "treat_announcement"
      ]
    ),
  
  `Std. Error` =
    unname(
      se(model_announcement)[
        "treat_announcement"
      ]
    ),
  
  `p-value` =
    unname(
      pvalue(model_announcement)[
        "treat_announcement"
      ]
    ),
  
  Observations =
    nobs(model_announcement)
)


# ============================================================
# A5 PANEL B
# OLYMPICS VERSUS FIFA WORLD CUP
# ============================================================


# ------------------------------------------------------------
# A5.B1 Add event-type indicator
# ------------------------------------------------------------

add_event_type <- function(data) {
  
  data %>%
    
    mutate(
      olympics =
        as.integer(
          stack_event_type ==
            "Olympics"
        )
    )
}


eventtype_baseline <- add_event_type(
  analysis_data
)

eventtype_window <- add_event_type(
  analysis_window_5_clean_controls
)

eventtype_strict <- add_event_type(
  analysis_window_5_strict_clean_controls
)


# ------------------------------------------------------------
# A5.B2 Estimate event-type model
# ------------------------------------------------------------

fit_event_type_model <- function(data) {
  
  feols(
    
    log_arrivals ~
      treat_post +
      treat_post:olympics +
      log_gdp_pc +
      gdp_growth +
      reer |
      iso3c^stack_id +
      year^stack_id,
    
    data = data,
    
    cluster = ~iso3c
  )
}


eventtype_model_baseline <- fit_event_type_model(
  eventtype_baseline
)

eventtype_model_window <- fit_event_type_model(
  eventtype_window
)

eventtype_model_strict <- fit_event_type_model(
  eventtype_strict
)


# ------------------------------------------------------------
# A5.B3 Extract World Cup, difference, Olympics effect
# ------------------------------------------------------------

extract_event_type_results <- function(
    model,
    specification
) {
  
  interaction_name <- grep(
    "treat_post:olympics|olympics:treat_post",
    names(coef(model)),
    value = TRUE
  )
  
  
  if (length(interaction_name) != 1) {
    stop(
      "Could not identify Olympics interaction term."
    )
  }
  
  
  b <- coef(model)
  V <- vcov(model)
  
  
  world_cup_effect <-
    unname(
      b[
        "treat_post"
      ]
    )
  
  
  world_cup_se <-
    unname(
      se(model)[
        "treat_post"
      ]
    )
  
  
  difference_effect <-
    unname(
      b[
        interaction_name
      ]
    )
  
  
  difference_se <-
    unname(
      se(model)[
        interaction_name
      ]
    )
  
  
  olympics_effect <-
    world_cup_effect +
    difference_effect
  
  
  olympics_variance <-
    
    V[
      "treat_post",
      "treat_post"
    ] +
    
    V[
      interaction_name,
      interaction_name
    ] +
    
    2 *
    V[
      "treat_post",
      interaction_name
    ]
  
  
  olympics_se <-
    sqrt(
      olympics_variance
    )
  
  
  df <-
    fixest::degrees_freedom(
      model,
      type = "t"
    )
  
  
  bind_rows(
    
    tibble(
      Result =
        "World Cup hosting effect",
      Estimate =
        world_cup_effect,
      `Std. Error` =
        world_cup_se
    ),
    
    tibble(
      Result =
        "Olympics - World Cup",
      Estimate =
        difference_effect,
      `Std. Error` =
        difference_se
    ),
    
    tibble(
      Result =
        "Implied Olympics hosting effect",
      Estimate =
        olympics_effect,
      `Std. Error` =
        olympics_se
    )
  ) %>%
    
    mutate(
      
      Panel =
        "B. Olympics versus FIFA World Cup",
      
      Specification =
        specification,
      
      `p-value` =
        2 *
        pt(
          abs(
            Estimate /
              `Std. Error`
          ),
          df = df,
          lower.tail = FALSE
        ),
      
      Observations =
        nobs(model)
    ) %>%
    
    select(
      Panel,
      Specification,
      Result,
      Estimate,
      `Std. Error`,
      `p-value`,
      Observations
    )
}


table_a5_eventtype <- bind_rows(
  
  extract_event_type_results(
    eventtype_model_baseline,
    "Baseline full sample"
  ),
  
  extract_event_type_results(
    eventtype_model_window,
    "Restricted [-5,+5], clean controls"
  ),
  
  extract_event_type_results(
    eventtype_model_strict,
    "Strict balanced [-5,+5], clean controls"
  )
)


# ============================================================
# A5 PANEL C
# CONTINUOUS-INCOME HETEROGENEITY
# ============================================================


continuous_income_data <- analysis_data %>%
  
  mutate(
    
    host_post =
      treat_post,
    
    log_gdp_interaction =
      host_post *
      log_gdp_pc
  )


model_continuous_income <- feols(
  
  log_arrivals ~
    host_post +
    log_gdp_interaction +
    log_gdp_pc +
    gdp_growth +
    reer |
    iso3c^stack_id +
    year^stack_id,
  
  data =
    continuous_income_data,
  
  cluster =
    ~iso3c
)


table_a5_continuous <- tibble(
  
  Panel =
    "C. Continuous-income heterogeneity",
  
  Specification =
    "Baseline full sample",
  
  Result =
    "Hosting x log GDP per capita",
  
  Estimate =
    unname(
      coef(model_continuous_income)[
        "log_gdp_interaction"
      ]
    ),
  
  `Std. Error` =
    unname(
      se(model_continuous_income)[
        "log_gdp_interaction"
      ]
    ),
  
  `p-value` =
    unname(
      pvalue(model_continuous_income)[
        "log_gdp_interaction"
      ]
    ),
  
  Observations =
    nobs(model_continuous_income)
)


# ------------------------------------------------------------
# A5.4 Assemble complete Table A5
# ------------------------------------------------------------

table_a5 <- bind_rows(
  table_a5_announcement,
  table_a5_eventtype,
  table_a5_continuous
)


cat("\nTABLE A5\n")

print(
  table_a5,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# A5.5 Validate announcement result
# ------------------------------------------------------------

if (
  abs(
    table_a5_announcement$Estimate -
    (-0.1015)
  ) > 0.0002
) {
  
  warning(
    "Table A5 announcement-year result differs from the final paper."
  )
  
} else {
  
  message(
    "Table A5 announcement-year result matches the final paper."
  )
}


# ------------------------------------------------------------
# A5.6 Validate event-type results
# ------------------------------------------------------------

expected_eventtype_estimates <- c(
  0.0270,
  -0.0251,
  0.0019,
  -0.0001,
  -0.0609,
  -0.0610,
  0.0925,
  -0.1250,
  -0.0326
)


if (
  any(
    abs(
      table_a5_eventtype$Estimate -
      expected_eventtype_estimates
    ) > 0.0003
  )
) {
  
  warning(
    "One or more Table A5 event-type results differ from the final paper."
  )
  
} else {
  
  message(
    "Table A5 Olympics/World Cup results match the final paper."
  )
}


# ------------------------------------------------------------
# A5.7 Validate continuous-income result
# ------------------------------------------------------------

if (
  abs(
    table_a5_continuous$Estimate -
    0.0787
  ) > 0.0002
) {
  
  warning(
    "Table A5 continuous-income result differs from the final paper."
  )
  
} else {
  
  message(
    "Table A5 continuous-income result matches the final paper."
  )
}


# ------------------------------------------------------------
# A5.8 Save
# ------------------------------------------------------------

table_a5_file <- file.path(
  RESULTS_DIR,
  "TableA05_Exploratory_Extensions.csv"
)


write.csv(
  table_a5,
  table_a5_file,
  row.names = FALSE
)


message(
  "Table A5 saved successfully."
)


# ============================================================
# FINAL APPENDIX-TABLE OUTPUT CHECK
# ============================================================

appendix_table_files <- c(
  table_a1_file,
  table_a2_file,
  table_a3_file,
  table_a4_file,
  table_a5_file
)


missing_appendix_tables <- appendix_table_files[
  !file.exists(
    appendix_table_files
  )
]


if (
  length(
    missing_appendix_tables
  ) > 0
) {
  
  stop(
    paste(
      "The following appendix tables were not created:",
      paste(
        missing_appendix_tables,
        collapse = "\n"
      )
    )
  )
}


message("\n============================================")
message("ALL APPENDIX TABLES A1-A5 COMPLETED")
message("============================================")

message(
  "Table A1: ",
  table_a1_file
)

message(
  "Table A2: ",
  table_a2_file
)

message(
  "Table A3: ",
  table_a3_file
)

message(
  "Table A4: ",
  table_a4_file
)

message(
  "Table A5: ",
  table_a5_file
)