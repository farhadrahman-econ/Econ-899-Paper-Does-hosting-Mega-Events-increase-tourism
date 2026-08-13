# ECON 899 REPLICATION PACKAGE
# Programs/02_analysis/00_models.R
#
# Purpose:
# Estimate the core stacked difference-in-differences models used in the main paper.
#
# Models:
#   1. Baseline pooled hosting effect
#   2. Restricted [-5,+5] pooled effect, clean controls
#   3. Strict balanced [-5,+5] pooled effect, clean controls
#   4. Baseline income-group interaction
#   5. Restricted income-group interaction
#   6. Strict balanced income-group interaction
#
# Input:
#   Data/data_for_analysis/analysis_samples.rds
#
# These models are used by the main tables and figures.



library(tidyverse)
library(fixest)



# 1. LOAD CONFIGURATION IF NECESSARY
# ============================================================

if (!exists("ANALYSIS_DATA_DIR")) {
  
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
# 2. LOAD ANALYSIS SAMPLES
# ============================================================

analysis_samples_file <- file.path(
  ANALYSIS_DATA_DIR,
  "analysis_samples.rds"
)


if (!file.exists(analysis_samples_file)) {
  stop(
    paste(
      "Required analysis-sample file was not found:",
      analysis_samples_file,
      "\nRun Programs/01_dataprep/main.R first."
    )
  )
}


analysis_samples <- readRDS(
  analysis_samples_file
)


analysis_data <-
  analysis_samples$analysis_data

analysis_window_5 <-
  analysis_samples$analysis_window_5

analysis_window_5_clean_controls <-
  analysis_samples$analysis_window_5_clean_controls

analysis_window_5_strict <-
  analysis_samples$analysis_window_5_strict

analysis_window_5_strict_clean_controls <-
  analysis_samples$analysis_window_5_strict_clean_controls

data_no2020 <-
  analysis_samples$data_no2020

data_no_russia <-
  analysis_samples$data_no_russia


# ============================================================
# 3. HELPER: POOLED HOSTING MODEL
# ============================================================

fit_main_model <- function(data) {
  
  feols(
    
    log_arrivals ~
      treat_post +
      log_gdp_pc +
      gdp_growth +
      reer |
      
      iso3c^stack_id +
      year^stack_id,
    
    data = data,
    
    cluster =
      ~iso3c
  )
}


# ============================================================
# 4. HELPER: INCOME-GROUP INTERACTION MODEL
# ============================================================

fit_interaction_model <- function(data) {
  
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
    
    cluster =
      ~iso3c
  )
}


# ============================================================
# 5. BASELINE POOLED MODEL
# ============================================================

model_with_gdp <-
  fit_main_model(
    analysis_data
  )


# ============================================================
# 6. RESTRICTED [-5,+5] CLEAN-CONTROL POOLED MODEL
# ============================================================

figure4_model_restricted <-
  fit_main_model(
    analysis_window_5_clean_controls
  )


# ============================================================
# 7. STRICT BALANCED [-5,+5] CLEAN-CONTROL POOLED MODEL
# ============================================================

figure4_model_strict <-
  fit_main_model(
    analysis_window_5_strict_clean_controls
  )


# ============================================================
# 8. BASELINE INCOME-GROUP INTERACTION MODEL
# ============================================================

model_interaction <-
  fit_interaction_model(
    analysis_data
  )


# ============================================================
# 9. RESTRICTED CLEAN INCOME-GROUP INTERACTION MODEL
# ============================================================

figure3_model_restricted <-
  fit_interaction_model(
    analysis_window_5_clean_controls
  )


# ============================================================
# 10. STRICT BALANCED CLEAN INCOME-GROUP INTERACTION MODEL
# ============================================================

figure3_model_strict <-
  fit_interaction_model(
    analysis_window_5_strict_clean_controls
  )


# ============================================================
# 11. HELPER: EXTRACT POOLED EFFECT
# ============================================================

extract_pooled_result <- function(
    model,
    specification
) {
  
  tibble(
    
    specification =
      specification,
    
    estimate =
      unname(
        coef(model)[
          "treat_post"
        ]
      ),
    
    standard_error =
      unname(
        se(model)[
          "treat_post"
        ]
      ),
    
    p_value =
      unname(
        pvalue(model)[
          "treat_post"
        ]
      ),
    
    observations =
      nobs(model),
    
    country_clusters =
      fixest::degrees_freedom(
        model,
        type = "t"
      ) + 1
  )
}


# ============================================================
# 12. POOLED MODEL CHECK
# ============================================================

pooled_model_check <-
  bind_rows(
    
    extract_pooled_result(
      model_with_gdp,
      "Baseline"
    ),
    
    extract_pooled_result(
      figure4_model_restricted,
      "Restricted -5/+5 clean"
    ),
    
    extract_pooled_result(
      figure4_model_strict,
      "Strict balanced -5/+5 clean"
    )
  )


cat("\n============================================\n")
cat("POOLED HOSTING MODELS\n")
cat("============================================\n")

print(
  pooled_model_check,
  n = Inf
)


# ============================================================
# 13. HELPER: FIND INTERACTION TERM
# ============================================================

get_interaction_term <- function(model) {
  
  interaction_term <- grep(
    "treat_post:stack_umi|stack_umi:treat_post",
    names(
      coef(model)
    ),
    value = TRUE
  )
  
  if (length(interaction_term) != 1) {
    stop(
      "Could not uniquely identify the income-group interaction term."
    )
  }
  
  interaction_term
}


# ============================================================
# 14. HELPER: EXTRACT INCOME-GROUP EFFECTS
# ============================================================

extract_interaction_summary <- function(
    model,
    specification
) {
  
  interaction_term <-
    get_interaction_term(
      model
    )
  
  
  coefficients <-
    coef(model)
  
  variance_matrix <-
    vcov(model)
  
  
  high_income_effect <-
    unname(
      coefficients[
        "treat_post"
      ]
    )
  
  
  high_income_se <-
    unname(
      se(model)[
        "treat_post"
      ]
    )
  
  
  income_group_difference <-
    unname(
      coefficients[
        interaction_term
      ]
    )
  
  
  interaction_se <-
    unname(
      se(model)[
        interaction_term
      ]
    )
  
  
  upper_middle_effect <-
    high_income_effect +
    income_group_difference
  
  
  upper_middle_variance <-
    variance_matrix[
      "treat_post",
      "treat_post"
    ] +
    
    variance_matrix[
      interaction_term,
      interaction_term
    ] +
    
    2 *
    variance_matrix[
      "treat_post",
      interaction_term
    ]
  
  
  upper_middle_se <-
    sqrt(
      upper_middle_variance
    )
  
  
  model_df <-
    fixest::degrees_freedom(
      model,
      type = "t"
    )
  
  
  upper_middle_t <-
    upper_middle_effect /
    upper_middle_se
  
  
  upper_middle_p_value <-
    2 *
    pt(
      abs(
        upper_middle_t
      ),
      df = model_df,
      lower.tail = FALSE
    )
  
  
  tibble(
    
    specification =
      specification,
    
    high_income_effect =
      high_income_effect,
    
    high_income_se =
      high_income_se,
    
    high_income_p_value =
      unname(
        pvalue(model)[
          "treat_post"
        ]
      ),
    
    income_group_difference =
      income_group_difference,
    
    interaction_se =
      interaction_se,
    
    interaction_p_value =
      unname(
        pvalue(model)[
          interaction_term
        ]
      ),
    
    upper_middle_effect =
      upper_middle_effect,
    
    upper_middle_se =
      upper_middle_se,
    
    upper_middle_p_value =
      upper_middle_p_value,
    
    observations =
      nobs(model),
    
    country_clusters =
      model_df + 1
  )
}


# ============================================================
# 15. INCOME-GROUP MODEL CHECK
# ============================================================

income_model_check <-
  bind_rows(
    
    extract_interaction_summary(
      model_interaction,
      "Baseline"
    ),
    
    extract_interaction_summary(
      figure3_model_restricted,
      "Restricted -5/+5 clean"
    ),
    
    extract_interaction_summary(
      figure3_model_strict,
      "Strict balanced -5/+5 clean"
    )
  )


cat("\n============================================\n")
cat("INCOME-GROUP INTERACTION MODELS\n")
cat("============================================\n")

print(
  income_model_check,
  n = Inf,
  width = Inf
)


# ============================================================
# 16. REPLICATION CHECKPOINTS
# ============================================================

expected_pooled_estimates <- c(
  0.0152,
  -0.0292,
  0.0360
)

actual_pooled_estimates <-
  pooled_model_check$estimate


if (
  any(
    abs(
      actual_pooled_estimates -
      expected_pooled_estimates
    ) > 0.0002
  )
) {
  
  warning(
    paste(
      "One or more pooled estimates differ from",
      "the values reported in the final paper."
    )
  )
  
} else {
  
  message(
    "All main pooled estimates match the final paper."
  )
}


expected_pooled_observations <- c(
  1084,
  400,
  253
)


if (
  any(
    pooled_model_check$observations !=
    expected_pooled_observations
  )
) {
  
  warning(
    "One or more pooled-model observation counts differ from the final paper."
  )
  
} else {
  
  message(
    "All main regression observation counts match the final paper."
  )
}


# ============================================================
# 17. COMPLETE
# ============================================================

message(
  "00_models.R completed successfully."
)