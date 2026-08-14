# ============================================================
# ECON 899 REPLICATION PACKAGE
# Table05_Main_Results.R
#
# Purpose:
# Reproduce Table 5 of the paper:
# "Mega-Event Hosting and International Tourist Arrivals"
#
# The reported results are calculated directly from the
# regression models estimated in 00_models.R.
#
# Output:
#   Results/Table05_Main_Results.csv
# ============================================================


library(tidyverse)
library(fixest)


# ============================================================
# 1. LOAD CONFIGURATION IF NECESSARY
# ============================================================

if (
  !exists("RESULTS_DIR") ||
  !exists("ANALYSIS_DIR")
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
# 2. MAKE SURE CORE MODELS EXIST
# ============================================================

required_models <- c(
  "model_with_gdp",
  "figure4_model_restricted",
  "figure4_model_strict",
  "model_interaction",
  "figure3_model_restricted",
  "figure3_model_strict"
)


models_available <-
  vapply(
    required_models,
    exists,
    logical(1),
    envir = .GlobalEnv
  )


if (!all(models_available)) {
  
  message(
    "Core models are not currently loaded. Running 00_models.R."
  )
  
  source(
    file.path(
      ANALYSIS_DIR,
      "00_models.R"
    )
  )
}


# ============================================================
# 3. DEFINE MODEL LISTS
# ============================================================

pooled_models <- list(
  
  "Baseline" =
    model_with_gdp,
  
  "Restricted clean" =
    figure4_model_restricted,
  
  "Strict balanced clean" =
    figure4_model_strict
)


interaction_models <- list(
  
  "Baseline" =
    model_interaction,
  
  "Restricted clean" =
    figure3_model_restricted,
  
  "Strict balanced clean" =
    figure3_model_strict
)


# ============================================================
# 4. HELPER: FIND INCOME INTERACTION TERM
# ============================================================

find_interaction_term <- function(model) {
  
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
      "Could not uniquely identify the income-group interaction term."
    )
  }
  
  
  interaction_term
}


# ============================================================
# 5. HELPER: EXTRACT POOLED RESULT
# ============================================================

extract_pooled <- function(
    model,
    specification
) {
  
  tibble(
    
    Specification =
      specification,
    
    pooled_effect =
      unname(
        coef(model)[
          "treat_post"
        ]
      ),
    
    pooled_se =
      unname(
        se(model)[
          "treat_post"
        ]
      ),
    
    observations =
      nobs(model),
    
    country_clusters =
      as.integer(
        fixest::degrees_freedom(
          model,
          type = "t"
        ) + 1
      )
  )
}


# ============================================================
# 6. HELPER: EXTRACT INCOME-GROUP RESULTS
# ============================================================

extract_income_results <- function(
    model,
    specification
) {
  
  interaction_term <-
    find_interaction_term(
      model
    )
  
  
  model_coefficients <-
    coef(model)
  
  
  variance_matrix <-
    vcov(model)
  
  
  high_income_effect <-
    unname(
      model_coefficients[
        "treat_post"
      ]
    )
  
  
  high_income_se <-
    unname(
      se(model)[
        "treat_post"
      ]
    )
  
  
  umi_high_difference <-
    unname(
      model_coefficients[
        interaction_term
      ]
    )
  
  
  umi_high_difference_se <-
    unname(
      se(model)[
        interaction_term
      ]
    )
  
  
  implied_umi_effect <-
    high_income_effect +
    umi_high_difference
  
  
  implied_umi_variance <-
    
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
  
  
  implied_umi_se <-
    sqrt(
      implied_umi_variance
    )
  
  
  tibble(
    
    Specification =
      specification,
    
    high_income_effect =
      high_income_effect,
    
    high_income_se =
      high_income_se,
    
    umi_high_difference =
      umi_high_difference,
    
    umi_high_difference_se =
      umi_high_difference_se,
    
    implied_umi_effect =
      implied_umi_effect,
    
    implied_umi_se =
      implied_umi_se
  )
}


# ============================================================
# 7. EXTRACT POOLED RESULTS
# ============================================================

pooled_results <-
  map2_dfr(
    
    pooled_models,
    
    names(
      pooled_models
    ),
    
    ~extract_pooled(
      .x,
      .y
    )
  )


# ============================================================
# 8. EXTRACT INCOME-GROUP RESULTS
# ============================================================

income_results <-
  map2_dfr(
    
    interaction_models,
    
    names(
      interaction_models
    ),
    
    ~extract_income_results(
      .x,
      .y
    )
  )


# ============================================================
# 9. EVENT-STACK COUNTS
# ============================================================

event_stack_counts <- tibble(
  
  Specification = c(
    "Baseline",
    "Restricted clean",
    "Strict balanced clean"
  ),
  
  event_stacks = c(
    
    n_distinct(
      analysis_data$stack_id
    ),
    
    n_distinct(
      analysis_window_5_clean_controls$stack_id
    ),
    
    n_distinct(
      analysis_window_5_strict_clean_controls$stack_id
    )
  )
)


# ============================================================
# 10. CONSTRUCT TABLE 5
# ============================================================

table05 <-
  pooled_results %>%
  
  left_join(
    income_results,
    by = "Specification"
  ) %>%
  
  left_join(
    event_stack_counts,
    by = "Specification"
  ) %>%
  
  mutate(
    
    time_varying_controls =
      "Yes",
    
    country_stack_fixed_effects =
      "Yes",
    
    year_stack_fixed_effects =
      "Yes"
  ) %>%
  
  select(
    
    Specification,
    
    pooled_effect,
    pooled_se,
    
    high_income_effect,
    high_income_se,
    
    umi_high_difference,
    umi_high_difference_se,
    
    implied_umi_effect,
    implied_umi_se,
    
    time_varying_controls,
    country_stack_fixed_effects,
    year_stack_fixed_effects,
    
    observations,
    country_clusters,
    event_stacks
  )


# ============================================================
# 11. DISPLAY TABLE
# ============================================================

cat("\n============================================\n")
cat("TABLE 5: MAIN REGRESSION RESULTS\n")
cat("============================================\n")


print(
  table05,
  n = Inf,
  width = Inf
)


# ============================================================
# 12. VALIDATE POOLED RESULTS
# ============================================================

expected_pooled_effects <- c(
  0.0152,
  -0.0292,
  0.0360
)


expected_pooled_se <- c(
  0.0927,
  0.0637,
  0.0577
)


if (
  any(
    abs(
      table05$pooled_effect -
      expected_pooled_effects
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Table 5 pooled estimates differ from the final paper."
  )
  
} else {
  
  message(
    "Table 5 pooled estimates match the final paper."
  )
}


if (
  any(
    abs(
      table05$pooled_se -
      expected_pooled_se
    ) > 0.0002
  )
) {
  
  warning(
    "One or more Table 5 pooled standard errors differ from the final paper."
  )
  
} else {
  
  message(
    "Table 5 pooled standard errors match the final paper."
  )
}


# ============================================================
# 13. VALIDATE INCOME-GROUP RESULTS
# ============================================================
#
# All income-group quantities are reconstructed directly from
# the regression models and their clustered covariance matrices.
#
# Note:
# The restricted-sample implied UMI standard error is about
# 0.1466 in the exact reconstruction. The final paper reports
# 0.1470. The original R console output displayed this value
# as 0.147 at limited print precision, so this one paper entry
# reflects a minor rounding/transcription difference.
# ============================================================


expected_high_income <- c(
  0.0963,
  0.0148,
  0.0369
)


expected_high_income_se <- c(
  0.1064,
  0.0374,
  0.0401
)


expected_umi_difference <- c(
  -0.2220,
  -0.1117,
  -0.0023
)


expected_umi_difference_se <- c(
  0.2526,
  0.1539,
  0.1535
)


expected_implied_umi <- c(
  -0.1257,
  -0.0970,
  0.0346
)


# Exact reconstruction targets.
#
# The second value is approximately 0.1466.
# The final paper displays 0.1470 because the earlier
# console output displayed the value as 0.147.

expected_implied_umi_se <- c(
  0.2125,
  0.1466,
  0.1446
)


income_checks <- c(
  
  abs(
    table05$high_income_effect -
      expected_high_income
  ),
  
  abs(
    table05$high_income_se -
      expected_high_income_se
  ),
  
  abs(
    table05$umi_high_difference -
      expected_umi_difference
  ),
  
  abs(
    table05$umi_high_difference_se -
      expected_umi_difference_se
  ),
  
  abs(
    table05$implied_umi_effect -
      expected_implied_umi
  ),
  
  abs(
    table05$implied_umi_se -
      expected_implied_umi_se
  )
)


if (
  any(
    income_checks > 0.0002
  )
) {
  
  warning(
    paste(
      "One or more reconstructed Table 5 income-group",
      "results differ from the expected model results."
    )
  )
  
} else {
  
  message(
    "Table 5 income-group model results reproduced successfully."
  )
}


# Document the minor paper-display discrepancy explicitly

restricted_umi_se <-
  table05$implied_umi_se[
    table05$Specification ==
      "Restricted clean"
  ]


message(
  paste0(
    "Note: restricted implied UMI SE = ",
    format(
      restricted_umi_se,
      digits = 7
    ),
    "; final paper displays 0.1470."
  )
)
# ============================================================
# 14. VALIDATE SAMPLE INFORMATION
# ============================================================

expected_observations <- c(
  1084,
  400,
  253
)


expected_clusters <- c(
  24,
  24,
  17
)


expected_stacks <- c(
  11,
  11,
  7
)


if (
  any(
    table05$observations !=
    expected_observations
  )
) {
  
  stop(
    "Table 5 observation counts do not match the final paper."
  )
}


if (
  any(
    table05$country_clusters !=
    expected_clusters
  )
) {
  
  stop(
    "Table 5 country-cluster counts do not match the final paper."
  )
}


if (
  any(
    table05$event_stacks !=
    expected_stacks
  )
) {
  
  stop(
    "Table 5 event-stack counts do not match the final paper."
  )
}


message(
  "Table 5 sample information matches the final paper."
)


# ============================================================
# 15. WRITE TABLE TO RESULTS FOLDER
# ============================================================

table05_file <- file.path(
  RESULTS_DIR,
  "Table05_Main_Results.csv"
)


write.csv(
  table05,
  table05_file,
  row.names = FALSE
)


# ============================================================
# 16. VERIFY OUTPUT FILE
# ============================================================

if (!file.exists(table05_file)) {
  
  stop(
    "Table05_Main_Results.csv was not created."
  )
}


table05_test <- read.csv(
  table05_file,
  stringsAsFactors = FALSE
)


if (nrow(table05_test) != 3) {
  
  stop(
    "Saved Table 5 should contain three model specifications."
  )
}


message(
  "Table 5 saved to: ",
  table05_file
)


message(
  "Table05_Main_Results.R completed successfully."
)