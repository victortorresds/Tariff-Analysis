# ============================================================================
# DIFFERENCE-IN-DIFFERENCES ANALYSIS: TARIFF IMPACTS ON US IMPORTS
# ============================================================================
# Author: Victor Torres
# Course: DATA 698 - Final Project
# Date: November 2025
#
# Research Question: What is the causal effect of Section 301 tariffs 
# on US imports from China?
#
# Method: Difference-in-Differences
# - Treatment: China (25% tariffs starting July 2018)
# - Control: Mexico & Canada (USMCA exempt from Section 301 tariffs)
# - Before: 2015-2017 (pre-tariff)
# - After: 2018-2025 (post-tariff)
# ============================================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(lmtest)      # For Chow test and diagnostics
library(sandwich)    # For robust standard errors
library(broom)       # For tidy regression output
library(scales)      # For plotting
library(patchwork)   # For multi-panel plots

# Set working directory (adjust to your path)
# setwd("~/Final_Project")

cat("=== LOADING IMPORT DATA ===\n\n")

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================

# Load China imports
china <- read_csv("data/fred_imports/imports_china_IMPCH.csv") %>%
  mutate(
    date = ymd(observation_date),
    country = "China",
    imports = IMPCH
  ) %>%
  select(date, country, imports)

cat("✓ China imports loaded:", nrow(china), "observations\n")

# Load Mexico imports
mexico <- read_csv("data/fred_imports/imports_mexico_IMPMX.csv") %>%
  mutate(
    date = ymd(observation_date),
    country = "Mexico",
    imports = IMPMX
  ) %>%
  select(date, country, imports)

cat("✓ Mexico imports loaded:", nrow(mexico), "observations\n")

# Load Canada imports
canada <- read_csv("data/fred_imports/imports_canada_IMPCA.csv") %>%
  mutate(
    date = ymd(observation_date),
    country = "Canada",
    imports = IMPCA
  ) %>%
  select(date, country, imports)

cat("✓ Canada imports loaded:", nrow(canada), "observations\n")

# Load Brazil imports (optional - for additional analysis)
brazil <- read_csv("data/fred_imports/imports_brazil_IMP5350.csv") %>%
  mutate(
    date = ymd(observation_date),
    country = "Brazil",
    imports = IMP5350
  ) %>%
  select(date, country, imports)

cat("✓ Brazil imports loaded:", nrow(brazil), "observations\n")

# ============================================================================
# 2. COMBINE AND PREPARE FOR DiD
# ============================================================================

cat("\n=== PREPARING DiD FRAMEWORK ===\n\n")

# Combine all countries
imports_all <- bind_rows(china, mexico, canada, brazil) %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = quarter(date),
    
    # Define treatment and control groups
    treatment = ifelse(country == "China", 1, 0),
    control_group = ifelse(country %in% c("Mexico", "Canada"), 1, 0),
    
    # Define time periods
    # Section 301 tariffs began July 6, 2018
    post_tariff = ifelse(date >= "2018-07-01", 1, 0),
    
    # Alternative specifications
    post_2018 = ifelse(year >= 2018, 1, 0),
    post_2019 = ifelse(year >= 2019, 1, 0),  # Full-year effect
    post_2025 = ifelse(year >= 2025, 1, 0),  # Universal tariffs
    
    # Tariff periods (from your midterm)
    tariff_period = case_when(
      year <= 2017 ~ "Pre-Tariff (2015-2017)",
      year >= 2018 & year <= 2024 ~ "Targeted Tariffs (2018-2024)",
      year >= 2025 ~ "Universal Tariffs (2025)",
      TRUE ~ "Other"
    ),
    
    # Log of imports (for elasticity interpretation)
    log_imports = log(imports)
  )

cat("Combined dataset:\n")
cat("  Total observations:", nrow(imports_all), "\n")
cat("  Countries:", unique(imports_all$country), "\n")
cat("  Date range:", min(imports_all$date), "to", max(imports_all$date), "\n\n")

# Create DiD dataset (China vs. Mexico/Canada only)
did_data <- imports_all %>%
  filter(country %in% c("China", "Mexico", "Canada")) %>%
  mutate(
    # DiD interaction term
    did_term = treatment * post_tariff
  )

cat("DiD dataset:\n")
cat("  Treatment observations (China):", sum(did_data$treatment == 1), "\n")
cat("  Control observations (Mexico + Canada):", sum(did_data$control_group == 1), "\n")
cat("  Pre-tariff observations:", sum(did_data$post_tariff == 0), "\n")
cat("  Post-tariff observations:", sum(did_data$post_tariff == 1), "\n\n")

# ============================================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================================

cat("=== DESCRIPTIVE STATISTICS ===\n\n")

# Summary by country and period
summary_stats <- did_data %>%
  group_by(country, tariff_period) %>%
  summarise(
    n_obs = n(),
    mean_imports = mean(imports, na.rm = TRUE),
    sd_imports = sd(imports, na.rm = TRUE),
    min_imports = min(imports, na.rm = TRUE),
    max_imports = max(imports, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# Calculate percent changes
baseline_imports <- did_data %>%
  filter(year <= 2017) %>%
  group_by(country) %>%
  summarise(baseline = mean(imports, na.rm = TRUE), .groups = 'drop')

period_changes <- did_data %>%
  group_by(country, tariff_period) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  left_join(baseline_imports, by = "country") %>%
  mutate(pct_change = ((avg_imports - baseline) / baseline) * 100)

cat("\n\nPercent changes from baseline (2015-2017):\n")
print(period_changes %>% select(country, tariff_period, pct_change))

# ============================================================================
# 4. PARALLEL TRENDS TEST
# ============================================================================

cat("\n\n=== TESTING PARALLEL TRENDS ASSUMPTION ===\n\n")

# Test: Do China and Mexico/Canada have similar trends pre-2018?
# This is CRITICAL for DiD validity

# Pre-trend regression (2015-2017 only)
pre_trend_data <- did_data %>%
  filter(year <= 2017) %>%
  mutate(time_trend = as.numeric(date - min(date)) / 30)  # Months since start

pre_trend_model <- lm(imports ~ treatment + time_trend + treatment:time_trend, 
                      data = pre_trend_data)

cat("Pre-Trend Test Results:\n")
cat("----------------------------\n")
print(summary(pre_trend_model))

# Key test: Is the treatment:time_trend coefficient significant?
# If NOT significant → parallel trends hold ✓
# If significant → parallel trends violated ✗

pre_trend_coef <- coef(pre_trend_model)["treatment:time_trend"]
pre_trend_pval <- summary(pre_trend_model)$coefficients["treatment:time_trend", "Pr(>|t|)"]

cat("\n\nParallel Trends Test:\n")
cat("  Coefficient on Treatment × Time:", round(pre_trend_coef, 3), "\n")
cat("  P-value:", round(pre_trend_pval, 4), "\n")

if (pre_trend_pval > 0.05) {
  cat("  ✓ PASS: Parallel trends assumption holds (p > 0.05)\n")
  cat("    China and control group had similar pre-treatment trends.\n")
} else {
  cat("  ✗ FAIL: Parallel trends violated (p < 0.05)\n")
  cat("    WARNING: DiD may not be appropriate. Consider alternative methods.\n")
}

# ============================================================================
# 5. MAIN DiD REGRESSION
# ============================================================================

cat("\n\n=== MAIN DiD REGRESSION ===\n\n")

# Basic DiD specification
did_model_1 <- lm(imports ~ treatment + post_tariff + did_term, 
                  data = did_data)

cat("Model 1: Basic DiD\n")
cat("----------------------------\n")
print(summary(did_model_1))

# Extract DiD estimate
did_estimate <- coef(did_model_1)["did_term"]
did_se <- summary(did_model_1)$coefficients["did_term", "Std. Error"]
did_pval <- summary(did_model_1)$coefficients["did_term", "Pr(>|t|)"]

cat("\n\nDiD Estimate (Causal Effect of Tariffs):\n")
cat("  Coefficient:", round(did_estimate, 2), "million USD\n")
cat("  Standard Error:", round(did_se, 2), "\n")
cat("  P-value:", round(did_pval, 4), "\n")

if (did_pval < 0.001) {
  cat("  *** Highly significant (p < 0.001)\n")
} else if (did_pval < 0.01) {
  cat("  ** Significant (p < 0.01)\n")
} else if (did_pval < 0.05) {
  cat("  * Significant (p < 0.05)\n")
} else {
  cat("  Not significant (p > 0.05)\n")
}

# Interpretation
if (did_estimate < 0) {
  cat("\n  Interpretation: Section 301 tariffs caused Chinese imports to")
  cat("\n  DECLINE by", abs(round(did_estimate, 0)), "million USD per month,")
  cat("\n  relative to Mexico/Canada.\n")
} else {
  cat("\n  Interpretation: Unexpected positive coefficient. Investigate further.\n")
}

# ============================================================================
# 6. ROBUSTNESS CHECKS
# ============================================================================

cat("\n\n=== ROBUSTNESS CHECKS ===\n\n")

# Model 2: DiD with time fixed effects (year dummies)
did_model_2 <- lm(imports ~ treatment + post_tariff + did_term + factor(year), 
                  data = did_data)

cat("Model 2: DiD with Year Fixed Effects\n")
cat("DiD Coefficient:", round(coef(did_model_2)["did_term"], 2), "\n")
cat("P-value:", round(summary(did_model_2)$coefficients["did_term", "Pr(>|t|)"], 4), "\n\n")

# Model 3: DiD with month fixed effects (control for seasonality)
did_model_3 <- lm(imports ~ treatment + post_tariff + did_term + factor(month), 
                  data = did_data)

cat("Model 3: DiD with Month Fixed Effects\n")
cat("DiD Coefficient:", round(coef(did_model_3)["did_term"], 2), "\n")
cat("P-value:", round(summary(did_model_3)$coefficients["did_term", "Pr(>|t|)"], 4), "\n\n")

# Model 4: DiD with both year and month fixed effects
did_model_4 <- lm(imports ~ treatment + post_tariff + did_term + 
                    factor(year) + factor(month), 
                  data = did_data)

cat("Model 4: DiD with Year + Month Fixed Effects\n")
cat("DiD Coefficient:", round(coef(did_model_4)["did_term"], 2), "\n")
cat("P-value:", round(summary(did_model_4)$coefficients["did_term", "Pr(>|t|)"], 4), "\n\n")

# Model 5: Log specification (for percentage interpretation)
did_model_5 <- lm(log_imports ~ treatment + post_tariff + did_term, 
                  data = did_data)

cat("Model 5: Log-Linear DiD (Percentage Effects)\n")
log_did_coef <- coef(did_model_5)["did_term"]
pct_effect <- (exp(log_did_coef) - 1) * 100
cat("DiD Coefficient:", round(log_did_coef, 4), "\n")
cat("Percentage Effect:", round(pct_effect, 2), "%\n")
cat("P-value:", round(summary(did_model_5)$coefficients["did_term", "Pr(>|t|)"], 4), "\n\n")

# Model 6: Robust standard errors
library(lmtest)
library(sandwich)

did_model_robust <- coeftest(did_model_1, vcov = vcovHC(did_model_1, type = "HC1"))

cat("Model 6: Basic DiD with Robust Standard Errors\n")
print(did_model_robust)

# ============================================================================
# 7. REGRESSION TABLE FOR REPORT
# ============================================================================

cat("\n\n=== CREATING REGRESSION TABLE ===\n\n")

# Combine all models into a table
models_list <- list(
  "Basic DiD" = did_model_1,
  "Year FE" = did_model_2,
  "Month FE" = did_model_3,
  "Year+Month FE" = did_model_4,
  "Log DiD" = did_model_5
)

# Extract key statistics
regression_table <- map_df(names(models_list), function(name) {
  model <- models_list[[name]]
  did_coef <- coef(model)["did_term"]
  did_se <- summary(model)$coefficients["did_term", "Std. Error"]
  did_pval <- summary(model)$coefficients["did_term", "Pr(>|t|)"]
  r2 <- summary(model)$r.squared
  n_obs <- nobs(model)
  
  tibble(
    Model = name,
    DiD_Coefficient = did_coef,
    Std_Error = did_se,
    P_Value = did_pval,
    R_Squared = r2,
    N = n_obs
  )
})

print(regression_table)

# Save for later use
saveRDS(regression_table, "outputs/tables/did_regression_table.rds")

# ============================================================================
# 8. SAVE PROCESSED DATA AND MODELS
# ============================================================================

cat("\n\n=== SAVING OUTPUTS ===\n\n")

# Save cleaned data
write_csv(did_data, "data/processed/did_analysis_data.csv")
cat("✓ Saved: data/processed/did_analysis_data.csv\n")

# Save model objects
saveRDS(did_model_1, "outputs/models/did_model_basic.rds")
saveRDS(did_model_4, "outputs/models/did_model_full_fe.rds")
saveRDS(did_model_5, "outputs/models/did_model_log.rds")
cat("✓ Saved: Model objects\n")

# Save summary statistics
write_csv(summary_stats, "outputs/tables/import_summary_stats.csv")
write_csv(period_changes, "outputs/tables/period_changes.csv")
cat("✓ Saved: Summary tables\n")

cat("\n=== DiD ANALYSIS COMPLETE ===\n")
cat("\nKey Results:\n")
cat("  1. Parallel Trends: ", ifelse(pre_trend_pval > 0.05, "PASS ✓", "FAIL ✗"), "\n")
cat("  2. DiD Estimate:", round(did_estimate, 0), "million USD\n")
cat("  3. Statistical Significance: p =", round(did_pval, 4), "\n")
cat("  4. Percentage Effect:", round(pct_effect, 1), "%\n")
