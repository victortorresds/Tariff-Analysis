# ============================================================================
# DiD ANALYSIS: 2025 UNIVERSAL TARIFFS
# ============================================================================
# This script analyzes the impact of the April 2025 universal tariffs
# separately from the 2018 Section 301 tariffs
# ============================================================================

library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)
library(broom)

cat("=== 2025 UNIVERSAL TARIFFS ANALYSIS ===\n\n")

# ============================================================================
# 1. LOAD DATA
# ============================================================================

# Load the processed data from the main analysis
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

cat("Data loaded:", nrow(did_data), "observations\n")
cat("Date range:", min(did_data$date), "to", max(did_data$date), "\n\n")

# ============================================================================
# 2. SETUP FOR 2025 UNIVERSAL TARIFFS DiD
# ============================================================================

cat("=== SETTING UP 2025 TARIFF DiD ===\n\n")

# For 2025 analysis, we need a different treatment/control setup:
# - PROBLEM: Universal tariffs affect ALL countries (China, Mexico, Canada)
# - SOLUTION: Use 2024 as "control" period and 2025 as "treatment" period
#             Compare China vs. Mexico/Canada WITHIN 2025

# Option 1: Time-Series DiD (2024 vs 2025)
# Option 2: Differential Impact DiD (China hit harder than Mexico/Canada in 2025)

# We'll do BOTH approaches

# ============================================================================
# 3. APPROACH 1: TIME-SERIES DiD (2024 vs 2025)
# ============================================================================

cat("=== APPROACH 1: TIME-SERIES DiD (2024 as Control, 2025 as Treatment) ===\n\n")

# Focus on recent period: 2023-2025
recent_data <- did_data %>%
  filter(year(date) >= 2023) %>%
  mutate(
    # Treatment group: China (highest tariff exposure)
    treatment = ifelse(country == "China", 1, 0),
    
    # Post period: April 2025 onwards (when universal tariffs started)
    post_2025 = ifelse(date >= "2025-04-01", 1, 0),
    
    # DiD interaction
    did_2025 = treatment * post_2025
  )

cat("Recent data (2023-2025):\n")
cat("  Observations:", nrow(recent_data), "\n")
cat("  Pre-2025:", sum(recent_data$post_2025 == 0), "\n")
cat("  Post-2025:", sum(recent_data$post_2025 == 1), "\n\n")

# Test parallel trends (2023-2024)
pre_2025_data <- recent_data %>%
  filter(year(date) < 2025) %>%
  mutate(time_trend = as.numeric(date - min(date)) / 30)

pre_2025_model <- lm(imports ~ treatment + time_trend + treatment:time_trend,
                     data = pre_2025_data)

cat("Parallel Trends Test (2023-2024):\n")
cat("-----------------------------------\n")
pre_2025_coef <- coef(pre_2025_model)["treatment:time_trend"]
pre_2025_pval <- summary(pre_2025_model)$coefficients["treatment:time_trend", "Pr(>|t|)"]

cat("Treatment × Time coefficient:", round(pre_2025_coef, 3), "\n")
cat("P-value:", round(pre_2025_pval, 4), "\n")

if (pre_2025_pval > 0.05) {
  cat("✓ PASS: Parallel trends hold for 2023-2024 period\n\n")
} else {
  cat("✗ WARNING: Pre-2025 trends differ. Interpret with caution.\n\n")
}

# DiD regression for 2025 universal tariffs
did_2025_model <- lm(imports ~ treatment + post_2025 + did_2025,
                     data = recent_data)

cat("DiD Model: 2025 Universal Tariffs\n")
cat("-----------------------------------\n")
print(summary(did_2025_model))

did_2025_estimate <- coef(did_2025_model)["did_2025"]
did_2025_se <- summary(did_2025_model)$coefficients["did_2025", "Std. Error"]
did_2025_pval <- summary(did_2025_model)$coefficients["did_2025", "Pr(>|t|)"]

cat("\n2025 Universal Tariff DiD Estimate:\n")
cat("  Coefficient:", round(did_2025_estimate, 2), "million USD\n")
cat("  Standard Error:", round(did_2025_se, 2), "\n")
cat("  P-value:", round(did_2025_pval, 4), "\n")

if (did_2025_pval < 0.05) {
  cat("  Statistically significant at 5% level\n")
  if (did_2025_estimate < 0) {
    cat("\n  Interpretation: Universal tariffs caused additional decline of")
    cat("\n  ", abs(round(did_2025_estimate, 0)), "million USD/month in Chinese imports\n")
    cat("  relative to Mexico/Canada.\n\n")
  }
} else {
  cat("  Not statistically significant\n\n")
}

# ============================================================================
# 4. APPROACH 2: DIFFERENTIAL IMPACT (China vs Others in 2025)
# ============================================================================

cat("=== APPROACH 2: DIFFERENTIAL IMPACT WITHIN 2025 ===\n\n")

# Focus only on 2025 data
data_2025 <- did_data %>%
  filter(year(date) == 2025) %>%
  mutate(
    # Create categories by tariff exposure
    tariff_tier = case_when(
      country == "China" ~ "High (China: ~69% cumulative)",
      country == "Mexico" ~ "Medium (Mexico: ~25%)",
      country == "Canada" ~ "Medium (Canada: ~25%)",
      country == "Brazil" ~ "Medium (Brazil: ~10%)",
      TRUE ~ "Other"
    ),
    
    # Dummy for high-tariff country
    high_tariff = ifelse(country == "China", 1, 0),
    
    # Time trend within 2025
    months_since_tariff = as.numeric(date - as.Date("2025-04-01")) / 30
  )

cat("2025 Data Only:\n")
cat("  Total observations:", nrow(data_2025), "\n")
cat("  Months covered:", n_distinct(data_2025$date), "\n\n")

# Model: How did imports change AFTER April 2025 tariffs?
# Compare high-tariff (China) to medium-tariff (others)

impact_2025_model <- lm(imports ~ high_tariff + months_since_tariff + 
                          high_tariff:months_since_tariff,
                        data = data_2025 %>% filter(date >= "2025-04-01"))

cat("2025 Impact Model (Post-April Only):\n")
cat("---------------------------------------\n")
print(summary(impact_2025_model))

impact_coef <- coef(impact_2025_model)["high_tariff:months_since_tariff"]
cat("\nDifferential trend for China:", round(impact_coef, 2), "per month\n")
cat("(Negative = China declining faster than others)\n\n")

# ============================================================================
# 5. COMPARE 2018 vs 2025 TARIFF IMPACTS
# ============================================================================

cat("=== COMPARING 2018 vs 2025 TARIFF IMPACTS ===\n\n")

# Load the 2018 DiD results
did_2018_model <- readRDS("outputs/models/did_model_basic.rds")
did_2018_estimate <- coef(did_2018_model)["did_term"]

cat("Comparison of Tariff Impacts:\n")
cat("-------------------------------\n")
cat("2018 Section 301 Tariffs (25% on China):\n")
cat("  DiD Estimate:", round(did_2018_estimate, 0), "million USD/month\n")
cat("\n2025 Universal Tariffs (10% baseline + reciprocal):\n")
cat("  DiD Estimate:", round(did_2025_estimate, 0), "million USD/month\n")

if (!is.na(did_2018_estimate) & !is.na(did_2025_estimate)) {
  ratio <- abs(did_2025_estimate) / abs(did_2018_estimate)
  cat("\n2025 impact relative to 2018:", round(ratio * 100, 1), "% of 2018 effect\n")
  
  if (ratio > 1) {
    cat("→ 2025 universal tariffs had LARGER impact than 2018 targeted tariffs\n")
  } else if (ratio > 0.5) {
    cat("→ 2025 universal tariffs had COMPARABLE impact to 2018 targeted tariffs\n")
  } else {
    cat("→ 2025 universal tariffs had SMALLER impact than 2018 targeted tariffs\n")
  }
}

# ============================================================================
# 6. CALCULATE CUMULATIVE EFFECTS
# ============================================================================

cat("\n\n=== CUMULATIVE TARIFF EFFECTS ===\n\n")

# Calculate average imports by period
cumulative_effects <- did_data %>%
  filter(country == "China") %>%
  mutate(
    period = case_when(
      year(date) <= 2017 ~ "Baseline (2015-2017)",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301 (2018-2024)",
      year(date) == 2025 & date < "2025-04-01" ~ "Pre-Universal (Jan-Mar 2025)",
      date >= "2025-04-01" ~ "Universal Tariffs (Apr-Aug 2025)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(period != "Other") %>%
  group_by(period) %>%
  summarise(
    avg_imports = mean(imports, na.rm = TRUE),
    .groups = 'drop'
  )

baseline <- cumulative_effects %>% 
  filter(period == "Baseline (2015-2017)") %>% 
  pull(avg_imports)

cumulative_effects <- cumulative_effects %>%
  mutate(
    change_from_baseline = avg_imports - baseline,
    pct_change = ((avg_imports - baseline) / baseline) * 100
  )

cat("Chinese Import Levels Across Tariff Regimes:\n")
print(cumulative_effects)

# ============================================================================
# 7. CREATE SUMMARY TABLE FOR REPORT
# ============================================================================

cat("\n\n=== CREATING SUMMARY TABLE ===\n\n")

tariff_comparison <- tibble(
  Tariff_Regime = c(
    "Baseline (2015-2017)",
    "Section 301 (2018-2024)",
    "Universal (2025)"
  ),
  Tariff_Rate_China = c(
    "0%",
    "25% (List 1-4)",
    "~69% (25% + 10% + 34%)"
  ),
  DiD_Estimate = c(
    NA,
    did_2018_estimate,
    did_2025_estimate
  ),
  Avg_China_Imports = c(
    cumulative_effects$avg_imports[1],
    cumulative_effects$avg_imports[2],
    cumulative_effects$avg_imports[4]
  ),
  Change_from_Baseline = c(
    0,
    cumulative_effects$pct_change[2],
    cumulative_effects$pct_change[4]
  )
)

print(tariff_comparison)

write_csv(tariff_comparison, "outputs/tables/tariff_regime_comparison.csv")
cat("\n✓ Saved: outputs/tables/tariff_regime_comparison.csv\n")

# ============================================================================
# 8. SAVE 2025 ANALYSIS RESULTS
# ============================================================================

saveRDS(did_2025_model, "outputs/models/did_model_2025.rds")
saveRDS(recent_data, "outputs/models/recent_data_2023_2025.rds")

write_csv(cumulative_effects, "outputs/tables/cumulative_tariff_effects.csv")

cat("\n✓ All 2025 analysis outputs saved\n")

# ============================================================================
# 9. KEY FINDINGS SUMMARY
# ============================================================================

cat("\n\n=== KEY FINDINGS: 2025 UNIVERSAL TARIFFS ===\n\n")

cat("1. PARALLEL TRENDS (2023-2024):\n")
cat("   Status:", ifelse(pre_2025_pval > 0.05, "PASS ✓", "FAIL ✗"), "\n")
cat("   P-value:", round(pre_2025_pval, 4), "\n\n")

cat("2. DiD ESTIMATE (2025 Universal Tariffs):\n")
cat("   Effect:", round(did_2025_estimate, 0), "million USD/month\n")
cat("   P-value:", round(did_2025_pval, 4), "\n")
cat("   Significant:", ifelse(did_2025_pval < 0.05, "YES", "NO"), "\n\n")

cat("3. COMPARISON TO 2018 TARIFFS:\n")
if (!is.na(did_2018_estimate) & !is.na(did_2025_estimate)) {
  cat("   2018 Section 301:", round(did_2018_estimate, 0), "million USD/month\n")
  cat("   2025 Universal:", round(did_2025_estimate, 0), "million USD/month\n")
  ratio <- abs(did_2025_estimate) / abs(did_2018_estimate)
  cat("   Relative Impact:", round(ratio * 100, 1), "% of 2018\n\n")
}

cat("4. CUMULATIVE DECLINE FROM BASELINE:\n")
cat("   Section 301 period:", round(cumulative_effects$pct_change[2], 1), "%\n")
cat("   Universal tariff period:", round(cumulative_effects$pct_change[4], 1), "%\n\n")

cat("=== 2025 ANALYSIS COMPLETE ===\n")
