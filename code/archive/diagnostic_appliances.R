# ============================================================================
# DIAGNOSTIC: Check Appliance Data
# ============================================================================

library(tidyverse)
library(lubridate)

cat("=== DIAGNOSTIC: CHECKING APPLIANCE DATA ===\n\n")

# 1. Check if appliance file exists and has data
cat("1. Checking appliance cleaned file...\n")
if (file.exists("data/processed/appliance_imports_clean.csv")) {
  appliance_clean <- read_csv("data/processed/appliance_imports_clean.csv", show_col_types = FALSE)
  cat("  ✓ File exists\n")
  cat("  Rows:", nrow(appliance_clean), "\n")
  cat("  Columns:", paste(names(appliance_clean), collapse = ", "), "\n")
  
  # Check if industry column exists
  if ("industry" %in% names(appliance_clean)) {
    cat("  ✓ industry column exists\n")
    cat("  Industry values:", unique(appliance_clean$industry), "\n")
  } else {
    cat("  ✗ industry column MISSING!\n")
  }
  
  # Check China data
  china_app <- appliance_clean %>% filter(country == "China")
  cat("  China rows:", nrow(china_app), "\n")
  
  if (nrow(china_app) > 0) {
    cat("  China date range:", min(china_app$date), "to", max(china_app$date), "\n")
    cat("  China avg imports:", round(mean(china_app$imports, na.rm = TRUE), 1), "million\n")
    
    # Check by year
    china_by_year <- china_app %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(avg = mean(imports, na.rm = TRUE), n = n())
    
    cat("\n  China appliances by year:\n")
    print(china_by_year)
  }
  
} else {
  cat("  ✗ File does NOT exist!\n")
  cat("  Run script 13 or 13_FIXED first!\n")
}

cat("\n2. Checking transportation data...\n")
transport_data <- read_csv("data/processed/did_analysis_data.csv", show_col_types = FALSE)
cat("  Rows:", nrow(transport_data), "\n")
cat("  Columns:", paste(names(transport_data), collapse = ", "), "\n")

china_trans <- transport_data %>% 
  filter(country == "China") %>%
  mutate(date = ymd(date))
cat("  China rows:", nrow(china_trans), "\n")
cat("  China avg imports:", round(mean(china_trans$imports, na.rm = TRUE), 1), "million\n")

cat("\n3. Testing data combination...\n")

# Manually add industry column if needed
if (file.exists("data/processed/appliance_imports_clean.csv")) {
  appliance_test <- read_csv("data/processed/appliance_imports_clean.csv", show_col_types = FALSE) %>%
    mutate(date = ymd(date))
  
  # Make sure industry column exists
  if (!"industry" %in% names(appliance_test)) {
    cat("  Adding industry column to appliances...\n")
    appliance_test <- appliance_test %>% mutate(industry = "Appliances")
  }
  
  transport_test <- transport_data %>%
    mutate(date = ymd(date), industry = "Transportation") %>%
    select(date, country, imports, industry)
  
  combined_test <- bind_rows(transport_test, appliance_test)
  
  cat("  Combined rows:", nrow(combined_test), "\n")
  cat("  Industries:", paste(unique(combined_test$industry), collapse = ", "), "\n")
  
  china_combined <- combined_test %>% filter(country == "China")
  cat("  China combined rows:", nrow(china_combined), "\n")
  
  by_industry <- china_combined %>%
    group_by(industry) %>%
    summarise(n = n(), avg_imports = mean(imports, na.rm = TRUE))
  
  cat("\n  China by industry:\n")
  print(by_industry)
  
  # Calculate regime averages
  cat("\n4. Testing regime calculations...\n")
  regime_test <- china_combined %>%
    mutate(
      regime = case_when(
        year(date) <= 2017 ~ "Baseline",
        year(date) >= 2018 & year(date) <= 2024 ~ "Section_301",
        year(date) >= 2025 ~ "Universal"
      )
    ) %>%
    filter(!is.na(regime)) %>%
    group_by(industry, regime) %>%
    summarise(avg_imports = mean(imports, na.rm = TRUE), n = n(), .groups = 'drop')
  
  cat("  Regime averages:\n")
  print(regime_test)
  
  # Calculate percentages
  pct_test <- regime_test %>%
    group_by(industry) %>%
    mutate(
      baseline = avg_imports[regime == "Baseline"],
      pct_change = ((avg_imports - baseline) / baseline) * 100
    ) %>%
    ungroup()
  
  cat("\n  Percentage changes:\n")
  print(pct_test %>% select(industry, regime, pct_change))
}

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
cat("\nLook for issues above - especially:\n")
cat("  - Are appliances showing for all regimes?\n")
cat("  - Are percentage changes calculated for both industries?\n")
cat("  - Are there any NA values?\n")
