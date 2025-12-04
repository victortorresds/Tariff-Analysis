# ============================================================================
# SCRIPT 13D: APPLIANCE COMPARISON - COMPLETE FRESH START
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== FRESH START: APPLIANCE COMPARISON ===\n\n")

# ============================================================================
# STEP 1: LOAD AND CLEAN APPLIANCE DATA FROM SCRATCH
# ============================================================================

cat("STEP 1: Loading raw appliance data...\n")

appliance_raw <- read_csv(
  "data/household_appliances/household_appliances.csv",
  skip = 3,
  col_types = cols(
    Commodity = col_character(),
    Country = col_character(),
    Time = col_character(),
    `Customs Value (Gen) ($US)` = col_character()
  )
)

cat("  Raw rows:", nrow(appliance_raw), "\n")

# Clean the data
appliance_data <- appliance_raw %>%
  # Remove annual totals
  filter(!str_detect(Time, "^\\d{4}$")) %>%
  # Parse date
  mutate(
    # Extract month and year
    month_str = str_extract(Time, "^[A-Za-z]+"),
    year_str = str_extract(Time, "\\d+$"),
    # Convert year
    year_num = as.numeric(year_str),
    year_full = ifelse(year_num < 50, 2000 + year_num, 1900 + year_num),
    # Get month number
    month_num = match(month_str, month.abb),
    # Create date
    date = make_date(year_full, month_num, 1),
    # Clean country
    country = str_trim(Country),
    # Clean imports - remove commas, convert to numeric, then to millions
    imports_str = `Customs Value (Gen) ($US)`,
    imports_num = as.numeric(str_replace_all(imports_str, ",", "")),
    imports = imports_num / 1000000  # Convert to millions
  ) %>%
  # Select final columns
  select(date, country, imports) %>%
  # Filter valid data
  filter(
    !is.na(date),
    !is.na(country),
    !is.na(imports),
    date >= "2015-01-01",
    date <= "2025-12-31"
  ) %>%
  # Add industry label
  mutate(industry = "Appliances")

cat("  Cleaned rows:", nrow(appliance_data), "\n")
cat("  Countries:", paste(unique(appliance_data$country), collapse = ", "), "\n")

# Check China specifically
china_appliance <- appliance_data %>% filter(country == "China")
cat("  China rows:", nrow(china_appliance), "\n")
cat("  China avg:", round(mean(china_appliance$imports), 1), "million USD/month\n\n")

# ============================================================================
# STEP 2: LOAD TRANSPORTATION DATA
# ============================================================================

cat("STEP 2: Loading transportation data...\n")

transport_data <- read_csv("data/processed/did_analysis_data.csv", show_col_types = FALSE) %>%
  mutate(
    date = ymd(date),
    industry = "Transportation"
  ) %>%
  select(date, country, imports, industry)

cat("  Rows:", nrow(transport_data), "\n\n")

# ============================================================================
# STEP 3: COMBINE DATA - CHINA ONLY
# ============================================================================

cat("STEP 3: Combining data (China only)...\n")

combined_data <- bind_rows(
  transport_data %>% filter(country == "China"),
  appliance_data %>% filter(country == "China")
)

cat("  Total rows:", nrow(combined_data), "\n")

# Verify both industries present
industry_check <- combined_data %>%
  group_by(industry) %>%
  summarise(
    n_rows = n(),
    avg_imports = mean(imports, na.rm = TRUE),
    .groups = 'drop'
  )

cat("  By industry:\n")
print(industry_check)

# ============================================================================
# STEP 4: CALCULATE REGIME AVERAGES
# ============================================================================

cat("\nSTEP 4: Calculating regime averages...\n")

regime_data <- combined_data %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section_301",
      year(date) >= 2025 ~ "Universal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(industry, regime) %>%
  summarise(
    avg_imports = mean(imports, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  )

cat("  Regime averages:\n")
print(regime_data)

# ============================================================================
# STEP 5: CALCULATE PERCENTAGE CHANGES
# ============================================================================

cat("\nSTEP 5: Calculating percentage changes...\n")

pct_data <- regime_data %>%
  group_by(industry) %>%
  mutate(
    baseline_value = avg_imports[regime == "Baseline"],
    pct_change = ((avg_imports - baseline_value) / baseline_value) * 100
  ) %>%
  ungroup() %>%
  mutate(
    # Create proper labels
    regime_label = case_when(
      regime == "Baseline" ~ "Baseline\n(2015-2017)",
      regime == "Section_301" ~ "Section 301\n(2018-2024)",
      regime == "Universal" ~ "Universal\n(2025)"
    ),
    regime_label = factor(regime_label, levels = c(
      "Baseline\n(2015-2017)",
      "Section 301\n(2018-2024)",
      "Universal\n(2025)"
    )),
    # Bar labels
    bar_label = ifelse(abs(pct_change) < 0.5, "0%", paste0(round(pct_change, 0), "%"))
  )

cat("  Percentage changes:\n")
print(pct_data %>% select(industry, regime, avg_imports, pct_change))

# ============================================================================
# STEP 6: CREATE VISUALIZATION
# ============================================================================

cat("\nSTEP 6: Creating visualization...\n")

p <- ggplot(pct_data, aes(x = regime_label, y = pct_change, fill = industry)) +
  # Zero line
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Bars
  geom_col(position = position_dodge(width = 0.8), 
           width = 0.7, color = "white", linewidth = 1) +
  
  # Labels on bars
  geom_text(
    aes(label = bar_label),
    position = position_dodge(width = 0.8),
    vjust = ifelse(pct_data$pct_change < 0, 1.3, -0.5),
    size = 6,
    fontface = "bold"
  ) +
  
  # Colors
  scale_fill_manual(
    values = c(
      "Appliances" = "#ff7f00",
      "Transportation" = "#e41a1c"
    ),
    name = "Industry"
  ) +
  
  # Y axis
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(-50, 10, 10),
    limits = c(-55, 10)
  ) +
  
  # Labels
  labs(
    title = "Transportation vs. Appliances: Similar Tariff Impact Patterns",
    subtitle = "% change from baseline (2015-2017) - both industries declined similarly",
    x = "Tariff Regime",
    y = "% Change from Baseline",
    caption = "Source: FRED (Transportation), Census HS 8418 (Appliances).\nBoth industries show similar percentage declines across tariff regimes.\nValidates transportation findings as representative of manufactured goods."
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Save
ggsave("outputs/figures/fig16_industry_comparison_percentages.png", p,
       width = 14, height = 9, dpi = 300)

cat("\n✓ Saved: outputs/figures/fig16_industry_comparison_percentages.png\n")

# ============================================================================
# STEP 7: VERIFY AND REPORT
# ============================================================================

cat("\n=== SUMMARY ===\n")

summary_wide <- pct_data %>%
  select(industry, regime, pct_change) %>%
  pivot_wider(names_from = regime, values_from = pct_change)

print(summary_wide)

cat("\nKEY FINDINGS:\n")
trans_baseline <- filter(pct_data, industry == "Transportation" & regime == "Baseline")$pct_change
trans_301 <- filter(pct_data, industry == "Transportation" & regime == "Section_301")$pct_change
trans_univ <- filter(pct_data, industry == "Transportation" & regime == "Universal")$pct_change

app_baseline <- filter(pct_data, industry == "Appliances" & regime == "Baseline")$pct_change
app_301 <- filter(pct_data, industry == "Appliances" & regime == "Section_301")$pct_change
app_univ <- filter(pct_data, industry == "Appliances" & regime == "Universal")$pct_change

cat("\nTransportation:\n")
cat("  Baseline:", round(trans_baseline, 1), "%\n")
cat("  Section 301:", round(trans_301, 1), "%\n")
cat("  Universal:", round(trans_univ, 1), "%\n")

cat("\nAppliances:\n")
cat("  Baseline:", round(app_baseline, 1), "%\n")
cat("  Section 301:", round(app_301, 1), "%\n")
cat("  Universal:", round(app_univ, 1), "%\n")

cat("\nDifferences:\n")
cat("  Section 301:", round(abs(trans_301 - app_301), 1), "pp\n")
cat("  Universal:", round(abs(trans_univ - app_univ), 1), "pp\n")

cat("\n✓ BOTH INDUSTRIES SHOULD BE VISIBLE IN CHART!\n")
cat("✓ If you see orange AND red bars, it worked!\n")

cat("\n=== COMPLETE ===\n")
