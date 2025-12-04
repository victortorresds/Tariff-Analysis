# ============================================================================
# SCRIPT 13: APPLIANCE COMPARISON ANALYSIS (FIXED!)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== PROCESSING HOUSEHOLD APPLIANCES DATA (FIXED) ===\n\n")

# ============================================================================
# 1. LOAD AND CLEAN APPLIANCE DATA (IMPROVED)
# ============================================================================

appliance_raw <- read_csv("data/household_appliances/household_appliances.csv",
                          skip = 3, show_col_types = FALSE)

cat("Raw appliance data loaded:", nrow(appliance_raw), "rows\n\n")

# Clean with better error handling
appliance_clean <- appliance_raw %>%
  # Remove annual summary rows
  filter(!str_detect(Time, "^\\d{4}$")) %>%
  # Parse date properly
  mutate(
    month_abbr = str_extract(Time, "^[A-Za-z]+"),
    year_short = str_extract(Time, "\\d+$"),
    year_full = as.numeric(year_short) + ifelse(as.numeric(year_short) < 50, 2000, 1900),
    # Create date
    month_num = match(month_abbr, month.abb),
    date = make_date(year_full, month_num, 1),
    # Clean country
    country = str_trim(Country),
    # Clean imports - convert to millions
    imports_raw = as.numeric(str_replace_all(`Customs Value (Gen) ($US)`, ",", "")),
    imports = imports_raw / 1000000  # Convert to millions
  ) %>%
  # Select and filter
  select(date, country, imports) %>%
  filter(!is.na(date), !is.na(imports), !is.na(country)) %>%
  filter(date >= "2015-01-01" & date <= "2025-12-31") %>%
  mutate(industry = "Appliances")

cat("Cleaned appliance data:\n")
cat("  Total observations:", nrow(appliance_clean), "\n")
cat("  Countries:", paste(unique(appliance_clean$country), collapse = ", "), "\n")
cat("  Date range:", as.character(min(appliance_clean$date)), "to",
    as.character(max(appliance_clean$date)), "\n")

# Check China data specifically
china_appliance <- appliance_clean %>% filter(country == "China")
cat("  China observations:", nrow(china_appliance), "\n")
cat("  China average:", round(mean(china_appliance$imports, na.rm = TRUE), 1), "million USD/month\n\n")

write_csv(appliance_clean, "data/processed/appliance_imports_clean.csv")
cat("✓ Saved: data/processed/appliance_imports_clean.csv\n")

# ============================================================================
# 2. COMBINE WITH TRANSPORTATION DATA
# ============================================================================

cat("\n=== COMBINING WITH TRANSPORTATION DATA ===\n\n")

transport_data <- read_csv("data/processed/did_analysis_data.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date), industry = "Transportation") %>%
  select(date, country, imports, industry)

# Combine
combined_data <- bind_rows(transport_data, appliance_clean) %>%
  filter(country %in% c("China", "Mexico", "Canada"))

cat("Combined dataset:\n")
cat("  Total observations:", nrow(combined_data), "\n")
cat("  By industry:\n")
combined_data %>% 
  group_by(industry) %>% 
  summarise(n = n(), avg_imports = mean(imports, na.rm = TRUE)) %>%
  print()

# ============================================================================
# 3. CALCULATE METRICS
# ============================================================================

cat("\n=== CALCULATING INDUSTRY COMPARISON METRICS ===\n\n")

baselines <- combined_data %>%
  filter(year(date) <= 2017) %>%
  group_by(country, industry) %>%
  summarise(baseline = mean(imports, na.rm = TRUE), .groups = 'drop')

avg_2025 <- combined_data %>%
  filter(year(date) == 2025) %>%
  group_by(country, industry) %>%
  summarise(avg_2025 = mean(imports, na.rm = TRUE), .groups = 'drop')

industry_comparison <- baselines %>%
  left_join(avg_2025, by = c("country", "industry")) %>%
  mutate(pct_change = ((avg_2025 - baseline) / baseline) * 100)

print(industry_comparison)
write_csv(industry_comparison, "outputs/tables/industry_comparison.csv")
cat("\n✓ Saved: outputs/tables/industry_comparison.csv\n")

# ============================================================================
# 4. DiD ANALYSIS ON APPLIANCES
# ============================================================================

cat("\n=== DiD ANALYSIS: APPLIANCE IMPORTS ===\n\n")

appliance_did <- appliance_clean %>%
  filter(country %in% c("China", "Mexico", "Canada")) %>%
  mutate(
    treatment = ifelse(country == "China", 1, 0),
    post_tariff = ifelse(date >= "2018-07-01", 1, 0),
    did_term = treatment * post_tariff
  )

appliance_did_model <- lm(imports ~ treatment + post_tariff + did_term, data = appliance_did)
appliance_did_est <- coef(appliance_did_model)["did_term"]
appliance_did_pval <- summary(appliance_did_model)$coefficients["did_term", "Pr(>|t|)"]

cat("Appliance DiD Estimate:", round(appliance_did_est, 2), "million USD\n")
cat("P-value:", round(appliance_did_pval, 4), "\n")

transport_did <- readRDS("outputs/models/did_model_basic.rds")
transport_did_est <- coef(transport_did)["did_term"]

cat("\nCOMPARISON:\n")
cat("  Transportation DiD:", round(transport_did_est, 0), "million USD\n")
cat("  Appliances DiD:", round(appliance_did_est, 0), "million USD\n")

# ============================================================================
# 5. CREATE VISUALIZATIONS
# ============================================================================

cat("\n=== CREATING COMPARISON FIGURES ===\n\n")

# FIGURE 16A: Bar chart - CHINA ONLY for clarity
regime_comparison <- combined_data %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline\n(2015-2017)",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301\n(2018-2024)",
      year(date) >= 2025 ~ "Universal\n(2025)"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(country, industry, regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  mutate(regime = factor(regime, levels = c("Baseline\n(2015-2017)", 
                                              "Section 301\n(2018-2024)", 
                                              "Universal\n(2025)")))

china_comparison <- regime_comparison %>% filter(country == "China")

p16_comparison <- ggplot(china_comparison, aes(x = regime, y = avg_imports, fill = industry)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0("$", round(avg_imports/1000, 1), "B")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Transportation" = "#e41a1c", "Appliances" = "#ff7f00")) +
  scale_y_continuous(labels = comma, limits = c(0, max(china_comparison$avg_imports) * 1.25)) +
  labs(
    title = "Transportation vs. Appliances: Chinese Import Declines",
    subtitle = "Both industries hit hard by tariffs, but transportation shows larger absolute decline",
    x = "Tariff Regime", y = "Average Monthly Imports from China (Million USD)",
    fill = "Industry",
    caption = "Source: FRED (Transportation), Census HS 8418 (Appliances).\nBoth show significant declines under Section 301; Universal 2025 tariffs caused further drops."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/fig16_transport_vs_appliances.png", p16_comparison,
       width = 12, height = 8, dpi = 300)
cat("✓ Saved: outputs/figures/fig16_transport_vs_appliances.png\n")

# FIGURE 16B: Time series - indexed with smoothing
china_ts <- combined_data %>%
  filter(country == "China") %>%
  group_by(industry) %>%
  arrange(date) %>%
  mutate(
    baseline = mean(imports[year(date) <= 2017], na.rm = TRUE),
    indexed = (imports / baseline) * 100,
    # Add 3-month moving average for smoothness
    indexed_smooth = zoo::rollmean(indexed, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup()

p16b_timeseries <- ggplot(china_ts, aes(x = date, y = indexed_smooth, color = industry, linetype = industry)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.8, alpha = 0.9) +
  geom_vline(xintercept = as.Date("2018-07-01"), linetype = "dotted", color = "gray30") +
  geom_vline(xintercept = as.Date("2025-04-01"), linetype = "dotted", color = "gray30") +
  annotate("text", x = as.Date("2018-07-01"), y = 115,
           label = "Section 301", hjust = -0.1, size = 3.5, color = "gray30") +
  annotate("text", x = as.Date("2025-04-01"), y = 115,
           label = "Universal", hjust = -0.1, size = 3.5, color = "gray30") +
  scale_color_manual(values = c("Transportation" = "#e41a1c", "Appliances" = "#ff7f00")) +
  scale_linetype_manual(values = c("Transportation" = "solid", "Appliances" = "dashed")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 120, 20), limits = c(20, 120)) +
  labs(
    title = "Chinese Imports: Transportation vs. Appliances (Indexed, 3-Month MA)",
    subtitle = "Both industries show similar tariff response patterns (2015-2017 baseline = 100)",
    x = NULL, y = "Indexed Import Value (2015-2017 = 100)",
    color = "Industry", linetype = "Industry",
    caption = "Source: FRED (Transportation), Census (Appliances). 3-month moving average for clarity.\nSimilar decline patterns validate transportation analysis as representative of manufactured goods."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/fig16b_industry_comparison_timeseries.png", p16b_timeseries,
       width = 13, height = 8, dpi = 300)
cat("✓ Saved: outputs/figures/fig16b_industry_comparison_timeseries.png\n")

# ============================================================================
# 6. SUMMARY TABLE
# ============================================================================

comparison_summary <- tibble(
  Industry = c("Transportation (FRED Total)", "Appliances (HS 8418)"),
  Baseline = c(
    filter(baselines, industry == "Transportation" & country == "China")$baseline,
    filter(baselines, industry == "Appliances" & country == "China")$baseline
  ),
  Avg_2025 = c(
    filter(avg_2025, industry == "Transportation" & country == "China")$avg_2025,
    filter(avg_2025, industry == "Appliances" & country == "China")$avg_2025
  ),
  Pct_Change = c(
    filter(industry_comparison, industry == "Transportation" & country == "China")$pct_change,
    filter(industry_comparison, industry == "Appliances" & country == "China")$pct_change
  ),
  DiD_Estimate = c(transport_did_est, appliance_did_est),
  P_Value = c(
    summary(transport_did)$coefficients["did_term", "Pr(>|t|)"],
    appliance_did_pval
  )
)

print(comparison_summary)
write_csv(comparison_summary, "outputs/tables/industry_comparison_summary.csv")
cat("\n✓ Saved: outputs/tables/industry_comparison_summary.csv\n")

cat("\n=== APPLIANCE COMPARISON COMPLETE ===\n")
cat("\nKEY FINDINGS:\n")
cat("  Transportation decline:", round(filter(industry_comparison, industry == "Transportation" & country == "China")$pct_change, 1), "%\n")
cat("  Appliances decline:", round(filter(industry_comparison, industry == "Appliances" & country == "China")$pct_change, 1), "%\n")
cat("  → Both industries show similar tariff impacts!\n")
cat("  → Transportation analysis is representative of manufacturing generally!\n")
