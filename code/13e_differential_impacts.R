# ============================================================================
# SCRIPT 13E: DIFFERENTIAL IMPACTS (APPLIANCES UP, TRANSPORTATION DOWN)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING DIFFERENTIAL IMPACT CHART ===\n\n")

# Use the diagnostic data structure
appliance_clean <- read_csv("data/processed/appliance_imports_clean.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date))

transport_data <- read_csv("data/processed/did_analysis_data.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date), industry = "Transportation") %>%
  select(date, country, imports, industry)

# Combine - China only
combined_data <- bind_rows(
  transport_data %>% filter(country == "China"),
  appliance_clean %>% filter(country == "China")
)

# Calculate regime averages
regime_data <- combined_data %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section_301",
      year(date) >= 2025 ~ "Universal"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(industry, regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop')

# Calculate percentages
pct_data <- regime_data %>%
  group_by(industry) %>%
  mutate(
    baseline_value = avg_imports[regime == "Baseline"],
    pct_change = ((avg_imports - baseline_value) / baseline_value) * 100
  ) %>%
  ungroup() %>%
  mutate(
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
    bar_label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change, 0), "%"),
    direction = ifelse(pct_change >= 0, "Increase", "Decrease")
  )

cat("Percentage changes (showing differential impacts):\n")
print(pct_data %>% select(industry, regime, pct_change))

# Create chart showing differential impacts
p_differential <- ggplot(pct_data, aes(x = regime_label, y = pct_change, fill = industry)) +
  # Zero line
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Bars
  geom_col(position = position_dodge(width = 0.8), 
           width = 0.7, color = "white", linewidth = 1) +
  
  # Labels
  geom_text(
    aes(label = bar_label),
    position = position_dodge(width = 0.8),
    vjust = ifelse(pct_data$pct_change < 0, 1.3, -0.5),
    size = 5.5,
    fontface = "bold"
  ) +
  
  # Colors
  scale_fill_manual(
    values = c(
      "Appliances" = "#ff7f00",
      "Transportation" = "#e41a1c"
    ),
    name = "Product Type"
  ) +
  
  # Y axis - expanded to show both positive and negative
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(-40, 40, 10),
    limits = c(-40, 45)
  ) +
  
  # Labels
  labs(
    title = "Differential Tariff Impacts: Transportation vs. Consumer Appliances",
    subtitle = "Capital goods (transportation) declined sharply; Consumer goods (appliances) increased",
    x = "Tariff Regime",
    y = "% Change from Baseline",
    caption = "Source: FRED (Transportation), Census HS 8418 (Appliances: Refrigerators, Freezers).\nTransportation equipment hit hard by tariffs (-32% by 2025).\nAppliances avoided impact, increased 35% under Section 301 (likely due to COVID demand surge + different tariff treatment).\nDemonstrates selective policy impacts across product categories."
  ) +
  
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/fig16_differential_impacts.png", p_differential,
       width = 14, height = 9, dpi = 300)

cat("\n✓ Saved: outputs/figures/fig16_differential_impacts.png\n")

# Create summary table
summary_table <- pct_data %>%
  select(industry, regime, avg_imports, pct_change) %>%
  pivot_wider(
    names_from = regime,
    values_from = c(avg_imports, pct_change)
  )

print("\n=== SUMMARY TABLE ===")
print(summary_table)

write_csv(summary_table, "outputs/tables/differential_impacts_summary.csv")
cat("\n✓ Saved: outputs/tables/differential_impacts_summary.csv\n")

cat("\n=== KEY FINDING ===\n")
cat("This shows DIFFERENTIAL impacts:\n")
cat("  - Transportation (capital/industrial): DECLINED (-32%)\n")
cat("  - Appliances (consumer goods): INCREASED (+35%)\n")
cat("  → Tariffs had SELECTIVE effects by product type!\n")
cat("  → Your transportation findings are specific to capital goods!\n")

cat("\n=== COMPLETE ===\n")
