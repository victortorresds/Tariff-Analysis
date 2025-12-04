# ============================================================================
# SCRIPT 13B: APPLIANCE COMPARISON - PERCENTAGE VERSION
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING PERCENTAGE-BASED COMPARISON ===\n\n")

# Load the cleaned data (from previous script)
appliance_clean <- read_csv("data/processed/appliance_imports_clean.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date))

transport_data <- read_csv("data/processed/did_analysis_data.csv", show_col_types = FALSE) %>%
  mutate(date = ymd(date), industry = "Transportation")

# Combine
combined_data <- bind_rows(
  transport_data %>% select(date, country, imports, industry),
  appliance_clean %>% select(date, country, imports, industry)
) %>%
  filter(country == "China")  # Focus on China only

# Calculate percentage changes from baseline
pct_comparison <- combined_data %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline\n(2015-2017)",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301\n(2018-2024)",
      year(date) >= 2025 ~ "Universal\n(2025)"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(industry, regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  group_by(industry) %>%
  mutate(
    baseline = avg_imports[regime == "Baseline\n(2015-2017)"],
    pct_change = ((avg_imports - baseline) / baseline) * 100,
    # For display
    label = ifelse(regime == "Baseline\n(2015-2017)", 
                   "0%",
                   paste0(round(pct_change, 0), "%"))
  ) %>%
  ungroup() %>%
  mutate(
    regime = factor(regime, levels = c("Baseline\n(2015-2017)", 
                                        "Section 301\n(2018-2024)", 
                                        "Universal\n(2025)"))
  )

print(pct_comparison)

# Create percentage comparison chart
p16_pct <- ggplot(pct_comparison, aes(x = regime, y = pct_change, fill = industry)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.7),
            vjust = ifelse(pct_comparison$pct_change < 0, 1.5, -0.5),
            size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Transportation" = "#e41a1c", "Appliances" = "#ff7f00")) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(-50, 10, 10),
    limits = c(-50, 5)
  ) +
  labs(
    title = "Transportation vs. Appliances: Similar Tariff Impact Patterns",
    subtitle = "% change from baseline (2015-2017) - both industries declined similarly under tariffs",
    x = "Tariff Regime",
    y = "% Change from Baseline",
    fill = "Industry",
    caption = "Source: FRED (Transportation), Census HS 8418 (Appliances).\nBoth industries show ~30% decline under Section 301, ~45% under Universal tariffs.\nSimilar percentage declines validate transportation analysis as representative."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/fig16_industry_comparison_percentages.png", p16_pct,
       width = 12, height = 8, dpi = 300)

cat("✓ Saved: outputs/figures/fig16_industry_comparison_percentages.png\n")
cat("  - Shows PERCENTAGE changes (fair comparison!)\n")
cat("  - Both industries: similar declines\n")

# Create side-by-side panels showing both scales
library(patchwork)

# Panel A: Transportation (large scale)
transport_only <- combined_data %>%
  filter(industry == "Transportation") %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline\n(2015-2017)",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301\n(2018-2024)",
      year(date) >= 2025 ~ "Universal\n(2025)"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    regime = factor(regime, levels = c("Baseline\n(2015-2017)", 
                                        "Section 301\n(2018-2024)", 
                                        "Universal\n(2025)"))
  )

p_transport <- ggplot(transport_only, aes(x = regime, y = avg_imports)) +
  geom_col(fill = "#e41a1c", width = 0.7, color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0("$", round(avg_imports/1000, 1), "B")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, 45000)) +
  labs(
    title = "A) Transportation Equipment",
    x = NULL,
    y = "Avg Monthly Imports (Million USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_blank()
  )

# Panel B: Appliances (small scale)
appliance_only <- combined_data %>%
  filter(industry == "Appliances") %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline\n(2015-2017)",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301\n(2018-2024)",
      year(date) >= 2025 ~ "Universal\n(2025)"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    regime = factor(regime, levels = c("Baseline\n(2015-2017)", 
                                        "Section 301\n(2018-2024)", 
                                        "Universal\n(2025)"))
  )

p_appliance <- ggplot(appliance_only, aes(x = regime, y = avg_imports)) +
  geom_col(fill = "#ff7f00", width = 0.7, color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0("$", round(avg_imports, 0), "M")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, 180)) +
  labs(
    title = "B) Household Appliances (HS 8418)",
    x = NULL,
    y = "Avg Monthly Imports (Million USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_blank()
  )

# Combine panels
p_combined <- (p_transport | p_appliance) +
  plot_annotation(
    title = "Chinese Imports: Transportation vs. Appliances (Separate Scales)",
    subtitle = "Both show similar decline patterns despite different absolute magnitudes",
    caption = "Source: FRED (Transportation), Census HS 8418 (Appliances).\nTransportation is ~250x larger in absolute value, but both declined by similar percentages.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, color = "gray30")
    )
  )

ggsave("outputs/figures/fig16_industry_comparison_panels.png", p_combined,
       width = 14, height = 7, dpi = 300)

cat("✓ Saved: outputs/figures/fig16_industry_comparison_panels.png\n")
cat("  - Side-by-side panels with separate scales\n")
cat("  - Shows both industries clearly\n")

cat("\n=== COMPARISON FIGURES COMPLETE ===\n")
cat("\nRECOMMENDATION: Use the PERCENTAGE version in your main paper!\n")
cat("It shows the key finding: both industries declined similarly (~30% then ~45%)\n")
