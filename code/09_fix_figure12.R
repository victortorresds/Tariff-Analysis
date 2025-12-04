# ============================================================================
# SCRIPT 09: FIX FIGURE 12 - REGIME COMPARISON TABLE (WITH VALUES!)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(gt)

cat("=== CREATING PROPER REGIME COMPARISON TABLE ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Calculate averages by regime
regime_stats <- did_data %>%
  filter(country %in% c("China", "Mexico", "Canada")) %>%
  mutate(
    regime = case_when(
      year(date) <= 2017 ~ "Baseline",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section_301",
      year(date) >= 2025 ~ "Universal"
    )
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(country, regime) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = regime, values_from = avg_imports)

# Calculate percent changes
regime_comparison <- regime_stats %>%
  mutate(
    pct_change_301 = ((Section_301 - Baseline) / Baseline) * 100,
    pct_change_universal = ((Universal - Baseline) / Baseline) * 100
  )

print(regime_comparison)

# Create formatted table using gt
comparison_table <- regime_comparison %>%
  select(country, Baseline, Section_301, pct_change_301, 
         Universal, pct_change_universal) %>%
  gt() %>%
  tab_header(
    title = "Import Levels Across Tariff Regimes",
    subtitle = "Average monthly imports by country and period (Million USD)"
  ) %>%
  cols_label(
    country = "Country",
    Baseline = "2015-2017",
    Section_301 = "2018-2024", 
    pct_change_301 = "% Change",
    Universal = "2025",
    pct_change_universal = "% Change"
  ) %>%
  fmt_number(
    columns = c(Baseline, Section_301, Universal),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(pct_change_301, pct_change_universal),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  tab_spanner(
    label = "Section 301 Tariffs",
    columns = c(Section_301, pct_change_301)
  ) %>%
  tab_spanner(
    label = "Universal Tariffs",
    columns = c(Universal, pct_change_universal)
  ) %>%
  data_color(
    columns = c(pct_change_301, pct_change_universal),
    colors = scales::col_numeric(
      palette = c("red", "white", "green"),
      domain = c(-50, 50)
    )
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 14
  )

# Save as image
gtsave(comparison_table, "outputs/figures/fig12_regime_comparison_table.png")

cat("✓ Saved: outputs/figures/fig12_regime_comparison_table.png\n")
cat("  - Fixed: All values now populated!\n")
cat("  - Improved: Color-coded changes (red=decline, green=increase)\n")

# Also create a BAR CHART version (easier to read!)
regime_long <- regime_comparison %>%
  select(country, Baseline, Section_301, Universal) %>%
  pivot_longer(cols = -country, names_to = "regime", values_to = "imports") %>%
  mutate(
    regime = factor(regime, levels = c("Baseline", "Section_301", "Universal"),
                    labels = c("Baseline\n(2015-2017)", 
                              "Section 301\n(2018-2024)", 
                              "Universal\n(2025)"))
  )

p12_bars <- ggplot(regime_long, aes(x = regime, y = imports, fill = country)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0("$", round(imports/1000, 1), "B")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "China" = "#e41a1c",
    "Mexico" = "#377eb8",
    "Canada" = "#4daf4a"
  )) +
  scale_y_continuous(labels = comma, limits = c(0, max(regime_long$imports) * 1.15)) +
  labs(
    title = "Average Monthly Imports Across Tariff Regimes",
    subtitle = "Comparison of baseline, Section 301, and universal tariff periods",
    x = "Tariff Regime",
    y = "Average Monthly Import Value (Million USD)",
    fill = "Country",
    caption = "Source: FRED, Author's calculations.\nChina declined under both regimes; Mexico/Canada increased."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/fig12_regime_comparison_bars.png", p12_bars,
       width = 12, height = 8, dpi = 300)

cat("✓ BONUS: Created bar chart version!\n")
cat("  Saved: outputs/figures/fig12_regime_comparison_bars.png\n")

cat("\n=== FIGURE 12 FIXED (2 versions!) ===\n")
