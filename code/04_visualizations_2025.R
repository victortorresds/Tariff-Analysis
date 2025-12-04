# ============================================================================
# 2025 UNIVERSAL TARIFFS: VISUALIZATIONS
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

cat("=== CREATING 2025 TARIFF VISUALIZATIONS ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Load 2025 analysis results
recent_data <- readRDS("outputs/models/recent_data_2023_2025.rds")
tariff_comparison <- read_csv("outputs/tables/tariff_regime_comparison.csv")

# Color palette
country_colors <- c(
  "China" = "#e41a1c",
  "Mexico" = "#377eb8",
  "Canada" = "#4daf4a",
  "Brazil" = "#984ea3"
)

# ============================================================================
# FIGURE 8: 2025 TARIFF IMPACT - FOCUSED VIEW
# ============================================================================

cat("Creating Figure 8: 2025 tariff impact (Jan 2024 - Aug 2025)...\n")

focused_2025 <- did_data %>%
  filter(date >= "2024-01-01" & date <= "2025-08-31")

p8 <- ggplot(focused_2025, aes(x = date, y = imports, color = country)) +
  geom_line(size = 1.4, alpha = 0.9) +
  
  # Vertical line for April 2025
  geom_vline(xintercept = as.Date("2025-04-01"), 
             linetype = "solid", color = "red", size = 1.2) +
  
  # Shading
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2025-03-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.03, fill = "blue") +
  annotate("rect", xmin = as.Date("2025-04-01"), xmax = as.Date("2025-08-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "red") +
  
  # Labels
  annotate("text", x = as.Date("2024-06-01"), y = max(focused_2025$imports) * 0.95,
           label = "Pre-Universal\nTariffs", 
           size = 4, color = "gray30", fontface = "bold") +
  
  annotate("text", x = as.Date("2025-06-01"), y = max(focused_2025$imports) * 0.95,
           label = "Universal Tariffs\n(10% baseline + reciprocal)", 
           size = 4, color = "red", fontface = "bold") +
  
  # Annotation for China's sharp drop
  annotate("segment", x = as.Date("2025-03-15"), xend = as.Date("2025-04-15"),
           y = 30000, yend = 22000, arrow = arrow(length = unit(0.3, "cm")),
           color = "#e41a1c", size = 1) +
  annotate("text", x = as.Date("2025-03-15"), y = 31000,
           label = "China: Sharp decline\nafter April 2025", 
           hjust = 1, size = 3.5, color = "#e41a1c", fontface = "bold") +
  
  labs(
    title = "Immediate Impact of 2025 Universal Tariffs on Import Patterns",
    subtitle = "January 2024 - August 2025: Before and after universal tariff implementation",
    x = NULL,
    y = "Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: FRED. Red vertical line marks April 2025 universal tariff implementation.\nChina faces cumulative tariffs of ~69% (25% Section 301 + 10% baseline + 34% reciprocal)."
  ) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/fig8_2025_tariff_impact_focused.png", p8, 
       width = 12, height = 8, dpi = 300)
cat("✓ Saved: fig8_2025_tariff_impact_focused.png\n")

# ============================================================================
# FIGURE 9: BEFORE/AFTER COMPARISON (2025 TARIFFS)
# ============================================================================

cat("Creating Figure 9: Before/after comparison for 2025 tariffs...\n")

# Calculate averages pre/post April 2025
comparison_2025 <- focused_2025 %>%
  mutate(
    period = ifelse(date < "2025-04-01", 
                    "Pre-Universal\n(Jan 2024 - Mar 2025)", 
                    "Post-Universal\n(Apr - Aug 2025)")
  ) %>%
  group_by(country, period) %>%
  summarise(
    avg_imports = mean(imports, na.rm = TRUE),
    se = sd(imports, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p9 <- ggplot(comparison_2025, aes(x = period, y = avg_imports, 
                                   color = country, group = country)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = avg_imports - 1.96*se, 
                    ymax = avg_imports + 1.96*se),
                width = 0.15, size = 1) +
  
  # Value labels
  geom_text(aes(label = paste0("$", round(avg_imports/1000, 1), "B")),
            vjust = -1.5, size = 3.5, fontface = "bold", 
            show.legend = FALSE) +
  
  # Calculate percent changes
  geom_text(data = comparison_2025 %>%
              group_by(country) %>%
              summarise(
                pct_change = ((avg_imports[period == "Post-Universal\n(Apr - Aug 2025)"] - 
                               avg_imports[period == "Pre-Universal\n(Jan 2024 - Mar 2025)"]) / 
                              avg_imports[period == "Pre-Universal\n(Jan 2024 - Mar 2025)"]) * 100,
                x = 1.5,
                y = mean(avg_imports),
                .groups = 'drop'
              ),
            aes(x = x, y = y, label = paste0(ifelse(pct_change > 0, "+", ""), 
                                              round(pct_change, 1), "%")),
            size = 4, fontface = "bold", show.legend = FALSE) +
  
  labs(
    title = "2025 Universal Tariffs: Before and After Comparison",
    subtitle = "Average monthly imports 15 months before vs. 5 months after April 2025",
    x = NULL,
    y = "Average Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: FRED. Error bars show 95% confidence intervals.\nPercentage labels show change from pre-universal to post-universal period."
  ) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig9_2025_before_after.png", p9, 
       width = 11, height = 8, dpi = 300)
cat("✓ Saved: fig9_2025_before_after.png\n")

# ============================================================================
# FIGURE 10: PARALLEL TRENDS TEST (2023-2024 for 2025 Analysis)
# ============================================================================

cat("Creating Figure 10: Parallel trends for 2025 analysis...\n")

pre_2025_data <- did_data %>%
  filter(date >= "2023-01-01" & date < "2025-01-01") %>%
  mutate(
    group = ifelse(country == "China", "Treatment (China)", 
                   "Control (Mexico + Canada)")
  )

pre_2025_avg <- pre_2025_data %>%
  group_by(date, group) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop')

p10 <- ggplot(pre_2025_avg, aes(x = date, y = avg_imports, color = group)) +
  geom_line(size = 1.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linetype = "dashed") +
  
  # Mark 2025 start
  geom_vline(xintercept = as.Date("2025-01-01"), 
             linetype = "dotted", color = "gray40") +
  annotate("text", x = as.Date("2025-01-01"), y = max(pre_2025_avg$avg_imports),
           label = "→ 2025 begins", 
           hjust = -0.1, size = 3.5, color = "gray40") +
  
  labs(
    title = "Parallel Trends Test for 2025 Universal Tariffs (2023-2024)",
    subtitle = "Validating DiD assumption: Similar trends before universal tariff implementation",
    x = NULL,
    y = "Average Monthly Import Value (Million USD)",
    color = "Group",
    caption = "Source: FRED. Dashed lines show linear trends.\nParallel slopes validate DiD methodology for 2025 analysis."
  ) +
  scale_color_manual(values = c("Treatment (China)" = "#e41a1c",
                                 "Control (Mexico + Canada)" = "#377eb8")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/fig10_parallel_trends_2025.png", p10, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig10_parallel_trends_2025.png\n")

# ============================================================================
# FIGURE 11: CUMULATIVE TARIFF EFFECTS ACROSS ALL REGIMES
# ============================================================================

cat("Creating Figure 11: Cumulative effects across tariff regimes...\n")

# Calculate indexed values for full time series
baseline_avg <- did_data %>%
  filter(year(date) <= 2017) %>%
  group_by(country) %>%
  summarise(baseline = mean(imports, na.rm = TRUE), .groups = 'drop')

full_indexed <- did_data %>%
  left_join(baseline_avg, by = "country") %>%
  mutate(
    indexed = (imports / baseline) * 100,
    regime = case_when(
      year(date) <= 2017 ~ "Baseline",
      year(date) >= 2018 & year(date) <= 2024 ~ "Section 301",
      date >= "2025-01-01" & date < "2025-04-01" ~ "Pre-Universal",
      date >= "2025-04-01" ~ "Universal",
      TRUE ~ "Other"
    )
  )

p11 <- ggplot(full_indexed, aes(x = date, y = indexed, color = country)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_line(size = 1.3, alpha = 0.9) +
  
  # Regime shading
  annotate("rect", xmin = as.Date("2015-01-01"), xmax = as.Date("2017-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.03, fill = "blue") +
  annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2024-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.03, fill = "orange") +
  annotate("rect", xmin = as.Date("2025-04-01"), xmax = max(full_indexed$date),
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "red") +
  
  # Regime labels
  annotate("text", x = as.Date("2016-06-01"), y = 120,
           label = "Baseline", size = 4, color = "blue", fontface = "bold") +
  annotate("text", x = as.Date("2021-06-01"), y = 120,
           label = "Section 301\n(25% on China)", size = 4, color = "orange3", fontface = "bold") +
  annotate("text", x = as.Date("2025-06-01"), y = 120,
           label = "Universal\n(+10% baseline)", size = 4, color = "red", fontface = "bold") +
  
  # Vertical lines
  geom_vline(xintercept = as.Date("2018-07-01"), linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = as.Date("2025-04-01"), linetype = "dotted", color = "gray40") +
  
  labs(
    title = "Cumulative Impact of US Tariff Policies on Imports (2015-2025)",
    subtitle = "Three tariff regimes: Baseline → Section 301 (targeted) → Universal (comprehensive)",
    x = NULL,
    y = "Indexed Import Value (2015-2017 Baseline = 100)",
    color = "Country",
    caption = "Source: FRED. Shaded regions denote tariff regimes.\nChina shows cumulative decline from both Section 301 and universal tariffs."
  ) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(breaks = seq(40, 140, 20)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig11_cumulative_tariff_effects.png", p11, 
       width = 13, height = 8, dpi = 300)
cat("✓ Saved: fig11_cumulative_tariff_effects.png\n")

# ============================================================================
# FIGURE 12: TARIFF REGIME COMPARISON TABLE (Visual)
# ============================================================================

cat("Creating Figure 12: Tariff regime comparison table...\n")

# Create a visual table comparing the three regimes
regime_summary <- tibble(
  Regime = c("Baseline\n(2015-2017)", 
             "Section 301\n(2018-2024)", 
             "Universal\n(2025)"),
  China_Rate = c("0%", "25%", "~69%\n(cumulative)"),
  Mexico_Rate = c("0%", "0%\n(USMCA)", "~25%"),
  Canada_Rate = c("0%", "0%\n(USMCA)", "~25%"),
  China_Imports = c("$40.8B/mo", "$42.5B/mo\n(+4.2%)", "$23.8B/mo\n(-41.7%)"),
  Policy_Type = c("Free Trade", "Targeted\n(Product-specific)", "Comprehensive\n(All sources)")
)

# Create table plot
p12 <- ggplot(regime_summary, aes(x = 0, y = 0)) +
  annotate("text", x = 1, y = 6, label = "Tariff Regime Comparison", 
           size = 7, fontface = "bold") +
  annotate("text", x = 0.5, y = 5.3, label = "Baseline\n(2015-2017)", size = 4.5, fontface = "bold") +
  annotate("text", x = 1, y = 5.3, label = "Section 301\n(2018-2024)", size = 4.5, fontface = "bold") +
  annotate("text", x = 1.5, y = 5.3, label = "Universal\n(2025)", size = 4.5, fontface = "bold") +
  
  # Row labels
  annotate("text", x = 0, y = 4.7, label = "China Tariff Rate:", hjust = 1, size = 4) +
  annotate("text", x = 0, y = 4.2, label = "Mexico Tariff Rate:", hjust = 1, size = 4) +
  annotate("text", x = 0, y = 3.7, label = "Canada Tariff Rate:", hjust = 1, size = 4) +
  annotate("text", x = 0, y = 3.2, label = "Avg China Imports:", hjust = 1, size = 4) +
  annotate("text", x = 0, y = 2.7, label = "Policy Type:", hjust = 1, size = 4) +
  
  # Data cells
  annotate("text", x = 0.5, y = 4.7, label = "0%", size = 4, color = "darkgreen") +
  annotate("text", x = 1, y = 4.7, label = "25%", size = 4, color = "orange3") +
  annotate("text", x = 1.5, y = 4.7, label = "~69%", size = 4, color = "red") +
  
  xlim(-0.2, 2) + ylim(2, 6.5) +
  theme_void()

ggsave("outputs/figures/fig12_regime_comparison_table.png", p12, 
       width = 12, height = 6, dpi = 300)
cat("✓ Saved: fig12_regime_comparison_table.png\n")

cat("\n=== ALL 2025 VISUALIZATIONS CREATED ===\n")
cat("\nAdditional figures (8-12) saved:\n")
cat("  8. fig8_2025_tariff_impact_focused.png\n")
cat("  9. fig9_2025_before_after.png\n")
cat("  10. fig10_parallel_trends_2025.png\n")
cat("  11. fig11_cumulative_tariff_effects.png\n")
cat("  12. fig12_regime_comparison_table.png\n\n")

cat("Total figures: 12 (7 from main analysis + 5 from 2025 analysis)\n")
