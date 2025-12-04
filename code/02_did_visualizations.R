# ============================================================================
# DiD VISUALIZATIONS: TARIFF IMPACTS ON US IMPORTS
# ============================================================================
# This script creates all publication-quality figures for the final project
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(ggthemes)

# Load the processed DiD data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Define color palette
country_colors <- c(
  "China" = "#e41a1c",
  "Mexico" = "#377eb8",
  "Canada" = "#4daf4a",
  "Brazil" = "#984ea3"
)

cat("=== CREATING DiD VISUALIZATIONS ===\n\n")

# ============================================================================
# FIGURE 1: RAW IMPORT TRENDS BY COUNTRY
# ============================================================================

cat("Creating Figure 1: Import trends by country...\n")

p1 <- ggplot(did_data, aes(x = date, y = imports, color = country)) +
  geom_line(size = 1.2, alpha = 0.9) +
  
  # Mark tariff implementation
  geom_vline(xintercept = as.Date("2018-07-01"), 
             linetype = "dashed", color = "gray30", size = 0.8) +
  annotate("text", x = as.Date("2018-07-01"), y = max(did_data$imports) * 0.95,
           label = "Section 301\nTariffs Begin", 
           hjust = -0.1, vjust = 1, size = 3.5, color = "gray30") +
  
  # Mark 2025 universal tariffs
  geom_vline(xintercept = as.Date("2025-04-01"), 
             linetype = "dashed", color = "gray30", size = 0.8) +
  annotate("text", x = as.Date("2025-04-01"), y = max(did_data$imports) * 0.85,
           label = "Universal\nTariffs", 
           hjust = -0.1, vjust = 1, size = 3.5, color = "gray30") +
  
  # Shading for periods
  annotate("rect", xmin = as.Date("2015-01-01"), xmax = as.Date("2017-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "blue") +
  annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2024-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "orange") +
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = as.Date("2025-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "red") +
  
  labs(
    title = "US Import Volumes by Country (2015-2025)",
    subtitle = "Treatment: China (Section 301 tariffs) vs. Control: Mexico & Canada (USMCA exempt)",
    x = NULL,
    y = "Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: Federal Reserve Economic Data (FRED)\nVertical lines mark tariff implementation dates"
  ) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig1_import_trends_raw.png", p1, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig1_import_trends_raw.png\n")

# ============================================================================
# FIGURE 2: INDEXED IMPORT TRENDS (Baseline = 100)
# ============================================================================

cat("Creating Figure 2: Indexed import trends...\n")

# Calculate baseline (2015-2017 average)
baseline_imports <- did_data %>%
  filter(year(date) <= 2017) %>%
  group_by(country) %>%
  summarise(baseline = mean(imports, na.rm = TRUE), .groups = 'drop')

# Create indexed data
indexed_data <- did_data %>%
  left_join(baseline_imports, by = "country") %>%
  mutate(indexed_imports = (imports / baseline) * 100)

p2 <- ggplot(indexed_data, aes(x = date, y = indexed_imports, color = country)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_line(size = 1.2, alpha = 0.9) +
  
  # Mark tariff dates
  geom_vline(xintercept = as.Date("2018-07-01"), 
             linetype = "dotted", color = "gray30", size = 0.6) +
  geom_vline(xintercept = as.Date("2025-04-01"), 
             linetype = "dotted", color = "gray30", size = 0.6) +
  
  annotate("text", x = as.Date("2016-06-01"), y = 102,
           label = "Pre-Tariff Baseline (100)", 
           hjust = 0, size = 3.5, color = "gray40", fontface = "italic") +
  
  labs(
    title = "Normalized Import Trends: Treatment vs. Control Groups",
    subtitle = "Indexed to 2015-2017 baseline = 100",
    x = NULL,
    y = "Index (2015-2017 Average = 100)",
    color = "Country",
    caption = "Source: FRED. Values normalized to pre-tariff baseline for comparability."
  ) +
  scale_color_manual(values = country_colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig2_import_trends_indexed.png", p2, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig2_import_trends_indexed.png\n")

# ============================================================================
# FIGURE 3: PARALLEL TRENDS TEST (Pre-2018)
# ============================================================================

cat("Creating Figure 3: Parallel trends visualization...\n")

# Pre-treatment data only
pre_treatment <- did_data %>%
  filter(year(date) <= 2017) %>%
  mutate(
    group = ifelse(country == "China", "Treatment (China)", 
                   "Control (Mexico + Canada)")
  )

# Calculate group averages
pre_treatment_avg <- pre_treatment %>%
  group_by(date, group) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop')

p3 <- ggplot(pre_treatment_avg, aes(x = date, y = avg_imports, color = group)) +
  geom_line(size = 1.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linetype = "dashed") +
  
  labs(
    title = "Parallel Trends Test: Pre-Treatment Period (2015-2017)",
    subtitle = "Testing DiD assumption: Do treatment and control groups have similar pre-tariff trends?",
    x = NULL,
    y = "Average Monthly Import Value (Million USD)",
    color = "Group",
    caption = "Source: FRED. Dashed lines show linear trends. Similar slopes indicate parallel trends."
  ) +
  scale_color_manual(values = c("Treatment (China)" = "#e41a1c",
                                 "Control (Mexico + Canada)" = "#377eb8")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/fig3_parallel_trends.png", p3, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig3_parallel_trends.png\n")

# ============================================================================
# FIGURE 4: DiD VISUALIZATION (Treatment Effect Over Time)
# ============================================================================

cat("Creating Figure 4: DiD visualization...\n")

# Calculate group × period averages
did_summary <- did_data %>%
  mutate(
    group = ifelse(country == "China", "Treatment", "Control"),
    period = ifelse(post_tariff == 1, "Post-Tariff", "Pre-Tariff")
  ) %>%
  group_by(group, period) %>%
  summarise(
    avg_imports = mean(imports, na.rm = TRUE),
    se = sd(imports, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p4 <- ggplot(did_summary, aes(x = period, y = avg_imports, 
                               color = group, group = group)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = avg_imports - 1.96*se, 
                    ymax = avg_imports + 1.96*se), 
                width = 0.1, size = 1) +
  
  # Add value labels
  geom_text(aes(label = paste0("$", round(avg_imports/1000, 1), "B")),
            vjust = -1.5, size = 4, fontface = "bold") +
  
  labs(
    title = "Difference-in-Differences: Tariff Impact on Import Volumes",
    subtitle = "Average monthly imports before and after Section 301 tariffs (July 2018)",
    x = NULL,
    y = "Average Monthly Import Value (Million USD)",
    color = "Group",
    caption = "Source: FRED. Error bars show 95% confidence intervals.\nDiD estimate = (Post-Pre)_Treatment - (Post-Pre)_Control"
  ) +
  scale_color_manual(values = c("Treatment" = "#e41a1c",
                                 "Control" = "#377eb8")) +
  scale_y_continuous(labels = comma, 
                     limits = c(min(did_summary$avg_imports) * 0.9,
                                max(did_summary$avg_imports) * 1.1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig4_did_visualization.png", p4, 
       width = 10, height = 7, dpi = 300)
cat("✓ Saved: fig4_did_visualization.png\n")

# ============================================================================
# FIGURE 5: CHINA vs. CONTROL GROUP (Side-by-Side)
# ============================================================================

cat("Creating Figure 5: Treatment vs. control comparison...\n")

# Calculate monthly averages for treatment vs. control
comparison_data <- did_data %>%
  mutate(
    group = ifelse(country == "China", "Treatment: China", 
                   "Control: Mexico + Canada")
  ) %>%
  group_by(date, group) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop')

p5 <- ggplot(comparison_data, aes(x = date, y = avg_imports, color = group)) +
  geom_line(size = 1.3, alpha = 0.9) +
  
  # Shading and labels
  annotate("rect", xmin = as.Date("2015-01-01"), xmax = as.Date("2018-06-30"),
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "blue") +
  annotate("rect", xmin = as.Date("2018-07-01"), xmax = max(comparison_data$date),
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "orange") +
  
  annotate("text", x = as.Date("2016-06-01"), y = max(comparison_data$avg_imports),
           label = "Pre-Treatment\n(Parallel Trends)", 
           vjust = 1, size = 4, color = "gray30", fontface = "italic") +
  
  annotate("text", x = as.Date("2021-06-01"), y = max(comparison_data$avg_imports),
           label = "Post-Treatment\n(Divergence = Causal Effect)", 
           vjust = 1, size = 4, color = "gray30", fontface = "italic") +
  
  geom_vline(xintercept = as.Date("2018-07-01"), 
             linetype = "solid", color = "red", size = 1) +
  
  labs(
    title = "DiD Framework: Treatment vs. Control Group Comparison",
    subtitle = "Section 301 tariffs created a natural experiment in July 2018",
    x = NULL,
    y = "Average Monthly Import Value (Million USD)",
    color = NULL,
    caption = "Source: FRED. Red vertical line marks tariff implementation.\nShaded regions show pre/post periods."
  ) +
  scale_color_manual(values = c("Treatment: China" = "#e41a1c",
                                 "Control: Mexico + Canada" = "#377eb8")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/fig5_treatment_vs_control.png", p5, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig5_treatment_vs_control.png\n")

# ============================================================================
# FIGURE 6: REGRESSION COEFFICIENT PLOT
# ============================================================================

cat("Creating Figure 6: Regression coefficient plot...\n")

# Load regression table
regression_table <- readRDS("outputs/tables/did_regression_table.rds")

# Create coefficient plot
p6 <- ggplot(regression_table, 
             aes(x = reorder(Model, -DiD_Coefficient), y = DiD_Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, color = "#e41a1c") +
  geom_errorbar(aes(ymin = DiD_Coefficient - 1.96*Std_Error,
                    ymax = DiD_Coefficient + 1.96*Std_Error),
                width = 0.2, size = 1, color = "#e41a1c") +
  
  # Add value labels
  geom_text(aes(label = round(DiD_Coefficient, 0)),
            hjust = -0.5, size = 3.5, fontface = "bold") +
  
  coord_flip() +
  
  labs(
    title = "DiD Estimates Across Model Specifications",
    subtitle = "Causal effect of Section 301 tariffs on Chinese imports (vs. control group)",
    x = NULL,
    y = "DiD Coefficient (Million USD per Month)",
    caption = "Source: Author's calculations. Error bars show 95% confidence intervals.\nNegative values indicate decreased imports."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11)
  )

ggsave("outputs/figures/fig6_coefficient_plot.png", p6, 
       width = 10, height = 7, dpi = 300)
cat("✓ Saved: fig6_coefficient_plot.png\n")

# ============================================================================
# FIGURE 7: 2025 UNIVERSAL TARIFF IMPACT
# ============================================================================

cat("Creating Figure 7: 2025 universal tariff impact...\n")

# Focus on 2024-2025 for clarity
recent_data <- did_data %>%
  filter(year(date) >= 2024)

p7 <- ggplot(recent_data, aes(x = date, y = imports, color = country)) +
  geom_line(size = 1.3, alpha = 0.9) +
  
  geom_vline(xintercept = as.Date("2025-04-01"), 
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = as.Date("2025-04-01"), y = max(recent_data$imports),
           label = "Universal Tariffs\n(April 2025)", 
           hjust = -0.1, vjust = 1, size = 4, color = "red", fontface = "bold") +
  
  labs(
    title = "Impact of 2025 Universal Tariffs on Import Patterns",
    subtitle = "10% baseline + country-specific reciprocal tariffs implemented April 2025",
    x = NULL,
    y = "Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: FRED. Sharp decline in Chinese imports visible after April 2025."
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

ggsave("outputs/figures/fig7_2025_universal_tariffs.png", p7, 
       width = 12, height = 7, dpi = 300)
cat("✓ Saved: fig7_2025_universal_tariffs.png\n")

cat("\n=== ALL VISUALIZATIONS CREATED ===\n")
cat("\nFigures saved in outputs/figures/:\n")
cat("  1. fig1_import_trends_raw.png\n")
cat("  2. fig2_import_trends_indexed.png\n")
cat("  3. fig3_parallel_trends.png\n")
cat("  4. fig4_did_visualization.png\n")
cat("  5. fig5_treatment_vs_control.png\n")
cat("  6. fig6_coefficient_plot.png\n")
cat("  7. fig7_2025_universal_tariffs.png\n")
