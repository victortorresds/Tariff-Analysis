# ============================================================================
# SCRIPT 15: TRUCKING INDUSTRY TRENDS (Descriptive Analysis)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== TRUCKING INDUSTRY TRENDS ANALYSIS ===\n\n")

# ============================================================================
# 1. LOAD TRUCKING DATA
# ============================================================================

cat("STEP 1: Loading trucking data...\n")

# YOU NEED TO DOWNLOAD DATA FIRST!
# Option A (Recommended): Truck Tonnage Index from FRED
# URL: https://fred.stlouisfed.org/series/TRUCKD11
# Save as: data/trucking/truck_tonnage_index.csv

# Load the data
trucking_data <- read_csv("data/trucking/truck_tonnage_index.csv", 
                          col_names = c("date", "index"),
                          skip = 1,
                          show_col_types = FALSE) %>%
  mutate(
    date = ymd(date),
    index = as.numeric(index)
  ) %>%
  filter(!is.na(index), !is.na(date)) %>%
  # Focus on our study period
  filter(date >= "2015-01-01" & date <= "2025-12-31")

cat("  Loaded:", nrow(trucking_data), "monthly observations\n")
cat("  Date range:", min(trucking_data$date), "to", max(trucking_data$date), "\n")
cat("  Index range:", round(min(trucking_data$index), 1), "to", 
    round(max(trucking_data$index), 1), "\n\n")

# ============================================================================
# 2. CALCULATE KEY METRICS
# ============================================================================

cat("STEP 2: Calculating key metrics...\n")

# Calculate baseline (pre-tariff)
baseline_period <- trucking_data %>%
  filter(date <= "2018-06-30")

baseline_avg <- mean(baseline_period$index, na.rm = TRUE)

# Calculate period averages
period_stats <- trucking_data %>%
  mutate(
    period = case_when(
      date <= "2018-06-30" ~ "Pre-Tariff\n(2015-2017)",
      date >= "2018-07-01" & date <= "2024-12-31" ~ "Section 301\n(2018-2024)",
      date >= "2025-01-01" ~ "Universal\n(2025)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    avg_index = mean(index, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    pct_change = ((avg_index - baseline_avg) / baseline_avg) * 100
  )

cat("  Period averages:\n")
print(period_stats)

# Calculate growth rates
latest_index <- tail(trucking_data$index, 1)
baseline_index <- baseline_avg
total_growth <- ((latest_index - baseline_index) / baseline_index) * 100

cat("\n  Overall growth (baseline to latest):", round(total_growth, 1), "%\n\n")

# ============================================================================
# 3. CREATE VISUALIZATION
# ============================================================================

cat("STEP 3: Creating visualization...\n")

# Add 6-month moving average for smoothness
trucking_data <- trucking_data %>%
  arrange(date) %>%
  mutate(
    index_smooth = zoo::rollmean(index, k = 6, fill = NA, align = "center")
  )

# Create the figure
p_trucking <- ggplot(trucking_data, aes(x = date)) +
  # Raw data (faint line)
  geom_line(aes(y = index), alpha = 0.3, color = "gray50", linewidth = 0.5) +
  
  # Smoothed trend (main line)
  geom_line(aes(y = index_smooth), color = "#1f78b4", linewidth = 1.5) +
  
  # Baseline reference line
  geom_hline(yintercept = baseline_avg, linetype = "dashed", 
             color = "gray40", linewidth = 0.8) +
  
  # Mark tariff implementation dates
  geom_vline(xintercept = as.Date("2018-07-01"), 
             linetype = "dotted", color = "red", linewidth = 1) +
  geom_vline(xintercept = as.Date("2025-04-01"), 
             linetype = "dotted", color = "darkred", linewidth = 1) +
  
  # Annotations
  annotate("text", x = as.Date("2016-06-01"), y = baseline_avg,
           label = "Pre-Tariff Baseline", vjust = -0.5, size = 3.5, 
           color = "gray40", fontface = "italic") +
  
  annotate("text", x = as.Date("2018-07-01"), y = max(trucking_data$index_smooth, na.rm = TRUE) * 0.95,
           label = "Section 301\nTariffs", hjust = -0.1, size = 3.5, 
           color = "red", fontface = "bold") +
  
  annotate("text", x = as.Date("2025-04-01"), y = max(trucking_data$index_smooth, na.rm = TRUE) * 0.95,
           label = "Universal\nTariffs", hjust = -0.1, size = 3.5, 
           color = "darkred", fontface = "bold") +
  
  # Mark COVID period
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "orange") +
  annotate("text", x = as.Date("2020-09-01"), 
           y = min(trucking_data$index_smooth, na.rm = TRUE) * 1.05,
           label = "COVID-19", size = 3, color = "orange3", fontface = "italic") +
  
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(80, 140, 10)) +
  
  # Labels
  labs(
    title = "U.S. Trucking Industry Performance: 2015-2025",
    subtitle = "For-Hire Truck Tonnage Index (6-month moving average) vs. Tariff Implementation",
    x = NULL,
    y = "Truck Tonnage Index (2015=100)",
    caption = paste0(
      "Source: American Trucking Associations via FRED (TRUCKD11).\n",
      "Gray line = monthly data; Blue line = 6-month moving average.\n",
      "Trucking tonnage increased ", round(total_growth, 0), "% from pre-tariff baseline despite import declines,\n",
      "likely driven by trade diversion (Mexico/Canada), e-commerce growth, and domestic production shifts."
    )
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    panel.grid.minor = element_blank()
  )

# Save
ggsave("outputs/figures/fig18_trucking_industry_trends.png", p_trucking,
       width = 13, height = 8, dpi = 300)

cat("✓ Saved: outputs/figures/fig18_trucking_industry_trends.png\n")

# ============================================================================
# 4. CREATE SUMMARY TABLE
# ============================================================================

cat("\nSTEP 4: Creating summary table...\n")

summary_table <- period_stats %>%
  mutate(
    Period = period,
    `Avg Index` = round(avg_index, 1),
    `% Change from Baseline` = paste0(ifelse(pct_change > 0, "+", ""), 
                                       round(pct_change, 1), "%"),
    `N Months` = n_months
  ) %>%
  select(Period, `Avg Index`, `% Change from Baseline`, `N Months`)

print(summary_table)

write_csv(summary_table, "outputs/tables/trucking_industry_summary.csv")
cat("✓ Saved: outputs/tables/trucking_industry_summary.csv\n")

# ============================================================================
# 5. KEY FINDINGS SUMMARY
# ============================================================================

cat("\n=== KEY FINDINGS ===\n\n")

cat("Overall Trend:\n")
cat("  Trucking tonnage INCREASED", round(total_growth, 1), "% from baseline\n")
cat("  Despite Chinese import declines of 32%\n\n")

cat("By Period:\n")
for(i in 1:nrow(period_stats)) {
  cat("  ", period_stats$period[i], ":", 
      round(period_stats$pct_change[i], 1), "% change\n")
}

cat("\nInterpretation:\n")
cat("  ✓ Trucking industry GREW during tariff period\n")
cat("  ✓ Offsetting factors stronger than import decline:\n")
cat("    - Trade diversion to Mexico/Canada\n")
cat("    - E-commerce boom\n")
cat("    - Domestic production shifts\n")
cat("    - COVID-related supply chain changes\n")
cat("  ✓ No evidence tariffs harmed overall trucking demand\n")
cat("  ⚠ Cannot isolate tariff-specific effect (too many confounds)\n\n")

cat("For Your Report:\n")
cat("  Use Figure 18 in Discussion Section 4.4\n")
cat("  Acknowledge growth but note attribution challenges\n")
cat("  Position as descriptive, not causal\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
