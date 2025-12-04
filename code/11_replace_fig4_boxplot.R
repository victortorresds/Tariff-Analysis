# ============================================================================
# SCRIPT 11: REPLACE FIGURE 4 - BOX PLOT (Treatment/Control Distributions)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING NEW FIGURE 4: BOX PLOT DISTRIBUTIONS ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Prepare for box plot
box_data <- did_data %>%
  filter(country %in% c("China", "Mexico", "Canada")) %>%
  mutate(
    group = ifelse(country == "China", "Treatment (China)", 
                   "Control (Mexico + Canada)"),
    period = ifelse(date < "2018-07-01", "Pre-Tariff\n(2015-2017)", 
                    "Post-Tariff\n(2018-2025)"),
    group_period = paste0(group, "\n", period)
  )

# Calculate summary stats
summary_stats <- box_data %>%
  group_by(group, period) %>%
  summarise(
    median = median(imports, na.rm = TRUE),
    mean = mean(imports, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# Create box plot
p4_boxplot <- ggplot(box_data, aes(x = period, y = imports, fill = group)) +
  # Box plots
  geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2,
               position = position_dodge(width = 0.85), width = 0.7) +
  
  # Add mean points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
               fill = "yellow", color = "black",
               position = position_dodge(width = 0.85)) +
  
  # Add significance bracket for China
  annotate("segment", x = 0.78, xend = 1.78, y = 52000, yend = 52000,
           color = "#e41a1c", linewidth = 1.2) +
  annotate("text", x = 1.28, y = 53500, 
           label = "DiD Effect: -$10B/month ***",
           color = "#e41a1c", fontface = "bold", size = 4) +
  
  scale_fill_manual(values = c(
    "Treatment (China)" = "#e41a1c",
    "Control (Mexico + Canada)" = "#377eb8"
  )) +
  scale_y_continuous(labels = comma, limits = c(20000, 55000)) +
  
  labs(
    title = "Import Distributions: Treatment vs. Control Groups",
    subtitle = "Box plots show full distribution; Yellow diamonds = means; DiD = difference in changes",
    x = "Period",
    y = "Monthly Import Value (Million USD)",
    fill = "Group",
    caption = "Source: FRED data, Author's calculations.\nBoxes show 25th-75th percentiles; whiskers show range; points show outliers.\nTreatment (China) declined significantly; Control (Mex+Can) remained stable/increased."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/figures/fig4_distributions_boxplot.png", p4_boxplot,
       width = 12, height = 9, dpi = 300)

cat("✓ Saved: outputs/figures/fig4_distributions_boxplot.png\n")
cat("  - REPLACED: Old simple DiD visualization\n")
cat("  - NEW: Box plot showing full distributions\n")
cat("  - Shows: Treatment declined, Control stable\n")
cat("  - Visualizes: DiD concept clearly\n")

cat("\n=== NEW FIGURE 4 CREATED ===\n")
