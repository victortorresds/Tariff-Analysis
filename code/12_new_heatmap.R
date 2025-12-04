# ============================================================================
# SCRIPT 12: NEW FIGURE 15 - HEATMAP (Country × Year Matrix)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING NEW FIGURE 15: HEATMAP ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Calculate yearly averages
yearly_avg <- did_data %>%
  mutate(year = year(date)) %>%
  group_by(country, year) %>%
  summarise(avg_imports = mean(imports, na.rm = TRUE), .groups = 'drop')

# Calculate baseline (2015-2017)
baseline <- yearly_avg %>%
  filter(year <= 2017) %>%
  group_by(country) %>%
  summarise(baseline = mean(avg_imports), .groups = 'drop')

# Calculate percent changes
heatmap_data <- yearly_avg %>%
  left_join(baseline, by = "country") %>%
  mutate(
    pct_change = ((avg_imports - baseline) / baseline) * 100,
    # Format for display
    label = ifelse(abs(pct_change) < 1, "0%",
                   paste0(ifelse(pct_change > 0, "+", ""),
                          round(pct_change, 0), "%"))
  )

print(heatmap_data %>% select(country, year, pct_change))

# Create heatmap
p15_heatmap <- ggplot(heatmap_data, aes(x = factor(year), y = country, 
                                         fill = pct_change)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = label), size = 4, fontface = "bold", color = "white") +
  
  # Color scale: red = decline, blue = increase
  scale_fill_gradient2(
    low = "#d73027",        # Red (decline)
    mid = "#ffffbf",        # Yellow (no change)
    high = "#4575b4",       # Blue (increase)
    midpoint = 0,
    limits = c(-50, 50),
    breaks = seq(-50, 50, 10),
    labels = function(x) paste0(x, "%"),
    name = "% Change from\nBaseline\n(2015-2017)"
  ) +
  
  # Mark tariff implementation years
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.5, ymax = 4.5,
           fill = NA, color = "orange", linewidth = 2) +
  annotate("text", x = 4, y = 4.8, label = "2018: Section 301",
           color = "orange", fontface = "bold", size = 3.5) +
  
  annotate("rect", xmin = 10.5, xmax = 11.5, ymin = 0.5, ymax = 4.5,
           fill = NA, color = "red", linewidth = 2) +
  annotate("text", x = 11, y = 4.8, label = "2025: Universal",
           color = "red", fontface = "bold", size = 3.5) +
  
  labs(
    title = "Import Changes Heatmap: Entire Tariff Story at a Glance",
    subtitle = "% change from baseline (2015-2017) by country and year",
    x = "Year",
    y = NULL,
    caption = "Source: FRED, Author's calculations.\nRed = decline; Blue = increase; Yellow = no change.\nChina: Persistent decline after 2018 tariffs, accelerating in 2025.\nMexico/Canada: Steady increases throughout (beneficiaries of trade diversion)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11)
  )

ggsave("outputs/figures/fig15_import_changes_heatmap.png", p15_heatmap,
       width = 14, height = 7, dpi = 300)

cat("✓ Saved: outputs/figures/fig15_import_changes_heatmap.png\n")
cat("  - NEW: Heatmap showing entire period at once\n")
cat("  - Red cells: China's persistent decline\n")
cat("  - Blue cells: Mexico/Canada's gains\n")
cat("  - Story: Trade diversion clearly visible!\n")

cat("\n=== NEW FIGURE 15 CREATED ===\n")
