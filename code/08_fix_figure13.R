# ============================================================================
# SCRIPT 08: FIX FIGURE 13 - 2025 PROJECTIONS (IMPROVED)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING IMPROVED FIGURE 13: 2025 PROJECTIONS ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

projections <- read_csv("outputs/tables/projections_2025.csv") %>%
  mutate(date = ymd(date))

# Prepare actual data - START FROM JAN 2024 (not Jun!)
actual_recent <- did_data %>%
  filter(date >= "2024-01-01") %>%  # FIXED: Start at Jan 2024
  mutate(
    scenario = "Actual",
    projection = FALSE
  )

# Colors
country_colors <- c(
  "China" = "#e41a1c",
  "Mexico" = "#377eb8", 
  "Canada" = "#4daf4a",
  "Brazil" = "#984ea3"
)

# Create improved projection figure
p13_improved <- ggplot() +
  # Actual data - BOLD solid lines
  geom_line(data = actual_recent,
            aes(x = date, y = imports, color = country),
            linewidth = 1.5, alpha = 1) +
  
  # Mark where projections begin
  geom_vline(xintercept = as.Date("2025-08-31"),
             linetype = "dashed", color = "gray30", linewidth = 1) +
  
  # Conservative scenario - dotted
  geom_line(data = projections %>% filter(scenario == "Conservative"),
            aes(x = date, y = imports, color = country),
            linewidth = 1.2, alpha = 0.5, linetype = "dotted") +
  
  # Moderate scenario - BOLDER dashed (main projection)
  geom_line(data = projections %>% filter(scenario == "Moderate"),
            aes(x = date, y = imports, color = country),
            linewidth = 1.8, alpha = 0.8, linetype = "dashed") +  # THICKER
  
  # Aggressive scenario - dotted
  geom_line(data = projections %>% filter(scenario == "Aggressive"),
            aes(x = date, y = imports, color = country),
            linewidth = 1.2, alpha = 0.5, linetype = "dotted") +
  
  # Shading for projection period - MORE VISIBLE
  annotate("rect", xmin = as.Date("2025-09-01"), xmax = as.Date("2025-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "orange") +
  
  # Labels - IMPROVED POSITIONING
  annotate("text", x = as.Date("2024-10-01"), y = max(actual_recent$imports) * 0.98,
           label = "Actual Data", size = 5, fontface = "bold", color = "gray20") +
  
  annotate("text", x = as.Date("2025-10-15"), y = max(actual_recent$imports) * 0.98,
           label = "Projections", size = 5, fontface = "bold", color = "orange3") +
  
  annotate("text", x = as.Date("2025-08-31"), y = min(actual_recent$imports) * 1.05,
           label = "Last Observed →", hjust = 1, size = 3.5, 
           color = "gray40", fontface = "italic") +
  
  # Mark April 2025 universal tariffs
  geom_vline(xintercept = as.Date("2025-04-01"),
             linetype = "solid", color = "red", linewidth = 0.8, alpha = 0.6) +
  annotate("text", x = as.Date("2025-04-01"), y = max(actual_recent$imports) * 0.88,
           label = "Universal Tariffs →", hjust = -0.05, size = 3.5, 
           color = "red", fontface = "bold") +
  
  labs(
    title = "2025 Import Projections: September - December",
    subtitle = "Moderate scenario (dashed) based on post-universal tariff trends (April-August 2025)",
    x = NULL,
    y = "Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: FRED (actual), Author's projections (Sep-Dec 2025).\nSolid lines = actual; Bold dashed = moderate scenario; Light dotted = conservative/aggressive."
  ) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(labels = comma, limits = c(15000, 50000)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y",
               limits = c(as.Date("2024-01-01"), as.Date("2025-12-31"))) +  # FULL RANGE
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save
ggsave("outputs/figures/fig13_2025_projections.png", p13_improved,
       width = 14, height = 8, dpi = 300)

cat("✓ Saved: outputs/figures/fig13_2025_projections.png\n")
cat("  - Fixed: X-axis now starts at Jan 2024\n")
cat("  - Fixed: Projection lines are bolder and more visible\n")
cat("  - Improved: Better labels and shading\n")

cat("\n=== FIGURE 13 IMPROVED ===\n")
