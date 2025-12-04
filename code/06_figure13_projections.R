# ============================================================================
# FIGURE 13: 2025 PROJECTIONS VISUALIZATION
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING FIGURE 13: 2025 PROJECTIONS ===\n\n")

# Load actual data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Load projections
projections <- read_csv("outputs/tables/projections_2025.csv") %>%
  mutate(date = ymd(date))

# Prepare actual data (2024 onwards for cleaner visualization)
actual_recent <- did_data %>%
  filter(date >= "2024-01-01") %>%
  mutate(
    scenario = "Actual",
    projection = FALSE
  )

# Color palette
country_colors <- c(
  "China" = "#e41a1c",
  "Mexico" = "#377eb8",
  "Canada" = "#4daf4a",
  "Brazil" = "#984ea3"
)

# Create the projection figure
p13 <- ggplot() +
  # Actual data - solid lines
  geom_line(data = actual_recent,
            aes(x = date, y = imports, color = country),
            size = 1.4, alpha = 0.9) +
  
  # Mark where projections begin
  geom_vline(xintercept = as.Date("2025-08-31"),
             linetype = "dashed", color = "gray30", size = 0.8) +
  
  # Conservative scenario - faint dotted
  geom_line(data = projections %>% filter(scenario == "Conservative"),
            aes(x = date, y = imports, color = country),
            size = 1, alpha = 0.3, linetype = "dotted") +
  
  # Moderate scenario - main projection (dashed)
  geom_line(data = projections %>% filter(scenario == "Moderate"),
            aes(x = date, y = imports, color = country),
            size = 1.2, alpha = 0.7, linetype = "dashed") +
  
  # Aggressive scenario - faint dotted
  geom_line(data = projections %>% filter(scenario == "Aggressive"),
            aes(x = date, y = imports, color = country),
            size = 1, alpha = 0.3, linetype = "dotted") +
  
  # Shading for projection period
  annotate("rect", xmin = as.Date("2025-09-01"), xmax = as.Date("2025-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "orange") +
  
  # Labels
  annotate("text", x = as.Date("2024-11-01"), y = max(actual_recent$imports) * 0.95,
           label = "Actual Data\n(Jan 2024 - Aug 2025)", 
           size = 4, fontface = "bold", color = "gray30") +
  
  annotate("text", x = as.Date("2025-10-15"), y = max(actual_recent$imports) * 0.95,
           label = "Projections\n(Sep - Dec 2025)", 
           size = 4, fontface = "bold", color = "orange3") +
  
  annotate("text", x = as.Date("2025-08-31"), y = max(actual_recent$imports) * 0.85,
           label = "← Aug 2025\n(Last observed)", 
           hjust = 1.1, size = 3.5, color = "gray40") +
  
  # Mark April 2025 universal tariffs
  geom_vline(xintercept = as.Date("2025-04-01"),
             linetype = "solid", color = "red", size = 0.6, alpha = 0.5) +
  annotate("text", x = as.Date("2025-04-01"), y = max(actual_recent$imports) * 0.75,
           label = "Universal\nTariffs", hjust = -0.1, size = 3, color = "red") +
  
  labs(
    title = "2025 Import Projections: September - December",
    subtitle = "Three scenarios based on post-universal tariff trends (April-August 2025)",
    x = NULL,
    y = "Monthly Import Value (Million USD)",
    color = "Country",
    caption = "Source: FRED (actual), Author's projections (Sep-Dec 2025).\nSolid lines = actual data; Dashed lines = moderate scenario; Dotted = conservative/aggressive scenarios."
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

# Save figure
ggsave("outputs/figures/fig13_2025_projections.png", p13,
       width = 13, height = 8, dpi = 300)

cat("✓ Saved: outputs/figures/fig13_2025_projections.png\n")

# Create projection summary table for report
projection_summary <- projections %>%
  group_by(country, scenario) %>%
  summarise(
    avg_sep_dec = mean(imports, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = scenario, values_from = avg_sep_dec)

# Add August 2025 actual for comparison
aug_2025 <- did_data %>%
  filter(date == "2025-08-01") %>%
  select(country, Aug_2025_Actual = imports)

projection_summary <- projection_summary %>%
  left_join(aug_2025, by = "country") %>%
  select(country, Aug_2025_Actual, Conservative, Moderate, Aggressive)

print(projection_summary)

write_csv(projection_summary, "outputs/tables/projection_summary_for_report.csv")
cat("✓ Saved: outputs/tables/projection_summary_for_report.csv\n")

cat("\n=== FIGURE 13 COMPLETE ===\n")
