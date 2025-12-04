# ============================================================================
# 2025 IMPORT PROJECTIONS (September - December)
# ============================================================================

library(tidyverse)
library(lubridate)
library(scales)

cat("=== CREATING 2025 IMPORT PROJECTIONS ===\n\n")

# Load data
did_data <- read_csv("data/processed/did_analysis_data.csv") %>%
  mutate(date = ymd(date))

# Focus on April-August 2025
post_universal <- did_data %>%
  filter(date >= "2025-04-01" & date <= "2025-08-31") %>%
  mutate(months_since = as.numeric(date - as.Date("2025-04-01")) / 30)

# Calculate trends by country
trends <- post_universal %>%
  group_by(country) %>%
  summarise(
    aug_value = last(imports),
    trend_model = list(lm(imports ~ months_since, data = cur_data())),
    monthly_change = coef(trend_model[[1]])[2],
    .groups = 'drop'
  )

print(trends)

# Create scenarios
scenarios <- tibble(
  scenario = c("Conservative", "Moderate", "Aggressive"),
  china_mult = c(0.5, 1.0, 1.5),
  mexico_mult = c(0.8, 1.0, 1.2),
  canada_mult = c(0.8, 1.0, 1.2),
  brazil_mult = c(1.0, 1.0, 1.0)
)

# Generate projections
projection_dates <- seq(as.Date("2025-09-01"), as.Date("2025-12-01"), by = "month")
projections_list <- list()

for(s in 1:nrow(scenarios)) {
  for(i in 1:nrow(trends)) {
    country_name <- trends$country[i]
    baseline <- trends$aug_value[i]
    monthly_trend <- trends$monthly_change[i]
    
    multiplier <- case_when(
      country_name == "China" ~ scenarios$china_mult[s],
      country_name == "Mexico" ~ scenarios$mexico_mult[s],
      country_name == "Canada" ~ scenarios$canada_mult[s],
      TRUE ~ scenarios$brazil_mult[s]
    )
    
    for(j in 1:length(projection_dates)) {
      set.seed(1000 + s*100 + i*10 + j)
      projected <- baseline + (monthly_trend * multiplier * j)
      projected <- projected + projected * rnorm(1, 0, 0.02)
      projected <- max(projected, 1000)
      
      projections_list <- append(projections_list, list(tibble(
        date = projection_dates[j],
        country = country_name,
        scenario = scenarios$scenario[s],
        imports = projected,
        projection = TRUE
      )))
    }
  }
}

projections <- bind_rows(projections_list)
write_csv(projections, "outputs/tables/projections_2025.csv")

cat("\n✓ Projections complete:", nrow(projections), "values\n")
