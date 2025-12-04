# Impact of US Tariffs on Transportation Equipment Imports

**A Difference-in-Differences Analysis (2015-2025)**

[![R](https://img.shields.io/badge/R-4.3+-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Complete-green.svg)]()

## Key Findings

- **Section 301 tariffs (2018):** Reduced Chinese imports by $10 billion/month (-25%)
- **Universal 2025 tariffs:** Additional decline to -32% from baseline  
- **Trade diversion:** Mexico +30%, Canada +10%
- **Product heterogeneity:** Transportation equipment declined while appliances increased

## Methodology

Difference-in-differences econometric analysis featuring:
- **Treatment group:** China (received tariffs)
- **Control groups:** Mexico, Canada (no new tariffs)
- **Sample:** 524 observations (4 countries × 131 months, 2015-2025)
- **Robustness:** Product-level validation, multiple specifications
- **Visualizations:** 20 publication-quality figures

## View Complete Analysis

**[→ Click here for full analysis with all figures and code](tariff_analysis.md)**

## Repository Structure
```
Tariff-Analysis/
├── PORTFOLIO_CODE_WITH_DATA.md    # Complete analysis (view this!)
├── PORTFOLIO_CODE_WITH_DATA.Rmd   # Source R Markdown
├── data/                          # All datasets (fully reproducible)
│   ├── fred_imports/              # FRED import data by country
│   ├── census_imports/            # Product-level validation
│   ├── household_appliances/      # Heterogeneity analysis
│   ├── trucking/                  # Industry context
│   └── processed/                 # Main DiD dataset
├── outputs/
│   └── figures/                   # 20 high-resolution visualizations
└── code/                          # R analysis scripts
```

## Replication Instructions


```r
# Clone repository
git clone https://github.com/victortorresds/Tariff-Analysis.git

# Navigate to directory
setwd("path/to/Tariff-Analysis")

# Render the analysis
rmarkdown::render("tariff_analysis.Rmd", output_format = "github_document")
```

## Key Visualizations

### Import Trends by Country
![Import Trends](outputs/figures/fig1_import_trends_raw.png)

### Trade Diversion Heatmap  
![Trade Diversion](outputs/figures/fig15_import_changes_heatmap.png)

### Product Heterogeneity
![Product Comparison](fig16b_industry_comparison_timeseries.png)

## Technical Details

- **Software:** R 4.3+
- **Key Packages:** tidyverse, ggplot2, broom
- **Method:** OLS difference-in-differences with clustered standard errors
- **Data Sources:** Federal Reserve Economic Data (FRED), US Census Bureau

## Author

**Victor Torres**  
MS Data Science Candidate  
CUNY School of Professional Studies  
Graduation: December 2025

**Portfolio:** [GitHub](https://github.com/victortorresds) | [LinkedIn](https://linkedin.com/in/vitugo)