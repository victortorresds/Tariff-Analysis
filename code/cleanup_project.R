# ============================================================================
# PROJECT CLEANUP SCRIPT - Final Organization
# ============================================================================

library(tidyverse)

cat("=== CLEANING UP YOUR FINAL PROJECT ===\n\n")

setwd("C:/Users/vitug/OneDrive/Desktop/CUNY Masters/DATA_698/Final_Project")

# ============================================================================
# 1. DELETE OLD/BROKEN FIGURES
# ============================================================================

cat("STEP 1: Deleting old/broken figures...\n")

figures_to_delete <- c(
  "outputs/figures/fig6_coefficient_plot.png",  # Old boring version
  "outputs/figures/fig16_transport_vs_appliances.png",  # Misleading scale
  "outputs/figures/fig16_industry_comparison_percentages.png",  # Broken (missing appliances)
  "outputs/figures/fig16_industry_comparison_panels.png",  # Not needed
  "outputs/figures/fig12_regime_comparison_table.png"  # Keep bar version only
)

deleted_count <- 0
for (fig in figures_to_delete) {
  if (file.exists(fig)) {
    file.remove(fig)
    cat("  ✓ Deleted:", basename(fig), "\n")
    deleted_count <- deleted_count + 1
  }
}

cat("  Total deleted:", deleted_count, "figures\n\n")

# ============================================================================
# 2. RENAME FINAL FIGURES FOR CLARITY
# ============================================================================

cat("STEP 2: Renaming figures for clarity...\n")

# Create renaming map (only if needed)
rename_map <- tribble(
  ~old_name, ~new_name,
  "fig4_distributions_boxplot.png", "fig4_treatment_control_boxplot.png",
  "fig6_tariff_vs_decline_scatter.png", "fig6_tariff_decline_relationship.png",
  "fig12_regime_comparison_bars.png", "fig12_regime_comparison.png",
  "fig16_differential_impacts.png", "fig16_differential_impacts.png"  # Keep as is
)

# Optional: Uncomment if you want to rename
# for (i in 1:nrow(rename_map)) {
#   old_path <- paste0("outputs/figures/", rename_map$old_name[i])
#   new_path <- paste0("outputs/figures/", rename_map$new_name[i])
#   if (file.exists(old_path) && !file.exists(new_path)) {
#     file.rename(old_path, new_path)
#     cat("  ✓ Renamed:", rename_map$old_name[i], "→", rename_map$new_name[i], "\n")
#   }
# }

cat("  (Skipped renaming - keeping original names)\n\n")

# ============================================================================
# 3. ORGANIZE CODE FILES
# ============================================================================

cat("STEP 3: Organizing code files...\n")

# Create archive folder for intermediate scripts
dir.create("code/archive", showWarnings = FALSE)

scripts_to_archive <- c(
  "code/13_appliance_comparison.R",  # First attempt (broken)
  "code/13_appliance_comparison_FIXED.R",  # Still had issues
  "code/13b_percentage_comparison.R",  # Intermediate
  "code/13c_percentage_fixed.R",  # Intermediate
  "code/13d_fresh_start.R",  # Intermediate
  "code/diagnostic_appliances.R"  # Diagnostic (keep for reference)
)

archived_count <- 0
for (script in scripts_to_archive) {
  if (file.exists(script)) {
    archive_path <- paste0("code/archive/", basename(script))
    file.copy(script, archive_path, overwrite = TRUE)
    file.remove(script)
    cat("  ✓ Archived:", basename(script), "\n")
    archived_count <- archived_count + 1
  }
}

cat("  Total archived:", archived_count, "scripts\n\n")

# ============================================================================
# 4. CREATE FINAL FIGURE INVENTORY
# ============================================================================

cat("STEP 4: Creating final figure inventory...\n")

all_figures <- list.files("outputs/figures", pattern = "\\.png$", full.names = FALSE)

figure_inventory <- tibble(
  filename = all_figures
) %>%
  mutate(
    fig_num = str_extract(filename, "fig\\d+[a-z]?"),
    category = case_when(
      str_detect(filename, "fig[1-9]_|fig10_|fig11_") ~ "Main Analysis",
      str_detect(filename, "fig1[2-3]") ~ "Main Analysis",
      str_detect(filename, "fig14") ~ "Robustness Check",
      str_detect(filename, "fig15") ~ "Overview/Summary",
      str_detect(filename, "fig16") ~ "Industry Comparison",
      str_detect(filename, "fig17") ~ "Distribution Analysis",
      TRUE ~ "Other"
    ),
    use_in_paper = case_when(
      category == "Main Analysis" ~ "Results Section",
      category == "Robustness Check" ~ "Appendix A",
      category == "Overview/Summary" ~ "Results Section",
      category == "Industry Comparison" ~ "Results Section 3.5",
      category == "Distribution Analysis" ~ "Appendix B",
      TRUE ~ "Decide"
    )
  ) %>%
  arrange(filename)

cat("\n=== FIGURE INVENTORY ===\n")
print(figure_inventory, n = 50)

write_csv(figure_inventory, "outputs/FIGURE_INVENTORY.csv")
cat("\n✓ Saved: outputs/FIGURE_INVENTORY.csv\n\n")

# ============================================================================
# 5. UPDATE TABLES
# ============================================================================

cat("STEP 5: Checking tables...\n")

all_tables <- list.files("outputs/tables", pattern = "\\.csv$", full.names = FALSE)

cat("  Current tables:\n")
for (tbl in all_tables) {
  cat("    -", tbl, "\n")
}

# Archive old/unnecessary tables
tables_to_archive <- c(
  "outputs/tables/industry_comparison.csv",  # Superseded by differential
  "outputs/tables/industry_comparison_summary.csv",  # Superseded
  "outputs/tables/industry_comparison_percentages.csv"  # Superseded
)

dir.create("outputs/tables/archive", showWarnings = FALSE)

for (tbl in tables_to_archive) {
  if (file.exists(tbl)) {
    archive_path <- paste0("outputs/tables/archive/", basename(tbl))
    file.copy(tbl, archive_path, overwrite = TRUE)
    file.remove(tbl)
    cat("  ✓ Archived:", basename(tbl), "\n")
  }
}

cat("\n")

# ============================================================================
# 6. CREATE FINAL CODE INVENTORY
# ============================================================================

cat("STEP 6: Creating final code inventory...\n")

all_scripts <- list.files("code", pattern = "\\.R$", full.names = FALSE)

code_inventory <- tibble(
  filename = all_scripts
) %>%
  mutate(
    script_num = str_extract(filename, "^\\d+[a-z]?"),
    purpose = case_when(
      str_detect(filename, "01_did_analysis") ~ "DiD Analysis 2018",
      str_detect(filename, "02_did_visualizations") ~ "DiD Visualizations 2018",
      str_detect(filename, "03_did_analysis_2025") ~ "DiD Analysis 2025",
      str_detect(filename, "04_visualizations_2025") ~ "DiD Visualizations 2025",
      str_detect(filename, "05_projections") ~ "2025 Projections",
      str_detect(filename, "06") ~ "Fix Figure 13",
      str_detect(filename, "07") ~ "Census Robustness Check",
      str_detect(filename, "08") ~ "Fix Figure 13 (Improved)",
      str_detect(filename, "09") ~ "Fix Figure 12",
      str_detect(filename, "10") ~ "New Figure 6 (Scatter)",
      str_detect(filename, "11") ~ "New Figure 4 (Boxplot)",
      str_detect(filename, "12") ~ "New Figure 15 (Heatmap)",
      str_detect(filename, "13e") ~ "Figure 16 (Differential Impacts) - FINAL",
      str_detect(filename, "14") ~ "Figure 17 (Violin Plots)",
      TRUE ~ "Other"
    ),
    keep = !str_detect(filename, "13[^e]")  # Keep 13e, archive others
  ) %>%
  arrange(filename)

cat("\n=== ACTIVE SCRIPTS ===\n")
print(code_inventory %>% filter(keep), n = 50)

write_csv(code_inventory, "outputs/CODE_INVENTORY.csv")
cat("\n✓ Saved: outputs/CODE_INVENTORY.csv\n\n")

# ============================================================================
# 7. CREATE FINAL PROJECT SUMMARY
# ============================================================================

cat("STEP 7: Creating project summary...\n")

summary_stats <- tibble(
  Metric = c(
    "Total Figures",
    "Main Analysis Figures",
    "Robustness/Extension Figures",
    "Active R Scripts",
    "Data Sources",
    "Countries Analyzed",
    "Time Period",
    "Analysis Methods"
  ),
  Count = c(
    length(all_figures),
    sum(figure_inventory$category == "Main Analysis"),
    length(all_figures) - sum(figure_inventory$category == "Main Analysis"),
    sum(code_inventory$keep),
    "3 (FRED, Census Truck Parts, Census Appliances)",
    "4 (China, Mexico, Canada, Brazil)",
    "2015-2025 (132 months)",
    "DiD, Robustness Checks, Projections, Product Comparison"
  )
)

cat("\n=== PROJECT SUMMARY ===\n")
print(summary_stats, n = 50)

write_csv(summary_stats, "outputs/PROJECT_SUMMARY.csv")
cat("\n✓ Saved: outputs/PROJECT_SUMMARY.csv\n\n")

# ============================================================================
# 8. FINAL VERIFICATION
# ============================================================================

cat("STEP 8: Final verification...\n\n")

# Check key files exist
key_files <- c(
  "outputs/figures/fig1_import_trends_raw.png",
  "outputs/figures/fig13_2025_projections.png",
  "outputs/figures/fig15_import_changes_heatmap.png",
  "outputs/figures/fig16_differential_impacts.png",
  "data/processed/did_analysis_data.csv",
  "outputs/models/did_model_basic.rds"
)

cat("Checking key files:\n")
all_exist <- TRUE
for (file in key_files) {
  exists <- file.exists(file)
  all_exist <- all_exist && exists
  cat("  ", ifelse(exists, "✓", "✗"), basename(file), "\n")
}

if (all_exist) {
  cat("\n🎉 ALL KEY FILES PRESENT! 🎉\n")
} else {
  cat("\n⚠ Some key files missing - check above\n")
}

# ============================================================================
# 9. SUMMARY
# ============================================================================

cat("\n\n")
cat("=" %>% rep(60) %>% paste(collapse = ""))
cat("\n=== CLEANUP COMPLETE ===\n")
cat("=" %>% rep(60) %>% paste(collapse = ""))
cat("\n\n")

cat("DELETED:\n")
cat("  - ", deleted_count, "old/broken figures\n")
cat("  - ", archived_count, "intermediate scripts\n")
cat("  - Old comparison tables\n\n")

cat("ORGANIZED:\n")
cat("  - Active scripts in code/\n")
cat("  - Archived scripts in code/archive/\n")
cat("  - All figures inventoried\n\n")

cat("FINAL COUNTS:\n")
cat("  - Figures:", length(all_figures), "\n")
cat("  - Active Scripts:", sum(code_inventory$keep), "\n")
cat("  - Tables:", length(list.files("outputs/tables", pattern = "\\.csv$")), "\n\n")

cat("NEXT STEPS:\n")
cat("  1. Review FIGURE_INVENTORY.csv\n")
cat("  2. Review CODE_INVENTORY.csv\n")
cat("  3. Start writing your paper!\n\n")

cat("✅ YOUR PROJECT IS CLEAN AND READY! ✅\n\n")
