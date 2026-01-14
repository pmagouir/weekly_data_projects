# =============================================================================
# 02_clean.R - CPI Food Price Data Cleaning
# Processes raw CPI data and creates analysis-ready datasets
# =============================================================================

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Raw Data
# -----------------------------------------------------------------------------

raw_path <- here("explorations", "(2)wholefood-inflation", "data", "raw")
processed_path <- here("explorations", "(2)wholefood-inflation", "data", "processed")

cpi_raw <- read_csv(
  file.path(raw_path, "cpi_food_data_2015_2024.csv"),
  show_col_types = FALSE
)

cat("Loaded", nrow(cpi_raw), "CPI observations\n")

# -----------------------------------------------------------------------------
# Categorize Series
# -----------------------------------------------------------------------------

# Define categories
whole_foods <- c("meats", "fruits_vegetables", "dairy", "eggs", "milk", "nuts", "beans")
processed_foods <- c("sweets", "candy", "desserts", "frozen_foods", "processed_foods")

cpi_clean <- cpi_raw |>
  mutate(
    # Extract year and month
    year = year(date),
    month = month(date),
    year_month = floor_date(date, "month"),
    
    # Categorize as whole food or processed
    category = case_when(
      series_name %in% whole_foods ~ "whole_food",
      series_name %in% processed_foods ~ "processed_food",
      str_detect(series_name, "_dc") ~ "dc_regional",
      TRUE ~ "other"
    ),
    
    # Identify regional data
    is_regional = str_detect(series_name, "_dc"),
    region = if_else(is_regional, "DC", "National")
  )

# -----------------------------------------------------------------------------
# Create Base Year Index (2015 = 100)
# -----------------------------------------------------------------------------

# Calculate index relative to 2015 average
cpi_indexed <- cpi_clean |>
  group_by(series_name) |>
  mutate(
    # Get 2015 average as base
    base_2015 = mean(value[year == 2015], na.rm = TRUE),
    # Calculate index (2015 = 100)
    index_2015 = (value / base_2015) * 100,
    # Calculate percent change from 2015
    pct_change_2015 = ((value - base_2015) / base_2015) * 100
  ) |>
  ungroup()

# -----------------------------------------------------------------------------
# Aggregate by Category
# -----------------------------------------------------------------------------

# Overall food categories (average of all items)
category_monthly <- cpi_indexed |>
  filter(category %in% c("whole_food", "processed_food")) |>
  group_by(year_month, category, region) |>
  summarize(
    n_series = n_distinct(series_name),
    avg_index = mean(index_2015, na.rm = TRUE),
    avg_pct_change = mean(pct_change_2015, na.rm = TRUE),
    .groups = "drop"
  )

# Annual aggregates
category_annual <- cpi_indexed |>
  filter(category %in% c("whole_food", "processed_food")) |>
  group_by(year, category, region) |>
  summarize(
    n_series = n_distinct(series_name),
    avg_index = mean(index_2015, na.rm = TRUE),
    avg_pct_change = mean(pct_change_2015, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Item-Level Data
# -----------------------------------------------------------------------------

# Individual item series
items_monthly <- cpi_indexed |>
  filter(category %in% c("whole_food", "processed_food")) |>
  select(year_month, year, month, series_name, category, region, 
         value, index_2015, pct_change_2015) |>
  arrange(series_name, year_month)

items_annual <- cpi_indexed |>
  filter(category %in% c("whole_food", "processed_food")) |>
  group_by(year, series_name, category, region) |>
  summarize(
    avg_index = mean(index_2015, na.rm = TRUE),
    avg_pct_change = mean(pct_change_2015, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# DC vs National Comparison
# -----------------------------------------------------------------------------

# Compare DC regional to national averages
dc_comparison <- cpi_indexed |>
  filter(is_regional | !str_detect(series_name, "_dc")) |>
  mutate(
    series_base = str_remove(series_name, "_dc"),
    region = if_else(is_regional, "DC", "National")
  ) |>
  select(year_month, year, series_base, region, index_2015, pct_change_2015) |>
  pivot_wider(
    names_from = region,
    values_from = c(index_2015, pct_change_2015),
    names_sep = "_"
  ) |>
  mutate(
    dc_vs_national_diff = index_2015_DC - index_2015_National,
    dc_premium_pct = ((index_2015_DC - index_2015_National) / index_2015_National) * 100
  ) |>
  filter(!is.na(index_2015_DC) & !is.na(index_2015_National))

# -----------------------------------------------------------------------------
# Save Processed Data
# -----------------------------------------------------------------------------

# Save individual files
write_csv(category_monthly, file.path(processed_path, "category_monthly.csv"))
write_csv(category_annual, file.path(processed_path, "category_annual.csv"))
write_csv(items_monthly, file.path(processed_path, "items_monthly.csv"))
write_csv(items_annual, file.path(processed_path, "items_annual.csv"))
write_csv(dc_comparison, file.path(processed_path, "dc_comparison.csv"))

cat("\nProcessed data saved:\n")
cat("  - category_monthly.csv\n")
cat("  - category_annual.csv\n")
cat("  - items_monthly.csv\n")
cat("  - items_annual.csv\n")
cat("  - dc_comparison.csv\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("Data Cleaning Summary\n")
cat(strrep("=", 60), "\n\n")

cat("Date range:", min(cpi_clean$year_month), "to", max(cpi_clean$year_month), "\n")
cat("Total series:", n_distinct(cpi_clean$series_name), "\n\n")

cat("Whole food series:", 
    n_distinct(cpi_clean$series_name[cpi_clean$category == "whole_food"]), "\n")
cat("Processed food series:", 
    n_distinct(cpi_clean$series_name[cpi_clean$category == "processed_food"]), "\n")
cat("DC regional series:", 
    n_distinct(cpi_clean$series_name[cpi_clean$is_regional]), "\n")

cat("\nCategory averages (2024 vs 2015):\n")
category_annual |>
  filter(year %in% c(2015, 2024), region == "National") |>
  select(year, category, avg_pct_change) |>
  pivot_wider(names_from = year, values_from = avg_pct_change) |>
  mutate(change_2015_2024 = `2024` - `2015`) |>
  print()

cat("\n", strrep("=", 60), "\n")
cat("Data cleaning complete.\n")
cat("Next step: Run 03_analyze.R\n")

