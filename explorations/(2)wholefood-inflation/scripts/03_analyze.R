# =============================================================================
# 03_analyze.R - CPI Food Price Analysis
# Questions:
#   1. How have overall food prices changed 2015-2024?
#   2. How do whole foods vs processed foods compare?
#   3. What are the trends by individual item?
#   4. How does DC compare to national averages?
# =============================================================================

library(tidyverse)
library(here)
library(broom)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Processed Data
# -----------------------------------------------------------------------------

processed_path <- here("explorations", "(2)wholefood-inflation", "data", "processed")

category_monthly <- read_csv(file.path(processed_path, "category_monthly.csv"), show_col_types = FALSE)
category_annual <- read_csv(file.path(processed_path, "category_annual.csv"), show_col_types = FALSE)
items_monthly <- read_csv(file.path(processed_path, "items_monthly.csv"), show_col_types = FALSE)
items_annual <- read_csv(file.path(processed_path, "items_annual.csv"), show_col_types = FALSE)
dc_comparison <- read_csv(file.path(processed_path, "dc_comparison.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. Overall Food Price Trends
# -----------------------------------------------------------------------------

cat("=" |> strrep(60), "\n")
cat("1. OVERALL FOOD PRICE TRENDS\n")
cat("=" |> strrep(60), "\n\n")

# Overall trend by category
overall_trends <- category_annual |>
  filter(region == "National") |>
  group_by(category) |>
  arrange(year) |>
  mutate(
    change_from_2015 = avg_pct_change - first(avg_pct_change),
    year_over_year_change = avg_pct_change - lag(avg_pct_change)
  )

cat("Overall price changes (2015-2024):\n")
category_annual |>
  filter(year %in% c(2015, 2024), region == "National") |>
  select(category, year, avg_pct_change) |>
  pivot_wider(names_from = year, values_from = avg_pct_change, names_prefix = "year_") |>
  mutate(
    total_change = year_2024 - year_2015,
    total_change_pct = sprintf("%.1f%%", total_change)
  ) |>
  select(category, year_2015, year_2024, total_change, total_change_pct) |>
  print()

# Linear trends
cat("\nLinear trends (annual rate of change):\n")
category_trends <- category_annual |>
  filter(region == "National") |>
  nest(data = -category) |>                       # 1. Nest data by category
  mutate(
    # 2. Fit the model on the nested data
    model = map(data, ~ lm(avg_pct_change ~ year, data = .x)),
    
    # 3. Extract coefficients safely
    trend_coef = map_dbl(model, ~ {
      # Safety check: ensure we have a slope coefficient
      coefs <- coef(.)
      if(length(coefs) >= 2) coefs[2] else NA_real_
    }),
    
    r_squared = map_dbl(model, ~ summary(.)$r.squared)
  )

print(category_trends |> select(category, trend_coef, r_squared))

# -----------------------------------------------------------------------------
# 2. Whole Foods vs Processed Foods Comparison
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("2. WHOLE FOODS VS PROCESSED FOODS\n")
cat("=" |> strrep(60), "\n\n")

# Calculate difference between categories
category_gap <- category_monthly |>
  filter(region == "National") |>
  select(year_month, category, avg_index) |>
  pivot_wider(names_from = category, values_from = avg_index) |>
  mutate(
    gap = whole_food - processed_food,
    gap_pct = ((whole_food - processed_food) / processed_food) * 100
  ) |>
  filter(!is.na(gap))

cat("Price gap: Whole Foods - Processed Foods\n")
cat("  Mean gap (index points):", sprintf("%.2f", mean(category_gap$gap, na.rm = TRUE)), "\n")
cat("  Mean gap (%):", sprintf("%.2f%%", mean(category_gap$gap_pct, na.rm = TRUE)), "\n")

# Trend in the gap
gap_trend <- lm(gap ~ year_month, data = category_gap)
cat("\nTrend in price gap:\n")
cat("  Annual change:", sprintf("%.2f index points/year", coef(gap_trend)[2] * 365.25), "\n")
cat("  R-squared:", sprintf("%.3f", summary(gap_trend)$r.squared), "\n")

# Year-by-year comparison
cat("\nAnnual comparison:\n")
category_annual |>
  filter(region == "National") |>
  select(year, category, avg_pct_change) |>
  pivot_wider(names_from = category, values_from = avg_pct_change) |>
  mutate(
    gap = whole_food - processed_food,
    gap_label = sprintf("%.1f pp", gap)
  ) |>
  select(year, whole_food, processed_food, gap, gap_label) |>
  print()

# -----------------------------------------------------------------------------
# 3. Item-Level Analysis
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("3. ITEM-LEVEL TRENDS\n")
cat("=" |> strrep(60), "\n\n")

# Items with largest increases
item_changes <- items_annual |>
  filter(region == "National") |>
  group_by(series_name, category) |>
  filter(year %in% c(2015, 2024)) |>
  summarize(
    change_2015_2024 = last(avg_pct_change) - first(avg_pct_change),
    .groups = "drop"
  ) |>
  arrange(desc(change_2015_2024))

cat("Top 10 items by price increase (2015-2024):\n")
item_changes |>
  head(10) |>
  mutate(change_label = sprintf("%+.1f%%", change_2015_2024)) |>
  select(series_name, category, change_2015_2024, change_label) |>
  print()

cat("\nBottom 10 items (smallest increase / largest decrease):\n")
item_changes |>
  tail(10) |>
  mutate(change_label = sprintf("%+.1f%%", change_2015_2024)) |>
  select(series_name, category, change_2015_2024, change_label) |>
  print()

# Category averages by item
cat("\nAverage change by category:\n")
item_changes |>
  group_by(category) |>
  summarize(
    n_items = n(),
    avg_change = mean(change_2015_2024),
    median_change = median(change_2015_2024),
    .groups = "drop"
  ) |>
  mutate(
    avg_change_label = sprintf("%+.1f%%", avg_change),
    median_change_label = sprintf("%+.1f%%", median_change)
  ) |>
  print()

# -----------------------------------------------------------------------------
# 4. DC Regional Analysis
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("4. DC REGIONAL COMPARISON\n")
cat("=" |> strrep(60), "\n\n")

# DC vs National averages
dc_summary <- dc_comparison |>
  group_by(year = year(year_month)) |>
  summarize(
    n_series = n(),
    avg_dc_premium = mean(dc_premium_pct, na.rm = TRUE),
    median_dc_premium = median(dc_premium_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat("DC premium vs National average (annual):\n")
dc_summary |>
  mutate(
    avg_premium_label = sprintf("%+.1f%%", avg_dc_premium),
    median_premium_label = sprintf("%+.1f%%", median_dc_premium)
  ) |>
  select(year, n_series, avg_dc_premium, avg_premium_label) |>
  print()

cat("\nOverall DC premium (2015-2024):\n")
cat("  Mean:", sprintf("%+.1f%%", mean(dc_comparison$dc_premium_pct, na.rm = TRUE)), "\n")
cat("  Median:", sprintf("%+.1f%%", median(dc_comparison$dc_premium_pct, na.rm = TRUE)), "\n")

# Trend in DC premium
dc_premium_trend <- dc_comparison |>
  mutate(year_decimal = decimal_date(year_month)) |>
  lm(dc_premium_pct ~ year_decimal, data = _)

cat("\nTrend in DC premium:\n")
cat("  Annual change:", sprintf("%+.2f pp/year", coef(dc_premium_trend)[2]), "\n")
cat("  R-squared:", sprintf("%.3f", summary(dc_premium_trend)$r.squared), "\n")

# -----------------------------------------------------------------------------
# 5. Save Analysis Results
# -----------------------------------------------------------------------------

results <- list(
  category_monthly = category_monthly,
  category_annual = category_annual,
  items_annual = items_annual,
  item_changes = item_changes,
  category_gap = category_gap,
  dc_comparison = dc_comparison,
  dc_summary = dc_summary
)

saveRDS(results, file.path(processed_path, "analysis_results.rds"))

cat("\n", "=" |> strrep(60), "\n")
cat("Analysis complete. Results saved to analysis_results.rds\n")
cat("Next step: Run 04_visualize.R\n")

