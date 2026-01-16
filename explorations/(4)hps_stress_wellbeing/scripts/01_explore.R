# ==============================================================================
# 01_explore.R
# Exploration: How do hardship and wellbeing vary by household structure,
# education, and age?
#
# Key insight from initial exploration:
# - Education is a "parallel shift" - helps everyone ~18-20pp equally
# - Household structure determines which "track" you're on
# - Single parenthood creates persistent disadvantage that education can't close
#
# This script explores ALL metrics along these dimensions.
# ==============================================================================

library(tidyverse)
library(arrow)
library(scales)
library(here)

# Load foundational config
source(here("foundational_data", "R", "00_config.R"))

# ==============================================================================
# Load Data
# ==============================================================================

hps <- read_mart("hps")

cat("=" |> strrep(70), "\n")
cat("HPS STRESS & WELLBEING EXPLORATION\n")
cat("=" |> strrep(70), "\n\n")
cat("Data: Household Pulse Survey, December 2024\n")
cat("Metrics available:\n")
cat("  HARDSHIP: pct_hardship_expenses, pct_food_insufficient,\n")
cat("            pct_housing_not_current, pct_medical_delay_cost\n")
cat("  MENTAL HEALTH: pct_phq2_positive (depression), pct_gad2_positive (anxiety),\n")
cat("                 pct_distress (either), mean_phq2_score, mean_gad2_score\n\n")

# ==============================================================================
# Prepare Data with Simplified Categories
# ==============================================================================

hps_clean <- hps |>
  filter(!is.na(age_group), !is.na(educ_group), !is.na(hh_type)) |>
  mutate(
    # Simplified education
    educ = if_else(educ_group %in% c("BA", "GRAD"), "BA+", "No BA"),
    educ = factor(educ, levels = c("No BA", "BA+")),

    # Simplified household type
    hh = case_when(
      hh_type == "SINGLE_CHILDREN" ~ "Single Parent",
      hh_type == "MARRIED_CHILDREN" ~ "Married Parent",
      hh_type == "SINGLE_NO_CHILDREN" ~ "Single, No Kids",
      hh_type == "MARRIED_NO_CHILDREN" ~ "Married, No Kids"
    ),
    hh = factor(hh, levels = c("Single Parent", "Married Parent",
                               "Single, No Kids", "Married, No Kids")),

    # Age buckets
    age_bucket = case_when(
      age_group %in% c("18-24", "25-34") ~ "Young (18-34)",
      age_group %in% c("35-44", "45-54") ~ "Prime (35-54)",
      TRUE ~ "Older (55+)"
    ),
    age_bucket = factor(age_bucket, levels = c("Young (18-34)", "Prime (35-54)", "Older (55+)"))
  )

# ==============================================================================
# SECTION 1: Overview of All Metrics by Household Type
# ==============================================================================

cat("\n")
cat("=" |> strrep(70), "\n")
cat("SECTION 1: ALL METRICS BY HOUSEHOLD TYPE\n")
cat("=" |> strrep(70), "\n\n")

by_hh <- hps_clean |>
  group_by(hh) |>
  summarise(
    n = sum(n_unweighted),
    # Hardship
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_behind = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    # Mental health
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(expense_difficulty))

cat("HARDSHIP METRICS BY HOUSEHOLD TYPE:\n")
cat("-" |> strrep(60), "\n\n")

by_hh |>
  select(hh, n, expense_difficulty, food_insufficient, housing_behind, medical_delayed) |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~percent(., 0.1))) |>
  print()

cat("\n\nMENTAL HEALTH METRICS BY HOUSEHOLD TYPE:\n")
cat("-" |> strrep(60), "\n\n")

by_hh |>
  select(hh, n, depression, anxiety, any_distress) |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~percent(., 0.1))) |>
  print()

# ==============================================================================
# SECTION 2: The "Parallel Tracks" Test - All Metrics
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(70), "\n")
cat("SECTION 2: EDUCATION PREMIUM BY HOUSEHOLD TYPE (All Metrics)\n")
cat("Is the ~20pp education premium consistent across all metrics?\n")
cat("=" |> strrep(70), "\n\n")

educ_premium <- hps_clean |>
  group_by(hh, educ) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate education premium for each metric
metrics <- c("expense", "food", "housing", "depression", "anxiety", "distress")

cat("EDUCATION PREMIUM (No BA rate minus BA+ rate) BY METRIC:\n")
cat("-" |> strrep(60), "\n\n")

for (metric in metrics) {
  cat(toupper(metric), ":\n")

  premium_df <- educ_premium |>
    select(hh, educ, value = !!sym(metric)) |>
    pivot_wider(names_from = educ, values_from = value) |>
    mutate(
      premium_pp = round((`No BA` - `BA+`) * 100, 1),
      `No BA` = percent(`No BA`, 0.1),
      `BA+` = percent(`BA+`, 0.1)
    )

  print(premium_df)
  cat("\n")
}

# ==============================================================================
# SECTION 3: Age Trajectories for Each Metric
# ==============================================================================

cat("\n")
cat("=" |> strrep(70), "\n")
cat("SECTION 3: AGE TRAJECTORIES BY HOUSEHOLD TYPE\n")
cat("Do all metrics improve with age? At the same rate?\n")
cat("=" |> strrep(70), "\n\n")

age_trajectory <- hps_clean |>
  group_by(hh, age_bucket) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

for (metric in c("expense", "food", "depression", "anxiety", "distress")) {
  cat(toupper(metric), " BY AGE:\n")
  cat("-" |> strrep(50), "\n")

  traj_wide <- age_trajectory |>
    select(hh, age_bucket, value = !!sym(metric)) |>
    pivot_wider(names_from = age_bucket, values_from = value) |>
    mutate(
      improvement = round((`Young (18-34)` - `Older (55+)`) * 100, 0),
      across(where(is.numeric) & !matches("improvement"), ~round(. * 100, 0))
    )

  print(traj_wide)
  cat("\n\n")
}

# ==============================================================================
# SECTION 4: The Full Intersection - Highest Risk Groups
# ==============================================================================

cat("=" |> strrep(70), "\n")
cat("SECTION 4: HIGHEST RISK GROUPS (Age × Education × Household)\n")
cat("=" |> strrep(70), "\n\n")

full_intersection <- hps_clean |>
  group_by(hh, educ, age_bucket) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 15)

cat("TOP 10 GROUPS BY EXPENSE DIFFICULTY:\n")
cat("-" |> strrep(50), "\n\n")

full_intersection |>
  arrange(desc(expense)) |>
  head(10) |>
  mutate(
    group = paste(hh, educ, age_bucket, sep = " | "),
    expense = percent(expense, 0),
    food = percent(food, 0),
    distress = percent(distress, 0)
  ) |>
  select(group, n, expense, food, distress) |>
  print()

cat("\n\nTOP 10 GROUPS BY PSYCHOLOGICAL DISTRESS:\n")
cat("-" |> strrep(50), "\n\n")

full_intersection |>
  arrange(desc(distress)) |>
  head(10) |>
  mutate(
    group = paste(hh, educ, age_bucket, sep = " | "),
    expense = percent(expense, 0),
    food = percent(food, 0),
    distress = percent(distress, 0)
  ) |>
  select(group, n, expense, food, distress) |>
  print()

# ==============================================================================
# SECTION 5: Correlation Between Hardship and Mental Health
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(70), "\n")
cat("SECTION 5: HARDSHIP → MENTAL HEALTH CONNECTION\n")
cat("Does financial hardship correlate with psychological distress?\n")
cat("=" |> strrep(70), "\n\n")

# At the segment level, correlate hardship with distress
correlation_data <- hps_clean |>
  select(n_unweighted, pct_hardship_expenses, pct_food_insufficient,
         pct_phq2_positive, pct_gad2_positive, pct_distress)

cat("CORRELATIONS (segment-level, weighted by n):\n")
cat("-" |> strrep(50), "\n\n")

# Weighted correlation function
weighted_cor <- function(x, y, w) {
  valid <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[valid]; y <- y[valid]; w <- w[valid]
  w <- w / sum(w)
  mx <- sum(w * x)
  my <- sum(w * y)
  cov_xy <- sum(w * (x - mx) * (y - my))
  sd_x <- sqrt(sum(w * (x - mx)^2))
  sd_y <- sqrt(sum(w * (y - my)^2))
  cov_xy / (sd_x * sd_y)
}

cat("Expense difficulty ↔ Depression:  r = ",
    round(weighted_cor(hps_clean$pct_hardship_expenses, hps_clean$pct_phq2_positive, hps_clean$n_unweighted), 2), "\n")
cat("Expense difficulty ↔ Anxiety:     r = ",
    round(weighted_cor(hps_clean$pct_hardship_expenses, hps_clean$pct_gad2_positive, hps_clean$n_unweighted), 2), "\n")
cat("Food insufficiency ↔ Depression:  r = ",
    round(weighted_cor(hps_clean$pct_food_insufficient, hps_clean$pct_phq2_positive, hps_clean$n_unweighted), 2), "\n")
cat("Food insufficiency ↔ Anxiety:     r = ",
    round(weighted_cor(hps_clean$pct_food_insufficient, hps_clean$pct_gad2_positive, hps_clean$n_unweighted), 2), "\n")

cat("\n\n")
cat("=" |> strrep(70), "\n")
cat("EXPLORATION COMPLETE\n")
cat("=" |> strrep(70), "\n")
