# ==============================================================================
# 02_item_tables.R
# Clean item-level tables by age, education, and household structure
# No interpretation - just data for sensemaking
# ==============================================================================

library(tidyverse)
library(arrow)
library(scales)
library(here)

source(here("foundational_data", "R", "00_config.R"))

hps <- read_mart("hps")

# ==============================================================================
# Prepare Data
# ==============================================================================

hps_clean <- hps |>
  filter(!is.na(age_group), !is.na(educ_group), !is.na(hh_type)) |>
  mutate(
    educ = if_else(educ_group %in% c("BA", "GRAD"), "BA+", "No BA"),
    educ = factor(educ, levels = c("No BA", "BA+")),

    hh = case_when(
      hh_type == "SINGLE_CHILDREN" ~ "Single Parent",
      hh_type == "MARRIED_CHILDREN" ~ "Married Parent",
      hh_type == "SINGLE_NO_CHILDREN" ~ "Single, No Kids",
      hh_type == "MARRIED_NO_CHILDREN" ~ "Married, No Kids"
    ),
    hh = factor(hh, levels = c("Single Parent", "Married Parent",
                               "Single, No Kids", "Married, No Kids"))
  )

# Helper to format as percent
fmt <- function(x) paste0(round(x * 100, 1), "%")

# ==============================================================================
# TABLE 1: BY HOUSEHOLD STRUCTURE ONLY
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 1: ALL ITEMS BY HOUSEHOLD STRUCTURE\n")
cat("=" |> strrep(80), "\n\n")

by_hh <- hps_clean |>
  group_by(hh) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_hh |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 2: BY EDUCATION ONLY
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 2: ALL ITEMS BY EDUCATION\n")
cat("=" |> strrep(80), "\n\n")

by_educ <- hps_clean |>
  group_by(educ) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_educ |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 3: BY AGE GROUP ONLY
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 3: ALL ITEMS BY AGE GROUP\n")
cat("=" |> strrep(80), "\n\n")

by_age <- hps_clean |>
  group_by(age_group) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_age |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 4: EDUCATION × HOUSEHOLD STRUCTURE
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 4: EDUCATION × HOUSEHOLD STRUCTURE\n")
cat("=" |> strrep(80), "\n\n")

by_educ_hh <- hps_clean |>
  group_by(educ, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_educ_hh |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 5: AGE × HOUSEHOLD STRUCTURE
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 5: AGE × HOUSEHOLD STRUCTURE\n")
cat("=" |> strrep(80), "\n\n")

by_age_hh <- hps_clean |>
  group_by(age_group, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_age_hh |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 6: AGE × EDUCATION
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 6: AGE × EDUCATION\n")
cat("=" |> strrep(80), "\n\n")

by_age_educ <- hps_clean |>
  group_by(age_group, educ) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_age_educ |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(width = Inf)

# ==============================================================================
# TABLE 7: THREE-WAY (AGE × EDUCATION × HOUSEHOLD)
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLE 7: AGE × EDUCATION × HOUSEHOLD (n >= 25)\n")
cat("=" |> strrep(80), "\n\n")

by_all <- hps_clean |>
  group_by(age_group, educ, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense_difficulty = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing_not_current = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    medical_delayed = weighted.mean(pct_medical_delay_cost, n_unweighted, na.rm = TRUE),
    depression_phq2 = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety_gad2 = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    any_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 25)

by_all |>
  mutate(across(where(is.numeric) & !matches("^n$"), ~fmt(.))) |>
  print(n = 100, width = Inf)

cat("\n")
cat("=" |> strrep(80), "\n")
cat("TABLES COMPLETE\n")
cat("=" |> strrep(80), "\n")
