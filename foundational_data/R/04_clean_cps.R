# ==============================================================================
# 04_clean_cps.R
# Build state-month segment mart from CPS microdata
# Run: After 03_ingest_cps_ipums.R
# ==============================================================================

library(tidyverse)
library(arrow)
library(survey)
library(srvyr)
library(here)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Load Clean Microdata
# ------------------------------------------------------------------------------

microdata_path <- file.path(PATHS$clean_cps, "cps_clean_latest.parquet")

if (!file.exists(microdata_path)) {
  stop(
    "CPS microdata not found: ", microdata_path, "\n",
    "Run 03_ingest_cps_ipums.R first."
  )
}

cps <- read_parquet(microdata_path)
message("Loaded ", format(nrow(cps), big.mark = ","), " records from CPS microdata.")

# ------------------------------------------------------------------------------
# Derive Segmentation Variables
# ------------------------------------------------------------------------------

message("\nDeriving segmentation variables...")

cps_seg <- cps |>
  mutate(
    # Age group
    age_group = make_age_group(age),

    # Education group (IPUMS CPS coding)
    educ_group = make_educ_group(educ, scheme = "ipums_cps"),

    # Marital status flag
    # MARST: 1=Married spouse present, 2=Married spouse absent,
    #        3=Separated, 4=Divorced, 5=Widowed, 6=Never married
    married_flag = marst %in% c(1, 2),

    # Has children
    has_child = nchild > 0
  )

# Derive adult count per household (need household ID)
# If SERIAL is available, count adults per household
if ("SERIAL" %in% names(cps_seg)) {
  adult_counts <- cps_seg |>
    group_by(year, month, SERIAL) |>
    summarise(adult_count = n(), .groups = "drop")

  cps_seg <- cps_seg |>
    left_join(adult_counts, by = c("year", "month", "SERIAL")) |>
    mutate(adult_count_bin = if_else(adult_count >= 2, "2+", "1"))
} else {
  # Fallback: assume household structure from marital status
  message("  Note: SERIAL not available, inferring adult_count from marital status")
  cps_seg <- cps_seg |>
    mutate(adult_count_bin = if_else(married_flag, "2+", "1"))
}

# Create household type
cps_seg <- cps_seg |>
  mutate(hh_type = make_hh_type(married_flag, has_child, adult_count_bin))

# ------------------------------------------------------------------------------
# Derive Income Quintiles (within state-month)
# ------------------------------------------------------------------------------

message("Computing income quintiles by state-month...")

# Handle IPUMS income coding (9999999 = N/A, 9999998 = missing)
cps_seg <- cps_seg |>
  mutate(
    hhincome_clean = case_when(
      hhincome >= 9999998 ~ NA_real_,
      hhincome < 0 ~ NA_real_,
      TRUE ~ as.numeric(hhincome)
    )
  )

# Compute quintiles within each state-month
cps_seg <- cps_seg |>
  group_by(state_fips, period) |>
  mutate(
    income_group = make_income_quintile_weighted(hhincome_clean, wtfinl)
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# Derive Labor Outcomes
# ------------------------------------------------------------------------------

message("Deriving labor outcomes...")

# EMPSTAT codes:
# 1 = Armed Forces
# 10-12 = At work (employed)
# 20-22 = Has job, not at work this week (employed)
# 30-36 = Unemployed (layoff, looking)
# 00 = NIU (not in universe - under 15, etc.)

# LABFORCE codes:
# 0 = NIU
# 1 = Not in labor force
# 2 = In labor force

cps_seg <- cps_seg |>
  mutate(
    # Employed
    employed = empstat %in% c(10, 12, 20, 21, 22),

    # Unemployed (in labor force but not employed)
    unemployed = empstat %in% c(30, 31, 32, 33, 34, 35, 36),

    # In labor force
    in_labor_force = labforce == 2,

    # Hours worked (clean: 999 = NIU, 997 = hours vary)
    hours_clean = case_when(
      uhrsworkt >= 997 ~ NA_real_,
      TRUE ~ as.numeric(uhrsworkt)
    ),

    # Works 40+ hours
    works_40plus = hours_clean >= 40 & !is.na(hours_clean)
  )

# ------------------------------------------------------------------------------
# Build Survey Design Object
# ------------------------------------------------------------------------------

message("Building survey design object...")

# Create survey design using srvyr
# Note: CPS uses a complex design, but WTFINL incorporates the design
# For simplicity, we use WTFINL as the final weight
cps_svy <- cps_seg |>
  as_survey_design(weights = wtfinl)

# ------------------------------------------------------------------------------
# Compute State-Month Segment Estimates
# ------------------------------------------------------------------------------

message("Computing weighted estimates by state-month-segment...")

# Define grouping variables
group_vars <- c("state_fips", "period", "age_group", "educ_group",
                "income_group", "hh_type")

# Compute estimates
mart_cps <- cps_svy |>
  group_by(across(all_of(group_vars))) |>
  summarise(
    # Sample size
    n_unweighted = unweighted(n()),

    # Labor outcomes
    unemployment_rate = survey_mean(unemployed, na.rm = TRUE, vartype = NULL),
    labor_force_participation = survey_mean(in_labor_force, na.rm = TRUE, vartype = NULL),
    mean_hours_worked = survey_mean(hours_clean, na.rm = TRUE, vartype = NULL),
    pct_40plus_hours = survey_mean(works_40plus, na.rm = TRUE, vartype = NULL),

    # Employment rate (for reference)
    employment_rate = survey_mean(employed, na.rm = TRUE, vartype = NULL),

    .groups = "drop"
  ) |>
  # Add geo_level for consistency
  mutate(
    geo_level = "state",
    geo_id = as.character(state_fips)
  ) |>
  # Reorder columns
  select(
    period, state_fips, geo_level, geo_id,
    age_group, educ_group, income_group, hh_type,
    n_unweighted,
    unemployment_rate, labor_force_participation,
    mean_hours_worked, pct_40plus_hours, employment_rate
  ) |>
  arrange(state_fips, period, age_group, educ_group, income_group, hh_type)

# ------------------------------------------------------------------------------
# Validate
# ------------------------------------------------------------------------------

message("\nValidating segmentation values...")
validate_segmentation(mart_cps)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("CPS State-Month Mart Summary")
message(strrep("-", 60))

message("\nDimensions: ", nrow(mart_cps), " rows x ", ncol(mart_cps), " cols")

message("\nTime coverage:")
mart_cps |>
  summarise(
    first_period = min(period),
    last_period = max(period),
    n_periods = n_distinct(period)
  ) |>
  print()

message("\nStates: ", n_distinct(mart_cps$state_fips))
message("DC (FIPS 11) present: ", DC_FIPS %in% mart_cps$state_fips)

message("\nDC summary (latest period):")
mart_cps |>
  filter(state_fips == DC_FIPS) |>
  filter(period == max(period)) |>
  group_by(period) |>
  summarise(
    n_segments = n(),
    total_unweighted_n = sum(n_unweighted),
    avg_unemployment = weighted.mean(unemployment_rate, n_unweighted, na.rm = TRUE),
    avg_lfpr = weighted.mean(labor_force_participation, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()

message("\nNational summary (latest period):")
mart_cps |>
  filter(period == max(period)) |>
  summarise(
    n_segments = n(),
    total_unweighted_n = sum(n_unweighted),
    avg_unemployment = weighted.mean(unemployment_rate, n_unweighted, na.rm = TRUE),
    avg_lfpr = weighted.mean(labor_force_participation, n_unweighted, na.rm = TRUE)
  ) |>
  print()

# ------------------------------------------------------------------------------
# Write Mart
# ------------------------------------------------------------------------------

write_mart(mart_cps, "cps")

message("\n", strrep("=", 60))
message("CPS mart build complete!")
message("Mart: ", PATHS$mart_cps)
message(strrep("=", 60))
