# ==============================================================================
# 02_clean_hps.R
# Build national segment mart from HPS Public Use Files
# Run: After 01_ingest_hps.R (when new PUF release is available)
# ==============================================================================
#
# This script:
# 1. Reads PUF microdata from ingested releases
# 2. Maps variables using a flexible approach (variable names shift by phase)
# 3. Derives segmentation variables
# 4. Computes hardship outcomes
# 5. Computes PHQ-2/GAD-2 mental health scores
# 6. Builds weighted national segment estimates
#
# ==============================================================================

library(tidyverse)
library(arrow)
library(survey)
library(srvyr)
library(readxl)
library(here)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Variable Mapping Configuration
# ------------------------------------------------------------------------------
# HPS variable names can shift between phases. This mapping defines the
# canonical names and common variants. Update as needed for new phases.

VAR_MAP <- list(

  # Weight
  weight = c("PWEIGHT", "HWEIGHT"),

  # Demographics / Segmentation
  # Note: Variable names shift across HPS phases. These cover multiple releases.
  age = c("TBIRTH_YEAR1", "TBIRTH_YEAR", "TAGE1", "A_AGE1", "TBIRTH"),
  education = c("REDUC1", "EEDUC", "A_EDUC1"),  # REDUC1 has values 1-7; A_EDUC1 is often a flag
  income = c("INCOME"),  # Note: Not in all releases (e.g., Dec 2024)
  marital_status = c("MARITAL1", "MS"),
  num_adults = c("THHLD_NUMADULT", "THHLD_NUMADLT", "NUMADLT"),
  num_children = c("THHLD_NUMKID", "AHHLD_NUMKID", "NUMKID", "KIDS"),

  # Hardship outcomes
  expense_difficulty = c("EXPNS_DIF", "EXPNS"),
  food_sufficiency = c("CURFOODSUF", "FOODSUFRSN"),
  housing_rent = c("RENTCUR"),
  housing_mortgage = c("MORTCUR"),
  medical_need = c("MHLTH_NEED", "DELAY", "NOTGET"),  # Needed but didn't get care

  # Mental health (PHQ-2 items)
  phq_interest = c("INTEREST"),      # Little interest/pleasure
  phq_depressed = c("DOWN"),         # Feeling down/depressed

  # Mental health (GAD-2 items)
  gad_anxious = c("ANXIOUS"),        # Feeling anxious
  gad_worry = c("WORRY")             # Unable to stop worrying
)

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Find first matching variable name in data
find_var <- function(df, candidates) {
  for (var in candidates) {
    if (var %in% names(df)) {
      return(var)
    }
  }
  return(NA_character_)
}

#' Map HPS variables to canonical names
map_hps_variables <- function(df) {

  mapped <- list()
  missing <- character()

  for (canonical in names(VAR_MAP)) {
    var_name <- find_var(df, VAR_MAP[[canonical]])
    if (!is.na(var_name)) {
      mapped[[canonical]] <- var_name
    } else {
      missing <- c(missing, canonical)
    }
  }

  if (length(missing) > 0) {
    message("Note: Could not find variables for: ", paste(missing, collapse = ", "))
    message("Check the data dictionary for this release and update VAR_MAP if needed.")
  }

  mapped
}

#' Derive age from birth year
derive_age <- function(birth_year, survey_year = NULL) {
  if (is.null(survey_year)) {
    survey_year <- as.integer(format(Sys.Date(), "%Y"))
  }
  survey_year - birth_year
}

#' Score PHQ-2 (0-6 scale)
# Items coded 1-4 in HPS: 1=Not at all, 2=Several days, 3=More than half, 4=Nearly every day
# Convert to 0-3 scale for standard PHQ scoring
score_phq2 <- function(interest, depressed) {
  # Convert 1-4 to 0-3
  i <- pmax(0, pmin(3, as.numeric(interest) - 1))
  d <- pmax(0, pmin(3, as.numeric(depressed) - 1))

  # Sum (0-6)
  score <- i + d
  score[is.na(interest) | is.na(depressed)] <- NA
  score
}

#' Score GAD-2 (0-6 scale)
score_gad2 <- function(anxious, worry) {
  # Convert 1-4 to 0-3
  a <- pmax(0, pmin(3, as.numeric(anxious) - 1))
  w <- pmax(0, pmin(3, as.numeric(worry) - 1))

  # Sum (0-6)
  score <- a + w
  score[is.na(anxious) | is.na(worry)] <- NA
  score
}

# ------------------------------------------------------------------------------
# Load HPS Release Catalog
# ------------------------------------------------------------------------------

catalog_path <- PATHS$raw_hps
release_dirs <- list.dirs(catalog_path, recursive = FALSE, full.names = TRUE)

if (length(release_dirs) == 0) {
  stop(
    "No HPS releases found in: ", catalog_path, "\n",
    "Run 01_ingest_hps.R first to ingest PUF data."
  )
}

# Find data files
catalog <- map_dfr(release_dirs, function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  data_file <- files[grepl("puf.*\\.csv$", files, ignore.case = TRUE) &
                     !grepl("repwgt", files, ignore.case = TRUE)][1]

  if (!is.na(data_file)) {
    tibble(
      release_id = basename(dir),
      data_file = data_file
    )
  } else {
    NULL
  }
})

message("Found ", nrow(catalog), " HPS release(s) with data files.")
print(catalog)

# ------------------------------------------------------------------------------
# Process Each Release
# ------------------------------------------------------------------------------

all_releases <- list()

for (i in seq_len(nrow(catalog))) {

  release_id <- catalog$release_id[i]
  data_file <- catalog$data_file[i]

  message("\n", strrep("-", 60))
  message("Processing: ", release_id)
  message(strrep("-", 60))

  # Read data
  hps_raw <- read_csv(data_file, show_col_types = FALSE)
  message("Read ", format(nrow(hps_raw), big.mark = ","), " records.")

  # Standardize column names to uppercase
  names(hps_raw) <- toupper(names(hps_raw))

  # Map variables
  var_map <- map_hps_variables(hps_raw)

  # Check for required variables
  if (is.null(var_map$weight)) {
    warning("No weight variable found for ", release_id, ". Skipping.")
    next
  }

  # Build clean data frame with mapped variables
  hps_clean <- hps_raw |>
    transmute(
      release_id = release_id,

      # Weight
      weight = .data[[var_map$weight]],

      # Demographics
      birth_year = if (!is.null(var_map$age)) .data[[var_map$age]] else NA_integer_,
      education = if (!is.null(var_map$education)) .data[[var_map$education]] else NA_integer_,
      income = if (!is.null(var_map$income)) .data[[var_map$income]] else NA_integer_,
      marital_status = if (!is.null(var_map$marital_status)) .data[[var_map$marital_status]] else NA_integer_,
      num_adults = if (!is.null(var_map$num_adults)) .data[[var_map$num_adults]] else NA_integer_,
      num_children = if (!is.null(var_map$num_children)) .data[[var_map$num_children]] else NA_integer_,

      # Hardship
      expense_difficulty = if (!is.null(var_map$expense_difficulty)) .data[[var_map$expense_difficulty]] else NA_integer_,
      food_sufficiency = if (!is.null(var_map$food_sufficiency)) .data[[var_map$food_sufficiency]] else NA_integer_,
      housing_rent = if (!is.null(var_map$housing_rent)) .data[[var_map$housing_rent]] else NA_integer_,
      housing_mortgage = if (!is.null(var_map$housing_mortgage)) .data[[var_map$housing_mortgage]] else NA_integer_,
      medical_need = if (!is.null(var_map$medical_need)) .data[[var_map$medical_need]] else NA_integer_,

      # Mental health
      phq_interest = if (!is.null(var_map$phq_interest)) .data[[var_map$phq_interest]] else NA_integer_,
      phq_depressed = if (!is.null(var_map$phq_depressed)) .data[[var_map$phq_depressed]] else NA_integer_,
      gad_anxious = if (!is.null(var_map$gad_anxious)) .data[[var_map$gad_anxious]] else NA_integer_,
      gad_worry = if (!is.null(var_map$gad_worry)) .data[[var_map$gad_worry]] else NA_integer_
    )

  # Derive segmentation variables
  hps_clean <- hps_clean |>
    mutate(
      # Age
      age = derive_age(birth_year),
      age_group = make_age_group(age),

      # Education (HPS coding)
      educ_group = make_educ_group(education, scheme = "hps"),

      # Married flag (MS: 1=Married, 2=Widowed, 3=Divorced, 4=Separated, 5=Never married)
      married_flag = marital_status == 1,

      # Has children
      has_child = num_children > 0 & !is.na(num_children),

      # Adult count bin
      adult_count_bin = if_else(num_adults >= 2, "2+", "1"),

      # Household type
      hh_type = make_hh_type(married_flag, has_child, adult_count_bin)
    )

  # Derive income quintiles
  hps_clean <- hps_clean |>
    mutate(
      income_group = make_income_quintile_weighted(as.numeric(income), weight)
    )

  # Derive hardship outcomes
  # Note: Specific coding varies by phase - these are common patterns
  hps_clean <- hps_clean |>
    mutate(
      # Expense difficulty: typically 1=Not difficult, 2-4=Difficult levels
      hardship_expenses = expense_difficulty >= 2 & !is.na(expense_difficulty),

      # Food sufficiency: typically 1=Enough, 2-4=Not enough
      food_insufficient = food_sufficiency >= 2 & !is.na(food_sufficiency),

      # Housing: combine rent and mortgage (1=Current, 2=Not current)
      # Use whichever is available; if both, prioritize the one that's "not current"
      housing_not_current = case_when(
        housing_rent == 2 | housing_mortgage == 2 ~ TRUE,
        !is.na(housing_rent) | !is.na(housing_mortgage) ~ FALSE,
        TRUE ~ NA
      ),

      # Medical need: 1=Yes needed but didn't get, 2=No
      medical_delay_cost = medical_need == 1
    )

  # Derive mental health scores
  hps_clean <- hps_clean |>
    mutate(
      phq2_score = score_phq2(phq_interest, phq_depressed),
      phq2_positive = phq2_score >= 3,

      gad2_score = score_gad2(gad_anxious, gad_worry),
      gad2_positive = gad2_score >= 3,

      distress_positive = phq2_positive | gad2_positive
    )

  # Filter to valid weights
  hps_clean <- hps_clean |>
    filter(!is.na(weight) & weight > 0)

  message("After filtering: ", format(nrow(hps_clean), big.mark = ","), " records with valid weights.")

  all_releases[[release_id]] <- hps_clean
}

# Combine all releases
hps_all <- bind_rows(all_releases)
message("\n\nCombined ", nrow(hps_all), " records across ", length(all_releases), " release(s).")

# ------------------------------------------------------------------------------
# Build Survey Design and Compute Estimates
# ------------------------------------------------------------------------------

message("\nBuilding survey design and computing weighted estimates...")

hps_svy <- hps_all |>
  as_survey_design(weights = weight)

# Compute estimates by release and segment
group_vars <- c("release_id", "age_group", "educ_group", "income_group", "hh_type")

mart_hps <- hps_svy |>
  group_by(across(all_of(group_vars))) |>
  summarise(
    # Sample size
    n_unweighted = unweighted(n()),

    # Hardship rates
    pct_hardship_expenses = survey_mean(hardship_expenses, na.rm = TRUE, vartype = NULL),
    pct_food_insufficient = survey_mean(food_insufficient, na.rm = TRUE, vartype = NULL),
    pct_housing_not_current = survey_mean(housing_not_current, na.rm = TRUE, vartype = NULL),
    pct_medical_delay_cost = survey_mean(medical_delay_cost, na.rm = TRUE, vartype = NULL),

    # Mental health
    mean_phq2_score = survey_mean(phq2_score, na.rm = TRUE, vartype = NULL),
    pct_phq2_positive = survey_mean(phq2_positive, na.rm = TRUE, vartype = NULL),
    mean_gad2_score = survey_mean(gad2_score, na.rm = TRUE, vartype = NULL),
    pct_gad2_positive = survey_mean(gad2_positive, na.rm = TRUE, vartype = NULL),
    pct_distress = survey_mean(distress_positive, na.rm = TRUE, vartype = NULL),

    .groups = "drop"
  ) |>
  # Rename release_id to pulse_wave for consistency
  rename(pulse_wave = release_id) |>
  # Add geo identifiers
  mutate(
    geo_level = "us",
    geo_id = "US"
  ) |>
  # Reorder columns
  select(
    pulse_wave, geo_level, geo_id,
    age_group, educ_group, income_group, hh_type,
    n_unweighted,
    pct_hardship_expenses, pct_food_insufficient,
    pct_housing_not_current, pct_medical_delay_cost,
    mean_phq2_score, pct_phq2_positive,
    mean_gad2_score, pct_gad2_positive,
    pct_distress
  )

# ------------------------------------------------------------------------------
# Validate
# ------------------------------------------------------------------------------

message("\nValidating segmentation values...")
validate_segmentation(mart_hps)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("HPS National Mart Summary")
message(strrep("-", 60))

message("\nDimensions: ", nrow(mart_hps), " rows x ", ncol(mart_hps), " cols")

message("\nPulse waves: ", paste(unique(mart_hps$pulse_wave), collapse = ", "))

message("\nOverall means (across all segments):")
mart_hps |>
  summarise(
    total_unweighted_n = sum(n_unweighted),
    avg_hardship_expenses = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    avg_food_insufficient = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    avg_phq2_positive = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    avg_gad2_positive = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    avg_distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE)
  ) |>
  print()

# ------------------------------------------------------------------------------
# Save Clean Microdata (optional)
# ------------------------------------------------------------------------------

ensure_dir(PATHS$clean_hps)
micro_path <- file.path(PATHS$clean_hps, "hps_clean_latest.parquet")
write_parquet(hps_all, micro_path)
message("\nClean microdata saved: ", micro_path)

# ------------------------------------------------------------------------------
# Write Mart
# ------------------------------------------------------------------------------

write_mart(mart_hps, "hps")

message("\n", strrep("=", 60))
message("HPS mart build complete!")
message("Mart: ", PATHS$mart_hps)
message(strrep("=", 60))
