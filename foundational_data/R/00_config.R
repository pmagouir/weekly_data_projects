# ==============================================================================
# 00_config.R
# Foundational Data Infrastructure - Configuration and Segmentation Helpers
# ==============================================================================

library(tidyverse)
library(arrow)
library(here)

# ------------------------------------------------------------------------------
# Directory Paths
# ------------------------------------------------------------------------------

FOUNDATION_ROOT <- here("foundational_data")

PATHS <- list(

  # Raw data landing zones
  raw_hps      = file.path(FOUNDATION_ROOT, "data_raw", "hps"),
  raw_cps      = file.path(FOUNDATION_ROOT, "data_raw", "cps_ipums"),
  raw_fred     = file.path(FOUNDATION_ROOT, "data_raw", "fred"),


  # Cleaned intermediate data
  clean_hps    = file.path(FOUNDATION_ROOT, "data_clean", "hps"),
  clean_cps    = file.path(FOUNDATION_ROOT, "data_clean", "cps"),
  clean_fred   = file.path(FOUNDATION_ROOT, "data_clean", "fred"),

  # Output marts (what explorations import)
  mart         = file.path(FOUNDATION_ROOT, "data_mart"),

  # Specific mart files
  mart_fred    = file.path(FOUNDATION_ROOT, "data_mart", "mart_fred_month.parquet"),
  mart_cps     = file.path(FOUNDATION_ROOT, "data_mart", "mart_cps_state_month.parquet"),
  mart_hps     = file.path(FOUNDATION_ROOT, "data_mart", "mart_hps_national.parquet")
)

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

DC_FIPS <- 11L

# FRED series IDs for macro context
FRED_SERIES <- c(

  # Inflation + prices
  "CPIAUCSL",      # Headline CPI
  "CPIFABSL",      # Food CPI
  "CPIENGSL",      # Energy CPI

  # Labor - Overall
  "UNRATE",        # Unemployment rate

  # Unemployment Rate by Education (25+ years)
  "LNS14027660",   # Less than High School
  "LNS14027662",   # High School, No College
  "LNS14027689",   # Some College or Associate
  "LNS14027659",   # Bachelor's Degree and Higher

  # Labor Force Participation Rate by Education (25+ years)
  "LNS11327660",   # Less than High School
  "LNS11327662",   # High School, No College
  "LNS11327689",   # Some College or Associate
  "LNS11327659",   # Bachelor's Degree and Higher

  # Employment-Population Ratio by Education (25+ years)
  "LNS12327660",   # Less than High School
  "LNS12327662",   # High School, No College
  "LNS12327689",   # Some College or Associate
  "LNS12327659",   # Bachelor's Degree and Higher

  # Median Usual Weekly Earnings by Education (25+ years, quarterly)
  "LEU0252916700Q",  # Less than High School
  "LEU0252917300Q",  # High School, No College
  "LEU0252918500Q",  # Bachelor's Degree and Higher

  # Rates
  "FEDFUNDS",      # Fed funds rate
  "MORTGAGE30US",  # 30-year fixed mortgage rate

  # Consumer sentiment (macro mood)
  "UMCSENT"        # University of Michigan: Consumer Sentiment
)

# ------------------------------------------------------------------------------
# Segmentation Helper Functions
# ------------------------------------------------------------------------------

#' Create age group from numeric age
#' @param age Numeric age in years
#' @return Factor with ordered age group levels
make_age_group <- function(age) {
  cut(
    age,
    breaks = c(17, 24, 34, 44, 54, 64, 74, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
    right = TRUE,
    include.lowest = FALSE
  )
}

#' Create education group from various coding schemes
#' @param educ Education variable (numeric or character)
#' @param scheme Coding scheme: "ipums_cps", "hps", or "generic"
#' @return Factor with education group levels
make_educ_group <- function(educ, scheme = "generic") {

  if (scheme == "ipums_cps") {
    # IPUMS CPS EDUC codes (detailed)
    # See: https://cps.ipums.org/cps-action/variables/EDUC
    case_when(
      educ <= 73 ~ "LE_HS",           # Up through HS diploma/GED
      educ %in% 74:110 ~ "SOME_COLLEGE", # Some college, AA
      educ == 111 ~ "BA",             # Bachelor's degree
      educ >= 112 ~ "GRAD",           # Master's, professional, doctorate
      TRUE ~ NA_character_
    )
  } else if (scheme == "hps") {
    # HPS EEDUC codes (will vary by phase - this is a template)
    # Typical coding: 1=Less than HS, 2=Some HS, 3=HS grad, 4=Some college,
    #                 5=AA, 6=BA, 7=Grad
    case_when(
      educ %in% 1:3 ~ "LE_HS",
      educ %in% 4:5 ~ "SOME_COLLEGE",
      educ == 6 ~ "BA",
      educ >= 7 ~ "GRAD",
      TRUE ~ NA_character_
    )
  } else {
    # Generic: assume years of education
    case_when(
      educ <= 12 ~ "LE_HS",
      educ %in% 13:15 ~ "SOME_COLLEGE",
      educ == 16 ~ "BA",
      educ > 16 ~ "GRAD",
      TRUE ~ NA_character_
    )
  }
}

#' Create income quintiles using survey weights
#' @param income Numeric income variable
#' @param weight Survey weight variable
#' @param na.rm Remove NA values before computing quintiles
#' @return Character vector with quintile labels Q1-Q5
make_income_quintile_weighted <- function(income, weight, na.rm = TRUE) {

  # Handle missing values
  valid_idx <- !is.na(income) & !is.na(weight)
  if (!any(valid_idx)) return(rep(NA_character_, length(income)))

  # Compute weighted quantiles
  # Using type 7 (default) for consistency
  ord <- order(income[valid_idx])
  cumwt <- cumsum(weight[valid_idx][ord])
  cumwt_pct <- cumwt / sum(weight[valid_idx])

  # Find quintile breakpoints
  breaks <- c(-Inf,
              income[valid_idx][ord][which.min(abs(cumwt_pct - 0.2))],
              income[valid_idx][ord][which.min(abs(cumwt_pct - 0.4))],
              income[valid_idx][ord][which.min(abs(cumwt_pct - 0.6))],
              income[valid_idx][ord][which.min(abs(cumwt_pct - 0.8))],
              Inf)

  # Assign quintiles
  result <- rep(NA_character_, length(income))
  result[valid_idx] <- cut(
    income[valid_idx],
    breaks = breaks,
    labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
    include.lowest = TRUE
  )

  result
}

#' Create household type from component flags
#' @param married_flag Logical: is person married/partnered
#' @param has_child Logical: has children in household
#' @param adult_count_bin Character or numeric: "1" or "2+" adults
#' @return Character with household type
make_hh_type <- function(married_flag, has_child, adult_count_bin) {

  # Standardize adult_count_bin to character
  adult_bin <- if_else(as.numeric(adult_count_bin) >= 2, "2+", "1")

  case_when(
    married_flag & has_child ~ "MARRIED_CHILDREN",
    married_flag & !has_child ~ "MARRIED_NO_CHILDREN",
    !married_flag & has_child ~ "SINGLE_CHILDREN",
    !married_flag & !has_child & adult_bin == "1" ~ "SINGLE_NO_CHILDREN",
    !married_flag & adult_bin == "2+" ~ "OTHER_MULTIADULT",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------------------------
# Standard Column Names (for mart consistency)
# ------------------------------------------------------------------------------

STANDARD_COLS <- list(
  # Time
  period = "period",           # YYYY-MM format
  pulse_wave = "pulse_wave",   # HPS wave identifier

 # Geography
  geo_level = "geo_level",     # "us" or "state"
  geo_id = "geo_id",           # "US" or state FIPS
  state_fips = "state_fips",   # Numeric state FIPS (for CPS)

  # Segmentation
  age_group = "age_group",
  educ_group = "educ_group",
  income_group = "income_group",
  hh_type = "hh_type",

  # Weights
  n_unweighted = "n_unweighted"
)

# ------------------------------------------------------------------------------
# Utility Functions
# ------------------------------------------------------------------------------

#' Ensure a directory exists
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}

#' Format period as YYYY-MM from year and month
make_period <- function(year, month) {
  sprintf("%04d-%02d", as.integer(year), as.integer(month))
}

#' Read a parquet mart file
read_mart <- function(mart_name) {
  path <- switch(
    mart_name,
    "fred" = PATHS$mart_fred,
    "cps" = PATHS$mart_cps,
    "hps" = PATHS$mart_hps,
    stop("Unknown mart: ", mart_name)
  )

  if (!file.exists(path)) {
    stop("Mart file does not exist: ", path, "\nRun the appropriate ingest script first.")
  }

  arrow::read_parquet(path)
}

#' Write a data frame to parquet mart
write_mart <- function(df, mart_name) {
  path <- switch(
    mart_name,
    "fred" = PATHS$mart_fred,
    "cps" = PATHS$mart_cps,
    "hps" = PATHS$mart_hps,
    stop("Unknown mart: ", mart_name)
  )

  ensure_dir(dirname(path))
  arrow::write_parquet(df, path)
  message("Wrote mart: ", path)
  invisible(path)
}

# ------------------------------------------------------------------------------
# Validation Helpers
# ------------------------------------------------------------------------------

#' Check that required columns exist in a data frame
check_required_cols <- function(df, required_cols, df_name = "data") {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

#' Validate segmentation values are in expected set
validate_segmentation <- function(df) {
  issues <- character()

  if ("age_group" %in% names(df)) {
    valid_age <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+", NA)
    bad_age <- setdiff(unique(df$age_group), valid_age)
    if (length(bad_age) > 0) {
      issues <- c(issues, paste("Invalid age_group values:", paste(bad_age, collapse = ", ")))
    }
  }

  if ("educ_group" %in% names(df)) {
    valid_educ <- c("LE_HS", "SOME_COLLEGE", "BA", "GRAD", NA)
    bad_educ <- setdiff(unique(df$educ_group), valid_educ)
    if (length(bad_educ) > 0) {
      issues <- c(issues, paste("Invalid educ_group values:", paste(bad_educ, collapse = ", ")))
    }
  }

  if ("income_group" %in% names(df)) {
    valid_inc <- c("Q1", "Q2", "Q3", "Q4", "Q5", NA)
    bad_inc <- setdiff(unique(df$income_group), valid_inc)
    if (length(bad_inc) > 0) {
      issues <- c(issues, paste("Invalid income_group values:", paste(bad_inc, collapse = ", ")))
    }
  }

  if ("hh_type" %in% names(df)) {
    valid_hh <- c("MARRIED_CHILDREN", "MARRIED_NO_CHILDREN", "SINGLE_CHILDREN",
                  "SINGLE_NO_CHILDREN", "OTHER_MULTIADULT", NA)
    bad_hh <- setdiff(unique(df$hh_type), valid_hh)
    if (length(bad_hh) > 0) {
      issues <- c(issues, paste("Invalid hh_type values:", paste(bad_hh, collapse = ", ")))
    }
  }

  if (length(issues) > 0) {
    warning("Segmentation validation issues:\n", paste("- ", issues, collapse = "\n"))
    return(FALSE)
  }

  TRUE
}

message("Foundational data config loaded. PATHS and helpers available.")
