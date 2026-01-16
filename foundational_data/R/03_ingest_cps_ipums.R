# ==============================================================================
# 03_ingest_cps_ipums.R
# Ingest CPS microdata from IPUMS extract
# Run: Monthly (after downloading new IPUMS extract)
# ==============================================================================
#
# PREREQUISITE: Create an IPUMS CPS extract at https://cps.ipums.org/
#
# Required variables for your extract:
#   Time/Geography: YEAR, MONTH, STATEFIP
#   Segmentation: AGE, EDUC, MARST, NCHILD, HHINCOME
#   Labor outcomes: EMPSTAT, LABFORCE, UHRSWORKT
#   Weights: WTFINL
#
# Download the extract and place the .xml (DDI) and .dat.gz files in:
#   foundational_data/data_raw/cps_ipums/
#
# ==============================================================================

library(tidyverse)
library(ipumsr)
library(arrow)
library(here)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Find IPUMS Extract Files
# ------------------------------------------------------------------------------

ipums_dir <- PATHS$raw_cps

# Look for DDI (xml) file - this contains the codebook
ddi_files <- list.files(ipums_dir, pattern = "\\.xml$", full.names = TRUE)

if (length(ddi_files) == 0) {
  stop(
    "No IPUMS DDI file (.xml) found in: ", ipums_dir, "\n\n",
    "To create an IPUMS CPS extract:\n",
    "1. Go to https://cps.ipums.org/\n",
    "2. Create an account (free)\n",
    "3. Select samples (e.g., monthly CPS 2020-2024)\n",
    "4. Add variables: YEAR, MONTH, STATEFIP, AGE, EDUC, MARST, NCHILD,\n",
    "   HHINCOME, EMPSTAT, LABFORCE, UHRSWORKT, WTFINL\n",
    "5. Submit extract and download when ready\n",
    "6. Place .xml and .dat.gz files in: ", ipums_dir
  )
}

# Use the most recent DDI file if multiple exist
ddi_file <- sort(ddi_files, decreasing = TRUE)[1]
message("Using DDI file: ", basename(ddi_file))

# ------------------------------------------------------------------------------
# Read IPUMS Extract
# ------------------------------------------------------------------------------

message("\nReading IPUMS CPS extract...")
message("(This may take a few minutes for large extracts)")

# Read the DDI to get variable info
ddi <- read_ipums_ddi(ddi_file)

# Read the data
cps_raw <- read_ipums_micro(ddi, verbose = TRUE)

message("\nRead ", format(nrow(cps_raw), big.mark = ","), " records.")

# ------------------------------------------------------------------------------
# Validate Required Variables
# ------------------------------------------------------------------------------

required_vars <- c(
  "YEAR", "MONTH", "STATEFIP",
  "AGE", "EDUC", "MARST", "NCHILD", "HHINCOME",
  "EMPSTAT", "LABFORCE", "UHRSWORKT",
  "WTFINL"
)

# Standardize column names to uppercase
names(cps_raw) <- toupper(names(cps_raw))

missing_vars <- setdiff(required_vars, names(cps_raw))
if (length(missing_vars) > 0) {
  stop(
    "Missing required variables in IPUMS extract: ",
    paste(missing_vars, collapse = ", "), "\n",
    "Please create a new extract with these variables."
  )
}

message("All required variables present.")

# ------------------------------------------------------------------------------
# Initial Cleaning
# ------------------------------------------------------------------------------

cps_clean <- cps_raw |>
  # Keep only required variables plus any useful household IDs
  select(
    # Time and geography
    year = YEAR,
    month = MONTH,
    state_fips = STATEFIP,

    # Demographics
    age = AGE,
    educ = EDUC,
    marst = MARST,
    nchild = NCHILD,
    hhincome = HHINCOME,

    # Labor
    empstat = EMPSTAT,
    labforce = LABFORCE,
    uhrsworkt = UHRSWORKT,

    # Weights
    wtfinl = WTFINL,

    # Keep household/person IDs if present
    any_of(c("SERIAL", "PERNUM", "CPSID", "CPSIDP"))
  ) |>
  # Create period
  mutate(period = make_period(year, month)) |>
  # Filter to civilian adults 18+
  filter(age >= 18)

message("\nAfter filtering to adults 18+: ",
        format(nrow(cps_clean), big.mark = ","), " records.")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("CPS Extract Summary")
message(strrep("-", 60))

message("\nTime coverage:")
cps_clean |>
  summarise(
    first_period = min(period),
    last_period = max(period),
    n_months = n_distinct(period)
  ) |>
  print()

message("\nState coverage:")
message("  ", n_distinct(cps_clean$state_fips), " states/territories")
message("  DC present: ", 11 %in% cps_clean$state_fips)

message("\nRecords by year:")
cps_clean |>
  count(year) |>
  print(n = Inf)

# ------------------------------------------------------------------------------
# Save Clean Microdata
# ------------------------------------------------------------------------------

ensure_dir(PATHS$clean_cps)
output_path <- file.path(PATHS$clean_cps, "cps_clean_latest.parquet")

write_parquet(cps_clean, output_path)

message("\n", strrep("=", 60))
message("CPS ingest complete!")
message("Clean microdata: ", output_path)
message("\nNext step: Run 04_clean_cps.R to build the state-month mart.")
message(strrep("=", 60))
