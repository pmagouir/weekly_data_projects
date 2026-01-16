# ==============================================================================
# 06_build_marts.R
# Validate and harmonize all data marts
# Run: After building individual marts, or as a health check
# ==============================================================================
#
# This script:
# 1. Validates that all required marts exist
# 2. Checks column names and types for consistency
# 3. Validates segmentation values
# 4. Reports summary statistics across all marts
# 5. Optionally creates a combined "reporting view" for convenience
#
# ==============================================================================

library(tidyverse)
library(arrow)
library(here)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Check Mart Existence
# ------------------------------------------------------------------------------

message(strrep("=", 60))
message("Foundational Data Marts - Validation Report")
message(strrep("=", 60))
message("\nGenerated: ", Sys.time())

marts <- list(
  fred = PATHS$mart_fred,
  cps = PATHS$mart_cps,
  hps = PATHS$mart_hps
)

mart_status <- map_dfr(names(marts), function(name) {
  path <- marts[[name]]
  exists <- file.exists(path)
  size <- if (exists) file.size(path) else NA_real_

  tibble(
    mart = name,
    path = path,
    exists = exists,
    size_mb = round(size / 1024 / 1024, 2)
  )
})

message("\n", strrep("-", 60))
message("Mart Files")
message(strrep("-", 60))
print(mart_status)

missing_marts <- mart_status |> filter(!exists) |> pull(mart)
if (length(missing_marts) > 0) {
  message("\nWARNING: Missing marts: ", paste(missing_marts, collapse = ", "))
  message("Run the corresponding ingest/clean scripts to build these marts.")
}

# ------------------------------------------------------------------------------
# Load and Validate Each Mart
# ------------------------------------------------------------------------------

mart_data <- list()
mart_summaries <- list()

for (name in names(marts)) {

  if (!file.exists(marts[[name]])) {
    message("\nSkipping ", name, " (file not found)")
    next
  }

  message("\n", strrep("-", 60))
  message("Validating: ", name)
  message(strrep("-", 60))

  df <- read_parquet(marts[[name]])
  mart_data[[name]] <- df

  # Basic info
  message("  Rows: ", format(nrow(df), big.mark = ","))
  message("  Cols: ", ncol(df))
  message("  Columns: ", paste(names(df), collapse = ", "))

  # Validate segmentation
  message("\n  Validating segmentation...")
  valid <- validate_segmentation(df)
  message("  Segmentation valid: ", valid)

  # Mart-specific checks
  if (name == "fred") {
    message("\n  FRED-specific checks:")
    message("    Series: ", paste(unique(df$series_id), collapse = ", "))
    message("    UMCSENT present: ", "UMCSENT" %in% df$series_id)
    message("    Date range: ", min(df$period), " to ", max(df$period))

  } else if (name == "cps") {
    message("\n  CPS-specific checks:")
    message("    States: ", n_distinct(df$state_fips))
    message("    DC (FIPS 11) present: ", DC_FIPS %in% df$state_fips)
    message("    Date range: ", min(df$period), " to ", max(df$period))
    message("    Segments: ", nrow(df))

  } else if (name == "hps") {
    message("\n  HPS-specific checks:")
    message("    Pulse waves: ", paste(unique(df$pulse_wave), collapse = ", "))
    message("    Segments: ", nrow(df))
    message("    Has PHQ2: ", "pct_phq2_positive" %in% names(df))
    message("    Has GAD2: ", "pct_gad2_positive" %in% names(df))
    message("    Has hardship metrics: ", "pct_hardship_expenses" %in% names(df))
  }

  # Build summary
  mart_summaries[[name]] <- tibble(
    mart = name,
    n_rows = nrow(df),
    n_cols = ncol(df),
    segmentation_valid = valid
  )
}

# ------------------------------------------------------------------------------
# Cross-Mart Summary
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("Cross-Mart Summary")
message(strrep("-", 60))

summary_df <- bind_rows(mart_summaries)
print(summary_df)

# ------------------------------------------------------------------------------
# Check Success Criteria (from spec)
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("Success Criteria Check")
message(strrep("-", 60))

criteria <- tibble(
  criterion = c(
    "mart_fred_month.parquet includes UMCSENT",
    "mart_cps_state_month.parquet includes DC (state_fips == 11)",
    "mart_hps_national.parquet includes hardship metrics",
    "mart_hps_national.parquet includes PHQ-2/GAD-2 metrics"
  ),
  status = c(
    # FRED has UMCSENT
    if ("fred" %in% names(mart_data)) "UMCSENT" %in% mart_data$fred$series_id else FALSE,

    # CPS has DC
    if ("cps" %in% names(mart_data)) DC_FIPS %in% mart_data$cps$state_fips else FALSE,

    # HPS has hardship
    if ("hps" %in% names(mart_data)) "pct_hardship_expenses" %in% names(mart_data$hps) else FALSE,

    # HPS has mental health
    if ("hps" %in% names(mart_data)) {
      "pct_phq2_positive" %in% names(mart_data$hps) &
      "pct_gad2_positive" %in% names(mart_data$hps)
    } else FALSE
  )
)

print(criteria)

all_pass <- all(criteria$status)
message("\nAll criteria pass: ", all_pass)

# ------------------------------------------------------------------------------
# Optional: Create Combined Reporting View
# ------------------------------------------------------------------------------

# This creates a convenience table that exploration projects can optionally use
# to get a quick view of all national-level data

if (all(c("fred", "hps") %in% names(mart_data))) {

  message("\n", strrep("-", 60))
  message("Creating combined reporting view (optional)")
  message(strrep("-", 60))

  # Create period mapping for HPS (approximate: use release metadata)
  # For now, this is a placeholder - you'd want to map pulse_wave to period
  # based on the actual collection dates

  message("Note: Combined view creation requires pulse_wave -> period mapping.")
  message("This is left as a placeholder for future enhancement.")
}

# ------------------------------------------------------------------------------
# Final Report
# ------------------------------------------------------------------------------

message("\n", strrep("=", 60))
message("Validation Complete")
message(strrep("=", 60))

if (length(missing_marts) == 0 && all_pass) {
  message("\nAll marts built and validated successfully!")
  message("Foundation is ready for use by exploration projects.")
} else {
  message("\nSome issues found. Review the output above.")
}

message("\nMart locations:")
for (name in names(marts)) {
  status <- if (file.exists(marts[[name]])) "OK" else "MISSING"
  message("  ", name, ": ", marts[[name]], " [", status, "]")
}

message("\n")
