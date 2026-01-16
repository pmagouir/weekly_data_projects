# ==============================================================================
# 05_ingest_fred.R
# Ingest macro context data from FRED API
# Run: Weekly (or as needed for fresh macro data)
# ==============================================================================

library(tidyverse)
library(fredr)
library(arrow)
library(here)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Setup FRED API
# ------------------------------------------------------------------------------

if (Sys.getenv("FRED_API_KEY") == "") {
  stop(
    "FRED_API_KEY not found.\n",
    "Get a free API key at: https://fred.stlouisfed.org/docs/api/api_key.html\n",
    "Add to your .Renviron file: FRED_API_KEY=your_key_here"
  )
}

fredr_set_key(Sys.getenv("FRED_API_KEY"))
message("FRED API key set.")

# ------------------------------------------------------------------------------
# Define Series to Pull
# ------------------------------------------------------------------------------

# Series defined in 00_config.R:
# - CPIAUCSL (headline CPI)
# - CPIFABSL (food CPI)
# - CPIENGSL (energy CPI)
# - UNRATE (unemployment rate)
# - FEDFUNDS (fed funds rate)
# - MORTGAGE30US (30-year mortgage rate)
# - UMCSENT (consumer sentiment)

message("\nFetching ", length(FRED_SERIES), " series from FRED...")

# ------------------------------------------------------------------------------
# Fetch Data
# ------------------------------------------------------------------------------

# Function to fetch a single series with metadata
fetch_series <- function(series_id) {
  message("  Fetching: ", series_id)

  # Detect if quarterly series (ends in Q)
  is_quarterly <- grepl("Q$", series_id)
  freq <- if (is_quarterly) "q" else "m"
  freq_label <- if (is_quarterly) "quarterly" else "monthly"

  tryCatch({
    # Get observations
    obs <- fredr(
      series_id = series_id,
      observation_start = as.Date("2000-01-01"),
      observation_end = Sys.Date(),
      frequency = freq
    )

    # Get series info for metadata
    info <- fredr_series(series_id)

    obs |>
      mutate(
        series_title = info$title,
        units = info$units,
        frequency = freq_label
      )

  }, error = function(e) {
    warning("Failed to fetch ", series_id, ": ", e$message)
    return(NULL)
  })
}

# Fetch all series
raw_data <- map(FRED_SERIES, fetch_series) |>
  compact() |>  # Remove any NULL results from failures
  bind_rows()

if (nrow(raw_data) == 0) {
  stop("No data retrieved from FRED. Check API key and connection.")
}

message("\nFetched ", nrow(raw_data), " observations across ",
        n_distinct(raw_data$series_id), " series.")

# ------------------------------------------------------------------------------
# Clean and Standardize
# ------------------------------------------------------------------------------

fred_clean <- raw_data |>
  # Create standard period column
  mutate(
    period = make_period(year(date), month(date)),
    geo_level = "us",
    geo_id = "US"
  ) |>
  # Select and rename to standard schema
  select(
    period,
    series_id,
    value,
    date,
    series_title,
    units,
    geo_level,
    geo_id
  ) |>
  # Sort
  arrange(series_id, period)

# ------------------------------------------------------------------------------
# Summary Statistics
# ------------------------------------------------------------------------------

message("\n", strrep("-", 60))
message("FRED Data Summary")
message(strrep("-", 60))

fred_clean |>
  group_by(series_id, series_title, units) |>
  summarise(
    first_period = min(period),
    last_period = max(period),
    n_obs = n(),
    latest_value = last(value),
    .groups = "drop"
  ) |>
  print(n = Inf)

# ------------------------------------------------------------------------------
# Save Raw Cache (optional, for debugging)
# ------------------------------------------------------------------------------

ensure_dir(PATHS$raw_fred)
raw_cache_path <- file.path(PATHS$raw_fred,
                            paste0("fred_raw_", Sys.Date(), ".parquet"))
write_parquet(raw_data, raw_cache_path)
message("\nRaw data cached: ", raw_cache_path)

# ------------------------------------------------------------------------------
# Write Mart
# ------------------------------------------------------------------------------

write_mart(fred_clean, "fred")

message("\n", strrep("=", 60))
message("FRED ingest complete!")
message("Mart: ", PATHS$mart_fred)
message(strrep("=", 60))
