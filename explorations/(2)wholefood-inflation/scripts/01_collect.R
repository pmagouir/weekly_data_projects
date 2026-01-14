# =============================================================================
# 01_collect.R - CPI Food Price Data Collection
# Question: How have whole foods vs processed foods prices changed 2015-2024?
# =============================================================================

library(tidyverse)
library(here)
library(fredr)

# Source common functions
source(here("functions.R"))

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

# Set FRED API key (get free key at https://fred.stlouisfed.org/docs/api/api_key.html)
# Store in .Renviron: FRED_API_KEY=your_key_here
if (Sys.getenv("FRED_API_KEY") == "") {
  stop("Please set FRED_API_KEY in your .Renviron file")
}
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Years to collect
years <- 2015:2024

cat("Collecting CPI food price data for", min(years), "to", max(years), "\n\n")

# -----------------------------------------------------------------------------
# Define CPI Series IDs
# -----------------------------------------------------------------------------

# Whole Foods Categories
# Note: These series IDs are examples - you'll need to verify actual IDs from FRED
# Search at https://fred.stlouisfed.org/ for specific series

whole_food_series <- list(
  # Overall food categories
  "food_overall" = "CUSR0000SAF11",  # Food at home (U.S. city average)
  
  # Whole Foods
  "meats" = "CUSR0000SAF111",        # Meats, poultry, fish, and eggs
  "fruits_vegetables" = "CUSR0000SAF112",  # Fruits and vegetables
  "dairy" = "CUSR0000SAF113",        # Dairy and related products
  "eggs" = "CUSR0000SAF1131",        # Eggs
  "milk" = "CUSR0000SAF1132",        # Milk
  "nuts" = "CUSR0000SAF1161",        # Nuts (if available)
  "beans" = "CUSR0000SAF1122"        # Dried beans (if available)
)

# Processed/Junk Foods
processed_food_series <- list(
  "sweets" = "CUSR0000SAF117",       # Sugar and sweets
  "candy" = "CUSR0000SAF1171",       # Candy and chewing gum
  "desserts" = "CUSR0000SAF1172",    # Other sweets (if available)
  "frozen_foods" = "CUSR0000SAF118", # Frozen and freeze dried prepared foods
  "processed_foods" = "CUSR0000SAF119"  # Other prepared foods
)

# DC Regional Series (replace with actual DC-specific series IDs)
dc_series <- list(
  "food_overall_dc" = "CUURA000SAF11",  # Food at home - DC area
  "meats_dc" = "CUURA000SAF111",
  "fruits_vegetables_dc" = "CUURA000SAF112"
)

# Combine all series
all_series <- c(whole_food_series, processed_food_series, dc_series)

# -----------------------------------------------------------------------------
# Fetch Data from FRED
# -----------------------------------------------------------------------------

cat("Fetching CPI data from FRED API...\n")
cat("(This may take a few minutes)\n\n")

# Function to fetch series with error handling
fetch_series <- function(series_id, series_name) {
  cat("Fetching:", series_name, "(", series_id, ")\n")
  
  tryCatch({
    data <- fredr(
      series_id = series_id,
      observation_start = as.Date(paste0(min(years), "-01-01")),
      observation_end = as.Date(paste0(max(years), "-12-31")),
      frequency = "m",  # Monthly
      units = "pc1"     # Percent change from year ago (optional, can use "lin" for index)
    )
    
    if (!is.null(data) && nrow(data) > 0) {
      data$series_name <- series_name
      data$series_id <- series_id
      return(data)
    } else {
      cat("  Warning: No data returned for", series_id, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("  Error fetching", series_id, ":", e$message, "\n")
    return(NULL)
  })
}

# Fetch all series
cpi_data_list <- map2(
  all_series,
  names(all_series),
  ~ fetch_series(.x, .y)
)

# Remove NULL results
cpi_data_list <- cpi_data_list[!map_lgl(cpi_data_list, is.null)]

# Combine into single dataframe
if (length(cpi_data_list) > 0) {
  cpi_raw <- bind_rows(cpi_data_list)
  
  cat("\nFetched", length(cpi_data_list), "series\n")
  cat("Total observations:", nrow(cpi_raw), "\n")
} else {
  stop("No data was successfully fetched. Please check series IDs and API key.")
}

# -----------------------------------------------------------------------------
# Initial Inspection
# -----------------------------------------------------------------------------

cat("\nSeries collected:\n")
cpi_raw |>
  distinct(series_name, series_id) |>
  print()

cat("\nDate range:\n")
cat("  Start:", min(cpi_raw$date), "\n")
cat("  End:", max(cpi_raw$date), "\n")

cat("\nSample data:\n")
cpi_raw |>
  select(date, series_name, value) |>
  head(10) |>
  print()

# -----------------------------------------------------------------------------
# Save Raw Data
# -----------------------------------------------------------------------------

raw_path <- here("explorations", "(2)wholefood-inflation", "data", "raw")

write_csv(
  cpi_raw,
  file.path(raw_path, "cpi_food_data_2015_2024.csv")
)

cat("\nRaw data saved to:", file.path(raw_path, "cpi_food_data_2015_2024.csv"), "\n")

# -----------------------------------------------------------------------------
# Notes
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("Data Collection Complete\n")
cat(strrep("=", 60), "\n")
cat("\nNext steps:\n")
cat("1. Verify series IDs are correct (search FRED website)\n")
cat("2. Run 02_clean.R to process the data\n")
cat("3. Update series IDs in this script if needed\n")
cat("\nFRED CPI series search: https://fred.stlouisfed.org/\n")

