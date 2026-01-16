# ==============================================================================
# 01_ingest_hps.R
# Ingest Household Pulse Survey (HPS) Public Use Files
# Run: Weekly (when new PUF release is available)
# ==============================================================================
#
# Data source: Census Bureau Household Pulse Survey
# https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html
#
# This script handles:
# 1. Downloading PUF ZIP files from Census (optional)
# 2. Unzipping and organizing files by release
# 3. Cataloging available releases and their dictionaries
#
# ==============================================================================

library(tidyverse)
library(here)
library(httr)
library(rvest)

# Load config
source(here("foundational_data", "R", "00_config.R"))

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

HPS_BASE_URL <- "https://www2.census.gov/programs-surveys/demo/datasets/hhp/"

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' List available HPS releases from Census website
#' @return Data frame with release info
list_hps_releases <- function() {
  message("Checking Census website for available HPS releases...")

  # Note: This is a simplified approach. Census organizes by year/week.
  # Manual download may be more reliable for specific releases.

  message("\nHPS data is organized at:")
  message("  ", HPS_BASE_URL)
  message("\nNavigate to the year folder, then download the PUF ZIP for your desired week.")
  message("Each ZIP contains:")
  message("  - CSV data file (pulse[year]_puf_[week].csv)")
  message("  - Data dictionary (pulse[year]_data.dictionary_[week].xlsx)")
  message("  - Replicate weights file (pulse[year]_repwgt_puf_[week].csv)")

  invisible(NULL)
}

#' Unzip HPS release to organized folder
#' @param zip_path Path to downloaded ZIP file
#' @param release_id Identifier for this release (e.g., "2024_week67")
unzip_hps_release <- function(zip_path, release_id = NULL) {

  if (!file.exists(zip_path)) {
    stop("ZIP file not found: ", zip_path)
  }

  # Auto-detect release ID from filename if not provided
  if (is.null(release_id)) {
    # Expected format: HPS_Week67_PUF_CSV.zip or similar
    fname <- basename(zip_path)
    release_id <- gsub("\\.zip$", "", fname, ignore.case = TRUE)
    release_id <- gsub("[^a-zA-Z0-9_-]", "_", release_id)
  }

  # Create release folder
  release_dir <- file.path(PATHS$raw_hps, release_id)
  ensure_dir(release_dir)

  # Unzip
  message("Unzipping to: ", release_dir)
  unzip(zip_path, exdir = release_dir)

  # List extracted files
  files <- list.files(release_dir, recursive = TRUE)
  message("Extracted ", length(files), " files:")
  for (f in files) {
    message("  - ", f)
  }

  invisible(release_dir)
}

#' Catalog all ingested HPS releases
#' @return Data frame with release info and file paths
catalog_hps_releases <- function() {

  release_dirs <- list.dirs(PATHS$raw_hps, recursive = FALSE, full.names = TRUE)

  if (length(release_dirs) == 0) {
    message("No HPS releases found in: ", PATHS$raw_hps)
    return(tibble(
      release_id = character(),
      release_dir = character(),
      data_file = character(),
      dict_file = character(),
      repwgt_file = character()
    ))
  }

  catalog <- map_dfr(release_dirs, function(dir) {
    release_id <- basename(dir)
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)

    # Find key files
    data_file <- files[grepl("puf.*\\.csv$", files, ignore.case = TRUE) &
                       !grepl("repwgt", files, ignore.case = TRUE)][1]
    dict_file <- files[grepl("dictionary.*\\.(xlsx|csv)$", files, ignore.case = TRUE)][1]
    repwgt_file <- files[grepl("repwgt.*\\.csv$", files, ignore.case = TRUE)][1]

    tibble(
      release_id = release_id,
      release_dir = dir,
      data_file = data_file,
      dict_file = dict_file,
      repwgt_file = repwgt_file
    )
  })

  catalog
}

# ------------------------------------------------------------------------------
# Main Workflow
# ------------------------------------------------------------------------------

message(strrep("=", 60))
message("HPS Ingest Script")
message(strrep("=", 60))

# Check what's already ingested
ensure_dir(PATHS$raw_hps)
catalog <- catalog_hps_releases()

if (nrow(catalog) > 0) {
  message("\nCurrently ingested releases:")
  print(catalog |> select(release_id, data_file))
} else {
  message("\nNo releases currently ingested.")
}

# Instructions for manual download
message("\n", strrep("-", 60))
message("To add a new HPS release:")
message(strrep("-", 60))
message("\n1. Go to: https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html")
message("\n2. Click on the desired year (e.g., 2024)")
message("\n3. Download the PUF ZIP for your desired week")
message("   Look for files like: HPS_Week67_PUF_CSV.zip")
message("\n4. Place the ZIP in: ", PATHS$raw_hps)
message("\n5. Run the following to unzip:")
message('   unzip_hps_release("path/to/your.zip")')
message("\n6. Then run 02_clean_hps.R to build the mart")

# ------------------------------------------------------------------------------
# Interactive Mode: Process any new ZIPs
# ------------------------------------------------------------------------------

# Look for unprocessed ZIP files
zip_files <- list.files(PATHS$raw_hps, pattern = "\\.zip$",
                        full.names = TRUE, recursive = FALSE)

if (length(zip_files) > 0) {
  message("\n", strrep("-", 60))
  message("Found ", length(zip_files), " ZIP file(s) to process:")
  message(strrep("-", 60))

  for (zip_path in zip_files) {
    message("\nProcessing: ", basename(zip_path))

    tryCatch({
      release_dir <- unzip_hps_release(zip_path)
      message("Success! Files extracted to: ", release_dir)

      # Optionally move/remove the ZIP after successful extraction
      # file.rename(zip_path, file.path(release_dir, basename(zip_path)))

    }, error = function(e) {
      warning("Failed to process ", basename(zip_path), ": ", e$message)
    })
  }

  # Update catalog
  catalog <- catalog_hps_releases()
  message("\n\nUpdated release catalog:")
  print(catalog |> select(release_id, data_file))
}

message("\n", strrep("=", 60))
message("HPS ingest complete!")
message("Next step: Run 02_clean_hps.R to build the national mart.")
message(strrep("=", 60))

# Return catalog for use in other scripts
invisible(catalog)
