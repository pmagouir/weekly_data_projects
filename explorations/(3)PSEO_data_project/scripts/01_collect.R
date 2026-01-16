# =============================================================================
# 01_collect.R - PSEO Data Collection
# Question: What do earnings outcomes reveal about the value of degrees?
# =============================================================================

library(tidyverse)
library(here)
library(janitor)

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

# Define paths
project_path <- here("explorations", "(3)PSEO_data_project")
raw_path <- file.path(project_path, "data", "raw")

# Create directories if needed
dir.create(raw_path, recursive = TRUE, showWarnings = FALSE)

cat("PSEO Data Collection\n")
cat(strrep("=", 60), "\n\n")

# -----------------------------------------------------------------------------
# Define Data URLs
# -----------------------------------------------------------------------------

base_url <- "https://lehd.ces.census.gov/data/pseo/latest_release/all/"

data_files <- list(
  # Main data files
  earnings = list(
    url = paste0(base_url, "pseoe_all.csv.gz"),
    local = file.path(raw_path, "pseoe_all.csv.gz"),
    csv = file.path(raw_path, "pseoe_all.csv"),
    description = "Graduate earnings data"
  ),
  flows = list(
    url = paste0(base_url, "pseof_all.csv.gz"),
    local = file.path(raw_path, "pseof_all.csv.gz"),
    csv = file.path(raw_path, "pseof_all.csv"),
    description = "Employment flows data"
  ),
  institutions = list(
    url = paste0(base_url, "pseo_all_institutions.csv"),
    local = file.path(raw_path, "pseo_all_institutions.csv"),
    csv = file.path(raw_path, "pseo_all_institutions.csv"),
    description = "Institution lookup table"
  ),
  partners = list(
    url = paste0(base_url, "pseo_all_partners.txt"),
    local = file.path(raw_path, "pseo_all_partners.txt"),
    csv = NULL,
    description = "Partner institutions list"
  )
)

# Schema/lookup tables
schema_base <- "https://lehd.ces.census.gov/data/schema/latest/"

lookup_files <- list(
  degree_level = list(
    url = paste0(schema_base, "label_degree_level.csv"),
    local = file.path(raw_path, "label_degree_level.csv"),
    description = "Degree level codes"
  ),
  cipcode = list(
    url = paste0(schema_base, "label_cipcode.csv"),
    local = file.path(raw_path, "label_cipcode.csv"),
    description = "CIP codes for majors"
  ),
  geography = list(
    url = paste0(schema_base, "label_geography.csv"),
    local = file.path(raw_path, "label_geography.csv"),
    description = "Geographic codes"
  ),
  industry = list(
    url = paste0(schema_base, "label_industry.csv"),
    local = file.path(raw_path, "label_industry.csv"),
    description = "NAICS industry codes"
  ),
  stusps = list(
    url = paste0(schema_base, "label_stusps.csv"),
    local = file.path(raw_path, "label_stusps.csv"),
    description = "State USPS codes"
  )
)

# -----------------------------------------------------------------------------
# Download Functions
# -----------------------------------------------------------------------------

download_file <- function(url, destfile, description) {
  cat("Downloading:", description, "\n")
  cat("  URL:", url, "\n")

  tryCatch({
    download.file(
      url = url,
      destfile = destfile,
      mode = "wb",
      quiet = TRUE
    )

    file_size <- file.info(destfile)$size
    cat("  Size:", format(file_size, big.mark = ","), "bytes\n")
    cat("  Saved to:", basename(destfile), "\n\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n\n")
    return(FALSE)
  })
}

decompress_gz <- function(gz_file, csv_file, description) {
  cat("Decompressing:", description, "\n")

  tryCatch({
    # Read compressed file and write uncompressed
    con <- gzfile(gz_file, "rb")
    data <- readLines(con, warn = FALSE)
    close(con)

    writeLines(data, csv_file)

    file_size <- file.info(csv_file)$size
    cat("  Output size:", format(file_size, big.mark = ","), "bytes\n")
    cat("  Saved to:", basename(csv_file), "\n\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n\n")
    return(FALSE)
  })
}

# -----------------------------------------------------------------------------
# Download Main Data Files
# -----------------------------------------------------------------------------

cat("Downloading main data files...\n")
cat(strrep("-", 60), "\n\n")

for (name in names(data_files)) {
  file_info <- data_files[[name]]

  # Download file
  success <- download_file(
    url = file_info$url,
    destfile = file_info$local,
    description = file_info$description
  )

  # Decompress if .gz file
  if (success && grepl("\\.gz$", file_info$local) && !is.null(file_info$csv)) {
    decompress_gz(
      gz_file = file_info$local,
      csv_file = file_info$csv,
      description = paste(file_info$description, "(decompressed)")
    )
  }
}

# -----------------------------------------------------------------------------
# Download Lookup Tables
# -----------------------------------------------------------------------------

cat("Downloading lookup tables...\n")
cat(strrep("-", 60), "\n\n")

for (name in names(lookup_files)) {
  file_info <- lookup_files[[name]]

  download_file(
    url = file_info$url,
    destfile = file_info$local,
    description = file_info$description
  )
}

# -----------------------------------------------------------------------------
# Initial Data Inspection
# -----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("Data Inspection\n")
cat(strrep("=", 60), "\n\n")

# Check earnings data
if (file.exists(data_files$earnings$csv)) {
  cat("Earnings Data Preview:\n")
  earnings_preview <- read_csv(
    data_files$earnings$csv,
    n_max = 5,
    show_col_types = FALSE
  )
  cat("  Columns:", ncol(earnings_preview), "\n")
  cat("  Column names:", paste(names(earnings_preview)[1:10], collapse = ", "), "...\n\n")

  # Count total rows
  earnings_rows <- read_csv(
    data_files$earnings$csv,
    show_col_types = FALSE,
    col_types = cols(.default = "c")
  ) |> nrow()
  cat("  Total rows:", format(earnings_rows, big.mark = ","), "\n\n")
}

# Check flows data
if (file.exists(data_files$flows$csv)) {
  cat("Flows Data Preview:\n")
  flows_preview <- read_csv(
    data_files$flows$csv,
    n_max = 5,
    show_col_types = FALSE
  )
  cat("  Columns:", ncol(flows_preview), "\n")
  cat("  Column names:", paste(names(flows_preview)[1:10], collapse = ", "), "...\n\n")

  # Count total rows (this file is large, so just estimate)
  cat("  Note: Flows file is large (~161MB compressed)\n\n")
}

# Check institutions
if (file.exists(data_files$institutions$csv)) {
  cat("Institutions Data:\n")
  institutions <- read_csv(
    data_files$institutions$csv,
    show_col_types = FALSE
  )
  cat("  Total institutions:", nrow(institutions), "\n")
  cat("  Sample institutions:\n")
  institutions |>
    head(5) |>
    print()
  cat("\n")
}

# Check degree levels
if (file.exists(lookup_files$degree_level$local)) {
  cat("Degree Levels:\n")
  degree_levels <- read_csv(
    lookup_files$degree_level$local,
    show_col_types = FALSE
  )
  print(degree_levels)
  cat("\n")
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("Data Collection Complete\n")
cat(strrep("=", 60), "\n\n")

cat("Files downloaded to:", raw_path, "\n\n")

cat("Downloaded files:\n")
files_list <- list.files(raw_path, full.names = TRUE)
for (f in files_list) {
  size <- file.info(f)$size
  cat("  ", basename(f), "-", format(size, big.mark = ","), "bytes\n")
}

cat("\nNext steps:\n")
cat("1. Run 02_clean.R to process and join the data\n")
cat("2. Review data quality and coverage\n")
cat("3. Identify specific research questions\n")

cat("\nDocumentation:\n")
cat("- PSEO Explorer: https://census.gov/data/data-tools/pseo.html\n")
cat("- API Docs: https://www.census.gov/data/developers/data-sets/pseo.html\n")
cat("- Schema: https://lehd.ces.census.gov/data/schema/latest/\n")
