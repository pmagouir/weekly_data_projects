# =============================================================================
# 02_clean.R - PSEO Data Cleaning and Processing
# Question: What do earnings outcomes reveal about the value of degrees?
# =============================================================================

library(tidyverse)
library(here)
library(janitor)

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

project_path <- here("explorations", "(3)PSEO_data_project")
raw_path <- file.path(project_path, "data", "raw")
processed_path <- file.path(project_path, "data", "processed")

dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)

cat("PSEO Data Cleaning\n")
cat(strrep("=", 60), "\n\n")

# -----------------------------------------------------------------------------
# Load Lookup Tables
# -----------------------------------------------------------------------------

cat("Loading lookup tables...\n\n")

# Degree levels
degree_levels <- read_csv(
  file.path(raw_path, "label_degree_level.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  rename(degree_level = 1, degree_label = 2)

cat("Degree levels loaded:", nrow(degree_levels), "levels\n")

# CIP codes (majors)
cip_codes <- read_csv(
  file.path(raw_path, "label_cipcode.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  rename(cipcode = 1, cip_label = 2)

cat("CIP codes loaded:", nrow(cip_codes), "majors\n")

# Institutions
institutions <- read_csv(
  file.path(raw_path, "pseo_all_institutions.csv"),
  show_col_types = FALSE
) |>
  clean_names()

cat("Institutions loaded:", nrow(institutions), "schools\n")

# States
states <- read_csv(
  file.path(raw_path, "label_stusps.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  rename(stusps = 1, state_name = 2)

cat("States loaded:", nrow(states), "states\n")

# Industries
industries <- read_csv(
  file.path(raw_path, "label_industry.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  rename(industry = 1, industry_label = 2)

cat("Industries loaded:", nrow(industries), "industries\n\n")

# -----------------------------------------------------------------------------
# Load and Clean Earnings Data
# -----------------------------------------------------------------------------

cat(strrep("-", 60), "\n")
cat("Processing Earnings Data...\n")
cat(strrep("-", 60), "\n\n")

earnings_raw <- read_csv(
  file.path(raw_path, "pseoe_all.csv"),
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character(),
    y1_p25_earnings = col_double(),
    y1_p50_earnings = col_double(),
    y1_p75_earnings = col_double(),
    y1_grads_earn = col_integer(),
    y5_p25_earnings = col_double(),
    y5_p50_earnings = col_double(),
    y5_p75_earnings = col_double(),
    y5_grads_earn = col_integer(),
    y10_p25_earnings = col_double(),
    y10_p50_earnings = col_double(),
    y10_p75_earnings = col_double(),
    y10_grads_earn = col_integer(),
    y1_ipeds_count = col_integer(),
    y5_ipeds_count = col_integer(),
    y10_ipeds_count = col_integer()
  )
) |>
  clean_names()

cat("Loaded earnings data:", format(nrow(earnings_raw), big.mark = ","), "rows\n")
cat("Columns:", ncol(earnings_raw), "\n\n")

# Join with lookup tables
earnings_clean <- earnings_raw |>
  # Join degree labels
  left_join(degree_levels, by = "degree_level") |>
  # Join CIP labels
  left_join(cip_codes, by = "cipcode") |>
  # Join institution names
  left_join(
    institutions |> select(institution, inst_name, inst_state),
    by = "institution"
  ) |>
  # Join state names
  left_join(
    states |> rename(inst_state = stusps),
    by = "inst_state"
  ) |>
  # Create derived variables
  mutate(
    # Earnings growth (Y5 vs Y1)
    earnings_growth_5y = case_when(
      y1_p50_earnings > 0 & y5_p50_earnings > 0 ~
        (y5_p50_earnings - y1_p50_earnings) / y1_p50_earnings,
      TRUE ~ NA_real_
    ),
    # Earnings growth (Y10 vs Y1)
    earnings_growth_10y = case_when(
      y1_p50_earnings > 0 & y10_p50_earnings > 0 ~
        (y10_p50_earnings - y1_p50_earnings) / y1_p50_earnings,
      TRUE ~ NA_real_
    ),
    # Earnings spread (inequality within cohort)
    y1_spread = y1_p75_earnings - y1_p25_earnings,
    y5_spread = y5_p75_earnings - y5_p25_earnings,
    y10_spread = y10_p75_earnings - y10_p25_earnings,
    # 2-digit CIP code for broader field
    cip_2digit = str_sub(cipcode, 1, 2),
    # Graduation cohort as numeric
    grad_cohort_num = as.integer(grad_cohort)
  )

cat("Earnings data cleaned and enriched\n")
cat("Final columns:", ncol(earnings_clean), "\n\n")

# Summary stats
cat("Summary of cleaned earnings data:\n")
cat("  Unique institutions:", n_distinct(earnings_clean$institution), "\n")
cat("  Unique degree levels:", n_distinct(earnings_clean$degree_level), "\n")
cat("  Unique CIP codes:", n_distinct(earnings_clean$cipcode), "\n")
cat("  Graduation cohorts:", paste(sort(unique(earnings_clean$grad_cohort)), collapse = ", "), "\n\n")

# -----------------------------------------------------------------------------
# Load and Clean Flows Data
# -----------------------------------------------------------------------------

cat(strrep("-", 60), "\n")
cat("Processing Flows Data...\n")
cat(strrep("-", 60), "\n\n")

cat("Note: Flows file is large, this may take a moment...\n\n")

flows_raw <- read_csv(
  file.path(raw_path, "pseof_all.csv"),
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character(),
    y1_grads_emp = col_integer(),
    y1_grads_emp_instate = col_integer(),
    y1_grads_nme = col_integer(),
    y5_grads_emp = col_integer(),
    y5_grads_emp_instate = col_integer(),
    y5_grads_nme = col_integer(),
    y10_grads_emp = col_integer(),
    y10_grads_emp_instate = col_integer(),
    y10_grads_nme = col_integer()
  )
) |>
  clean_names()

cat("Loaded flows data:", format(nrow(flows_raw), big.mark = ","), "rows\n")
cat("Columns:", ncol(flows_raw), "\n\n")

# Join with lookup tables
flows_clean <- flows_raw |>
  # Join degree labels
  left_join(degree_levels, by = "degree_level") |>
  # Join CIP labels
  left_join(cip_codes, by = "cipcode") |>
  # Join institution names
  left_join(
    institutions |> select(institution, inst_name, inst_state),
    by = "institution"
  ) |>
  # Join industry labels
  left_join(industries, by = "industry") |>
  # Create derived variables
  mutate(
    # In-state retention rate
    y1_retention_rate = case_when(
      y1_grads_emp > 0 ~ y1_grads_emp_instate / y1_grads_emp,
      TRUE ~ NA_real_
    ),
    y5_retention_rate = case_when(
      y5_grads_emp > 0 ~ y5_grads_emp_instate / y5_grads_emp,
      TRUE ~ NA_real_
    ),
    y10_retention_rate = case_when(
      y10_grads_emp > 0 ~ y10_grads_emp_instate / y10_grads_emp,
      TRUE ~ NA_real_
    ),
    # Employment rate (employed / total tracked)
    y1_emp_rate = case_when(
      (y1_grads_emp + y1_grads_nme) > 0 ~
        y1_grads_emp / (y1_grads_emp + y1_grads_nme),
      TRUE ~ NA_real_
    ),
    # 2-digit CIP code
    cip_2digit = str_sub(cipcode, 1, 2),
    # Graduation cohort as numeric
    grad_cohort_num = as.integer(grad_cohort)
  )

cat("Flows data cleaned and enriched\n")
cat("Final columns:", ncol(flows_clean), "\n\n")

# Summary stats
cat("Summary of cleaned flows data:\n")
cat("  Unique institutions:", n_distinct(flows_clean$institution), "\n")
cat("  Unique industries:", n_distinct(flows_clean$industry), "\n")
cat("  Unique geographies:", n_distinct(flows_clean$geography), "\n\n")

# -----------------------------------------------------------------------------
# Create Summary Tables
# -----------------------------------------------------------------------------

cat(strrep("-", 60), "\n")
cat("Creating Summary Tables...\n")
cat(strrep("-", 60), "\n\n")

# Summary by degree level
earnings_by_degree <- earnings_clean |>
  filter(
    inst_level == "I",  # Institution level
    cip_level == "A",   # All CIP codes
    !is.na(y1_p50_earnings)
  ) |>
  group_by(degree_level, degree_label) |>
  summarize(
    n_institutions = n_distinct(institution),
    n_cohorts = n_distinct(grad_cohort),
    median_y1 = median(y1_p50_earnings, na.rm = TRUE),
    median_y5 = median(y5_p50_earnings, na.rm = TRUE),
    median_y10 = median(y10_p50_earnings, na.rm = TRUE),
    .groups = "drop"
  )

cat("Earnings by degree level:\n")
print(earnings_by_degree)
cat("\n")

# Summary by field (2-digit CIP)
earnings_by_field <- earnings_clean |>
  filter(
    degree_level == "05",  # Bachelor's only
    cip_level %in% c("2", "4"),  # Specific field
    !is.na(y1_p50_earnings)
  ) |>
  group_by(cip_2digit) |>
  summarize(
    n_programs = n(),
    median_y1 = median(y1_p50_earnings, na.rm = TRUE),
    median_y5 = median(y5_p50_earnings, na.rm = TRUE),
    median_y10 = median(y10_p50_earnings, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(median_y1))

cat("Top earning fields (Bachelor's, by median Year 1 earnings):\n")
print(head(earnings_by_field, 10))
cat("\n")

# -----------------------------------------------------------------------------
# Save Processed Data
# -----------------------------------------------------------------------------

cat(strrep("-", 60), "\n")
cat("Saving Processed Data...\n")
cat(strrep("-", 60), "\n\n")

# Save main processed files
write_csv(earnings_clean, file.path(processed_path, "earnings_clean.csv"))
cat("Saved: earnings_clean.csv\n")

write_csv(flows_clean, file.path(processed_path, "flows_clean.csv"))
cat("Saved: flows_clean.csv\n")

# Save summary tables
write_csv(earnings_by_degree, file.path(processed_path, "summary_by_degree.csv"))
cat("Saved: summary_by_degree.csv\n")

write_csv(earnings_by_field, file.path(processed_path, "summary_by_field.csv"))
cat("Saved: summary_by_field.csv\n")

# Save lookup tables for convenience
write_csv(degree_levels, file.path(processed_path, "lookup_degree_levels.csv"))
write_csv(cip_codes, file.path(processed_path, "lookup_cip_codes.csv"))
write_csv(institutions, file.path(processed_path, "lookup_institutions.csv"))
write_csv(industries, file.path(processed_path, "lookup_industries.csv"))
cat("Saved: lookup tables\n\n")

# Save as RDS for faster loading in R
saveRDS(
  list(
    earnings = earnings_clean,
    flows = flows_clean,
    degree_levels = degree_levels,
    cip_codes = cip_codes,
    institutions = institutions,
    industries = industries,
    earnings_by_degree = earnings_by_degree,
    earnings_by_field = earnings_by_field
  ),
  file.path(processed_path, "pseo_processed.rds")
)
cat("Saved: pseo_processed.rds (all data objects)\n\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("Data Cleaning Complete\n")
cat(strrep("=", 60), "\n\n")

cat("Processed files saved to:", processed_path, "\n\n")

cat("Data summary:\n")
cat("  Earnings records:", format(nrow(earnings_clean), big.mark = ","), "\n")
cat("  Flows records:", format(nrow(flows_clean), big.mark = ","), "\n")
cat("  Institutions:", nrow(institutions), "\n")
cat("  CIP codes:", nrow(cip_codes), "\n\n")

cat("Next steps:\n")
cat("1. Run 03_analyze.R for statistical analysis\n")
cat("2. Explore specific research questions\n")
cat("3. Create visualizations with 04_visualize.R\n")
