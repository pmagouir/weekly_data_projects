# =============================================================================
# 01_collect.R - NBA Shot Selection Data Collection
# Question: How has 2pt vs 3pt shot selection evolved over 20 years?
# =============================================================================

library(tidyverse)
library(here)
library(nbastatR)
library(future)

# Setup parallel processing for faster API calls
plan(multisession)

# Suppress nbastatR's verbose output
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# Source common functions
source(here("functions.R"))

# -----------------------------------------------------------------------------
# Define Parameters
# -----------------------------------------------------------------------------

# 20 seasons: 2005-06 through 2024-25
# nbastatR uses the year the season ends (e.g., 2025 = 2024-25 season)
seasons <- 2006:2025

cat("Collecting NBA team game logs for", length(seasons), "seasons\n")
cat("Seasons:", min(seasons), "to", max(seasons), "\n\n")

# -----------------------------------------------------------------------------
# Fetch Team Game Logs
# -----------------------------------------------------------------------------

# game_logs returns one row per team per game with box score stats
# Includes: FGA, FG3A, FGM, FG3M, wins, losses, etc.

cat("Fetching team game logs (this may take several minutes)...\n")

nba_team_logs <- game_logs(
  seasons = seasons,
  result_types = "team",
  season_types = "Regular Season"
)

cat("\nFetched", nrow(nba_team_logs), "team-game records\n")

# -----------------------------------------------------------------------------
# Initial Inspection
# -----------------------------------------------------------------------------

# Check available columns related to shooting
shooting_cols <- names(nba_team_logs) |>
  str_subset("fg|FG|shot|Shot|3p|3P|pt|Pt")

cat("\nShooting-related columns:\n")
print(shooting_cols)

# Quick sanity check on key fields
cat("\nSample of key fields:\n")
nba_team_logs |>
  select(
    yearSeason, dateGame, nameTeam,
    matches("fga|fg3a|fgm|fg3m", ignore.case = TRUE)
  ) |>
  head(10) |>
  print()

# Check season coverage
cat("\nGames per season:\n")
nba_team_logs |>
  count(yearSeason) |>
  print(n = 25)

# -----------------------------------------------------------------------------
# Save Raw Data
# -----------------------------------------------------------------------------

raw_path <- here("explorations", "2025-03_nba-shot-selection", "data", "raw")

write_csv(
  nba_team_logs,
  file.path(raw_path, "nba_team_game_logs_2006_2025.csv")
)

cat("\nRaw data saved to:", file.path(raw_path, "nba_team_game_logs_2006_2025.csv"), "\n")

# -----------------------------------------------------------------------------
# Data Dictionary Notes
# -----------------------------------------------------------------------------

# Key columns for shot selection analysis:
# - fgaTeam: Total field goal attempts
# - fg3aTeam: 3-point field goal attempts
# - fg2a (derived): 2-point attempts = fgaTeam - fg3aTeam
# - fgmTeam: Field goals made
# - fg3mTeam: 3-pointers made
# - ptsTeam: Total points
# - outcomeGame: W/L
# - yearSeason: Season year (e.g., 2025 = 2024-25)

cat("\n", strrep("=", 60), "\n")
cat("Data collection complete.\n")
cat("Next step: Run 02_clean.R to process shot share calculations.\n")
