# =============================================================================
# 05_collect_playoffs.R - NBA Playoff Game Logs
# Extends analysis to postseason data for comparison
# =============================================================================

library(tidyverse)
library(here)
library(nbastatR)
library(future)

# Setup parallel processing
plan(multisession)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Fetch Playoff Game Logs
# -----------------------------------------------------------------------------

seasons <- 2006:2025

cat("Collecting NBA playoff game logs for", length(seasons), "seasons\n\n")

nba_playoff_logs <- game_logs(
  seasons = seasons,
  result_types = "team",
  season_types = "Playoffs"
)

cat("Fetched", nrow(nba_playoff_logs), "playoff team-game records\n")

# -----------------------------------------------------------------------------
# Inspect Coverage
# -----------------------------------------------------------------------------

cat("\nPlayoff games per season:\n")
nba_playoff_logs |>
  count(yearSeason) |>
  print(n = 25)

# Check team coverage (not all teams make playoffs)
cat("\nTeams with most playoff appearances:\n")
nba_playoff_logs |>
  count(nameTeam, sort = TRUE) |>
  head(10) |>
  print()

# -----------------------------------------------------------------------------
# Save Raw Playoff Data
# -----------------------------------------------------------------------------

raw_path <- here("explorations", "2025-03_nba-shot-selection", "data", "raw")

write_csv(
  nba_playoff_logs,
  file.path(raw_path, "nba_playoff_game_logs_2006_2025.csv")
)

cat("\nPlayoff data saved to:", file.path(raw_path, "nba_playoff_game_logs_2006_2025.csv"), "\n")

# -----------------------------------------------------------------------------
# Process Playoff Data (same as regular season)
# -----------------------------------------------------------------------------

library(janitor)

processed_path <- here("explorations", "2025-03_nba-shot-selection", "data", "processed")

playoff_clean <- nba_playoff_logs |>
  clean_names() |>
  select(
    season = year_season,
    game_id = id_game,
    game_date = date_game,
    team = name_team,
    team_id = id_team,
    outcome = outcome_game,
    fga = fga_team,
    fgm = fgm_team,
    fg3a = fg3a_team,
    fg3m = fg3m_team,
    fta = fta_team,
    ftm = ftm_team,
    pts = pts_team,
    matches("pace|ortg|drtg", ignore.case = TRUE)
  ) |>
  mutate(
    fg2a = fga - fg3a,
    fg2m = fgm - fg3m,
    share_3pt = fg3a / fga,
    share_2pt = fg2a / fga,
    fg_pct = fgm / fga,
    fg3_pct = if_else(fg3a > 0, fg3m / fg3a, NA_real_),
    fg2_pct = if_else(fg2a > 0, fg2m / fg2a, NA_real_),
    tsa = fga + 0.44 * fta,
    ts_pct = pts / (2 * tsa),
    is_win = outcome == "W",
    season_type = "Playoffs"
  )

# Team-season playoff aggregation
playoff_team_season <- playoff_clean |>
  group_by(season, team, team_id) |>
  summarize(
    playoff_games = n(),
    playoff_wins = sum(is_win),
    playoff_losses = playoff_games - playoff_wins,
    playoff_win_pct = playoff_wins / playoff_games,

    total_fga = sum(fga),
    total_fg3a = sum(fg3a),
    share_3pt = sum(fg3a) / sum(fga),
    share_2pt = sum(fg2a) / sum(fga),

    fg_pct = sum(fgm) / sum(fga),
    fg3_pct = sum(fg3m) / sum(fg3a),
    fg2_pct = sum(fg2m) / sum(fg2a),
    ts_pct = sum(pts) / (2 * sum(tsa)),

    avg_3pa = mean(fg3a),
    avg_pts = mean(pts),

    .groups = "drop"
  )

# League-season playoff aggregation
playoff_league_season <- playoff_clean |>
  group_by(season) |>
  summarize(
    games = n() / 2,
    share_3pt = sum(fg3a) / sum(fga),
    share_2pt = sum(fg2a) / sum(fga),
    fg3_pct = sum(fg3m) / sum(fg3a),
    fg2_pct = sum(fg2m) / sum(fg2a),
    ts_pct = sum(pts) / (2 * sum(tsa)),
    avg_3pa_per_team = mean(fg3a),
    .groups = "drop"
  ) |>
  mutate(season_type = "Playoffs")

# Save processed playoff data
write_csv(playoff_clean, file.path(processed_path, "playoff_game_level.csv"))
write_csv(playoff_team_season, file.path(processed_path, "playoff_team_season.csv"))
write_csv(playoff_league_season, file.path(processed_path, "playoff_league_season.csv"))

cat("\nProcessed playoff data saved:\n")
cat("  - playoff_game_level.csv (", nrow(playoff_clean), " rows)\n")
cat("  - playoff_team_season.csv (", nrow(playoff_team_season), " rows)\n")
cat("  - playoff_league_season.csv (", nrow(playoff_league_season), " rows)\n")

cat("\n", strrep("=", 60), "\n")
cat("Playoff data collection complete.\n")
cat("Next step: Run 06_analyze_playoffs.R\n")
