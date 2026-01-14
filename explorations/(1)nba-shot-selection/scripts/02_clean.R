# =============================================================================
# 02_clean.R - Calculate Shot Share Metrics
# Derives 2pt vs 3pt shot shares at game, team-season, and league levels
# =============================================================================

library(tidyverse)
library(here)
library(janitor)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Raw Data
# -----------------------------------------------------------------------------

raw_path <- here("explorations", "2025-03_nba-shot-selection", "data", "raw")
processed_path <- here("explorations", "2025-03_nba-shot-selection", "data", "processed")

nba_raw <- read_csv(
  file.path(raw_path, "nba_team_game_logs_2006_2025.csv"),
  show_col_types = FALSE
)

cat("Loaded", nrow(nba_raw), "team-game records\n")

# -----------------------------------------------------------------------------
# Standardize Column Names & Select Relevant Fields
# -----------------------------------------------------------------------------

# nbastatR returns camelCase; standardize to snake_case
nba_clean <- nba_raw |>
  clean_names() |>
  select(
    # Identifiers
    season = year_season,
    game_id = id_game,
    game_date = date_game,
    team = name_team,
    team_id = id_team,
    
    # Outcome
    outcome = outcome_game,
    # REMOVED: wins = wins,   <-- These columns don't exist yet!
    # REMOVED: losses = losses,
    
    # Shooting stats
    fga = fga_team,
    fgm = fgm_team,
    fg3a = fg3a_team,
    fg3m = fg3m_team,
    fta = fta_team,
    ftm = ftm_team,
    pts = pts_team,
    # Advanced (if available)
    matches("pace|ortg|drtg", ignore.case = TRUE)
  )

# -----------------------------------------------------------------------------
# Derive Shot Share Metrics
# -----------------------------------------------------------------------------

nba_clean <- nba_clean |>
  mutate(
    # 2-point attempts = total FGA - 3PA
    fg2a = fga - fg3a,
    fg2m = fgm - fg3m,

    # Shot share: proportion of shots that are 3s vs 2s
    share_3pt = fg3a / fga,
    share_2pt = fg2a / fga,

    # Efficiency
    fg_pct = fgm / fga,
    fg3_pct = if_else(fg3a > 0, fg3m / fg3a, NA_real_),
    fg2_pct = if_else(fg2a > 0, fg2m / fg2a, NA_real_),

    # True shooting attempts (accounts for FTs)
    tsa = fga + 0.44 * fta,
    ts_pct = pts / (2 * tsa),

    # Win indicator
    is_win = outcome == "W"
  )

cat("\nGame-level data processed\n")
cat("Sample shot shares:\n")
nba_clean |>
  select(season, team, share_3pt, share_2pt, fg3_pct, fg2_pct) |>
  head(10) |>
  print()

# -----------------------------------------------------------------------------
# Team-Season Aggregation
# -----------------------------------------------------------------------------

team_season <- nba_clean |>
  group_by(season, team, team_id) |>
  summarize(
    games = n(),
    wins = sum(is_win),
    losses = games - wins,
    win_pct = wins / games,

    # Total shots
    total_fga = sum(fga),
    total_fg3a = sum(fg3a),
    total_fg2a = sum(fg2a),

    # Season shot share
    share_3pt = sum(fg3a) / sum(fga),
    share_2pt = sum(fg2a) / sum(fga),

    # Efficiency
    fg_pct = sum(fgm) / sum(fga),
    fg3_pct = sum(fg3m) / sum(fg3a),
    fg2_pct = sum(fg2m) / sum(fg2a),
    ts_pct = sum(pts) / (2 * sum(tsa)),

    # Per-game averages
    avg_3pa = mean(fg3a),
    avg_2pa = mean(fg2a),
    avg_pts = mean(pts),

    .groups = "drop"
  )

cat("\nTeam-season aggregation complete:", nrow(team_season), "team-seasons\n")

# -----------------------------------------------------------------------------
# League-Season Aggregation
# -----------------------------------------------------------------------------

league_season <- nba_clean |>
  group_by(season) |>
  summarize(
    games = n() / 2,  # Each game counted twice (both teams)

    # League totals
    total_fga = sum(fga) / 2,
    total_fg3a = sum(fg3a) / 2,
    total_fg2a = sum(fg2a) / 2,

    # League shot share
    share_3pt = sum(fg3a) / sum(fga),
    share_2pt = sum(fg2a) / sum(fga),

    # League efficiency
    fg_pct = sum(fgm) / sum(fga),
    fg3_pct = sum(fg3m) / sum(fg3a),
    fg2_pct = sum(fg2m) / sum(fg2a),
    ts_pct = sum(pts) / (2 * sum(tsa)),

    # Per-game averages (league-wide)
    avg_3pa_per_team = mean(fg3a),
    avg_pts_per_team = mean(pts),

    .groups = "drop"
  )

cat("\nLeague-season aggregation complete:", nrow(league_season), "seasons\n")

# Preview league trend
cat("\nLeague 3pt shot share by season:\n")
league_season |>
  select(season, share_3pt, avg_3pa_per_team, fg3_pct) |>
  print(n = 25)

# -----------------------------------------------------------------------------
# Save Processed Data
# -----------------------------------------------------------------------------

write_csv(nba_clean, file.path(processed_path, "game_level.csv"))
write_csv(team_season, file.path(processed_path, "team_season.csv"))
write_csv(league_season, file.path(processed_path, "league_season.csv"))

cat("\nProcessed data saved to:", processed_path, "\n")
cat("  - game_level.csv (", nrow(nba_clean), " rows)\n")
cat("  - team_season.csv (", nrow(team_season), " rows)\n")
cat("  - league_season.csv (", nrow(league_season), " rows)\n")

cat("\n", strrep("=", 60), "\n")
cat("Data cleaning complete.\n")
cat("Next step: Run 03_analyze.R to identify trends and team drivers.\n")
