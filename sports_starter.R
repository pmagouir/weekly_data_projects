# =============================================================================
# Sports Data Starter Script
# Demonstrates: NFL (nflverse) and NBA (nbastatR) data acquisition
# =============================================================================

library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# NFL: Play-by-Play with EPA
# -----------------------------------------------------------------------------

# Install if needed: install.packages("nflverse")
library(nflverse)

# Load one season of play-by-play
# Returns tibble with ~50k rows per season, includes EPA, WP, drive info
nfl_pbp <- load_pbp(seasons = 2024)

# Quick look at structure
nfl_pbp |> 
  select(game_id, play_id, posteam, defteam, play_type, yards_gained, epa, wp) |>
  head(10)

# Team-level EPA summary
nfl_epa_summary <- nfl_pbp |>
  filter(
    !is.na(epa),
    play_type %in% c("pass", "run")
  ) |>
  group_by(posteam) |>
  summarize(
    plays = n(),
    epa_per_play = mean(epa),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    run_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    success_rate = mean(success),
    pass_rate = mean(play_type == "pass")
  ) |>
  arrange(desc(epa_per_play))

print(nfl_epa_summary)

# Time-bucketed analysis: EPA by quarter
nfl_quarter_epa <- nfl_pbp |>
  filter(!is.na(epa), play_type %in% c("pass", "run")) |>
  group_by(qtr) |>
  summarize(
    plays = n(),
    epa_per_play = mean(epa),
    pass_rate = mean(play_type == "pass")
  )

print(nfl_quarter_epa)

# -----------------------------------------------------------------------------
# NBA: Game Logs and Team Stats
# -----------------------------------------------------------------------------

# Install if needed: install.packages("nbastatR")
# Note: May need dev version for some functions
# devtools::install_github("abresler/nbastatR")

library(nbastatR)

# Enable parallel fetching (recommended for large pulls)
library(future)
plan(multisession)

# Suppress excessive messaging
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# Get team game logs for current season
# Returns one row per team per game with box score stats
nba_games <- game_logs(
seasons = 2025,
  result_types = "team",
  season_types = "Regular Season"
)

# Quick look
nba_games |>
  select(dateGame, nameTeam, ptsTeam, astTeam, rebTeam, paceTeam) |>
  head(10)

# Team-level pace and efficiency summary
nba_pace_summary <- nba_games |>
  group_by(nameTeam) |>
  summarize(
    games = n(),
    avg_pace = mean(paceTeam, na.rm = TRUE),
    avg_pts = mean(ptsTeam, na.rm = TRUE),
    avg_ortg = mean(ortgTeam, na.rm = TRUE),
    avg_drtg = mean(drtgTeam, na.rm = TRUE),
    net_rtg = mean(ortgTeam - drtgTeam, na.rm = TRUE)
  ) |>
  arrange(desc(avg_pace))

print(nba_pace_summary)

# Time-bucketed analysis: Scoring by month
nba_monthly <- nba_games |>
  mutate(month = floor_date(dateGame, "month")) |>
  group_by(month) |>
  summarize(
    games = n(),
    avg_pace = mean(paceTeam, na.rm = TRUE),
    avg_total_pts = mean(ptsTeam + opp_ptsTeam, na.rm = TRUE)
  )

print(nba_monthly)

# -----------------------------------------------------------------------------
# Combined Summary Table
# -----------------------------------------------------------------------------

# Create comparable efficiency metrics across sports
combined_summary <- bind_rows(
  nfl_epa_summary |>
    slice_head(n = 5) |>
    transmute(
      sport = "NFL",
      team = posteam,
      metric = "EPA/Play",
      value = round(epa_per_play, 3)
    ),
  nba_pace_summary |>
    slice_head(n = 5) |>
    transmute(
      sport = "NBA",
      team = nameTeam,
      metric = "Pace",
      value = round(avg_pace, 1)
    )
)

print(combined_summary)

# -----------------------------------------------------------------------------
# Save for exploration
# -----------------------------------------------------------------------------

# Uncomment to save processed data
# write_csv(nfl_epa_summary, here("data", "nfl_epa_2024.csv"))
# write_csv(nba_pace_summary, here("data", "nba_pace_2025.csv"))

cat("\n✓ Data pulls complete. Ready for exploration.\n")
