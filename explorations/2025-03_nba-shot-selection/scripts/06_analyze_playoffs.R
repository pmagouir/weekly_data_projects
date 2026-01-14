# =============================================================================
# 06_analyze_playoffs.R - Playoff vs Regular Season Analysis
# Questions:
#   1. Did the 3pt revolution happen in playoffs too?
#   2. Do teams adjust shot selection for playoffs?
#   3. Did championship teams lead or follow the trend?
# =============================================================================

library(tidyverse)
library(here)
library(broom)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

processed_path <- here("explorations", "2025-03_nba-shot-selection", "data", "processed")

# Regular season
rs_league <- read_csv(file.path(processed_path, "league_season.csv"), show_col_types = FALSE)
rs_team <- read_csv(file.path(processed_path, "team_season.csv"), show_col_types = FALSE)

# Playoffs
playoff_league <- read_csv(file.path(processed_path, "playoff_league_season.csv"), show_col_types = FALSE)
playoff_team <- read_csv(file.path(processed_path, "playoff_team_season.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. Compare League-Wide Trends: RS vs Playoffs
# -----------------------------------------------------------------------------

cat("=" |> strrep(60), "\n")
cat("1. LEAGUE-WIDE TRENDS: REGULAR SEASON VS PLAYOFFS\n")
cat("=" |> strrep(60), "\n\n")

# Combine for comparison
league_combined <- bind_rows(
  rs_league |> mutate(season_type = "Regular Season"),
  playoff_league |> select(-season_type) |> mutate(season_type = "Playoffs")
)

# Summary by season type
cat("Average 3pt share by era and season type:\n")
league_combined |>
  mutate(era = case_when(
    season <= 2010 ~ "2006-2010",
    season <= 2015 ~ "2011-2015",
    season <= 2020 ~ "2016-2020",
    TRUE ~ "2021-2025"
  )) |>
  group_by(era, season_type) |>
  summarize(avg_3pt_share = mean(share_3pt), .groups = "drop") |>
  pivot_wider(names_from = season_type, values_from = avg_3pt_share) |>
  mutate(
    `Regular Season` = sprintf("%.1f%%", `Regular Season` * 100),
    Playoffs = sprintf("%.1f%%", Playoffs * 100)
  ) |>
  print()

# Test if playoff 3pt rate differs from regular season
cat("\nSeason-by-season difference (Playoffs - Regular Season):\n")
rs_vs_playoff <- rs_league |>
  select(season, rs_share = share_3pt, rs_fg3_pct = fg3_pct) |>
  inner_join(
    playoff_league |> select(season, playoff_share = share_3pt, playoff_fg3_pct = fg3_pct),
    by = "season"
  ) |>
  mutate(
    share_diff = playoff_share - rs_share,
    pct_diff = playoff_fg3_pct - rs_fg3_pct
  )

cat("  Mean difference in 3pt share:", sprintf("%.2f pp", mean(rs_vs_playoff$share_diff) * 100), "\n")
cat("  Mean difference in 3pt %:", sprintf("%.2f pp", mean(rs_vs_playoff$pct_diff, na.rm = TRUE) * 100), "\n")

t_test_share <- t.test(rs_vs_playoff$share_diff)
cat("  t-test p-value (share diff from 0):", sprintf("%.4f", t_test_share$p.value), "\n")

# Trend comparison
rs_trend <- lm(share_3pt ~ season, data = rs_league)
playoff_trend <- lm(share_3pt ~ season, data = playoff_league)

cat("\nTrend comparison (annual increase in 3pt share):\n")
cat("  Regular season:", sprintf("%.3f pp/year", coef(rs_trend)[2] * 100), "\n")
cat("  Playoffs:", sprintf("%.3f pp/year", coef(playoff_trend)[2] * 100), "\n")

# -----------------------------------------------------------------------------
# 2. Team-Level: Do Teams Adjust for Playoffs?
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("2. DO TEAMS ADJUST SHOT SELECTION FOR PLAYOFFS?\n")
cat("=" |> strrep(60), "\n\n")

# Match team-seasons with playoff appearances
team_both <- rs_team |>
  select(season, team, rs_share_3pt = share_3pt, rs_win_pct = win_pct,
         rs_fg3_pct = fg3_pct, rs_games = games) |>
  inner_join(
    playoff_team |>
      select(season, team, playoff_share_3pt = share_3pt, playoff_win_pct = playoff_win_pct,
             playoff_fg3_pct = fg3_pct, playoff_games, playoff_wins),
    by = c("season", "team")
  ) |>
  mutate(
    share_diff = playoff_share_3pt - rs_share_3pt,
    pct_diff = playoff_fg3_pct - rs_fg3_pct
  )

cat("Team-seasons with both RS and playoff data:", nrow(team_both), "\n\n")

# Do teams increase or decrease 3pt rate in playoffs?
cat("Shot selection adjustment in playoffs:\n")
cat("  Mean change:", sprintf("%.2f pp", mean(team_both$share_diff) * 100), "\n")
cat("  Median change:", sprintf("%.2f pp", median(team_both$share_diff) * 100), "\n")
cat("  Teams that increased 3pt share:", sprintf("%.0f%%", mean(team_both$share_diff > 0) * 100), "\n")

# Has this changed over time?
cat("\nPlayoff adjustment by era:\n")
team_both |>
  mutate(era = case_when(
    season <= 2010 ~ "2006-2010",
    season <= 2015 ~ "2011-2015",
    season <= 2020 ~ "2016-2020",
    TRUE ~ "2021-2025"
  )) |>
  group_by(era) |>
  summarize(
    n = n(),
    mean_adjustment = mean(share_diff),
    pct_increased = mean(share_diff > 0)
  ) |>
  mutate(
    mean_adjustment = sprintf("%.2f pp", mean_adjustment * 100),
    pct_increased = sprintf("%.0f%%", pct_increased * 100)
  ) |>
  print()

# -----------------------------------------------------------------------------
# 3. Championship Team Analysis
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("3. CHAMPIONSHIP TEAM SHOT SELECTION\n")
cat("=" |> strrep(60), "\n\n")

# Identify championship teams (most playoff wins each season = likely champion)
# Note: This is approximate; true champions would need separate data
champions <- playoff_team |>
  group_by(season) |>
  slice_max(playoff_wins, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(season, champion = team, champ_playoff_wins = playoff_wins,
         champ_3pt_share = share_3pt, champ_fg3_pct = fg3_pct)

cat("Likely champions (team with most playoff wins each season):\n")
champions |>
  mutate(champ_3pt_share = sprintf("%.1f%%", champ_3pt_share * 100)) |>
  print(n = 25)

# Compare champions to league average
champ_vs_league <- champions |>
  left_join(playoff_league |> select(season, league_3pt_share = share_3pt), by = "season") |>
  mutate(
    champ_vs_league = champ_3pt_share - league_3pt_share
  )

cat("\nChampions vs league average 3pt share:\n")
cat("  Mean difference:", sprintf("%.2f pp", mean(champ_vs_league$champ_vs_league) * 100), "\n")
cat("  Champions above avg:", sprintf("%.0f%%", mean(champ_vs_league$champ_vs_league > 0) * 100), "\n")

# Champions' trend
cat("\nChampion 3pt share trend:\n")
champ_trend <- lm(champ_3pt_share ~ season, data = champions)
cat("  Annual increase:", sprintf("%.3f pp/year", coef(champ_trend)[2] * 100), "\n")

# Were champions early adopters or followers?
cat("\nChampions' position relative to league:\n")
champ_vs_league |>
  mutate(era = case_when(
    season <= 2010 ~ "2006-2010",
    season <= 2015 ~ "2011-2015",
    season <= 2020 ~ "2016-2020",
    TRUE ~ "2021-2025"
  )) |>
  group_by(era) |>
  summarize(
    avg_diff_from_league = mean(champ_vs_league),
    above_avg_count = sum(champ_vs_league > 0),
    n = n()
  ) |>
  mutate(avg_diff_from_league = sprintf("%.2f pp", avg_diff_from_league * 100)) |>
  print()

# -----------------------------------------------------------------------------
# 4. Playoff Success and Shot Selection
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("4. PLAYOFF SUCCESS AND SHOT SELECTION\n")
cat("=" |> strrep(60), "\n\n")

# Does higher 3pt share correlate with playoff success?
# Using playoff wins as measure of success

cat("Correlation: Playoff 3pt share vs playoff wins:\n")
cor_playoff <- cor(playoff_team$share_3pt, playoff_team$playoff_wins)
cat("  Raw correlation:", sprintf("%.3f", cor_playoff), "\n")

# Control for era (since both increased over time)
playoff_team_era <- playoff_team |>
  left_join(playoff_league |> select(season, league_3pt_share = share_3pt), by = "season") |>
  mutate(relative_3pt = share_3pt - league_3pt_share)

cor_relative <- cor(playoff_team_era$relative_3pt, playoff_team_era$playoff_wins)
cat("  Relative to league avg:", sprintf("%.3f", cor_relative), "\n")

# Model playoff wins
mod_playoff_wins <- lm(
  playoff_wins ~ relative_3pt + fg3_pct + ts_pct,
  data = playoff_team_era
)

cat("\nRegression: Playoff wins ~ Shot selection + Efficiency:\n")
tidy(mod_playoff_wins) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  print()

# Deep playoff runs (Conference Finals+)
deep_runs <- playoff_team |>
  filter(playoff_wins >= 8) |>  # At least 2 series wins
  left_join(playoff_league |> select(season, league_share = share_3pt), by = "season") |>
  mutate(vs_league = share_3pt - league_share)

cat("\nDeep playoff runs (8+ wins) vs league average:\n")
cat("  N teams:", nrow(deep_runs), "\n")
cat("  Mean 3pt share vs league:", sprintf("%.2f pp", mean(deep_runs$vs_league) * 100), "\n")
cat("  Above league avg:", sprintf("%.0f%%", mean(deep_runs$vs_league > 0) * 100), "\n")

# -----------------------------------------------------------------------------
# 5. Save Results
# -----------------------------------------------------------------------------

playoff_results <- list(
  league_combined = league_combined,
  rs_vs_playoff = rs_vs_playoff,
  team_both = team_both,
  champions = champions,
  champ_vs_league = champ_vs_league,
  playoff_team_era = playoff_team_era,
  deep_runs = deep_runs
)

saveRDS(playoff_results, file.path(processed_path, "playoff_analysis_results.rds"))

cat("\n", "=" |> strrep(60), "\n")
cat("Playoff analysis complete. Results saved.\n")
cat("Next step: Run 07_visualize_playoffs.R\n")
