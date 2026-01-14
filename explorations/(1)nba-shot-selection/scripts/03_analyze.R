# =============================================================================
# 03_analyze.R - Shot Selection Trend Analysis
# Questions:
#   1. How has league-wide shot selection evolved?
#   2. Which teams drove the 3pt revolution?
#   3. How does shot selection change relate to winning?
# =============================================================================

library(tidyverse)
library(here)
library(broom)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Processed Data
# -----------------------------------------------------------------------------

processed_path <- here("explorations", "2025-03_nba-shot-selection", "data", "processed")

team_season <- read_csv(file.path(processed_path, "team_season.csv"), show_col_types = FALSE)
league_season <- read_csv(file.path(processed_path, "league_season.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. League-Wide Trend Analysis
# -----------------------------------------------------------------------------

cat("=" |> strrep(60), "\n")
cat("1. LEAGUE-WIDE 3PT SHOT SHARE TREND\n")
cat("=" |> strrep(60), "\n\n")

# Fit linear trend
league_trend <- lm(share_3pt ~ season, data = league_season)

cat("Linear trend in 3pt shot share:\n")
tidy(league_trend) |> print()

cat("\nKey stats:\n")
cat("  - 2006 3pt share:", sprintf("%.1f%%", league_season$share_3pt[league_season$season == 2006] * 100), "\n")
cat("  - 2025 3pt share:", sprintf("%.1f%%", league_season$share_3pt[league_season$season == 2025] * 100), "\n")
cat("  - Annual increase:", sprintf("%.2f pp", coef(league_trend)[2] * 100), "\n")
cat("  - Total change:", sprintf("%.1f pp",
  (league_season$share_3pt[league_season$season == 2025] -
   league_season$share_3pt[league_season$season == 2006]) * 100), "\n")

# Calculate acceleration (is the trend accelerating?)
league_season <- league_season |>
  mutate(
    share_3pt_change = share_3pt - lag(share_3pt),
    era = case_when(
      season <= 2010 ~ "2006-2010",
      season <= 2015 ~ "2011-2015",
      season <= 2020 ~ "2016-2020",
      TRUE ~ "2021-2025"
    )
  )

era_summary <- league_season |>
  group_by(era) |>
  summarize(
    avg_share_3pt = mean(share_3pt),
    avg_annual_change = mean(share_3pt_change, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nEvolution by era:\n")
print(era_summary)

# -----------------------------------------------------------------------------
# 2. Team-Level Change Analysis
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("2. TEAMS DRIVING THE 3PT REVOLUTION\n")
cat("=" |> strrep(60), "\n\n")

# Calculate each team's change from first to last season
team_change <- team_season |>
  group_by(team) |>
  filter(season == min(season) | season == max(season)) |>
  arrange(team, season) |>
  summarize(
    first_season = first(season),
    last_season = last(season),
    first_3pt_share = first(share_3pt),
    last_3pt_share = last(share_3pt),
    change_3pt_share = last(share_3pt) - first(share_3pt),
    first_win_pct = first(win_pct),
    last_win_pct = last(win_pct),
    change_win_pct = last(win_pct) - first(win_pct),
    .groups = "drop"
  )

cat("Top 10 teams by 3pt share increase:\n")
team_change |>
  arrange(desc(change_3pt_share)) |>
  select(team, first_3pt_share, last_3pt_share, change_3pt_share) |>
  mutate(across(contains("share"), ~sprintf("%.1f%%", . * 100))) |>
  head(10) |>
  print()

cat("\nBottom 10 teams (smallest increase):\n")
team_change |>
  arrange(change_3pt_share) |>
  select(team, first_3pt_share, last_3pt_share, change_3pt_share) |>
  mutate(across(contains("share"), ~sprintf("%.1f%%", . * 100))) |>
  head(10) |>
  print()

# Identify early adopters vs late adopters
early_adopters <- team_season |>
  filter(season <= 2015) |>
  group_by(team) |>
  summarize(
    early_avg_3pt_share = mean(share_3pt),
    early_avg_win_pct = mean(win_pct),
    .groups = "drop"
  ) |>
  arrange(desc(early_avg_3pt_share))

cat("\nEarly adopters (highest 3pt share 2006-2015):\n")
early_adopters |>
  mutate(early_avg_3pt_share = sprintf("%.1f%%", early_avg_3pt_share * 100)) |>
  head(10) |>
  print()

# -----------------------------------------------------------------------------
# 3. Shot Selection vs Winning Analysis
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("3. SHOT SELECTION AND WINNING\n")
cat("=" |> strrep(60), "\n\n")

# Relative shot share: team's 3pt share vs league average that season
team_season <- team_season |>
  left_join(
    league_season |> select(season, league_3pt_share = share_3pt),
    by = "season"
  ) |>
  mutate(
    relative_3pt_share = share_3pt - league_3pt_share,
    above_league_avg = relative_3pt_share > 0
  )

# Correlation between shot selection and winning (controlling for era)
cat("Correlation: 3pt share vs win pct (all team-seasons):\n")
cor_all <- cor(team_season$share_3pt, team_season$win_pct)
cat("  Raw correlation:", sprintf("%.3f", cor_all), "\n")

cor_relative <- cor(team_season$relative_3pt_share, team_season$win_pct)
cat("  Relative to league avg:", sprintf("%.3f", cor_relative), "\n")

# Regression: win pct ~ relative 3pt share + efficiency
mod_wins <- lm(
  win_pct ~ relative_3pt_share + fg3_pct + fg2_pct + ts_pct,
  data = team_season
)

cat("\nRegression: Win% ~ Shot Selection + Efficiency:\n")
tidy(mod_wins) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  print()

# Within-team analysis: did teams that increased 3pt share also improve wins?
team_trends <- team_season |>
  group_by(team) |>
  filter(n() >= 10) |>  # Teams with at least 10 seasons
  summarize(
    n_seasons = n(),
    share_3pt_trend = coef(lm(share_3pt ~ season))[2],
    win_pct_trend = coef(lm(win_pct ~ season))[2],
    avg_win_pct = mean(win_pct),
    .groups = "drop"
  )

cat("\nWithin-team correlation (3pt trend vs win trend):\n")
cor_within <- cor(team_trends$share_3pt_trend, team_trends$win_pct_trend)
cat("  Correlation:", sprintf("%.3f", cor_within), "\n")

# Top improvers in both dimensions
cat("\nTeams that increased both 3pt share AND wins:\n")
team_trends |>
  filter(share_3pt_trend > median(share_3pt_trend),
         win_pct_trend > 0) |>
  arrange(desc(win_pct_trend)) |>
  mutate(
    share_3pt_trend = sprintf("%.2f pp/yr", share_3pt_trend * 100),
    win_pct_trend = sprintf("%.2f pp/yr", win_pct_trend * 100)
  ) |>
  head(10) |>
  print()

# -----------------------------------------------------------------------------
# 4. Efficiency Matters: 3pt Volume vs 3pt Accuracy
# -----------------------------------------------------------------------------

cat("\n", "=" |> strrep(60), "\n")
cat("4. VOLUME VS EFFICIENCY TRADEOFF\n")
cat("=" |> strrep(60), "\n\n")

# Has league 3pt% declined as volume increased?
efficiency_trend <- lm(fg3_pct ~ share_3pt + season, data = league_season)

cat("Has 3pt accuracy declined with increased volume?\n")
tidy(efficiency_trend) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  print()

# Team-level: do high-volume 3pt teams shoot worse?
cat("\nTeam-level correlation (3pt share vs 3pt %):\n")
cor_vol_eff <- cor(team_season$share_3pt, team_season$fg3_pct, use = "complete.obs")
cat("  Correlation:", sprintf("%.3f", cor_vol_eff), "\n")

# -----------------------------------------------------------------------------
# 5. Save Analysis Results
# -----------------------------------------------------------------------------

results <- list(
  league_season = league_season,
  team_season = team_season,
  team_change = team_change,
  team_trends = team_trends,
  early_adopters = early_adopters
)

saveRDS(results, file.path(processed_path, "analysis_results.rds"))

cat("\n", "=" |> strrep(60), "\n")
cat("Analysis complete. Results saved to analysis_results.rds\n")
cat("Next step: Run 04_visualize.R to create charts.\n")
