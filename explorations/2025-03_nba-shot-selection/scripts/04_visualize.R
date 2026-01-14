# =============================================================================
# 04_visualize.R - NBA Shot Selection Visualizations
# Creates publication-ready charts for the exploration
# =============================================================================

library(tidyverse)
library(here)
library(scales)
library(ggrepel)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

processed_path <- here("explorations", "2025-03_nba-shot-selection", "data", "processed")
output_path <- here("explorations", "2025-03_nba-shot-selection", "output", "figures")

results <- readRDS(file.path(processed_path, "analysis_results.rds"))

league_season <- results$league_season
team_season <- results$team_season
team_change <- results$team_change
team_trends <- results$team_trends

# -----------------------------------------------------------------------------
# Figure 1: League-Wide 3pt Shot Share Trend
# -----------------------------------------------------------------------------

p_league_trend <- league_season |>
  ggplot(aes(x = season, y = share_3pt)) +
  geom_line(linewidth = 1.2, color = colors_brand["primary"]) +
  geom_point(size = 2.5, color = colors_brand["primary"]) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              color = colors_brand["secondary"], linewidth = 0.8) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.15, 0.45),
    breaks = seq(0.15, 0.45, 0.05)
  ) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  labs(
    title = "The NBA's Three-Point Revolution",
    subtitle = "League-wide 3pt shot share has more than doubled in 20 years",
    x = "Season",
    y = "3-Point Attempts as % of All FGA",
    caption = "Source: NBA Stats via nbastatR | Regular season games"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_league_trend, file.path(output_path, "fig_01_league_trend.png"))

cat("Saved: fig_01_league_trend.png\n")

# -----------------------------------------------------------------------------
# Figure 2: Team Dispersion Over Time
# -----------------------------------------------------------------------------

# Show how team-level variation has evolved
team_spread <- team_season |>
  group_by(season) |>
  summarize(
    min_share = min(share_3pt),
    q25_share = quantile(share_3pt, 0.25),
    median_share = median(share_3pt),
    q75_share = quantile(share_3pt, 0.75),
    max_share = max(share_3pt),
    .groups = "drop"
  )

p_team_spread <- team_spread |>
  ggplot(aes(x = season)) +
  geom_ribbon(aes(ymin = min_share, ymax = max_share),
              fill = colors_brand["primary"], alpha = 0.15) +
  geom_ribbon(aes(ymin = q25_share, ymax = q75_share),
              fill = colors_brand["primary"], alpha = 0.3) +
  geom_line(aes(y = median_share), linewidth = 1.2, color = colors_brand["primary"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  labs(
    title = "The League Converged on the Three",
    subtitle = "Spread between most and least 3pt-heavy teams narrowed as everyone adapted",
    x = "Season",
    y = "3-Point Shot Share",
    caption = "Shaded regions show full range (light) and interquartile range (dark)"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_team_spread, file.path(output_path, "fig_02_team_spread.png"))

cat("Saved: fig_02_team_spread.png\n")

# -----------------------------------------------------------------------------
# Figure 3: Teams with Biggest Changes
# -----------------------------------------------------------------------------

# Top and bottom movers
top_changers <- team_change |>
  arrange(desc(change_3pt_share)) |>
  slice(1:5) |>
  mutate(group = "Biggest Increase")

bottom_changers <- team_change |>
  arrange(change_3pt_share) |>
  slice(1:5) |>
  mutate(group = "Smallest Increase")

changers <- bind_rows(top_changers, bottom_changers) |>
  mutate(team = fct_reorder(team, change_3pt_share))

p_changers <- changers |>
  ggplot(aes(x = change_3pt_share, y = team, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = sprintf("+%.0f pp", change_3pt_share * 100)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.35),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(values = c(
    "Biggest Increase" = colors_brand["primary"],
    "Smallest Increase" = colors_brand["secondary"]
  )) +
  labs(
    title = "Who Embraced the Three Most?",
    subtitle = "Change in 3pt shot share from first to most recent season",
    x = "Change in 3pt Shot Share (percentage points)",
    y = NULL,
    fill = NULL
  ) +
  theme_exploration() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

save_plot_web(p_changers, file.path(output_path, "fig_03_biggest_changers.png"))

cat("Saved: fig_03_biggest_changers.png\n")

# -----------------------------------------------------------------------------
# Figure 4: Shot Selection Change vs Win Change
# -----------------------------------------------------------------------------

p_shot_wins <- team_change |>
  ggplot(aes(x = change_3pt_share, y = change_win_pct)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = mean(team_change$change_3pt_share),
             color = "gray70", linetype = "dashed") +
  geom_point(size = 3, alpha = 0.7, color = colors_brand["primary"]) +
  geom_smooth(method = "lm", se = TRUE, color = colors_brand["secondary"],
              fill = colors_brand["secondary"], alpha = 0.2) +
  geom_text_repel(
    data = team_change |>
      filter(abs(change_win_pct) > 0.15 | change_3pt_share > 0.22),
    aes(label = team),
    size = 3,
    max.overlaps = 15
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Did Going Three-Heavy Pay Off?",
    subtitle = "Comparing each team's shot selection shift to their win percentage change",
    x = "Change in 3pt Shot Share",
    y = "Change in Win Percentage",
    caption = "Each point is one team; comparing first available season to most recent"
  ) +
  theme_exploration()

save_plot_web(p_shot_wins, file.path(output_path, "fig_04_shot_vs_wins.png"))

cat("Saved: fig_04_shot_vs_wins.png\n")

# -----------------------------------------------------------------------------
# Figure 5: Season-by-Season for Select Teams
# -----------------------------------------------------------------------------

# Pick a few interesting teams to highlight
highlight_teams <- c(
  "Houston Rockets",      # Early adopter, Morey-ball
  "Golden State Warriors", # Championship dynasty
  "San Antonio Spurs",     # Traditional team that adapted
  "Miami Heat"             # Modern 3pt adopter
)

team_trajectories <- team_season |>
  filter(team %in% highlight_teams)

p_trajectories <- team_trajectories |>
  ggplot(aes(x = season, y = share_3pt, color = team)) +
  geom_line(
    data = team_season |> filter(!team %in% highlight_teams),
    aes(group = team),
    color = "gray85",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors_extended[1:4]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2006, 2025, 4)) +
  labs(
    title = "Trailblazers and Followers",
    subtitle = "How key teams led or adapted to the 3pt revolution",
    x = "Season",
    y = "3-Point Shot Share",
    color = NULL
  ) +
  theme_exploration() +
  theme(legend.position = "bottom")

save_plot_web(p_trajectories, file.path(output_path, "fig_05_team_trajectories.png"),
              width = 9, height = 6)

cat("Saved: fig_05_team_trajectories.png\n")

# -----------------------------------------------------------------------------
# Figure 6: Volume vs Efficiency
# -----------------------------------------------------------------------------

p_volume_efficiency <- team_season |>
  ggplot(aes(x = share_3pt, y = fg3_pct, color = season)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = colors_brand["secondary"],
              linetype = "dashed") +
  scale_color_gradientn(colors = colors_seq_green) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "More Threes, Same Accuracy",
    subtitle = "Teams shooting more 3s haven't seen efficiency decline",
    x = "3-Point Shot Share (Volume)",
    y = "3-Point Percentage (Efficiency)",
    color = "Season"
  ) +
  theme_exploration() +
  theme(legend.position = "right")

save_plot_web(p_volume_efficiency, file.path(output_path, "fig_06_volume_vs_efficiency.png"))

cat("Saved: fig_06_volume_vs_efficiency.png\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("All visualizations saved to:", output_path, "\n")
cat("Figures created:\n")
cat("  1. League-wide 3pt trend\n")
cat("  2. Team spread over time\n")
cat("  3. Biggest changers (bar chart)\n")
cat("  4. Shot selection vs win change\n")
cat("  5. Team trajectories\n")
cat("  6. Volume vs efficiency\n")
