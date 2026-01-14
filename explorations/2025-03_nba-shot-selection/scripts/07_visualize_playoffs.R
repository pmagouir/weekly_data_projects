# =============================================================================
# 07_visualize_playoffs.R - Playoff Analysis Visualizations
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

results <- readRDS(file.path(processed_path, "playoff_analysis_results.rds"))

league_combined <- results$league_combined
rs_vs_playoff <- results$rs_vs_playoff
team_both <- results$team_both
champions <- results$champions
champ_vs_league <- results$champ_vs_league
playoff_team_era <- results$playoff_team_era

# Also load league data for comparison lines
playoff_league <- read_csv(file.path(processed_path, "playoff_league_season.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# Figure 7: Regular Season vs Playoffs Trend Comparison
# -----------------------------------------------------------------------------

p_rs_vs_playoffs <- league_combined |>
  ggplot(aes(x = season, y = share_3pt, color = season_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Regular Season" = colors_brand["primary"],
               "Playoffs" = colors_brand["secondary"])
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.15, 0.45)
  ) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  labs(
    title = "The Three-Point Revolution: Regular Season vs Playoffs",
    subtitle = "Both followed nearly identical trajectories over 20 years",
    x = "Season",
    y = "3-Point Attempts as % of All FGA",
    color = NULL,
    caption = "Source: NBA Stats via nbastatR"
  ) +
  theme_exploration() +
  theme(
    legend.position = c(0.15, 0.85),
    panel.grid.major.x = element_blank()
  )

save_plot_web(p_rs_vs_playoffs, file.path(output_path, "fig_07_rs_vs_playoffs.png"))
cat("Saved: fig_07_rs_vs_playoffs.png\n")

# -----------------------------------------------------------------------------
# Figure 8: Team Playoff Adjustment (RS to Playoffs)
# -----------------------------------------------------------------------------

p_adjustment <- team_both |>
  ggplot(aes(x = rs_share_3pt, y = playoff_share_3pt)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = season), alpha = 0.6, size = 2) +
  scale_color_gradientn(colors = colors_seq_green) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_fixed() +
  labs(
    title = "Do Teams Adjust Shot Selection for Playoffs?",
    subtitle = "Points above diagonal = increased 3pt rate in playoffs",
    x = "Regular Season 3pt Share",
    y = "Playoff 3pt Share",
    color = "Season",
    caption = "Each point is one team-season with playoff appearance"
  ) +
  theme_exploration()

save_plot_web(p_adjustment, file.path(output_path, "fig_08_playoff_adjustment.png"))
cat("Saved: fig_08_playoff_adjustment.png\n")

# -----------------------------------------------------------------------------
# Figure 9: Championship Teams vs League Average
# -----------------------------------------------------------------------------

p_champions <- champ_vs_league |>
  ggplot(aes(x = season)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_col(aes(y = champ_vs_league),
           fill = colors_brand["primary"], alpha = 0.8, width = 0.7) +
  geom_text(
    aes(y = champ_vs_league,
        label = champion,
        vjust = if_else(champ_vs_league > 0, -0.3, 1.3)),
    size = 2.5,
    angle = 45,
    hjust = 0.5
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%+.0f pp", x * 100),
    limits = c(-0.08, 0.12)
  ) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  labs(
    title = "Did Champions Lead or Follow the Three-Point Revolution?",
    subtitle = "Champion's playoff 3pt share relative to league average",
    x = "Season",
    y = "Difference from League Average",
    caption = "Champion = team with most playoff wins that season"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_champions, file.path(output_path, "fig_09_champions_vs_league.png"),
              width = 10, height = 6)
cat("Saved: fig_09_champions_vs_league.png\n")

# -----------------------------------------------------------------------------
# Figure 10: Playoff Success vs Shot Selection
# -----------------------------------------------------------------------------

p_playoff_success <- playoff_team_era |>
  ggplot(aes(x = relative_3pt, y = playoff_wins)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = season), alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = colors_brand["secondary"],
              fill = colors_brand["secondary"], alpha = 0.2) +
  geom_text_repel(
    data = playoff_team_era |> filter(playoff_wins >= 12),
    aes(label = paste(team, season)),
    size = 2.8,
    max.overlaps = 10
  ) +
  scale_color_gradientn(colors = colors_seq_green) +
  scale_x_continuous(labels = function(x) sprintf("%+.0f pp", x * 100)) +
  labs(
    title = "Does Shooting More Threes Win Playoff Games?",
    subtitle = "3pt share relative to league average vs playoff wins",
    x = "3pt Share vs League Average",
    y = "Playoff Wins",
    color = "Season",
    caption = "16 wins = championship; labeled teams won 12+ games"
  ) +
  theme_exploration()

save_plot_web(p_playoff_success, file.path(output_path, "fig_10_playoff_success.png"))
cat("Saved: fig_10_playoff_success.png\n")

# -----------------------------------------------------------------------------
# Figure 11: Champions' 3pt Share Over Time
# -----------------------------------------------------------------------------

p_champ_trend <- champions |>
  left_join(playoff_league |> select(season, league_share = share_3pt), by = "season") |>
  pivot_longer(
    cols = c(champ_3pt_share, league_share),
    names_to = "group",
    values_to = "share_3pt"
  ) |>
  mutate(group = if_else(group == "champ_3pt_share", "Champion", "League Average")) |>
  ggplot(aes(x = season, y = share_3pt, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Champion" = colors_brand["secondary"],
    "League Average" = colors_brand["primary"]
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  labs(
    title = "Champions Tracked the League's Three-Point Surge",
    subtitle = "Championship teams' playoff 3pt share vs league average",
    x = "Season",
    y = "Playoff 3pt Shot Share",
    color = NULL
  ) +
  theme_exploration() +
  theme(
    legend.position = c(0.15, 0.85),
    panel.grid.major.x = element_blank()
  )

save_plot_web(p_champ_trend, file.path(output_path, "fig_11_champ_trend.png"))
cat("Saved: fig_11_champ_trend.png\n")

# -----------------------------------------------------------------------------
# Figure 12: Era Comparison - RS vs Playoff Adjustment
# -----------------------------------------------------------------------------

adjustment_by_era <- team_both |>
  mutate(era = case_when(
    season <= 2010 ~ "2006-2010",
    season <= 2015 ~ "2011-2015",
    season <= 2020 ~ "2016-2020",
    TRUE ~ "2021-2025"
  )) |>
  group_by(era) |>
  summarize(
    mean_adjustment = mean(share_diff),
    se = sd(share_diff) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_era_adjustment <- adjustment_by_era |>
  ggplot(aes(x = era, y = mean_adjustment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_col(fill = colors_brand["primary"], width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_adjustment - 1.96 * se, ymax = mean_adjustment + 1.96 * se),
    width = 0.2
  ) +
  scale_y_continuous(labels = function(x) sprintf("%+.1f pp", x * 100)) +
  labs(
    title = "How Teams Adjusted 3pt Rate for Playoffs Over Time",
    subtitle = "Average change in 3pt share from regular season to playoffs",
    x = NULL,
    y = "Playoff Adjustment (vs Regular Season)",
    caption = "Error bars show 95% confidence interval"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_era_adjustment, file.path(output_path, "fig_12_era_adjustment.png"))
cat("Saved: fig_12_era_adjustment.png\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("Playoff visualizations saved to:", output_path, "\n")
cat("New figures:\n")
cat("  7.  Regular season vs playoffs trend\n")
cat("  8.  Team playoff adjustment scatter\n")
cat("  9.  Champions vs league average (bar)\n")
cat("  10. Playoff success vs shot selection\n")
cat("  11. Champion trend line vs league\n")
cat("  12. Era comparison of playoff adjustment\n")
