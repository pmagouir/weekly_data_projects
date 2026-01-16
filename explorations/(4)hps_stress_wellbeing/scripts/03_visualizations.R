# ==============================================================================
# 03_visualizations.R
# Visualizations of HPS hardship and wellbeing by age, education, household
# ==============================================================================

library(tidyverse)
library(arrow)
library(scales)
library(here)

source(here("foundational_data", "R", "00_config.R"))

# Output directory
output_dir <- here("explorations", "(4)hps_stress_wellbeing", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Theme and Colors (per CLAUDE.md brand guidelines)
# ==============================================================================

theme_exploration <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F3D2B"),
      plot.subtitle = element_text(size = 10, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E5E5"),
      axis.title = element_text(size = 9, color = "#333333"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
}

colors_brand <- c(
  primary = "#1F3D2B",
  secondary = "#7A1E2C",
  accent1 = "#4A7C59",
  accent2 = "#A85D68",
  neutral = "#666666"
)

# Diverging palette for heatmaps (low = green, high = burgundy)
heatmap_colors <- c("#1F3D2B", "#4A7C59", "#F5F5F4", "#A85D68", "#7A1E2C")

# ==============================================================================
# Load and Prepare Data
# ==============================================================================

hps <- read_mart("hps")

hps_clean <- hps |>
  filter(!is.na(age_group), !is.na(educ_group), !is.na(hh_type)) |>
  mutate(
    educ = if_else(educ_group %in% c("BA", "GRAD"), "BA+", "No BA"),
    educ = factor(educ, levels = c("No BA", "BA+")),

    hh = case_when(
      hh_type == "SINGLE_CHILDREN" ~ "Single Parent",
      hh_type == "MARRIED_CHILDREN" ~ "Married Parent",
      hh_type == "SINGLE_NO_CHILDREN" ~ "Single, No Kids",
      hh_type == "MARRIED_NO_CHILDREN" ~ "Married, No Kids"
    ),
    hh = factor(hh, levels = c("Single Parent", "Married Parent",
                               "Single, No Kids", "Married, No Kids")),

    # Shorter household labels for plots
    hh_short = case_when(
      hh == "Single Parent" ~ "Single\nParent",
      hh == "Married Parent" ~ "Married\nParent",
      hh == "Single, No Kids" ~ "Single\nNo Kids",
      hh == "Married, No Kids" ~ "Married\nNo Kids"
    ),
    hh_short = factor(hh_short, levels = c("Single\nParent", "Married\nParent",
                                           "Single\nNo Kids", "Married\nNo Kids"))
  )

# Aggregate for three-way
by_all <- hps_clean |>
  group_by(age_group, educ, hh, hh_short) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 25)  # Filter sparse cells

# ==============================================================================
# FIGURE 1: Heatmap - Expense Difficulty by Age × Education × Household
# ==============================================================================

p1 <- by_all |>
  ggplot(aes(x = age_group, y = hh_short, fill = expense)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(expense * 100), "%")),
            size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~educ, ncol = 2) +
  scale_fill_gradientn(
    colors = c("#4A7C59", "#F5F5F4", "#A85D68", "#7A1E2C"),
    values = rescale(c(0, 0.4, 0.6, 1)),
    limits = c(0, 1),
    labels = percent,
    name = "Rate"
  ) +
  labs(
    title = "Expense Difficulty by Age, Education, and Household Type",
    subtitle = "Percent reporting difficulty paying usual household expenses",
    x = "Age Group",
    y = NULL,
    caption = "Source: Census Household Pulse Survey, December 2024 | Cells with n < 25 excluded"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(file.path(output_dir, "fig_01_expense_heatmap.png"), p1,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_01_expense_heatmap.png\n")

# ==============================================================================
# FIGURE 2: Heatmap - Food Insufficiency
# ==============================================================================

p2 <- by_all |>
  ggplot(aes(x = age_group, y = hh_short, fill = food)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(food * 100), "%")),
            size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~educ, ncol = 2) +
  scale_fill_gradientn(
    colors = c("#4A7C59", "#F5F5F4", "#A85D68", "#7A1E2C"),
    values = rescale(c(0, 0.3, 0.5, 1)),
    limits = c(0, 0.7),
    labels = percent,
    name = "Rate"
  ) +
  labs(
    title = "Food Insufficiency by Age, Education, and Household Type",
    subtitle = "Percent reporting sometimes or often not enough to eat",
    x = "Age Group",
    y = NULL,
    caption = "Source: Census Household Pulse Survey, December 2024 | Cells with n < 25 excluded"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(file.path(output_dir, "fig_02_food_heatmap.png"), p2,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_02_food_heatmap.png\n")

# ==============================================================================
# FIGURE 3: Heatmap - Psychological Distress
# ==============================================================================

p3 <- by_all |>
  ggplot(aes(x = age_group, y = hh_short, fill = distress)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(distress * 100), "%")),
            size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~educ, ncol = 2) +
  scale_fill_gradientn(
    colors = c("#4A7C59", "#F5F5F4", "#A85D68", "#7A1E2C"),
    values = rescale(c(0, 0.15, 0.25, 1)),
    limits = c(0, 0.4),
    labels = percent,
    name = "Rate"
  ) +
  labs(
    title = "Psychological Distress by Age, Education, and Household Type",
    subtitle = "Percent screening positive on PHQ-2 (depression) or GAD-2 (anxiety)",
    x = "Age Group",
    y = NULL,
    caption = "Source: Census Household Pulse Survey, December 2024 | Cells with n < 25 excluded"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(file.path(output_dir, "fig_03_distress_heatmap.png"), p3,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_03_distress_heatmap.png\n")

# ==============================================================================
# FIGURE 4: Line Plot - Age Trajectories by Household Type (Expense)
# ==============================================================================

by_age_hh <- hps_clean |>
  group_by(age_group, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 15)

p4 <- by_age_hh |>
  ggplot(aes(x = age_group, y = expense, color = hh, group = hh)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Single Parent" = "#7A1E2C",
    "Married Parent" = "#A85D68",
    "Single, No Kids" = "#4A7C59",
    "Married, No Kids" = "#1F3D2B"
  )) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Expense Difficulty by Age and Household Type",
    subtitle = "Single parents face persistently higher hardship across all ages",
    x = "Age Group",
    y = "Percent with Expense Difficulty",
    color = "Household Type",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(file.path(output_dir, "fig_04_expense_by_age_hh.png"), p4,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_04_expense_by_age_hh.png\n")

# ==============================================================================
# FIGURE 5: Slope Chart - Education Gap by Household Type (Expense)
# ==============================================================================

by_educ_hh <- hps_clean |>
  group_by(educ, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

p5 <- by_educ_hh |>
  ggplot(aes(x = educ, y = expense, group = hh, color = hh)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(
    data = by_educ_hh |> filter(educ == "BA+"),
    aes(label = hh),
    hjust = -0.1, size = 3, fontface = "bold"
  ) +
  scale_color_manual(values = c(
    "Single Parent" = "#7A1E2C",
    "Married Parent" = "#A85D68",
    "Single, No Kids" = "#4A7C59",
    "Married, No Kids" = "#1F3D2B"
  )) +
  scale_y_continuous(labels = percent, limits = c(0, 0.85)) +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.6))) +
  labs(
    title = "Education Gap in Expense Difficulty by Household Type",
    subtitle = "Parallel slopes suggest education provides similar benefit across household types",
    x = "Education",
    y = "Percent with Expense Difficulty",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "fig_05_expense_educ_slope.png"), p5,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_05_expense_educ_slope.png\n")

# ==============================================================================
# FIGURE 6: Faceted Age Trajectories by Education and Household
# ==============================================================================

p6 <- by_all |>
  ggplot(aes(x = age_group, y = expense, group = educ, color = educ)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~hh, ncol = 2) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Expense Difficulty: Age Trajectories by Education within Household Type",
    x = "Age Group",
    y = "Percent with Expense Difficulty",
    color = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024 | Cells with n < 25 excluded"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "fig_06_expense_faceted.png"), p6,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_06_expense_faceted.png\n")

# ==============================================================================
# FIGURE 7: Same faceted view for Distress
# ==============================================================================

p7 <- by_all |>
  ggplot(aes(x = age_group, y = distress, group = educ, color = educ)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~hh, ncol = 2) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.4)) +
  labs(
    title = "Psychological Distress: Age Trajectories by Education within Household Type",
    x = "Age Group",
    y = "Percent with Distress (PHQ-2 or GAD-2 positive)",
    color = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024 | Cells with n < 25 excluded"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "fig_07_distress_faceted.png"), p7,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_07_distress_faceted.png\n")

# ==============================================================================
# FIGURE 8: Scatter - Expense Difficulty vs Distress (segment level)
# ==============================================================================

p8 <- by_all |>
  ggplot(aes(x = expense, y = distress, size = n, color = hh)) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight = n), method = "lm", se = FALSE,
              color = "#666666", linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c(
    "Single Parent" = "#7A1E2C",
    "Married Parent" = "#A85D68",
    "Single, No Kids" = "#4A7C59",
    "Married, No Kids" = "#1F3D2B"
  )) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  labs(
    title = "Financial Hardship and Psychological Distress Move Together",
    subtitle = "Each point is an age × education × household segment; size = sample size",
    x = "Expense Difficulty Rate",
    y = "Psychological Distress Rate",
    color = "Household Type",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(legend.position = "right")

ggsave(file.path(output_dir, "fig_08_expense_vs_distress.png"), p8,
       width = 9, height = 7, dpi = 300, bg = "white")

cat("Saved: fig_08_expense_vs_distress.png\n")

# ==============================================================================
# FIGURE 9: Bar chart - All metrics for Single Parents vs Married No Kids
# ==============================================================================

comparison <- hps_clean |>
  filter(hh %in% c("Single Parent", "Married, No Kids")) |>
  group_by(hh) |>
  summarise(
    n = sum(n_unweighted),
    `Expense\nDifficulty` = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    `Food\nInsufficient` = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    `Housing\nBehind` = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    `Depression\n(PHQ-2)` = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    `Anxiety\n(GAD-2)` = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-c(hh, n), names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = c("Expense\nDifficulty", "Food\nInsufficient",
                                            "Housing\nBehind", "Depression\n(PHQ-2)",
                                            "Anxiety\n(GAD-2)")))

p9 <- comparison |>
  ggplot(aes(x = metric, y = value, fill = hh)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(value * 100), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Single Parent" = "#7A1E2C", "Married, No Kids" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.85), expand = c(0, 0)) +
  labs(
    title = "Single Parents vs Married Couples Without Children",
    subtitle = "Gap persists across all hardship and mental health measures",
    x = NULL,
    y = "Percent",
    fill = "Household Type",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "fig_09_comparison_bar.png"), p9,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_09_comparison_bar.png\n")

# ==============================================================================
cat("\n")
cat("=" |> strrep(60), "\n")
cat("All visualizations saved to:\n")
cat(output_dir, "\n")
cat("=" |> strrep(60), "\n")
