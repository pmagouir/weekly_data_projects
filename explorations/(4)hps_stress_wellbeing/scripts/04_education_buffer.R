# ==============================================================================
# 04_education_buffer.R
# Question: Conditional on age and household structure, how much does
# education buffer hardship and distress?
# ==============================================================================

library(tidyverse)
library(arrow)
library(scales)
library(here)

source(here("foundational_data", "R", "00_config.R"))

output_dir <- here("explorations", "(4)hps_stress_wellbeing", "output")

# ==============================================================================
# Theme
# ==============================================================================

theme_exploration <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F3D2B"),
      plot.subtitle = element_text(size = 10, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = 9, color = "#333333"),
      axis.text = element_text(size = 8),
      legend.position = "top",
      strip.text = element_text(face = "bold", size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
}

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

    # Broader age buckets for cleaner display
    age_bucket = case_when(
      age_group %in% c("18-24", "25-34") ~ "18-34",
      age_group %in% c("35-44", "45-54") ~ "35-54",
      age_group %in% c("55-64", "65-74", "75+") ~ "55+"
    ),
    age_bucket = factor(age_bucket, levels = c("18-34", "35-54", "55+"))
  )

# ==============================================================================
# Compute education gap within each age × household cell
# ==============================================================================

by_cell <- hps_clean |>
  group_by(age_bucket, hh, educ) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    housing = weighted.mean(pct_housing_not_current, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

# Compute the education gap (No BA - BA+) for each cell
educ_gap <- by_cell |>
  pivot_longer(cols = c(expense, food, housing, depression, anxiety, distress),
               names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = educ, values_from = c(rate, n)) |>
  mutate(
    gap_pp = (`rate_No BA` - `rate_BA+`) * 100,  # in percentage points
    metric_label = case_when(
      metric == "expense" ~ "Expense Difficulty",
      metric == "food" ~ "Food Insufficiency",
      metric == "housing" ~ "Housing Behind",
      metric == "depression" ~ "Depression (PHQ-2)",
      metric == "anxiety" ~ "Anxiety (GAD-2)",
      metric == "distress" ~ "Any Distress"
    ),
    metric_label = factor(metric_label, levels = c(
      "Expense Difficulty", "Food Insufficiency", "Housing Behind",
      "Depression (PHQ-2)", "Anxiety (GAD-2)", "Any Distress"
    ))
  )

# ==============================================================================
# TABLE: Education Gap by Age × Household
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("EDUCATION GAP (No BA rate minus BA+ rate, in percentage points)\n")
cat("Within each Age × Household cell\n")
cat("=" |> strrep(80), "\n\n")

for (m in unique(educ_gap$metric)) {
  cat(toupper(m), ":\n")
  cat("-" |> strrep(60), "\n")

  educ_gap |>
    filter(metric == m) |>
    select(age_bucket, hh, gap_pp) |>
    pivot_wider(names_from = hh, values_from = gap_pp) |>
    mutate(across(where(is.numeric), ~paste0(round(., 1), " pp"))) |>
    print()

  cat("\n")
}

# ==============================================================================
# FIGURE 1: Dumbbell plot - Education gap for Expense Difficulty
# ==============================================================================

plot_data <- by_cell |>
  filter(n >= 20) |>
  mutate(cell = paste(age_bucket, hh, sep = "\n"))

# Order by No BA rate within each metric
cell_order <- plot_data |>
  filter(educ == "No BA") |>
  arrange(expense) |>
  pull(cell)

plot_data <- plot_data |>
  mutate(cell = factor(cell, levels = cell_order))

p1 <- plot_data |>
  ggplot(aes(y = cell)) +
  geom_segment(
    data = plot_data |>
      select(cell, educ, expense) |>
      pivot_wider(names_from = educ, values_from = expense),
    aes(x = `BA+`, xend = `No BA`, y = cell, yend = cell),
    color = "#999999", linewidth = 1
  ) +
  geom_point(aes(x = expense, color = educ), size = 3) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Education Buffer: Expense Difficulty",
    subtitle = "Within each age × household group, BA+ consistently lower than No BA",
    x = "Percent Reporting Expense Difficulty",
    y = NULL,
    color = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(
    panel.grid.major.y = element_line(color = "#E5E5E5", linetype = "dotted"),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "fig_10_expense_dumbbell.png"), p1,
       width = 8, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_10_expense_dumbbell.png\n")

# ==============================================================================
# FIGURE 2: Dumbbell plot - Education gap for Food Insufficiency
# ==============================================================================

cell_order_food <- plot_data |>
  filter(educ == "No BA") |>
  arrange(food) |>
  pull(cell)

plot_data_food <- plot_data |>
  mutate(cell = factor(cell, levels = cell_order_food))

p2 <- plot_data_food |>
  ggplot(aes(y = cell)) +
  geom_segment(
    data = plot_data_food |>
      select(cell, educ, food) |>
      pivot_wider(names_from = educ, values_from = food),
    aes(x = `BA+`, xend = `No BA`, y = cell, yend = cell),
    color = "#999999", linewidth = 1
  ) +
  geom_point(aes(x = food, color = educ), size = 3) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_x_continuous(labels = percent, limits = c(0, 0.7)) +
  labs(
    title = "Education Buffer: Food Insufficiency",
    subtitle = "Within each age × household group, BA+ consistently lower than No BA",
    x = "Percent Reporting Food Insufficiency",
    y = NULL,
    color = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(
    panel.grid.major.y = element_line(color = "#E5E5E5", linetype = "dotted"),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "fig_11_food_dumbbell.png"), p2,
       width = 8, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_11_food_dumbbell.png\n")

# ==============================================================================
# FIGURE 3: Dumbbell plot - Education gap for Distress
# ==============================================================================

cell_order_distress <- plot_data |>
  filter(educ == "No BA") |>
  arrange(distress) |>
  pull(cell)

plot_data_distress <- plot_data |>
  mutate(cell = factor(cell, levels = cell_order_distress))

p3 <- plot_data_distress |>
  ggplot(aes(y = cell)) +
  geom_segment(
    data = plot_data_distress |>
      select(cell, educ, distress) |>
      pivot_wider(names_from = educ, values_from = distress),
    aes(x = `BA+`, xend = `No BA`, y = cell, yend = cell),
    color = "#999999", linewidth = 1
  ) +
  geom_point(aes(x = distress, color = educ), size = 3) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_x_continuous(labels = percent, limits = c(0, 0.4)) +
  labs(
    title = "Education Buffer: Psychological Distress",
    subtitle = "Within each age × household group, BA+ consistently lower than No BA",
    x = "Percent Screening Positive (PHQ-2 or GAD-2)",
    y = NULL,
    color = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(
    panel.grid.major.y = element_line(color = "#E5E5E5", linetype = "dotted"),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "fig_12_distress_dumbbell.png"), p3,
       width = 8, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_12_distress_dumbbell.png\n")

# ==============================================================================
# FIGURE 4: Bar chart of education gaps across all metrics
# ==============================================================================

# Average gap by metric
avg_gap <- educ_gap |>
  group_by(metric_label) |>
  summarise(
    mean_gap = mean(gap_pp, na.rm = TRUE),
    min_gap = min(gap_pp, na.rm = TRUE),
    max_gap = max(gap_pp, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- avg_gap |>
  ggplot(aes(x = reorder(metric_label, mean_gap), y = mean_gap)) +
  geom_col(fill = "#1F3D2B", width = 0.7) +
  geom_errorbar(aes(ymin = min_gap, ymax = max_gap), width = 0.2, color = "#666666") +
  geom_text(aes(label = paste0(round(mean_gap, 0), " pp")),
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  labs(
    title = "Average Education Buffer by Outcome",
    subtitle = "Mean gap (No BA minus BA+) across all age × household cells; bars show range",
    x = NULL,
    y = "Education Gap (percentage points)",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_13_avg_educ_gap.png"), p4,
       width = 8, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_13_avg_educ_gap.png\n")

# ==============================================================================
# FIGURE 5: Education gap varies by context - faceted by household type
# ==============================================================================

gap_by_hh <- educ_gap |>
  filter(metric %in% c("expense", "food", "distress"))

p5 <- gap_by_hh |>
  ggplot(aes(x = age_bucket, y = gap_pp, fill = metric_label)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~hh, ncol = 2) +
  scale_fill_manual(values = c(
    "Expense Difficulty" = "#1F3D2B",
    "Food Insufficiency" = "#4A7C59",
    "Any Distress" = "#7A1E2C"
  )) +
  scale_y_continuous(limits = c(-5, 35)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  labs(
    title = "Education Gap by Age and Household Type",
    subtitle = "How much does a BA reduce each outcome? (in percentage points)",
    x = "Age Group",
    y = "Education Gap (No BA − BA+, pp)",
    fill = "Outcome",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "fig_14_educ_gap_by_context.png"), p5,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("Saved: fig_14_educ_gap_by_context.png\n")

# ==============================================================================
cat("\n")
cat("=" |> strrep(60), "\n")
cat("Education buffer analysis complete.\n")
cat("=" |> strrep(60), "\n")
