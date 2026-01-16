# ==============================================================================
# 05_compare_2020_2024.R
# Compare December 2020 vs December 2024
# Question: Are things getting worse? Worse faster for those without a BA?
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

# Create cleaner wave labels
hps_clean <- hps |>
  filter(!is.na(age_group), !is.na(educ_group), !is.na(hh_type)) |>
  mutate(
    wave = case_when(
      str_detect(pulse_wave, "Week20") ~ "Dec 2020",
      str_detect(pulse_wave, "DECEMBER2024") ~ "Dec 2024"
    ),
    wave = factor(wave, levels = c("Dec 2020", "Dec 2024")),

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

    age_bucket = case_when(
      age_group %in% c("18-24", "25-34") ~ "18-34",
      age_group %in% c("35-44", "45-54") ~ "35-54",
      age_group %in% c("55-64", "65-74", "75+") ~ "55+"
    ),
    age_bucket = factor(age_bucket, levels = c("18-34", "35-54", "55+"))
  )

# ==============================================================================
# TABLE 1: Overall change by education
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("CHANGE FROM DEC 2020 TO DEC 2024 BY EDUCATION\n")
cat("=" |> strrep(80), "\n\n")

by_wave_educ <- hps_clean |>
  group_by(wave, educ) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

print(by_wave_educ |> mutate(across(where(is.numeric) & !matches("^n$"), ~round(. * 100, 1))))

# Calculate change
change_by_educ <- by_wave_educ |>
  pivot_longer(cols = c(expense, food, depression, anxiety, distress),
               names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = wave, values_from = c(rate, n)) |>
  mutate(
    change_pp = (`rate_Dec 2024` - `rate_Dec 2020`) * 100,
    pct_change = ((`rate_Dec 2024` - `rate_Dec 2020`) / `rate_Dec 2020`) * 100
  )

cat("\n\nCHANGE (2024 minus 2020) in percentage points:\n")
cat("-" |> strrep(60), "\n\n")

change_by_educ |>
  select(educ, metric, change_pp) |>
  pivot_wider(names_from = metric, values_from = change_pp) |>
  mutate(across(where(is.numeric), ~paste0(ifelse(. > 0, "+", ""), round(., 1), " pp"))) |>
  print()

# ==============================================================================
# TABLE 2: Change in education gap over time
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("CHANGE IN EDUCATION GAP (No BA minus BA+) OVER TIME\n")
cat("=" |> strrep(80), "\n\n")

educ_gap_by_wave <- by_wave_educ |>
  select(-n) |>
  pivot_longer(cols = c(expense, food, depression, anxiety, distress),
               names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = educ, values_from = rate) |>
  mutate(gap = (`No BA` - `BA+`) * 100)

gap_change <- educ_gap_by_wave |>
  select(wave, metric, gap) |>
  pivot_wider(names_from = wave, values_from = gap) |>
  mutate(
    gap_change = `Dec 2024` - `Dec 2020`,
    direction = case_when(
      gap_change > 1 ~ "Gap WIDENED",
      gap_change < -1 ~ "Gap NARROWED",
      TRUE ~ "~Same"
    )
  )

cat("Education gap (No BA - BA+) by wave and change:\n\n")
gap_change |>
  mutate(
    `Dec 2020` = paste0(round(`Dec 2020`, 1), " pp"),
    `Dec 2024` = paste0(round(`Dec 2024`, 1), " pp"),
    gap_change = paste0(ifelse(gap_change > 0, "+", ""), round(gap_change, 1), " pp")
  ) |>
  print()

# ==============================================================================
# TABLE 3: Change by education × household type
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("CHANGE BY EDUCATION × HOUSEHOLD TYPE\n")
cat("=" |> strrep(80), "\n\n")

by_wave_educ_hh <- hps_clean |>
  group_by(wave, educ, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

change_by_educ_hh <- by_wave_educ_hh |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = wave, values_from = c(rate, n)) |>
  mutate(change_pp = (`rate_Dec 2024` - `rate_Dec 2020`) * 100)

for (m in c("expense", "food", "distress")) {
  cat(toupper(m), " change (pp):\n")
  cat("-" |> strrep(50), "\n")

  change_by_educ_hh |>
    filter(metric == m) |>
    select(educ, hh, change_pp) |>
    pivot_wider(names_from = hh, values_from = change_pp) |>
    mutate(across(where(is.numeric), ~paste0(ifelse(. > 0, "+", ""), round(., 1)))) |>
    print()

  cat("\n")
}

# ==============================================================================
# FIGURE 1: Slope chart - 2020 to 2024 by education
# ==============================================================================

p1 <- by_wave_educ |>
  ggplot(aes(x = wave, y = expense, group = educ, color = educ)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(expense * 100), "%")),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.7)) +
  labs(
    title = "Expense Difficulty: December 2020 vs December 2024",
    subtitle = "By education level",
    x = NULL,
    y = "Percent Reporting Expense Difficulty",
    color = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_15_expense_2020_2024.png"), p1,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_15_expense_2020_2024.png\n")

# ==============================================================================
# FIGURE 2: Same for food
# ==============================================================================

p2 <- by_wave_educ |>
  ggplot(aes(x = wave, y = food, group = educ, color = educ)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(food * 100), "%")),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
  labs(
    title = "Food Insufficiency: December 2020 vs December 2024",
    subtitle = "By education level",
    x = NULL,
    y = "Percent Reporting Food Insufficiency",
    color = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_16_food_2020_2024.png"), p2,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_16_food_2020_2024.png\n")

# ==============================================================================
# FIGURE 3: Same for distress
# ==============================================================================

p3 <- by_wave_educ |>
  ggplot(aes(x = wave, y = distress, group = educ, color = educ)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(distress * 100), "%")),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.35)) +
  labs(
    title = "Psychological Distress: December 2020 vs December 2024",
    subtitle = "By education level",
    x = NULL,
    y = "Percent Screening Positive (PHQ-2 or GAD-2)",
    color = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_17_distress_2020_2024.png"), p3,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_17_distress_2020_2024.png\n")

# ==============================================================================
# FIGURE 4: Change by education × household type (expense)
# ==============================================================================

p4 <- change_by_educ_hh |>
  filter(metric == "expense") |>
  ggplot(aes(x = hh, y = change_pp, fill = educ)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  geom_text(aes(label = paste0(ifelse(change_pp > 0, "+", ""), round(change_pp, 0))),
            position = position_dodge(width = 0.7),
            vjust = ifelse(change_by_educ_hh$change_pp[change_by_educ_hh$metric == "expense"] > 0, -0.5, 1.5),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(limits = c(-20, 10)) +
  labs(
    title = "Change in Expense Difficulty: 2020 to 2024",
    subtitle = "By education and household type (in percentage points)",
    x = NULL,
    y = "Change (pp)",
    fill = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(output_dir, "fig_18_expense_change_by_group.png"), p4,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_18_expense_change_by_group.png\n")

# ==============================================================================
# FIGURE 5: Dumbbell - 2020 vs 2024 by education × household
# ==============================================================================

dumbbell_data <- by_wave_educ_hh |>
  mutate(group = paste(educ, hh, sep = "\n")) |>
  select(wave, group, expense) |>
  pivot_wider(names_from = wave, values_from = expense)

# Order by 2024 rate
group_order <- dumbbell_data |>
  arrange(`Dec 2024`) |>
  pull(group)

dumbbell_data <- dumbbell_data |>
  mutate(group = factor(group, levels = group_order))

p5 <- dumbbell_data |>
  ggplot(aes(y = group)) +
  geom_segment(aes(x = `Dec 2020`, xend = `Dec 2024`, y = group, yend = group),
               color = "#999999", linewidth = 1.5) +
  geom_point(aes(x = `Dec 2020`), color = "#A85D68", size = 4) +
  geom_point(aes(x = `Dec 2024`), color = "#1F3D2B", size = 4) +
  scale_x_continuous(labels = percent, limits = c(0, 0.9)) +
  annotate("text", x = 0.85, y = 8.5, label = "2020", color = "#A85D68", fontface = "bold", size = 3) +
  annotate("text", x = 0.85, y = 8, label = "2024", color = "#1F3D2B", fontface = "bold", size = 3) +
  labs(
    title = "Expense Difficulty: December 2020 → December 2024",
    subtitle = "By education × household type",
    x = "Percent Reporting Expense Difficulty",
    y = NULL,
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration() +
  theme(panel.grid.major.y = element_line(color = "#E5E5E5", linetype = "dotted"))

ggsave(file.path(output_dir, "fig_19_expense_dumbbell_2020_2024.png"), p5,
       width = 9, height = 8, dpi = 300, bg = "white")

cat("Saved: fig_19_expense_dumbbell_2020_2024.png\n")

# ==============================================================================
# FIGURE 6: Gap change visualization
# ==============================================================================

gap_plot_data <- educ_gap_by_wave |>
  filter(metric %in% c("expense", "food", "distress")) |>
  mutate(
    metric_label = case_when(
      metric == "expense" ~ "Expense Difficulty",
      metric == "food" ~ "Food Insufficiency",
      metric == "distress" ~ "Psychological Distress"
    )
  )

p6 <- gap_plot_data |>
  ggplot(aes(x = wave, y = gap, group = metric_label, color = metric_label)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(gap), " pp")),
            vjust = -1, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(
    "Expense Difficulty" = "#1F3D2B",
    "Food Insufficiency" = "#4A7C59",
    "Psychological Distress" = "#7A1E2C"
  )) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    title = "Education Gap Over Time",
    subtitle = "Gap = No BA rate minus BA+ rate (in percentage points)",
    x = NULL,
    y = "Education Gap (pp)",
    color = "Outcome",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_20_educ_gap_over_time.png"), p6,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_20_educ_gap_over_time.png\n")

# ==============================================================================
cat("\n")
cat("=" |> strrep(60), "\n")
cat("2020 vs 2024 comparison complete.\n")
cat("=" |> strrep(60), "\n")
