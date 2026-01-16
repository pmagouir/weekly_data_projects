# ==============================================================================
# 06_education_buffer_2021_2024.R
# Comprehensive analysis: Education as a buffer against hardship and distress
# Comparing late 2021 (Week 40) to late 2024
#
# Questions:
# 1. What are Americans' sentiments on hardship/wellbeing today?
# 2. How have they changed from 2021 to 2024?
# 3. How do they vary by household structure, age, and education?
# 4. Is education a buffer? Is it more or less of a buffer now?
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
    wave = case_when(
      str_detect(pulse_wave, "Week40") ~ "Late 2021",
      str_detect(pulse_wave, "DECEMBER2024") ~ "Late 2024"
    ),
    wave = factor(wave, levels = c("Late 2021", "Late 2024")),

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
# PART 1: CURRENT STATE (2024)
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("PART 1: CURRENT STATE - LATE 2024\n")
cat("=" |> strrep(80), "\n\n")

current <- hps_clean |>
  filter(wave == "Late 2024")

# Overall by education
cat("OVERALL BY EDUCATION (Late 2024):\n")
cat("-" |> strrep(60), "\n\n")

by_educ_2024 <- current |>
  group_by(educ) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    depression = weighted.mean(pct_phq2_positive, n_unweighted, na.rm = TRUE),
    anxiety = weighted.mean(pct_gad2_positive, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_educ_2024 |>
  mutate(across(c(expense, food, depression, anxiety, distress), ~paste0(round(. * 100, 1), "%"))) |>
  print()

# By education and household
cat("\n\nBY EDUCATION × HOUSEHOLD TYPE (Late 2024):\n")
cat("-" |> strrep(60), "\n\n")

by_educ_hh_2024 <- current |>
  group_by(educ, hh) |>
  summarise(
    n = sum(n_unweighted),
    expense = weighted.mean(pct_hardship_expenses, n_unweighted, na.rm = TRUE),
    food = weighted.mean(pct_food_insufficient, n_unweighted, na.rm = TRUE),
    distress = weighted.mean(pct_distress, n_unweighted, na.rm = TRUE),
    .groups = "drop"
  )

by_educ_hh_2024 |>
  mutate(across(c(expense, food, distress), ~paste0(round(. * 100, 1), "%"))) |>
  print()

# Education gap within each household type (2024)
cat("\n\nEDUCATION GAP BY HOUSEHOLD TYPE (Late 2024):\n")
cat("Gap = No BA rate minus BA+ rate\n")
cat("-" |> strrep(60), "\n\n")

gap_2024 <- by_educ_hh_2024 |>
  select(-n) |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = educ, values_from = rate) |>
  mutate(gap_pp = round((`No BA` - `BA+`) * 100, 1))

gap_2024 |>
  select(hh, metric, gap_pp) |>
  pivot_wider(names_from = metric, values_from = gap_pp) |>
  print()

# ==============================================================================
# PART 2: CHANGE OVER TIME (2021 → 2024)
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 2: CHANGE FROM LATE 2021 TO LATE 2024\n")
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

cat("RATES BY WAVE AND EDUCATION:\n")
cat("-" |> strrep(60), "\n\n")

by_wave_educ |>
  mutate(across(c(expense, food, depression, anxiety, distress), ~round(. * 100, 1))) |>
  print()

# Calculate change
cat("\n\nCHANGE (2024 minus 2021) BY EDUCATION:\n")
cat("-" |> strrep(60), "\n\n")

change_by_educ <- by_wave_educ |>
  select(-n) |>
  pivot_longer(cols = c(expense, food, depression, anxiety, distress),
               names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = wave, values_from = rate) |>
  mutate(change_pp = round((`Late 2024` - `Late 2021`) * 100, 1))

change_by_educ |>
  select(educ, metric, change_pp) |>
  pivot_wider(names_from = metric, values_from = change_pp) |>
  print()

# ==============================================================================
# PART 3: HAS THE EDUCATION GAP CHANGED?
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 3: HAS THE EDUCATION BUFFER CHANGED?\n")
cat("=" |> strrep(80), "\n\n")

educ_gap_by_wave <- by_wave_educ |>
  select(-n) |>
  pivot_longer(cols = c(expense, food, depression, anxiety, distress),
               names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = educ, values_from = rate) |>
  mutate(gap_pp = round((`No BA` - `BA+`) * 100, 1))

cat("EDUCATION GAP (No BA - BA+) BY WAVE:\n")
cat("-" |> strrep(60), "\n\n")

gap_summary <- educ_gap_by_wave |>
  select(wave, metric, gap_pp) |>
  pivot_wider(names_from = wave, values_from = gap_pp) |>
  mutate(
    gap_change = `Late 2024` - `Late 2021`,
    direction = case_when(
      gap_change > 2 ~ "WIDENED",
      gap_change < -2 ~ "NARROWED",
      TRUE ~ "STABLE"
    )
  )

gap_summary |>
  mutate(
    `Late 2021` = paste0(`Late 2021`, " pp"),
    `Late 2024` = paste0(`Late 2024`, " pp"),
    gap_change = paste0(ifelse(gap_change > 0, "+", ""), round(gap_change, 1), " pp")
  ) |>
  print()

# ==============================================================================
# PART 4: CHANGE BY EDUCATION × HOUSEHOLD TYPE
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 4: CHANGE BY EDUCATION × HOUSEHOLD TYPE\n")
cat("Who got better? Who got worse?\n")
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
  select(-n) |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = wave, values_from = rate) |>
  mutate(change_pp = round((`Late 2024` - `Late 2021`) * 100, 1))

for (m in c("expense", "food", "distress")) {
  cat(toupper(m), " CHANGE (pp):\n")
  cat("-" |> strrep(50), "\n")

  change_by_educ_hh |>
    filter(metric == m) |>
    select(educ, hh, change_pp) |>
    pivot_wider(names_from = hh, values_from = change_pp) |>
    print()

  cat("\n")
}

# ==============================================================================
# PART 5: EDUCATION GAP WITHIN HOUSEHOLD TYPES - 2021 vs 2024
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("PART 5: EDUCATION GAP BY HOUSEHOLD TYPE - 2021 vs 2024\n")
cat("Is education more or less protective within each household type?\n")
cat("=" |> strrep(80), "\n\n")

gap_by_hh_wave <- by_wave_educ_hh |>
  select(-n) |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  pivot_wider(names_from = educ, values_from = rate) |>
  mutate(gap_pp = round((`No BA` - `BA+`) * 100, 1))

gap_change_by_hh <- gap_by_hh_wave |>
  select(wave, hh, metric, gap_pp) |>
  pivot_wider(names_from = wave, values_from = gap_pp) |>
  mutate(gap_change = `Late 2024` - `Late 2021`)

for (m in c("expense", "food", "distress")) {
  cat(toupper(m), " - Education gap by household type:\n")
  cat("-" |> strrep(50), "\n")

  gap_change_by_hh |>
    filter(metric == m) |>
    mutate(
      `2021 gap` = paste0(`Late 2021`, " pp"),
      `2024 gap` = paste0(`Late 2024`, " pp"),
      change = paste0(ifelse(gap_change > 0, "+", ""), round(gap_change, 1), " pp")
    ) |>
    select(hh, `2021 gap`, `2024 gap`, change) |>
    print()

  cat("\n")
}

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# FIG 1: Current state - rates by education × household
p1 <- by_educ_hh_2024 |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  mutate(
    metric_label = case_when(
      metric == "expense" ~ "Expense\nDifficulty",
      metric == "food" ~ "Food\nInsufficiency",
      metric == "distress" ~ "Psychological\nDistress"
    ),
    metric_label = factor(metric_label, levels = c("Expense\nDifficulty", "Food\nInsufficiency", "Psychological\nDistress"))
  ) |>
  ggplot(aes(x = hh, y = rate, fill = educ)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(rate * 100), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~metric_label) +
  scale_fill_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, limits = c(0, 0.9), expand = c(0, 0)) +
  labs(
    title = "Current State: Hardship and Distress by Education and Household Type",
    subtitle = "Late 2024",
    x = NULL,
    y = "Percent",
    fill = "Education",
    caption = "Source: Census Household Pulse Survey, December 2024"
  ) +
  theme_exploration() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))

ggsave(file.path(output_dir, "fig_21_current_state.png"), p1,
       width = 11, height = 6, dpi = 300, bg = "white")

cat("\nSaved: fig_21_current_state.png\n")

# FIG 2: Change over time by education
p2 <- by_wave_educ |>
  pivot_longer(cols = c(expense, food, distress), names_to = "metric", values_to = "rate") |>
  mutate(
    metric_label = case_when(
      metric == "expense" ~ "Expense Difficulty",
      metric == "food" ~ "Food Insufficiency",
      metric == "distress" ~ "Psychological Distress"
    )
  ) |>
  ggplot(aes(x = wave, y = rate, color = educ, group = educ)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(rate * 100), "%")),
            vjust = -1.2, size = 3, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~metric_label, scales = "free_y") +
  scale_color_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "Change Over Time: Late 2021 to Late 2024",
    subtitle = "By education level",
    x = NULL,
    y = "Percent",
    color = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_22_change_over_time.png"), p2,
       width = 11, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_22_change_over_time.png\n")

# FIG 3: Education gap over time
p3 <- gap_summary |>
  mutate(
    metric_label = case_when(
      metric == "expense" ~ "Expense Difficulty",
      metric == "food" ~ "Food Insufficiency",
      metric == "depression" ~ "Depression (PHQ-2)",
      metric == "anxiety" ~ "Anxiety (GAD-2)",
      metric == "distress" ~ "Any Distress"
    )
  ) |>
  select(metric_label, `Late 2021`, `Late 2024`) |>
  pivot_longer(cols = c(`Late 2021`, `Late 2024`), names_to = "wave", values_to = "gap") |>
  mutate(wave = factor(wave, levels = c("Late 2021", "Late 2024"))) |>
  ggplot(aes(x = wave, y = gap, group = metric_label, color = metric_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(gap, " pp")),
            vjust = -1, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(
    "Expense Difficulty" = "#1F3D2B",
    "Food Insufficiency" = "#4A7C59",
    "Depression (PHQ-2)" = "#A85D68",
    "Anxiety (GAD-2)" = "#C08090",
    "Any Distress" = "#7A1E2C"
  )) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    title = "Education Gap Over Time",
    subtitle = "Gap = No BA rate minus BA+ rate (in percentage points)",
    x = NULL,
    y = "Education Gap (pp)",
    color = "Outcome",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration() +
  theme(legend.position = "right")

ggsave(file.path(output_dir, "fig_23_education_gap_over_time.png"), p3,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_23_education_gap_over_time.png\n")

# FIG 4: Change by education × household (expense)
p4 <- change_by_educ_hh |>
  filter(metric == "expense") |>
  ggplot(aes(x = hh, y = change_pp, fill = educ)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(aes(label = ifelse(change_pp >= 0, paste0("+", change_pp), change_pp)),
            position = position_dodge(width = 0.7),
            vjust = ifelse(change_by_educ_hh$change_pp[change_by_educ_hh$metric == "expense"] >= 0, -0.5, 1.5),
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("No BA" = "#7A1E2C", "BA+" = "#1F3D2B")) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(
    title = "Change in Expense Difficulty: 2021 → 2024",
    subtitle = "By education and household type",
    x = NULL,
    y = "Change (percentage points)",
    fill = "Education",
    caption = "Source: Census Household Pulse Survey"
  ) +
  theme_exploration() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(output_dir, "fig_24_expense_change_by_group.png"), p4,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_24_expense_change_by_group.png\n")

# FIG 5: Dumbbell - education buffer within each household type
dumbbell_data <- by_educ_hh_2024 |>
  select(educ, hh, expense) |>
  pivot_wider(names_from = educ, values_from = expense) |>
  mutate(gap = `No BA` - `BA+`)

p5 <- dumbbell_data |>
  ggplot(aes(y = reorder(hh, gap))) +
  geom_segment(aes(x = `BA+`, xend = `No BA`, y = reorder(hh, gap), yend = reorder(hh, gap)),
               color = "#999999", linewidth = 2) +
  geom_point(aes(x = `BA+`), color = "#1F3D2B", size = 5) +
  geom_point(aes(x = `No BA`), color = "#7A1E2C", size = 5) +
  geom_text(aes(x = (`BA+` + `No BA`) / 2, label = paste0(round(gap * 100), " pp gap")),
            vjust = -1, size = 3, fontface = "bold") +
  scale_x_continuous(labels = percent, limits = c(0, 0.85)) +
  labs(
    title = "Education Buffer: Expense Difficulty (Late 2024)",
    subtitle = "Within each household type, how much does a BA reduce expense difficulty?",
    x = "Percent Reporting Expense Difficulty",
    y = NULL,
    caption = "Green = BA+, Red = No BA | Source: Census Household Pulse Survey"
  ) +
  theme_exploration() +
  theme(panel.grid.major.y = element_line(color = "#E5E5E5", linetype = "dotted"))

ggsave(file.path(output_dir, "fig_25_education_buffer_dumbbell.png"), p5,
       width = 8, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_25_education_buffer_dumbbell.png\n")

# ==============================================================================
cat("\n")
cat("=" |> strrep(60), "\n")
cat("Analysis complete.\n")
cat("=" |> strrep(60), "\n")
