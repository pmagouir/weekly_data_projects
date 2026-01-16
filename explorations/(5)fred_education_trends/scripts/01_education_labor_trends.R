# ==============================================================================
# 01_education_labor_trends.R
# Explore labor market outcomes by education level over time
#
# Data: FRED/BLS education-stratified series (25+ years old)
# Outcomes:
#   - Unemployment Rate
#   - Labor Force Participation Rate
#   - Employment-Population Ratio
#   - Median Weekly Earnings (quarterly)
# ==============================================================================

library(tidyverse)
library(arrow)
library(scales)
library(here)

source(here("foundational_data", "R", "00_config.R"))

output_dir <- here("explorations", "(5)fred_education_trends", "output")

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
      axis.title = element_text(size = 9, color = "#333333"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
}

educ_colors <- c(
  "Less than HS" = "#7A1E2C",
  "High School" = "#A85D68",
  "Some College" = "#4A7C59",
  "Bachelor's+" = "#1F3D2B"
)

# ==============================================================================
# Load and Prepare Data
# ==============================================================================

fred <- read_mart("fred")

# Map series IDs to education levels and outcome types
# Note: BLS series IDs based on actual FRED titles
series_map <- tribble(
  ~series_id,     ~educ_level,      ~outcome,
  # Unemployment Rate
  "LNS14027659",  "Less than HS",   "Unemployment Rate",
  "LNS14027660",  "High School",    "Unemployment Rate",
  "LNS14027662",  "Bachelor's+",    "Unemployment Rate",
  "LNS14027689",  "Some College",   "Unemployment Rate",
  # Labor Force Participation
  "LNS11327659",  "Less than HS",   "Labor Force Participation",
  "LNS11327660",  "High School",    "Labor Force Participation",
  "LNS11327662",  "Bachelor's+",    "Labor Force Participation",
  "LNS11327689",  "Some College",   "Labor Force Participation",
  # Employment-Population Ratio
  "LNS12327659",  "Less than HS",   "Employment-Pop Ratio",
  "LNS12327660",  "High School",    "Employment-Pop Ratio",
  "LNS12327662",  "Bachelor's+",    "Employment-Pop Ratio",
  "LNS12327689",  "Some College",   "Employment-Pop Ratio",
  # Median Weekly Earnings (quarterly)
  "LEU0252916700Q",  "Less than HS",   "Weekly Earnings",
  "LEU0252917300Q",  "High School",    "Weekly Earnings",
  "LEU0252918500Q",  "Bachelor's+",    "Weekly Earnings"
)

# Join and prepare
educ_data <- fred |>
  inner_join(series_map, by = "series_id") |>
  mutate(
    educ_level = factor(educ_level, levels = c("Less than HS", "High School", "Some College", "Bachelor's+")),
    year = year(date),
    month = month(date)
  )

# ==============================================================================
# PART 1: Current State
# ==============================================================================

cat("\n")
cat("=" |> strrep(80), "\n")
cat("PART 1: CURRENT STATE (Latest Available)\n")
cat("=" |> strrep(80), "\n\n")

latest <- educ_data |>
  group_by(outcome, educ_level) |>
  filter(date == max(date)) |>
  ungroup()

cat("LATEST VALUES BY EDUCATION AND OUTCOME:\n")
cat("-" |> strrep(60), "\n\n")

latest |>
  select(outcome, educ_level, value, date) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  print()

# Education gap (Less than HS minus Bachelor's+)
cat("\n\nEDUCATION GAP (Less than HS minus Bachelor's+):\n")
cat("-" |> strrep(60), "\n\n")

latest |>
  select(outcome, educ_level, value) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  mutate(
    gap = `Less than HS` - `Bachelor's+`,
    gap_label = paste0(round(gap, 1), " pp")
  ) |>
  select(outcome, `Less than HS`, `Bachelor's+`, gap_label) |>
  print()

# ==============================================================================
# PART 2: Long-term Trends
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 2: LONG-TERM TRENDS (2000-2024)\n")
cat("=" |> strrep(80), "\n\n")

# Annual averages
annual <- educ_data |>
  group_by(year, outcome, educ_level) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# Key years comparison
key_years <- c(2000, 2007, 2010, 2019, 2024)

cat("UNEMPLOYMENT RATE BY EDUCATION - KEY YEARS:\n")
cat("-" |> strrep(60), "\n\n")

annual |>
  filter(outcome == "Unemployment Rate", year %in% key_years) |>
  select(year, educ_level, value) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print()

cat("\n\nEMPLOYMENT-POP RATIO BY EDUCATION - KEY YEARS:\n")
cat("-" |> strrep(60), "\n\n")

annual |>
  filter(outcome == "Employment-Pop Ratio", year %in% key_years) |>
  select(year, educ_level, value) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print()

# ==============================================================================
# PART 3: Change Over Time
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 3: CHANGE OVER TIME\n")
cat("=" |> strrep(80), "\n\n")

# Compare 2000 vs 2024
change <- annual |>
  filter(year %in% c(2000, 2024)) |>
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") |>
  mutate(change = y2024 - y2000)

cat("CHANGE FROM 2000 TO 2024:\n")
cat("-" |> strrep(60), "\n\n")

change |>
  select(outcome, educ_level, y2000, y2024, change) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print(n = 20)

# Has the education gap widened?
cat("\n\nHAS THE EDUCATION GAP CHANGED? (Less than HS minus Bachelor's+)\n")
cat("-" |> strrep(60), "\n\n")

gap_over_time <- annual |>
  select(year, outcome, educ_level, value) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  mutate(gap = `Less than HS` - `Bachelor's+`)

gap_2000_vs_2024 <- gap_over_time |>
  filter(year %in% c(2000, 2024)) |>
  select(year, outcome, gap) |>
  pivot_wider(names_from = year, values_from = gap, names_prefix = "gap_") |>
  mutate(gap_change = gap_2024 - gap_2000)

gap_2000_vs_2024 |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print()

# ==============================================================================
# PART 4: Recession Impacts
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 4: RECESSION IMPACTS BY EDUCATION\n")
cat("=" |> strrep(80), "\n\n")

# Great Recession (2007-2010) and COVID (2019-2020)
recession_impact <- annual |>
  filter(outcome == "Unemployment Rate") |>
  filter(year %in% c(2007, 2010, 2019, 2020)) |>
  select(year, educ_level, value) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(
    great_recession_spike = `2010` - `2007`,
    covid_spike = `2020` - `2019`
  )

cat("UNEMPLOYMENT RATE SPIKES BY EDUCATION:\n\n")
recession_impact |>
  select(educ_level, great_recession_spike, covid_spike) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print()

# ==============================================================================
# PART 5: EARNINGS BY EDUCATION
# ==============================================================================

cat("\n\n")
cat("=" |> strrep(80), "\n")
cat("PART 5: MEDIAN WEEKLY EARNINGS BY EDUCATION\n")
cat("=" |> strrep(80), "\n\n")

# Separate earnings data (quarterly, only 3 education levels)
earnings_data <- educ_data |>
  filter(outcome == "Weekly Earnings")

# Current earnings
cat("CURRENT MEDIAN WEEKLY EARNINGS:\n")
cat("-" |> strrep(60), "\n\n")

earnings_latest <- earnings_data |>
  group_by(educ_level) |>
  filter(date == max(date)) |>
  ungroup() |>
  arrange(educ_level) |>
  mutate(
    earnings_formatted = dollar(value, accuracy = 1),
    relative_to_hs = value / value[educ_level == "High School"],
    vs_hs = paste0(round((relative_to_hs - 1) * 100), "% vs HS")
  )

earnings_latest |>
  select(educ_level, earnings_formatted, vs_hs) |>
  print()

# Bachelor's premium over time
cat("\n\nBACHELOR'S DEGREE EARNINGS PREMIUM OVER TIME:\n")
cat("-" |> strrep(60), "\n\n")

earnings_annual <- earnings_data |>
  group_by(year, educ_level) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

earnings_premium <- earnings_annual |>
  select(year, educ_level, value) |>
  pivot_wider(names_from = educ_level, values_from = value) |>
  filter(!is.na(`Bachelor's+`), !is.na(`High School`), !is.na(`Less than HS`)) |>
  mutate(
    bachelors_vs_hs = `Bachelor's+` / `High School`,
    bachelors_vs_lths = `Bachelor's+` / `Less than HS`,
    hs_vs_lths = `High School` / `Less than HS`
  )

# Show key years
key_earnings_years <- c(2000, 2007, 2015, 2020, 2024)
earnings_premium |>
  filter(year %in% key_earnings_years) |>
  select(year, `Less than HS`, `High School`, `Bachelor's+`, bachelors_vs_hs) |>
  mutate(
    across(c(`Less than HS`, `High School`, `Bachelor's+`), ~dollar(., accuracy = 1)),
    bachelors_vs_hs = paste0(round(bachelors_vs_hs, 2), "x")
  ) |>
  print()

# Earnings change over time
cat("\n\nEARNINGS CHANGE (2000 to 2024) - NOMINAL DOLLARS:\n")
cat("-" |> strrep(60), "\n\n")

earnings_change <- earnings_annual |>
  filter(year %in% c(2000, 2024)) |>
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") |>
  mutate(
    change_dollars = y2024 - y2000,
    change_pct = (y2024 / y2000 - 1) * 100
  )

earnings_change |>
  mutate(
    across(c(y2000, y2024, change_dollars), ~dollar(., accuracy = 1)),
    change_pct = paste0("+", round(change_pct, 0), "%")
  ) |>
  print()

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# FIG 1: Unemployment Rate Over Time by Education
p1 <- educ_data |>
  filter(outcome == "Unemployment Rate") |>
  ggplot(aes(x = date, y = value, color = educ_level)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date(c("2008-12-01", "2020-03-01")),
             linetype = "dashed", color = "#999999", alpha = 0.7) +
  annotate("text", x = as.Date("2008-12-01"), y = 16, label = "Great\nRecession",
           size = 2.5, hjust = 1.1, color = "#666666") +
  annotate("text", x = as.Date("2020-03-01"), y = 16, label = "COVID",
           size = 2.5, hjust = -0.1, color = "#666666") +
  scale_color_manual(values = educ_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Unemployment Rate by Education Level",
    subtitle = "Monthly, ages 25+, 2000-2024",
    x = NULL,
    y = "Unemployment Rate",
    color = "Education",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_01_unemployment_by_educ.png"), p1,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("\nSaved: fig_01_unemployment_by_educ.png\n")

# FIG 2: Employment-Population Ratio Over Time
p2 <- educ_data |>
  filter(outcome == "Employment-Pop Ratio") |>
  ggplot(aes(x = date, y = value, color = educ_level)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date(c("2008-12-01", "2020-03-01")),
             linetype = "dashed", color = "#999999", alpha = 0.7) +
  scale_color_manual(values = educ_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Employment-Population Ratio by Education Level",
    subtitle = "Monthly, ages 25+, 2000-2024",
    x = NULL,
    y = "Employment-Population Ratio",
    color = "Education",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_02_emppop_by_educ.png"), p2,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_02_emppop_by_educ.png\n")

# FIG 3: Labor Force Participation Over Time
p3 <- educ_data |>
  filter(outcome == "Labor Force Participation") |>
  ggplot(aes(x = date, y = value, color = educ_level)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date(c("2008-12-01", "2020-03-01")),
             linetype = "dashed", color = "#999999", alpha = 0.7) +
  scale_color_manual(values = educ_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Labor Force Participation Rate by Education Level",
    subtitle = "Monthly, ages 25+, 2000-2024",
    x = NULL,
    y = "Labor Force Participation Rate",
    color = "Education",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_03_lfpr_by_educ.png"), p3,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_03_lfpr_by_educ.png\n")

# FIG 4: Education Gap Over Time (Unemployment)
p4 <- gap_over_time |>
  filter(outcome == "Unemployment Rate") |>
  ggplot(aes(x = year, y = gap)) +
  geom_line(linewidth = 1.2, color = "#7A1E2C") +
  geom_point(size = 2, color = "#7A1E2C") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999") +
  geom_vline(xintercept = c(2008, 2020), linetype = "dotted", color = "#999999") +
  scale_y_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    title = "Unemployment Gap: Less than HS vs Bachelor's+",
    subtitle = "Annual average, ages 25+",
    x = NULL,
    y = "Gap (percentage points)",
    caption = "Positive = Less than HS has higher unemployment | Source: BLS via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_04_unemployment_gap.png"), p4,
       width = 9, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_04_unemployment_gap.png\n")

# FIG 5: Recession Impact Comparison
recession_long <- recession_impact |>
  select(educ_level, great_recession_spike, covid_spike) |>
  pivot_longer(cols = c(great_recession_spike, covid_spike),
               names_to = "recession", values_to = "spike") |>
  mutate(
    recession = case_when(
      recession == "great_recession_spike" ~ "Great Recession\n(2007→2010)",
      recession == "covid_spike" ~ "COVID\n(2019→2020)"
    )
  )

p5 <- recession_long |>
  ggplot(aes(x = educ_level, y = spike, fill = recession)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0("+", round(spike, 1))),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Great Recession\n(2007→2010)" = "#1F3D2B",
                               "COVID\n(2019→2020)" = "#7A1E2C")) +
  scale_y_continuous(limits = c(0, 12)) +
  labs(
    title = "Recession Unemployment Spikes by Education",
    subtitle = "Change in unemployment rate (percentage points)",
    x = NULL,
    y = "Unemployment Rate Increase (pp)",
    fill = NULL,
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration() +
  theme(legend.position = "top")

ggsave(file.path(output_dir, "fig_05_recession_impact.png"), p5,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_05_recession_impact.png\n")

# FIG 6: Current Snapshot - Labor Outcomes (not earnings, different units)
current_snapshot <- latest |>
  filter(outcome != "Weekly Earnings") |>
  mutate(
    outcome_short = case_when(
      outcome == "Unemployment Rate" ~ "Unemployment\nRate",
      outcome == "Labor Force Participation" ~ "Labor Force\nParticipation",
      outcome == "Employment-Pop Ratio" ~ "Employment-Pop\nRatio"
    )
  )

p6 <- current_snapshot |>
  ggplot(aes(x = educ_level, y = value, fill = educ_level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(value, 1), "%")),
            vjust = -0.5, size = 3, fontface = "bold") +
  facet_wrap(~outcome_short, scales = "free_y") +
  scale_fill_manual(values = educ_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Current Labor Market Outcomes by Education",
    subtitle = paste0("Latest available data (", format(max(latest$date), "%B %Y"), ")"),
    x = NULL,
    y = "Percent",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7)
  )

ggsave(file.path(output_dir, "fig_06_current_snapshot.png"), p6,
       width = 11, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_06_current_snapshot.png\n")

# FIG 7: Weekly Earnings Over Time by Education
p7 <- earnings_data |>
  ggplot(aes(x = date, y = value, color = educ_level)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date(c("2008-12-01", "2020-03-01")),
             linetype = "dashed", color = "#999999", alpha = 0.7) +
  scale_color_manual(values = educ_colors) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Median Weekly Earnings by Education Level",
    subtitle = "Quarterly, ages 25+, nominal dollars",
    x = NULL,
    y = "Median Weekly Earnings",
    color = "Education",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_07_earnings_by_educ.png"), p7,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_07_earnings_by_educ.png\n")

# FIG 8: Bachelor's Earnings Premium Over Time
p8 <- earnings_premium |>
  ggplot(aes(x = year, y = bachelors_vs_hs)) +
  geom_line(linewidth = 1.2, color = "#1F3D2B") +
  geom_point(size = 2, color = "#1F3D2B") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#999999") +
  scale_y_continuous(
    limits = c(1, 2.2),
    breaks = seq(1, 2.2, 0.2),
    labels = function(x) paste0(x, "x")
  ) +
  labs(
    title = "Bachelor's Degree Earnings Premium vs High School",
    subtitle = "Ratio of median weekly earnings, ages 25+",
    x = NULL,
    y = "Earnings Multiple",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration()

ggsave(file.path(output_dir, "fig_08_bachelors_premium.png"), p8,
       width = 9, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_08_bachelors_premium.png\n")

# FIG 9: Current Earnings Snapshot
p9 <- earnings_latest |>
  ggplot(aes(x = educ_level, y = value, fill = educ_level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = dollar(value, accuracy = 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = educ_colors) +
  scale_y_continuous(
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Current Median Weekly Earnings by Education",
    subtitle = paste0("Latest quarter: Q", ceiling(month(max(earnings_latest$date)) / 3), " ", year(max(earnings_latest$date))),
    x = NULL,
    y = "Median Weekly Earnings",
    caption = "Source: Bureau of Labor Statistics via FRED"
  ) +
  theme_exploration() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "fig_09_earnings_snapshot.png"), p9,
       width = 8, height = 5, dpi = 300, bg = "white")

cat("Saved: fig_09_earnings_snapshot.png\n")

# ==============================================================================
cat("\n")
cat("=" |> strrep(60), "\n")
cat("Analysis complete.\n")
cat("=" |> strrep(60), "\n")
