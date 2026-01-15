# =============================================================================
# 04_visualize.R - CPI Food Price Visualizations
# Creates publication-ready charts for the exploration
# =============================================================================

library(tidyverse)
library(here)
library(scales)
library(ggrepel)
library(lubridate)

source(here("functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

processed_path <- here("explorations", "(2)wholefood-inflation", "data", "processed")
output_path <- here("explorations", "(2)wholefood-inflation", "output", "figures")

results <- readRDS(file.path(processed_path, "analysis_results.rds"))

category_monthly <- results$category_monthly
category_annual <- results$category_annual
items_annual <- results$items_annual
item_changes <- results$item_changes
category_gap <- results$category_gap
dc_comparison <- results$dc_comparison
dc_summary <- results$dc_summary

# -----------------------------------------------------------------------------
# Figure 1: Overall Food Price Trends
# -----------------------------------------------------------------------------

p_overall_trend <- category_annual |>
  filter(region == "National") |>
  ggplot(aes(x = year, y = avg_pct_change, colour = category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_discrete(
  # values = c("whole_food" = colors_brand[1],
  #            "processed_food" = colors_brand[2]),
    labels = c("whole_food" = "Whole Foods",
               "processed_food" = "Processed Foods")
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Food Price Inflation: Whole Foods vs Processed Foods",
    subtitle = "Percent change from 2015 baseline (2015 = 0%)",
    x = "Year",
    y = "Price Change (%)",
    color = NULL,
    caption = "Source: FRED CPI Data via Bureau of Labor Statistics"
  ) +
  theme_exploration() +
  theme(legend.position = "bottom")

save_plot_web(p_overall_trend, file.path(output_path, "fig_01_overall_trend.png"))
cat("Saved: fig_01_overall_trend.png\n")

# -----------------------------------------------------------------------------
# Figure 2: Price Gap Over Time
# -----------------------------------------------------------------------------

p_gap_trend <- category_gap |>
  ggplot(aes(x = year_month, y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.2, color = colors_brand["primary"]) +
  geom_point(size = 2, color = colors_brand["primary"]) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              color = colors_brand["secondary"], alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  labs(
    title = "Price Gap: Whole Foods vs Processed Foods",
    subtitle = "Index difference (Whole Foods - Processed Foods); positive = whole foods more expensive",
    x = "Date",
    y = "Index Point Difference",
    caption = "Source: FRED CPI Data | 2015 = 100 baseline"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_gap_trend, file.path(output_path, "fig_02_price_gap.png"))
cat("Saved: fig_02_price_gap.png\n")

# -----------------------------------------------------------------------------
# Figure 3: Item-Level Changes (2015-2024)
# -----------------------------------------------------------------------------

top_items <- item_changes |>
  arrange(desc(change_2015_2024)) |>
  head(10) |>
  mutate(
    item_label = str_replace_all(series_name, "_", " ") |>
      str_to_title(),
    group = "Top Increases"
  )

bottom_items <- item_changes |>
  arrange(change_2015_2024) |>
  head(10) |>
  mutate(
    item_label = str_replace_all(series_name, "_", " ") |>
      str_to_title(),
    group = "Smallest Increases"
  )

plot_items <- bind_rows(top_items, bottom_items) |>
  mutate(item_label = fct_reorder(item_label, change_2015_2024))

p_item_changes <- plot_items |>
  ggplot(aes(x = change_2015_2024, y = item_label, fill = category)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(
    values = c("whole_food" = colors_brand["primary"],
               "processed_food" = colors_brand["secondary"]),
    labels = c("whole_food" = "Whole Food",
               "processed_food" = "Processed Food")
  ) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Price Changes by Food Item (2015-2024)",
    subtitle = "Top and bottom 10 items by price change",
    x = "Price Change (%)",
    y = NULL,
    fill = "Category",
    caption = "Source: FRED CPI Data"
  ) +
  theme_exploration() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

save_plot_web(p_item_changes, file.path(output_path, "fig_03_item_changes.png"),
              width = 9, height = 6)
cat("Saved: fig_03_item_changes.png\n")

# -----------------------------------------------------------------------------
# Figure 4: Category Comparison Bar Chart
# -----------------------------------------------------------------------------

p_category_comparison <- category_annual |>
  filter(year %in% c(2015, 2024), region == "National") |>
  select(year, category, avg_pct_change) |>
  pivot_wider(names_from = year, values_from = avg_pct_change, names_prefix = "year_") |>
  mutate(
    total_change = year_2024 - year_2015,
    category_label = if_else(category == "whole_food", "Whole Foods", "Processed Foods")
  ) |>
  ggplot(aes(x = category_label, y = total_change, fill = category)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%+.1f%%", total_change)),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c("whole_food" = colors_brand["primary"],
               "processed_food" = colors_brand["secondary"]),
    guide = "none"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Total Price Change: Whole Foods vs Processed Foods",
    subtitle = "2015-2024 price change comparison",
    x = NULL,
    y = "Total Price Change (%)",
    caption = "Source: FRED CPI Data"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_category_comparison, file.path(output_path, "fig_04_category_comparison.png"))
cat("Saved: fig_04_category_comparison.png\n")

# -----------------------------------------------------------------------------
# Figure 5: DC Regional Comparison
# -----------------------------------------------------------------------------

dc_comparison_annual <- dc_comparison |>
  mutate(year = year(year_month)) |>
  group_by(year, series_base) |>
  summarize(
    avg_premium = mean(dc_premium_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarize(
    n_series = n(),
    mean_premium = mean(avg_premium, na.rm = TRUE),
    .groups = "drop"
  )

p_dc_comparison <- dc_comparison_annual |>
  ggplot(aes(x = year, y = mean_premium)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_col(fill = colors_brand["primary"], width = 0.7, alpha = 0.8) +
  geom_text(
    aes(label = sprintf("%+.1f%%", mean_premium)),
    vjust = if_else(dc_comparison_annual$mean_premium > 0, -0.5, 1.5),
    size = 3.5
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "DC Food Prices vs National Average",
    subtitle = "DC premium (positive = DC more expensive than national average)",
    x = "Year",
    y = "DC Premium (%)",
    caption = "Source: FRED CPI Data | DC area vs U.S. city average"
  ) +
  theme_exploration() +
  theme(panel.grid.major.x = element_blank())

save_plot_web(p_dc_comparison, file.path(output_path, "fig_05_dc_comparison.png"))
cat("Saved: fig_05_dc_comparison.png\n")

# -----------------------------------------------------------------------------
# Figure 6: Monthly Trend Detail
# -----------------------------------------------------------------------------

p_monthly_detail <- category_monthly |>
  filter(region == "National") |>
  ggplot(aes(x = year_month, y = avg_index, color = category)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
  scale_color_manual(
    values = c("whole_food" = colors_brand["primary"],
               "processed_food" = colors_brand["secondary"]),
    labels = c("whole_food" = "Whole Foods",
               "processed_food" = "Processed Foods")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(90, 130), breaks = seq(90, 130, 10)) +
  labs(
    title = "Food Price Index Over Time",
    subtitle = "Index level (2015 = 100) with monthly detail",
    x = "Date",
    y = "Price Index (2015 = 100)",
    color = NULL,
    caption = "Source: FRED CPI Data"
  ) +
  theme_exploration() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

save_plot_web(p_monthly_detail, file.path(output_path, "fig_06_monthly_detail.png"))
cat("Saved: fig_06_monthly_detail.png\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("All visualizations saved to:", output_path, "\n")
cat("Figures created:\n")
cat("  1. Overall food price trends\n")
cat("  2. Price gap over time\n")
cat("  3. Item-level changes\n")
cat("  4. Category comparison\n")
cat("  5. DC regional comparison\n")
cat("  6. Monthly trend detail\n")
cat("\n", strrep("=", 60), "\n")

