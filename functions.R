# =============================================================================
# Common Functions for Weekly Data Explorations
# Preston Magouirk
# =============================================================================

library(tidyverse)
library(scales)

# -----------------------------------------------------------------------------
# Package Management
# -----------------------------------------------------------------------------

# Install exploration dependencies if missing
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  }
}

# Core packages for all explorations
core_packages <- c(
"tidyverse", "janitor", "scales", "here", "glue", "fs"
)

# Domain-specific packages
sports_packages <- c("nflverse", "nbastatR", "hoopR", "baseballr", "future")
econ_packages <- c("fredr", "tidycensus", "blsR", "wbstats")
health_packages <- c("RNHANES", "cdcfluview")
edu_packages <- c("rscorecard")
society_packages <- c("gssr", "owidR")

# -----------------------------------------------------------------------------
# Brand Colors
# -----------------------------------------------------------------------------

colors_brand <- c(
  primary   = "#1F3D2B",
  secondary = "#7A1E2C",
  accent1   = "#4A7C59",
  accent2   = "#A85D68",
  neutral   = "#666666",
  light     = "#F5F5F4"
)

# Extended palette for multi-category plots
colors_extended <- c(
  "#1F3D2B",
  "#7A1E2C",
  "#4A7C59",
  "#A85D68",
  "#2E5A3F",
  "#8B4555",
  "#3D6B4D",
  "#996070"
)

# Sequential palette (light to dark green)
colors_seq_green <- c(
  "#E8F0EA",
  "#B8D4BE",
  "#88B892",
  "#589C66",
  "#1F3D2B"
)

# Diverging palette (burgundy to green)
colors_div <- c(
  "#7A1E2C",
  "#A85D68",
  "#D4A6AE",
  "#F5F5F4",
  "#B8D4BE",
  "#4A7C59",
  "#1F3D2B"
)

# -----------------------------------------------------------------------------
# Custom ggplot2 Theme
# -----------------------------------------------------------------------------

theme_exploration <- function(base_size = 12, base_family = "") {
  
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Title and subtitle
      plot.title = element_text(
        face = "bold",
        size = rel(1.3),
        color = colors_brand["primary"],
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = rel(0.95),
        color = "#666666",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        size = rel(0.75),
        color = "#999999",
        hjust = 0,
        margin = margin(t = 12)
      ),
      
      # Panel
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Axes
      axis.title = element_text(size = rel(0.85), color = "#333333"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = rel(0.8), color = "#666666"),
      axis.ticks = element_line(color = "#CCCCCC", linewidth = 0.3),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.8)),
      legend.key.size = unit(0.8, "lines"),
      legend.background = element_blank(),
      
      # Facets
      strip.text = element_text(
        face = "bold",
        size = rel(0.9),
        color = colors_brand["primary"],
        margin = margin(b = 4, t = 4)
      ),
      strip.background = element_rect(fill = "#F5F5F4", color = NA),
      
      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# -----------------------------------------------------------------------------
# Scale Functions
# -----------------------------------------------------------------------------

scale_color_brand <- function(...) {
  scale_color_manual(values = colors_extended, ...)
}

scale_fill_brand <- function(...) {
  scale_fill_manual(values = colors_extended, ...)
}

scale_color_seq <- function(...) {
  scale_color_gradientn(colors = colors_seq_green, ...)
}

scale_fill_seq <- function(...) {
  scale_fill_gradientn(colors = colors_seq_green, ...)
}

scale_color_div <- function(...) {
  scale_color_gradientn(colors = colors_div, ...)
}

scale_fill_div <- function(...) {
  scale_fill_gradientn(colors = colors_div, ...)
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

# Format numbers for display
fmt_num <- function(x, accuracy = 1) {
  scales::number(x, accuracy = accuracy, big.mark = ",")
}

# Format percentages
fmt_pct <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy)
}

# Format currency
fmt_dollar <- function(x, accuracy = 1) {
  scales::dollar(x, accuracy = accuracy, big.mark = ",")
}

# Clean column names and basic prep
prep_data <- function(df) {
  df |>
    janitor::clean_names() |>
    janitor::remove_empty(c("rows", "cols"))
}

# Save plot for web with standard settings
save_plot_web <- function(plot, filename, width = 8, height = 5, dpi = 150) {
  ggsave(
    filename,
    plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

# Save plot for print
save_plot_print <- function(plot, filename, width = 8, height = 5) {
  ggsave(
    filename,
    plot,
    width = width,
    height = height,
    device = cairo_pdf
  )
}

# Quick data summary
quick_summary <- function(df) {
  cat("Rows:", nrow(df), "\n")
  cat("Cols:", ncol(df), "\n")
  cat("\nColumn types:\n")
  df |> 
    summarise(across(everything(), class)) |> 
    pivot_longer(everything(), names_to = "column", values_to = "type") |> 
    count(type) |> 
    print()
  cat("\nMissing values:\n")
  df |> 
    summarise(across(everything(), ~sum(is.na(.)))) |> 
    pivot_longer(everything(), names_to = "column", values_to = "missing") |> 
    filter(missing > 0) |> 
    arrange(desc(missing)) |> 
    print()
}

# -----------------------------------------------------------------------------
# Project Paths
# -----------------------------------------------------------------------------

# Use with here::here() for project-relative paths
# Example: here("explorations", "2025-03_topic", "data", "raw")

# -----------------------------------------------------------------------------
# Sports Data Helpers
# -----------------------------------------------------------------------------

# Load NFL play-by-play with common filters
load_nfl_pbp <- function(seasons, play_types = c("pass", "run")) {
  if (!requireNamespace("nflverse", quietly = TRUE)) {
    stop("Install nflverse: install.packages('nflverse')")
  }
  
  nflverse::load_pbp(seasons = seasons) |>
    filter(
      !is.na(epa),
      play_type %in% play_types
    )
}

# NFL team EPA summary
nfl_team_epa <- function(pbp) {
  pbp |>
    group_by(season, posteam) |>
    summarize(
      plays = n(),
      epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      pass_rate = mean(play_type == "pass", na.rm = TRUE),
      .groups = "drop"
    )
}

# Setup parallel processing for nbastatR
setup_nba_parallel <- function() {
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Install future: install.packages('future')")
  }
  future::plan(future::multisession)
  message("Parallel processing enabled for NBA data fetches")
}

# -----------------------------------------------------------------------------
# Time Bucket Helpers (useful for sports pace/tempo analysis)
# -----------------------------------------------------------------------------

# Create time buckets for game analysis
bucket_game_time <- function(df, time_col, period_col, bucket_size = 2) {
  df |>
    mutate(
      time_bucket = floor({{ time_col }} / 60 / bucket_size) * bucket_size,
      game_minute = ({{ period_col }} - 1) * 12 + (12 - {{ time_col }} / 60)
    )
}

# Summarize by time bucket
summarize_by_bucket <- function(df, bucket_col, ...) {
  df |>
    group_by({{ bucket_col }}) |>
    summarize(..., .groups = "drop")
}
