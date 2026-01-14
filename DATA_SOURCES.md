# Data Sources Reference

A guide to preferred data sources and R packages for each domain. Each entry includes the source, recommended package, installation, and a minimal working example.

## Sports

### NFL (nflverse)

The nflverse ecosystem is the definitive R toolkit for NFL data. SportsDataverse explicitly defers NFL work here.

```r
# Install
install.packages("nflverse")

# Load
library(nflverse)
library(tidyverse)

# Play-by-play with EPA, WP, drive info (returns tibble)
pbp <- load_pbp(seasons = 2020:2024)

# Roster data
rosters <- load_rosters(seasons = 2024)

# Team info and colors
teams <- load_teams()

# Common analysis: EPA per play by team
pbp |>
  filter(season == 2024, !is.na(epa), play_type %in% c("pass", "run")) |>
  group_by(posteam) |>
  summarize(
    plays = n(),
    epa_per_play = mean(epa),
    success_rate = mean(success)
  ) |>
  arrange(desc(epa_per_play))
```

**Key functions**: `load_pbp()`, `load_rosters()`, `load_teams()`, `load_schedules()`, `load_player_stats()`

### NBA (nbastatR)
 
R interface to the NBA Stats API. Supports game-level and play-by-play data.

```r
# Install
install.packages("nbastatR")
# devtools::install_github("abresler/nbastatR") # for dev version

# Load
library(nbastatR)
library(tidyverse)

# Set up parallel processing for faster fetches
library(future)
plan(multisession)

# Get game logs for a season
games <- game_logs(
  seasons = 2024,
  result_types = "team"
)

# Play-by-play for specific games
pbp <- play_by_play(game_ids = c("0022400001", "0022400002"))

# Common analysis: team pace
games |>
  group_by(nameTeam) |>
  summarize(
    games = n(),
    avg_pace = mean(paceTeam, na.rm = TRUE),
    avg_pts = mean(ptsTeam)
  ) |>
  arrange(desc(avg_pace))
```

**Note**: NBA Stats API can be rate-limited. Use `Sys.sleep()` between large requests.

### College Basketball (hoopR)

```r
# Install
install.packages("hoopR")

library(hoopR)

# Men's CBB play-by-play
pbp <- load_mbb_pbp(seasons = 2024)

# Team box scores
box <- load_mbb_team_box(seasons = 2024)
```

### Baseball (baseballr)

```r
# Install
install.packages("baseballr")

library(baseballr)

# Statcast data
statcast <- statcast_search(
  start_date = "2024-04-01",
  end_date = "2024-04-30"
)

# FanGraphs leaderboards
fg <- fg_batter_leaders(x = 2024, y = 2024)
```

### Soccer (worldfootballR)

```r
# Install
install.packages("worldfootballR")

library(worldfootballR)

# Match results from FBref
epl <- fb_match_results(
  country = "ENG",
  gender = "M", 
  season_end_year = 2024,
  tier = "1st"
)
```

---

## Economics

### FRED (fredr)

Federal Reserve Economic Data: macro indicators, interest rates, employment.

```r
# Install
install.packages("fredr")

library(fredr)

# Set API key (get free at https://fred.stlouisfed.org/docs/api/api_key.html)
fredr_set_key("YOUR_API_KEY")

# Fetch a series
unemployment <- fredr(series_id = "UNRATE")

# Multiple series
series_ids <- c("UNRATE", "CPIAUCSL", "GDP")
data <- map_dfr(series_ids, fredr)
```

### Census / ACS (tidycensus)

American Community Survey and decennial census data.

```r
# Install
install.packages("tidycensus")

library(tidycensus)

# Set API key (get at https://api.census.gov/data/key_signup.html)
census_api_key("YOUR_API_KEY", install = TRUE)

# ACS 5-year estimates
income <- get_acs(
  geography = "county",
  state = "DC",
  variables = "B19013_001",  # Median household income
  year = 2022
)

# With geometry for mapping
dc_tracts <- get_acs(
  geography = "tract",
  state = "DC",
  variables = "B01003_001",  # Total population
  year = 2022,
  geometry = TRUE
)
```

### Bureau of Labor Statistics (blsR)

Employment, prices, productivity data.

```r
# Install
install.packages("blsR")

library(blsR)

# CPI data
cpi <- get_series("CUSR0000SA0", start_year = 2020, end_year = 2024)
```

---

## Health

### NHANES (RNHANES)

National Health and Nutrition Examination Survey.

```r
# Install
install.packages("RNHANES")

library(RNHANES)

# List available files
files <- nhanes_data_files()

# Download specific dataset
demo <- nhanes_load_data("DEMO_J", "2017-2018")
```

### CDC Flu Data (cdcfluview)

```r
# Install
install.packages("cdcfluview")

library(cdcfluview)

# ILI surveillance data
ili <- ilinet(region = "national")
```

---

## Education

### Urban Institute Education Data (educationdata)

IPEDS, CCD, College Scorecard via single API.

```r
# Install
devtools::install_github("UrbanInstitute/education-data-package-r")

library(educationdata)

# College completion rates
completion <- get_education_data(
  level = "college",
  source = "ipeds",
  topic = "completion-rates",
  filters = list(year = 2022)
)

# K-12 enrollment
enrollment <- get_education_data(
  level = "schools",
  source = "ccd",
  topic = "enrollment",
  filters = list(year = 2022, fips = 11)  # DC
)
```

### College Scorecard (rscorecard)

```r
# Install
install.packages("rscorecard")

library(rscorecard)

# Set API key
sc_key("YOUR_API_KEY")

# Query institutions
df <- sc_init() |>
  sc_filter(stabbr == "DC") |>
  sc_select(unitid, instnm, adm_rate, costt4_a) |>
  sc_year(2022) |>
  sc_get()
```

---

## Politics & Society

### General Social Survey (gssr)

```r
# Install
install.packages("gssr")

library(gssr)

# Load cumulative file
data(gss_all)

# Example: confidence in institutions over time
gss_all |>
  filter(!is.na(conlegis)) |>
  group_by(year) |>
  summarize(pct_confidence = mean(conlegis <= 2, na.rm = TRUE))
```

### Our World in Data (owidR)

```r
# Install
install.packages("owidR")

library(owidR)

# Search datasets
owid_search("life expectancy")

# Load specific dataset
life_exp <- owid("life-expectancy")
```

---

## API Keys

Many data sources require free API keys. Store them in your `.Renviron` file:

```
FRED_API_KEY=your_key_here
CENSUS_API_KEY=your_key_here
```

Then access with `Sys.getenv("FRED_API_KEY")`.

---

## Rate Limiting

When fetching large datasets, respect API limits:

```r
# Simple throttling
walk(game_ids, ~{
  result <- fetch_game(.x)
  Sys.sleep(1)  # 1 second between requests
})

# Or use polite rate limiting
library(ratelimitr)
fetch_limited <- limit_rate(fetch_game, rate(n = 10, period = 60))
```
