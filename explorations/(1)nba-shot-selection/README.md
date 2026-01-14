# NBA Shot Selection: The Three-Point Revolution

## Question

How has NBA shot selection evolved over the past 20 years (2005-06 through 2024-25)? Which teams led the shift toward three-point shooting, and did embracing the three-pointer correlate with improved winning?

## Key Findings

1. **League-wide transformation**: 3-point attempts as a share of total field goals increased from 20.2% in 2006 to 42.1% in 2025—a 21.9 percentage point increase that more than doubled the league's reliance on three-pointers over 20 years.

2. **Early adopters**: Teams like the Houston Rockets and Golden State Warriors led the charge in the early era (2006-2015), establishing the foundation for what became a league-wide shift.

3. **Convergence**: The gap between 3pt-heavy and 3pt-light teams narrowed dramatically as the entire league adapted to the three-point revolution, with team-level variation decreasing significantly over time.

4. **Volume vs efficiency**: Despite the massive increase in 3pt volume, league-wide 3pt shooting percentage remained relatively stable, suggesting teams successfully scaled up three-point attempts without sacrificing efficiency.

5. **Regular season vs playoffs**: The three-point revolution occurred simultaneously in both regular season and playoffs, with teams maintaining similar shot selection patterns across both contexts.

6. **Championship teams**: Championship teams (those with the most playoff wins) generally tracked closely with league-average 3pt share over time, suggesting that the revolution was embraced broadly rather than being driven by the most successful teams.

## Data Source

- **Source**: NBA Stats API via `nbastatR` R package
- **Coverage**: 20 NBA seasons (2005-06 through 2024-25)
- **Granularity**: Team-level box scores for all regular season and playoff games
- **Key metrics**: FGA, FG3A, FGM, FG3M, wins/losses, playoff performance

## Methodology

### Shot Share Calculation
- **3pt share** = 3-point attempts / total field goal attempts
- **2pt share** = (FGA - FG3A) / FGA

### Team Change Analysis
- Compare each team's shot share from their first to most recent available season
- Calculate within-team trends using season-over-season regression

### Win Correlation
- Correlate team-level shot selection changes with win percentage changes
- Control for shooting efficiency when modeling win outcomes

## Scripts

### Regular Season Analysis

| Script | Purpose |
|--------|---------|
| `01_collect.R` | Fetch 20 years of team game logs from NBA Stats API |
| `02_clean.R` | Calculate shot share metrics at game, team-season, and league levels |
| `03_analyze.R` | Identify trends, early adopters, and shot-selection/winning relationships |
| `04_visualize.R` | Create publication-ready charts (figures 1-6) |

### Playoff Analysis

| Script | Purpose |
|--------|---------|
| `05_collect_playoffs.R` | Fetch playoff game logs from NBA Stats API |
| `06_analyze_playoffs.R` | Compare playoff vs regular season trends, analyze championship teams |
| `07_visualize_playoffs.R` | Create playoff analysis visualizations (figures 7-12) |

## Running the Analysis

### Regular Season Analysis

```r
# From project root
source("explorations/(1)nba-shot-selection/scripts/01_collect.R")
source("explorations/(1)nba-shot-selection/scripts/02_clean.R")
source("explorations/(1)nba-shot-selection/scripts/03_analyze.R")
source("explorations/(1)nba-shot-selection/scripts/04_visualize.R")
```

### Playoff Analysis

```r
# After completing regular season analysis
source("explorations/(1)nba-shot-selection/scripts/05_collect_playoffs.R")
source("explorations/(1)nba-shot-selection/scripts/06_analyze_playoffs.R")
source("explorations/(1)nba-shot-selection/scripts/07_visualize_playoffs.R")
```

**Note**: Data collection scripts (`01_collect.R` and `05_collect_playoffs.R`) require API calls and may take several minutes. Subsequent scripts run on saved data.

## Output Files

### Data

**Regular Season:**
- `data/raw/nba_team_game_logs_2006_2025.csv` - Raw API response
- `data/processed/game_level.csv` - Game-level with derived metrics
- `data/processed/team_season.csv` - Team-season aggregations
- `data/processed/league_season.csv` - League-season aggregations
- `data/processed/analysis_results.rds` - Analysis objects for visualization

**Playoffs:**
- `data/raw/nba_playoff_game_logs_2006_2025.csv` - Raw playoff API response
- `data/processed/playoff_game_level.csv` - Playoff game-level metrics
- `data/processed/playoff_team_season.csv` - Playoff team-season aggregations
- `data/processed/playoff_league_season.csv` - Playoff league-season aggregations
- `data/processed/playoff_analysis_results.rds` - Playoff analysis objects

### Figures

**Regular Season Analysis (1-6):**
- `fig_01_league_trend.png` - League-wide 3pt share over time
- `fig_02_team_spread.png` - Team dispersion narrowing over time
- `fig_03_biggest_changers.png` - Teams with largest shot selection changes
- `fig_04_shot_vs_wins.png` - Shot selection change vs win change scatter
- `fig_05_team_trajectories.png` - Individual team paths (highlighted teams)
- `fig_06_volume_vs_efficiency.png` - Volume vs accuracy tradeoff

**Playoff Analysis (7-12):**
- `fig_07_rs_vs_playoffs.png` - Regular season vs playoffs trend comparison
- `fig_08_playoff_adjustment.png` - Team playoff shot selection adjustments
- `fig_09_champions_vs_league.png` - Championship teams vs league average
- `fig_10_playoff_success.png` - Playoff wins vs shot selection
- `fig_11_champ_trend.png` - Champion 3pt share trend over time
- `fig_12_era_adjustment.png` - Playoff adjustment by era

## Dependencies

```r
library(tidyverse)
library(nbastatR)
library(future)
library(here)
library(janitor)
library(broom)
library(scales)
library(ggrepel)
```
