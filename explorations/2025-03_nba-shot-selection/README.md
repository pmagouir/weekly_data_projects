# NBA Shot Selection: The Three-Point Revolution

## Question

How has NBA shot selection evolved over the past 20 years (2005-06 through 2024-25)? Which teams led the shift toward three-point shooting, and did embracing the three-pointer correlate with improved winning?

## Key Findings

*(To be completed after running analysis)*

1. **League-wide transformation**: 3-point attempts as a share of total field goals increased from ~X% in 2006 to ~Y% in 2025
2. **Early adopters**: Teams like the Houston Rockets and Golden State Warriors led the charge
3. **Convergence**: The gap between 3pt-heavy and 3pt-light teams narrowed dramatically
4. **Winning correlation**: Relationship between shot selection change and win percentage change

## Data Source

- **Source**: NBA Stats API via `nbastatR` R package
- **Coverage**: 20 NBA seasons (2005-06 through 2024-25)
- **Granularity**: Team-level box scores for all regular season games
- **Key metrics**: FGA, FG3A, FGM, FG3M, wins/losses

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

| Script | Purpose |
|--------|---------|
| `01_collect.R` | Fetch 20 years of team game logs from NBA Stats API |
| `02_clean.R` | Calculate shot share metrics at game, team-season, and league levels |
| `03_analyze.R` | Identify trends, early adopters, and shot-selection/winning relationships |
| `04_visualize.R` | Create publication-ready charts |

## Running the Analysis

```r
# From project root
source("explorations/2025-03_nba-shot-selection/scripts/01_collect.R")
source("explorations/2025-03_nba-shot-selection/scripts/02_clean.R")
source("explorations/2025-03_nba-shot-selection/scripts/03_analyze.R")
source("explorations/2025-03_nba-shot-selection/scripts/04_visualize.R")
```

**Note**: Data collection (`01_collect.R`) requires API calls and may take several minutes. Subsequent scripts run on saved data.

## Output Files

### Data
- `data/raw/nba_team_game_logs_2006_2025.csv` - Raw API response
- `data/processed/game_level.csv` - Game-level with derived metrics
- `data/processed/team_season.csv` - Team-season aggregations
- `data/processed/league_season.csv` - League-season aggregations
- `data/processed/analysis_results.rds` - Analysis objects for visualization

### Figures
- `fig_01_league_trend.png` - League-wide 3pt share over time
- `fig_02_team_spread.png` - Team dispersion narrowing over time
- `fig_03_biggest_changers.png` - Teams with largest shot selection changes
- `fig_04_shot_vs_wins.png` - Shot selection change vs win change scatter
- `fig_05_team_trajectories.png` - Individual team paths (highlighted teams)
- `fig_06_volume_vs_efficiency.png` - Volume vs accuracy tradeoff

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
