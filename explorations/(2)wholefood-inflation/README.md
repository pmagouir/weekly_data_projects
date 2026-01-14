# Whole Food vs Processed Food Inflation Analysis

## Question

How have prices changed for whole foods (meats, fruits, vegetables, nuts, beans, eggs, milk/yogurt) compared to processed and junk foods (sweets, candy, desserts, frozen foods, processed/prepared foods) from 2015-2024? How does the Washington, DC region compare to national averages?

## Key Findings

*(To be completed after running analysis)*

1. **Overall food inflation**: Food prices increased by X% from 2015 to 2024
2. **Whole foods vs processed foods**: Whole foods increased by X% while processed foods increased by X%
3. **Price gap**: The gap between whole foods and processed foods [widened/narrowed] over the period
4. **Item-level variation**: [Specific items] saw the largest price increases
5. **DC regional premium**: DC food prices are X% higher than the national average

## Data Source

- **Source**: Bureau of Labor Statistics Consumer Price Index (CPI) data via FRED API
- **Coverage**: 2015-2024 (10 years)
- **Granularity**: Monthly CPI data by food category
- **Regions**: U.S. city average and Washington, DC area
- **Key metrics**: CPI index values and percent changes

## Methodology

### Data Collection
- CPI data collected via FRED API using `fredr` R package
- Monthly frequency data for food categories
- Series IDs verified from FRED database

### Categories
- **Whole Foods**: Meats, fruits, vegetables, nuts, beans, eggs, milk, yogurt
- **Processed Foods**: Sweets, candy, desserts, frozen foods, processed/prepared foods

### Analysis Approach
- Index baseline: 2015 = 100 for all series
- Calculate percent changes from 2015 baseline
- Compare whole foods vs processed foods categories
- Analyze DC regional data vs national averages
- Identify trends and patterns over time

## Scripts

| Script | Purpose |
|--------|---------|
| `01_collect.R` | Fetch CPI food price data from FRED API (2015-2024) |
| `02_clean.R` | Process and categorize CPI data, create indices |
| `03_analyze.R` | Analyze trends, compare categories, regional analysis |
| `04_visualize.R` | Create publication-ready charts |

## Running the Analysis

```r
# From project root
# Note: Requires FRED API key (free at https://fred.stlouisfed.org/docs/api/api_key.html)
# Store in .Renviron: FRED_API_KEY=your_key_here

source("explorations/(2)wholefood-inflation/scripts/01_collect.R")
source("explorations/(2)wholefood-inflation/scripts/02_clean.R")
source("explorations/(2)wholefood-inflation/scripts/03_analyze.R")
source("explorations/(2)wholefood-inflation/scripts/04_visualize.R")
```

**Note**: 
- Data collection (`01_collect.R`) requires a free FRED API key
- Series IDs in `01_collect.R` may need to be verified/updated from the FRED website
- Subsequent scripts run on saved data

## Output Files

### Data
- `data/raw/cpi_food_data_2015_2024.csv` - Raw CPI data from FRED API
- `data/processed/category_monthly.csv` - Monthly aggregates by category
- `data/processed/category_annual.csv` - Annual aggregates by category
- `data/processed/items_monthly.csv` - Individual item monthly data
- `data/processed/items_annual.csv` - Individual item annual data
- `data/processed/dc_comparison.csv` - DC vs national comparison
- `data/processed/analysis_results.rds` - Analysis objects for visualization

### Figures
- `fig_01_overall_trend.png` - Overall food price trends (whole vs processed)
- `fig_02_price_gap.png` - Price gap between categories over time
- `fig_03_item_changes.png` - Item-level price changes (2015-2024)
- `fig_04_category_comparison.png` - Category comparison bar chart
- `fig_05_dc_comparison.png` - DC regional premium analysis
- `fig_06_monthly_detail.png` - Monthly trend detail with index levels

## Dependencies

```r
library(tidyverse)
library(fredr)      # FRED API access
library(here)
library(janitor)
library(lubridate)  # Date handling
library(broom)      # Model summaries
library(scales)     # Plot formatting
library(ggrepel)    # Label positioning
```

## Setup

1. Get a free FRED API key: https://fred.stlouisfed.org/docs/api/api_key.html
2. Add to your `.Renviron` file: `FRED_API_KEY=your_key_here`
3. Verify CPI series IDs in `01_collect.R` (search FRED database: https://fred.stlouisfed.org/)
4. Run scripts in order (01 → 02 → 03 → 04)

## Security & Privacy

### Data Sources
- **All data is public**: This project uses only publicly available Consumer Price Index (CPI) data from the Federal Reserve Economic Data (FRED) API
- **No PII**: No personal identifiable information is accessed, collected, or stored
- **Public data only**: All data sources are publicly available economic statistics
- **Official sources**: Data comes from the Bureau of Labor Statistics via FRED

### API Key Security
- **API keys stored securely**: Keys are stored in `.Renviron` file (not in code)
- **Never commit credentials**: The `.Renviron` file is excluded from git (via `.gitignore`)
- **Free API keys**: FRED API keys are free and only used for rate limiting (not authentication)
- **Local only**: API keys remain on your local machine

### Repository Safety
- ✅ All code is transparent and readable
- ✅ No hardcoded credentials or secrets
- ✅ Only public data is accessed
- ✅ All data sources are documented
- ✅ Safe for public repositories

See `SECURITY_AUDIT.md` for detailed security audit.

## Notes

- CPI series IDs in `01_collect.R` are examples and may need to be updated
- Search FRED database for correct series IDs for specific food categories
- DC regional series IDs may differ from national series
- Some food categories may not have separate CPI series (e.g., individual nuts or beans)
- Data collection respects FRED API rate limits
- **Never commit `.Renviron` file**: Contains API keys and should remain local

