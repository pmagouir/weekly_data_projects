# Post-Secondary Employment Outcomes (PSEO) Analysis

## Question

What do earnings outcomes reveal about the value of different degrees, institutions, and fields of study? How do graduate earnings vary by degree level, major, and institution, and what factors predict higher earnings trajectories 1, 5, and 10 years post-graduation?

## Key Findings

*(To be completed after running analysis)*

1. **Earnings variation by field**: [Findings on CIP codes]
2. **Degree level returns**: [Bachelor's vs Master's vs Doctoral earnings]
3. **Institutional differences**: [Elite vs regional institutions]
4. **Geographic mobility**: [In-state retention vs out-migration patterns]
5. **Industry pathways**: [Which fields lead to which industries]

## Data Source

- **Source**: U.S. Census Bureau LEHD Post-Secondary Employment Outcomes (PSEO)
- **URL**: https://lehd.ces.census.gov/data/pseo_experimental.html
- **Coverage**: 825 institutions (~29% of all college graduates)
- **Time horizon**: Earnings measured at 1, 5, and 10 years post-graduation
- **Graduation cohorts**: 2001-2019 (3-year cohorts for Bachelor's, 5-year for others)
- **Last updated**: 2025-06-18 (R2025Q2 release)

## Data Files

### Primary Files (from `latest_release/all/`)

| File | Description | Size |
|------|-------------|------|
| `pseoe_all.csv.gz` | Graduate earnings (all institutions) | 11 MB |
| `pseof_all.csv.gz` | Employment flows (all institutions) | 161 MB |
| `pseo_all_institutions.csv` | Institution lookup table | 42 KB |
| `pseo_all_partners.txt` | Partner institution list | 3.6 KB |

### Lookup Tables (from schema)

| File | Description |
|------|-------------|
| `label_degree_level.csv` | Degree level codes (01-18) |
| `label_cipcode.csv` | CIP codes for degree fields |
| `label_geography.csv` | Geographic identifiers |
| `label_industry.csv` | NAICS industry codes |

## Data Dictionary

### Earnings Variables (PSEOE)

| Variable | Description |
|----------|-------------|
| `y1_p25_earnings` | 25th percentile earnings, Year 1 |
| `y1_p50_earnings` | Median earnings, Year 1 |
| `y1_p75_earnings` | 75th percentile earnings, Year 1 |
| `y1_grads_earn` | Count of employed graduates, Year 1 |
| `y5_p25_earnings` | 25th percentile earnings, Year 5 |
| `y5_p50_earnings` | Median earnings, Year 5 |
| `y5_p75_earnings` | 75th percentile earnings, Year 5 |
| `y5_grads_earn` | Count of employed graduates, Year 5 |
| `y10_p25_earnings` | 25th percentile earnings, Year 10 |
| `y10_p50_earnings` | Median earnings, Year 10 |
| `y10_p75_earnings` | 75th percentile earnings, Year 10 |
| `y10_grads_earn` | Count of employed graduates, Year 10 |

*Note: All earnings are inflation-adjusted to 2023 dollars using CPI-U.*

### Flows Variables (PSEOF)

| Variable | Description |
|----------|-------------|
| `y1_grads_emp` | Total employed graduates, Year 1 |
| `y1_grads_emp_instate` | Employed in-state, Year 1 |
| `y1_grads_nme` | Not employed/marginally employed, Year 1 |
| `y5_grads_emp` | Total employed graduates, Year 5 |
| `y5_grads_emp_instate` | Employed in-state, Year 5 |
| `y5_grads_nme` | Not employed/marginally employed, Year 5 |
| `y10_grads_emp` | Total employed graduates, Year 10 |
| `y10_grads_emp_instate` | Employed in-state, Year 10 |
| `y10_grads_nme` | Not employed/marginally employed, Year 10 |

### Key Identifiers

| Variable | Description | Example |
|----------|-------------|---------|
| `institution` | 8-digit OPEID code | "00365800" (UT Austin) |
| `degree_level` | 2-digit degree code | "05" (Bachelor's) |
| `cipcode` | CIP code for major | "11.01" (Computer Science) |
| `grad_cohort` | Graduation year | 2016 |
| `geography` | Geographic code | State/Division |
| `industry` | 2-digit NAICS | "54" (Professional Services) |

### Degree Level Codes

| Code | Label |
|------|-------|
| 00 | All Degree Levels |
| 01 | Certificate < 1 year |
| 02 | Certificate 1-2 years |
| 03 | Associates |
| 04 | Certificate 2-4 years |
| 05 | Baccalaureate |
| 06 | Post-Bacc Certificate |
| 07 | Masters |
| 08 | Post-Masters Certificate |
| 17 | Doctoral - Research/Scholarship |
| 18 | Doctoral - Professional Practice |

### Key CIP Codes (2-digit)

| Code | Field |
|------|-------|
| 01 | Agriculture |
| 09 | Communication |
| 11 | Computer Science |
| 13 | Education |
| 14 | Engineering |
| 22 | Legal Professions |
| 24 | Liberal Arts |
| 26 | Biological Sciences |
| 27 | Mathematics |
| 40 | Physical Sciences |
| 45 | Social Sciences |
| 51 | Health Professions |
| 52 | Business |
| 54 | History |

## Methodology

### Data Collection
- Bulk CSV downloads from Census LEHD data portal
- Lookup tables for institution names, CIP codes, degree levels
- No API key required for bulk downloads

### Analysis Approach
- Compare earnings distributions across degree levels and fields
- Track earnings growth trajectories (Y1 → Y5 → Y10)
- Analyze geographic mobility patterns (in-state retention)
- Examine industry placement by degree field
- Control for institution type and selectivity

### Limitations
- Coverage: Only ~29% of graduates (participating institutions)
- Selection: Institutions self-select to participate
- Suppression: Small cells suppressed for privacy
- Geographic: Limited to graduates with US employment
- Timing: 3-year cohorts smooth over economic cycles

## Scripts

| Script | Purpose |
|--------|---------|
| `01_collect.R` | Download PSEO data files from Census |
| `02_clean.R` | Process and join earnings/flows data |
| `03_analyze.R` | Statistical analysis and comparisons |
| `04_visualize.R` | Publication-ready charts |

## Running the Analysis

```r
# From project root
source("explorations/(3)PSEO_data_project/scripts/01_collect.R")
source("explorations/(3)PSEO_data_project/scripts/02_clean.R")
source("explorations/(3)PSEO_data_project/scripts/03_analyze.R")
source("explorations/(3)PSEO_data_project/scripts/04_visualize.R")
```

## Output Files

### Data
- `data/raw/pseoe_all.csv` - Raw earnings data
- `data/raw/pseof_all.csv` - Raw flows data
- `data/raw/institutions.csv` - Institution lookup
- `data/processed/earnings_clean.csv` - Processed earnings
- `data/processed/flows_clean.csv` - Processed flows
- `data/processed/analysis_results.rds` - Analysis objects

### Figures
- TBD based on analysis direction

## Dependencies

```r
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales)
library(broom)
library(ggrepel)
```

## Security & Privacy

### Data Sources
- **All data is public**: PSEO is a public-use dataset from Census Bureau
- **No PII**: Data is aggregated with cell suppression for privacy
- **No API key required**: Bulk downloads are freely available
- **Official source**: U.S. Census Bureau LEHD program

### Repository Safety
- All code is transparent and readable
- No credentials or secrets required
- Only public data is accessed
- All sources documented

## Key Resources

- [PSEO Main Page](https://www.census.gov/data/experimental-data-products/post-secondary-employment-outcomes.html)
- [PSEO Explorer Tool](https://census.gov/data/data-tools/pseo.html)
- [API Documentation](https://www.census.gov/data/developers/data-sets/pseo.html)
- [LEHD Documentation](https://lehd.ces.census.gov/data/pseo_documentation.html)
- [Data Schema](https://lehd.ces.census.gov/data/schema/latest/lehd_public_use_schema.html)

## Contact

For PSEO-specific questions: CES.PSEO.Feedback@census.gov
