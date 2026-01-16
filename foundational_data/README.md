# Foundational Data Infrastructure

Single source of truth for refreshable public data, standard segmentation, and ready-to-use data marts for all exploration projects.

## Purpose

This infrastructure provides:
- **Consistent segmentation** across all projects (age, education, income, household type)
- **Refreshable data pipelines** for FRED, CPS, and HPS
- **Pre-built data marts** that exploration projects import directly

## Quick Start

```r
# Load config and helpers
source(here::here("foundational_data", "R", "00_config.R"))

# Read a mart
cps <- read_mart("cps")
fred <- read_mart("fred")
hps <- read_mart("hps")

# Filter to DC
dc_labor <- cps |> filter(state_fips == DC_FIPS)
```

## Data Marts

| Mart | Description | Grain | Key Variables |
|------|-------------|-------|---------------|
| `mart_fred_month.parquet` | Macro context | series × month | CPI, unemployment, sentiment |
| `mart_cps_state_month.parquet` | DC/state labor | state × month × segment | Unemployment, LFPR, hours |
| `mart_hps_national.parquet` | National hardship + sentiment | wave × segment | Hardship rates, PHQ-2, GAD-2 |

## Segmentation

All marts use consistent segmentation bins:

- **age_group:** 18-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75+
- **educ_group:** LE_HS, SOME_COLLEGE, BA, GRAD
- **income_group:** Q1-Q5 (weighted quintiles)
- **hh_type:** MARRIED_CHILDREN, MARRIED_NO_CHILDREN, SINGLE_CHILDREN, SINGLE_NO_CHILDREN, OTHER_MULTIADULT

See `docs/segmentation_specs.md` for details.

## Directory Structure

```
foundational_data/
├── R/
│   ├── 00_config.R          # Paths, constants, helpers
│   ├── 01_ingest_hps.R      # Download/unzip HPS PUFs
│   ├── 02_clean_hps.R       # Build HPS mart
│   ├── 03_ingest_cps_ipums.R # Read IPUMS CPS extracts
│   ├── 04_clean_cps.R       # Build CPS mart
│   ├── 05_ingest_fred.R     # Pull FRED series
│   └── 06_build_marts.R     # Validation
├── docs/
│   ├── segmentation_specs.md
│   ├── runbook.md
│   └── data_dictionary_internal.md
├── data_raw/                # Landing zone for source files
├── data_clean/              # Intermediate cleaned data
└── data_mart/               # Output marts (import from here)
```

## Refresh Cadence

| Frequency | Action |
|-----------|--------|
| Weekly | Run `05_ingest_fred.R` |
| Biweekly | Download new HPS PUF, run `01_ingest_hps.R` + `02_clean_hps.R` |
| Monthly | Download new IPUMS CPS, run `03_ingest_cps_ipums.R` + `04_clean_cps.R` |

See `docs/runbook.md` for detailed instructions.

## For Exploration Projects

All projects in `explorations/(x)_...` should:

1. **Import from marts only:**
   ```r
   source(here::here("foundational_data", "R", "00_config.R"))
   cps <- read_mart("cps")
   ```

2. **Use pre-computed segmentation** — never redefine bins

3. **Filter as needed:**
   ```r
   # DC only
   dc_data <- cps |> filter(state_fips == DC_FIPS)

   # Young adults
   young <- cps |> filter(age_group == "18-24")
   ```

## Reproducibility (Bring Your Own Data)

**No data files are stored in this repository** — only scripts and documentation. This ensures compliance with IPUMS terms of use.

To reproduce the data marts locally:

### 1. FRED (5 minutes)
- Get free API key: https://fred.stlouisfed.org/docs/api/api_key.html
- Add to `~/.Renviron`: `FRED_API_KEY=your_key`
- Run: `source(here::here("foundational_data", "R", "05_ingest_fred.R"))`

### 2. IPUMS CPS (30 minutes)
- Create account: https://cps.ipums.org/
- Create extract with required variables (see `docs/runbook.md`)
- Download `.xml` + `.dat.gz` to `data_raw/cps_ipums/`
- Run: `03_ingest_cps_ipums.R` then `04_clean_cps.R`

### 3. Census HPS (15 minutes)
- Download PUF ZIP: https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html
- Place in `data_raw/hps/`
- Run: `01_ingest_hps.R` then `02_clean_hps.R`

See `DATA_LICENSES.md` for full terms of use.

## Data Sources

- **FRED:** https://fred.stlouisfed.org/
- **IPUMS CPS:** https://cps.ipums.org/
- **Household Pulse Survey:** https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html
