# Foundational Data Runbook

Operational guide for maintaining the foundational data infrastructure.

---

## Quick Reference

| Task | Script | Frequency | Prerequisites |
|------|--------|-----------|---------------|
| Refresh macro data | `05_ingest_fred.R` | Weekly | FRED API key |
| Ingest new HPS release | `01_ingest_hps.R` | When available | PUF ZIP downloaded |
| Build HPS mart | `02_clean_hps.R` | After ingest | HPS data ingested |
| Ingest new CPS extract | `03_ingest_cps_ipums.R` | Monthly | IPUMS extract downloaded |
| Build CPS mart | `04_clean_cps.R` | After ingest | CPS data ingested |
| Validate all marts | `06_build_marts.R` | After any build | At least one mart exists |

---

## Setup (One-Time)

### 1. FRED API Key
1. Get a free API key: https://fred.stlouisfed.org/docs/api/api_key.html
2. Add to `~/.Renviron`:
   ```
   FRED_API_KEY=your_key_here
   ```
3. Restart R session

### 2. IPUMS Account
1. Create account: https://cps.ipums.org/
2. Request access to CPS data (free, instant approval)

### 3. Install Required Packages
```r
# If using renv (recommended):
renv::restore()

# Or install manually:
install.packages(c(
  "tidyverse", "data.table", "arrow", "janitor", "lubridate",
  "survey", "srvyr", "fredr", "ipumsr", "gt", "scales", "here"
))
```

---

## Weekly Refresh: FRED

**When:** Every Monday, or when fresh macro data is needed

**Steps:**
```r
# From project root
source(here::here("foundational_data", "R", "05_ingest_fred.R"))
```

**Expected output:**
- `data_mart/mart_fred_month.parquet` updated
- Console shows series fetched and date ranges

**Verify:**
```r
source(here::here("foundational_data", "R", "00_config.R"))
fred <- read_mart("fred")
fred |> filter(series_id == "UMCSENT") |> tail()
```

---

## Weekly/Biweekly Refresh: HPS

**When:** Census releases new PUF (typically every 2 weeks)

### Step 1: Download PUF
1. Go to: https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html
2. Select the latest year
3. Download the PUF ZIP (e.g., `HPS_Week67_PUF_CSV.zip`)
4. Place in: `foundational_data/data_raw/hps/`

### Step 2: Ingest
```r
source(here::here("foundational_data", "R", "01_ingest_hps.R"))
```

This will automatically unzip any ZIP files in the hps folder.

### Step 3: Build Mart
```r
source(here::here("foundational_data", "R", "02_clean_hps.R"))
```

**Expected output:**
- `data_mart/mart_hps_national.parquet` updated
- Contains hardship + PHQ-2/GAD-2 metrics by segment

**Verify:**
```r
source(here::here("foundational_data", "R", "00_config.R"))
hps <- read_mart("hps")
hps |> distinct(pulse_wave)
```

---

## Monthly Refresh: CPS

**When:** After BLS releases new CPS data (typically mid-month for previous month)

### Step 1: Create/Update IPUMS Extract
1. Log in to https://cps.ipums.org/
2. Create a new extract (or revise existing):
   - **Samples:** Select months needed (e.g., 2020-2024 monthly)
   - **Variables:** YEAR, MONTH, STATEFIP, AGE, EDUC, MARST, NCHILD, HHINCOME, EMPSTAT, LABFORCE, UHRSWORKT, WTFINL
3. Submit extract
4. Wait for email notification (usually minutes to hours)
5. Download the `.xml` (DDI) and `.dat.gz` files

### Step 2: Place Files
Put downloaded files in: `foundational_data/data_raw/cps_ipums/`

### Step 3: Ingest
```r
source(here::here("foundational_data", "R", "03_ingest_cps_ipums.R"))
```

### Step 4: Build Mart
```r
source(here::here("foundational_data", "R", "04_clean_cps.R"))
```

**Expected output:**
- `data_mart/mart_cps_state_month.parquet` updated
- Contains labor metrics by state × segment

**Verify:**
```r
source(here::here("foundational_data", "R", "00_config.R"))
cps <- read_mart("cps")
cps |> filter(state_fips == 11) |> summarise(periods = n_distinct(period))
```

---

## Validation

**When:** After any mart is updated, or periodically as health check

```r
source(here::here("foundational_data", "R", "06_build_marts.R"))
```

This will:
- Check all marts exist
- Validate column names and types
- Verify segmentation values
- Check success criteria (UMCSENT present, DC present, etc.)

---

## Troubleshooting

### FRED: "FRED_API_KEY not found"
- Check `~/.Renviron` contains `FRED_API_KEY=your_key`
- Restart R session after editing `.Renviron`

### IPUMS: "No DDI file found"
- Ensure `.xml` file is in `data_raw/cps_ipums/`
- File must end in `.xml`

### HPS: "No weight variable found"
- Check the data dictionary for the release
- Variable names may have changed - update `VAR_MAP` in `02_clean_hps.R`

### Mart: "Segmentation validation issues"
- Check that derived variables use correct coding
- Review source data dictionary for coding changes

---

## File Outputs

After a full refresh, you should have:

```
foundational_data/
├── data_mart/
│   ├── mart_fred_month.parquet      # Macro context
│   ├── mart_cps_state_month.parquet # DC/state labor
│   └── mart_hps_national.parquet    # National hardship + sentiment
├── data_clean/
│   ├── cps/cps_clean_latest.parquet
│   └── hps/hps_clean_latest.parquet
└── data_raw/
    ├── fred/fred_raw_YYYY-MM-DD.parquet
    ├── cps_ipums/*.xml, *.dat.gz
    └── hps/<release_id>/...
```

---

## Contact

For issues with this infrastructure, check:
1. This runbook
2. The data source documentation (links in Appendix A of spec)
3. Script comments and error messages
