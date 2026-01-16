# Segmentation Specifications

Canonical segmentation definitions for all foundational data marts. These definitions are implemented in `R/00_config.R` and must be used consistently across all exploration projects.

---

## Geography Strategy

### Geo Levels
| `geo_level` | Description |
|-------------|-------------|
| `"us"` | National aggregate |
| `"state"` | State-level (includes DC) |

### Geo IDs
| `geo_id` | Description |
|---------|-------------|
| `"US"` | National (for `geo_level="us"`) |
| Numeric FIPS | State identifier (for `geo_level="state"`) |

**DC FIPS: `11`**

### Dataset Usage
| Dataset | Geography | Notes |
|---------|-----------|-------|
| HPS/HTOPS | National (`geo_level="us"`) | Sample too small for reliable state estimates |
| CPS/IPUMS | State (`geo_level="state"`) | DC = `state_fips=11` |
| FRED | National (`geo_level="us"`) | Macro indicators |

---

## Age Groups (`age_group`)

| Value | Age Range | Notes |
|-------|-----------|-------|
| `18-24` | 18-24 years | Young adults |
| `25-34` | 25-34 years | Early career |
| `35-44` | 35-44 years | Mid-career |
| `45-54` | 45-54 years | Established career |
| `55-64` | 55-64 years | Pre-retirement |
| `65-74` | 65-74 years | Early retirement |
| `75+` | 75+ years | Late retirement |

**Implementation:** `make_age_group(age)` in `00_config.R`

---

## Education Groups (`educ_group`)

| Value | Description | Typical Years |
|-------|-------------|---------------|
| `LE_HS` | High school or less | ≤12 years |
| `SOME_COLLEGE` | Some college or associate's | 13-15 years |
| `BA` | Bachelor's degree | 16 years |
| `GRAD` | Graduate/professional degree | >16 years |

**Implementation:** `make_educ_group(educ, scheme)` in `00_config.R`

### Source-Specific Mappings

#### IPUMS CPS (`scheme = "ipums_cps"`)
| EDUC codes | `educ_group` |
|------------|--------------|
| ≤73 | `LE_HS` |
| 74-110 | `SOME_COLLEGE` |
| 111 | `BA` |
| ≥112 | `GRAD` |

#### HPS (`scheme = "hps"`)
| EEDUC codes | `educ_group` |
|-------------|--------------|
| 1-3 | `LE_HS` |
| 4-5 | `SOME_COLLEGE` |
| 6 | `BA` |
| ≥7 | `GRAD` |

*Note: HPS variable names may shift by phase. Always verify against the data dictionary shipped with each PUF release.*

---

## Income Groups (`income_group`)

| Value | Description |
|-------|-------------|
| `Q1` | Bottom 20% (lowest income quintile) |
| `Q2` | 20th-40th percentile |
| `Q3` | 40th-60th percentile (middle) |
| `Q4` | 60th-80th percentile |
| `Q5` | Top 20% (highest income quintile) |

**Implementation:** `make_income_quintile_weighted(income, weight)` in `00_config.R`

### Calculation Method
- Quintiles are computed using survey weights
- Computed within each dataset and time period (not pooled across sources)
- CPS: quintiles within each state-month
- HPS: quintiles within each pulse wave

---

## Household Type (`hh_type`)

| Value | Description | Rules |
|-------|-------------|-------|
| `MARRIED_CHILDREN` | Married/partnered with children | `married_flag=TRUE` AND `has_child=TRUE` |
| `MARRIED_NO_CHILDREN` | Married/partnered, no children | `married_flag=TRUE` AND `has_child=FALSE` |
| `SINGLE_CHILDREN` | Single parent with children | `married_flag=FALSE` AND `has_child=TRUE` |
| `SINGLE_NO_CHILDREN` | Single person, no children | `married_flag=FALSE` AND `has_child=FALSE` AND `adult_count=1` |
| `OTHER_MULTIADULT` | Multiple adults, not married | `married_flag=FALSE` AND `adult_count>=2` |

**Implementation:** `make_hh_type(married_flag, has_child, adult_count_bin)` in `00_config.R`

### Input Variables by Source

#### CPS/IPUMS
| Input | IPUMS Variable | Derivation |
|-------|----------------|------------|
| `married_flag` | `MARST` | `MARST %in% c(1,2)` (married, spouse present/absent) |
| `has_child` | `NCHILD` | `NCHILD > 0` |
| `adult_count_bin` | Derived | Count persons with `AGE >= 18` in household |

#### HPS
| Input | Typical HPS Variable | Notes |
|-------|---------------------|-------|
| `married_flag` | `MS` or similar | Check dictionary for each wave |
| `has_child` | `THHLD_NUMKID` or similar | Children under 18 in household |
| `adult_count_bin` | `THHLD_NUMADLT` or similar | Adults 18+ in household |

---

## Standard Column Names

All marts must use these exact column names:

### Time
- `period` - YYYY-MM format (e.g., "2024-01")
- `pulse_wave` - HPS wave identifier (e.g., "week67")

### Geography
- `geo_level` - "us" or "state"
- `geo_id` - "US" or state FIPS
- `state_fips` - Numeric state FIPS (CPS mart only)

### Segmentation
- `age_group` - As defined above
- `educ_group` - As defined above
- `income_group` - As defined above
- `hh_type` - As defined above

### Metadata
- `n_unweighted` - Unweighted sample size for cell

---

## Validation

The `validate_segmentation(df)` function in `00_config.R` checks that all segmentation values conform to the specifications above. Run this before writing any mart file.

---

## Usage in Exploration Projects

Exploration projects should:
1. Import marts from `foundational_data/data_mart/`
2. Use the pre-computed segmentation variables as-is
3. **Never** redefine segmentation bins or create competing definitions
4. Filter to segments of interest (e.g., `filter(state_fips == 11)` for DC)

Example:
```r
source(here("foundational_data", "R", "00_config.R"))

# Load DC data from CPS mart
dc_labor <- read_mart("cps") |>
  filter(state_fips == DC_FIPS)

# Load national hardship data
hardship <- read_mart("hps")

# Load macro context
macro <- read_mart("fred") |>
  filter(series_id == "UMCSENT")
```
