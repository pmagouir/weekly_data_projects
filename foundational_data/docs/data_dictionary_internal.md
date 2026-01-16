# Data Dictionary - Foundational Data Marts

Internal documentation of all variables in the foundational data marts.

---

## mart_fred_month.parquet

Macro economic context from FRED (Federal Reserve Economic Data).

| Column | Type | Description |
|--------|------|-------------|
| `period` | string | Year-month (YYYY-MM) |
| `series_id` | string | FRED series identifier |
| `value` | double | Observation value |
| `date` | date | Exact observation date |
| `series_title` | string | Human-readable series name |
| `units` | string | Unit of measurement |
| `geo_level` | string | Always "us" |
| `geo_id` | string | Always "US" |

### Series Included

| series_id | Description | Units |
|-----------|-------------|-------|
| `CPIAUCSL` | Consumer Price Index for All Urban Consumers | Index 1982-1984=100 |
| `CPIFABSL` | CPI: Food and Beverages | Index 1982-1984=100 |
| `CPIENGSL` | CPI: Energy | Index 1982-1984=100 |
| `UNRATE` | Unemployment Rate | Percent |
| `FEDFUNDS` | Federal Funds Effective Rate | Percent |
| `MORTGAGE30US` | 30-Year Fixed Rate Mortgage Average | Percent |
| `UMCSENT` | University of Michigan: Consumer Sentiment | Index 1966:Q1=100 |

---

## mart_cps_state_month.parquet

State-level labor market indicators from Current Population Survey via IPUMS.

### Identifiers

| Column | Type | Description |
|--------|------|-------------|
| `period` | string | Year-month (YYYY-MM) |
| `state_fips` | integer | State FIPS code (DC = 11) |
| `geo_level` | string | Always "state" |
| `geo_id` | string | State FIPS as string |

### Segmentation

| Column | Type | Values |
|--------|------|--------|
| `age_group` | string | 18-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75+ |
| `educ_group` | string | LE_HS, SOME_COLLEGE, BA, GRAD |
| `income_group` | string | Q1, Q2, Q3, Q4, Q5 (quintiles) |
| `hh_type` | string | MARRIED_CHILDREN, MARRIED_NO_CHILDREN, SINGLE_CHILDREN, SINGLE_NO_CHILDREN, OTHER_MULTIADULT |

### Outcomes

| Column | Type | Description |
|--------|------|-------------|
| `n_unweighted` | integer | Sample size for this cell |
| `unemployment_rate` | double | Proportion unemployed (among labor force) |
| `labor_force_participation` | double | Proportion in labor force |
| `mean_hours_worked` | double | Mean usual hours worked per week |
| `pct_40plus_hours` | double | Proportion working 40+ hours/week |
| `employment_rate` | double | Proportion employed (among all adults) |

### Grain
One row per: `state_fips × period × age_group × educ_group × income_group × hh_type`

---

## mart_hps_national.parquet

National hardship and mental health indicators from Household Pulse Survey.

### Identifiers

| Column | Type | Description |
|--------|------|-------------|
| `pulse_wave` | string | HPS release identifier (e.g., "HPS_Week67_PUF_CSV") |
| `geo_level` | string | Always "us" |
| `geo_id` | string | Always "US" |

### Segmentation

| Column | Type | Values |
|--------|------|--------|
| `age_group` | string | 18-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75+ |
| `educ_group` | string | LE_HS, SOME_COLLEGE, BA, GRAD |
| `income_group` | string | Q1, Q2, Q3, Q4, Q5 (quintiles) |
| `hh_type` | string | MARRIED_CHILDREN, MARRIED_NO_CHILDREN, SINGLE_CHILDREN, SINGLE_NO_CHILDREN, OTHER_MULTIADULT |

### Hardship Outcomes

| Column | Type | Description |
|--------|------|-------------|
| `n_unweighted` | integer | Sample size for this cell |
| `pct_hardship_expenses` | double | Proportion reporting difficulty paying expenses |
| `pct_food_insufficient` | double | Proportion reporting food insufficiency |
| `pct_housing_not_current` | double | Proportion not current on rent/mortgage |
| `pct_medical_delay_cost` | double | Proportion delaying medical care due to cost |

### Mental Health Outcomes ("How People Feel")

| Column | Type | Description |
|--------|------|-------------|
| `mean_phq2_score` | double | Mean PHQ-2 depression screener score (0-6) |
| `pct_phq2_positive` | double | Proportion with PHQ-2 >= 3 (depressive symptoms) |
| `mean_gad2_score` | double | Mean GAD-2 anxiety screener score (0-6) |
| `pct_gad2_positive` | double | Proportion with GAD-2 >= 3 (anxiety symptoms) |
| `pct_distress` | double | Proportion with PHQ-2 >= 3 OR GAD-2 >= 3 |

### PHQ-2 and GAD-2 Scoring

The PHQ-2 (Patient Health Questionnaire-2) screens for depression using two questions:
1. Little interest or pleasure in doing things
2. Feeling down, depressed, or hopeless

The GAD-2 (Generalized Anxiety Disorder-2) screens for anxiety using two questions:
1. Feeling nervous, anxious, or on edge
2. Not being able to stop or control worrying

Each item is scored 0-3 (Not at all to Nearly every day). Total scores range 0-6.
A score of 3 or higher suggests possible depression/anxiety warranting further evaluation.

### Grain
One row per: `pulse_wave × age_group × educ_group × income_group × hh_type`

---

## Joining Marts

### CPS ↔ FRED
Join on `period` (YYYY-MM).

```r
cps |>
  left_join(fred |> filter(series_id == "UMCSENT"), by = "period")
```

### HPS ↔ FRED
Requires mapping `pulse_wave` to `period`. Each pulse wave has a collection date range.
Approximate by mapping to the month containing the collection midpoint.

```r
# Create wave-to-period mapping based on HPS schedule
# See: https://www.census.gov/programs-surveys/household-pulse-survey/data.html
```

### CPS vs HPS
These surveys have different geographies (state vs national) and cannot be directly merged at the person level. Compare by using:
- CPS for state/DC-specific trends
- HPS for national hardship/sentiment context

---

## Data Quality Notes

### Coverage Gaps
- **CPS:** Some state × segment cells may have small samples. Check `n_unweighted`.
- **HPS:** Early waves (2020) had higher response rates than recent waves.
- **FRED:** Consumer sentiment (UMCSENT) is monthly but may have revisions.

### Missing Values
- Segmentation variables may be NA if source data is missing.
- Filter to `n_unweighted >= 30` for reliable estimates.

### Weighting
All proportions and means are computed using survey weights:
- CPS: `WTFINL` (final composite weight)
- HPS: `PWEIGHT` (person weight)

Standard errors are not included in marts but can be computed from microdata in `data_clean/`.
