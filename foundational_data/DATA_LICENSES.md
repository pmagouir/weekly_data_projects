# Data Source Licenses and Terms of Use

## Summary

| Source | Microdata Redistribution | Aggregated Stats | Citation Required |
|--------|-------------------------|------------------|-------------------|
| IPUMS CPS | **Prohibited** | Allowed | Yes |
| Census HPS PUF | Allowed (public use) | Allowed | Encouraged |
| FRED | Allowed | Allowed | Yes (attribute to Fed) |

---

## IPUMS CPS

**Terms of Use:** https://www.ipums.org/about/terms

### Key Restrictions
1. **No redistribution of microdata.** You may not share the downloaded extract files (`.dat.gz`, `.xml`) or any person-level derivatives.
2. **No re-identification.** Do not attempt to identify individuals.
3. **Citation required.** Any publication using IPUMS data must cite appropriately.

### What This Means for This Project
- `data_raw/cps_ipums/` — **Never commit.** Contains downloaded extracts.
- `data_clean/cps/` — **Never commit.** Contains person-level microdata.
- `data_mart/mart_cps_state_month.parquet` — **OK to commit.** Aggregated statistics only.

### Required Citation
> Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. IPUMS CPS: Version 11.0 [dataset]. Minneapolis, MN: IPUMS, 2023. https://doi.org/10.18128/D030.V11.0

---

## Census Household Pulse Survey (HPS)

**Terms:** https://www.census.gov/programs-surveys/household-pulse-survey/technical-documentation.html

### Status
The HPS Public Use File (PUF) is **public use data** — it can be freely downloaded and shared.

### Restrictions
1. **No re-identification.** Do not attempt to identify individuals.
2. **Acknowledge source.** Citation encouraged but not strictly required.

### What This Means for This Project
- `data_raw/hps/` — Could technically commit, but excluded for size/cleanliness.
- `data_clean/hps/` — Could commit, but excluded (still person-level).
- `data_mart/mart_hps_national.parquet` — **OK to commit.**

### Suggested Citation
> U.S. Census Bureau. Household Pulse Survey Public Use File. [Dataset]. Retrieved from https://www.census.gov/programs-surveys/household-pulse-survey/data.html

---

## FRED (Federal Reserve Economic Data)

**Terms:** https://fred.stlouisfed.org/docs/api/terms_of_use.html

### Status
FRED data is **freely available** for commercial and non-commercial use.

### Requirements
1. **Attribution required.** Credit the Federal Reserve Bank of St. Louis.
2. **No misrepresentation.** Do not imply endorsement by the Federal Reserve.

### What This Means for This Project
- `data_raw/fred/` — Could commit, but excluded for cleanliness.
- `data_mart/mart_fred_month.parquet` — **OK to commit.**

### Required Attribution
> Source: Federal Reserve Bank of St. Louis, FRED. https://fred.stlouisfed.org/

---

## Files Committed to GitHub

**No data files are committed.** Only scripts and documentation.

```
foundational_data/
├── R/                    ✅ Committed (scripts)
├── docs/                 ✅ Committed (documentation)
├── README.md             ✅ Committed
├── DATA_LICENSES.md      ✅ Committed
├── data_raw/             ❌ Not committed (excluded via .gitignore)
├── data_clean/           ❌ Not committed (excluded via .gitignore)
└── data_mart/            ❌ Not committed (excluded via .gitignore)
```

This approach ensures full compliance with IPUMS terms of use. Anyone reproducing this work must:
1. Create their own IPUMS account and download their own CPS extract
2. Download HPS PUF files from Census
3. Obtain their own FRED API key
4. Run the pipeline scripts to generate the marts locally

---

## For Exploration Projects

When using data from the foundation:
1. **Always cite sources** in your README and any publications.
2. **Never export person-level data** from the clean files to your project's output.
3. **Aggregate before sharing** — the marts are pre-aggregated and safe to use.
