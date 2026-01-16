# PSEO Research Project Ideas

Ten high-level research directions using Census Bureau Post-Secondary Employment Outcomes data.

---

## 1. The Engineering Premium: Persistence and Decay

**Question**: How durable is the engineering earnings premium over time? Does the gap between engineering and other STEM fields widen or narrow from Year 1 to Year 10?

**Data Required**:
- Earnings data (pseoe_all.csv) ✓
- CIP codes for engineering (14.xx) and comparison fields ✓
- Graduation cohorts 2001-2016 (to capture Y10 data) ✓

**Key Variables**: y1_p50_earnings, y5_p50_earnings, y10_p50_earnings, cipcode, degree_level

**Approach**: Track median earnings trajectories by CIP code family. Calculate compound annual growth rates (CAGR) and compare relative earnings premiums at each time horizon.

---

## 2. The Value of Selectivity: Do Elite Institutions Deliver?

**Question**: How much does institutional selectivity matter for graduate earnings, controlling for field of study? Does the "elite premium" vary by major?

**Data Required**:
- Earnings data by institution ✓
- Institution identifiers (OPEID) ✓
- External selectivity data (IPEDS admission rates) - **requires supplemental data**

**Key Variables**: institution, y1_p50_earnings, y5_p50_earnings, cipcode, degree_level

**Approach**: Merge PSEO with IPEDS selectivity metrics. Compare earnings distributions across selectivity tiers within the same field. Test for interaction effects between selectivity and major.

---

## 3. Geographic Mobility and Earnings: Brain Drain Economics

**Question**: Do graduates who leave their state of graduation earn more than those who stay? Which states are net exporters vs. importers of talent?

**Data Required**:
- Flows data (pseof_all.csv) ✓
- Geographic identifiers ✓
- In-state retention variables ✓
- Earnings data for comparison ✓

**Key Variables**: y1_grads_emp_instate, y1_grads_emp, geography, institution_state

**Approach**: Calculate in-state retention rates by state and field. Merge with earnings data to test whether geographic mobility correlates with higher earnings within the same degree/field combination.

---

## 4. Earnings Inequality Within Fields

**Question**: Which majors produce the highest earnings inequality (spread between 25th and 75th percentile)? Does inequality grow, shrink, or remain stable from Year 1 to Year 10?

**Data Required**:
- Earnings data with percentile variables ✓
- CIP codes ✓

**Key Variables**: y1_p25_earnings, y1_p75_earnings, y5_p25/p75, y10_p25/p75, cipcode

**Approach**: Calculate interquartile range (IQR) ratios by field. Track how the P75/P25 ratio evolves over time. Identify "winner-take-all" fields vs. more egalitarian ones.

---

## 5. The Graduate Degree Premium by Field

**Question**: For which undergraduate majors does a graduate degree (Master's or PhD) pay off most? Are there fields where the graduate premium is negative?

**Data Required**:
- Earnings data by degree level ✓
- CIP codes at 2-digit level ✓
- Matching cohorts across degree levels ✓

**Key Variables**: degree_level (05, 07, 17, 18), cipcode, y5_p50_earnings, y10_p50_earnings

**Approach**: Compare earnings at the 2-digit CIP level across Bachelor's, Master's, and Doctoral degrees. Calculate the graduate premium as percentage increase over Bachelor's earnings at Year 5 and Year 10.

---

## 6. Industry Pathways: Where Do Graduates Go?

**Question**: Which industries absorb graduates from different fields? How does industry placement correlate with earnings?

**Data Required**:
- Flows data with industry codes ✓
- NAICS industry labels ✓
- Earnings data for cross-reference ✓

**Key Variables**: industry, cipcode, y1_grads_emp, degree_level

**Approach**: Map CIP codes to primary employing industries. Identify "crossover" fields that feed multiple industries. Test whether placement in high-paying industries (Tech, Finance) explains earnings variation within fields.

---

## 7. The Cohort Effect: Economic Timing and Career Outcomes

**Question**: Do graduates who enter the labor market during recessions (2008-2010) show persistent earnings scars compared to boom-time cohorts?

**Data Required**:
- Earnings data across graduation cohorts ✓
- Cohort identifiers (2001, 2004, 2007, 2010, 2013, 2016, 2019) ✓

**Key Variables**: grad_cohort, y1_p50_earnings, y5_p50_earnings, y10_p50_earnings

**Approach**: Compare earnings trajectories across cohorts, controlling for field. Test whether the 2007-2010 cohorts show persistently lower earnings at Y5 and Y10 compared to adjacent cohorts.

---

## 8. The Nursing Outlier: Healthcare vs. Engineering

**Question**: Nursing (51.38) is the only non-engineering field in the top 20 highest-earning Bachelor's degrees. What explains nursing's exceptional returns, and how do they compare to engineering over time?

**Data Required**:
- Earnings data for nursing and engineering programs ✓
- Institution-level variation ✓
- Geographic variation ✓

**Key Variables**: cipcode (51.38, 14.xx), y1_p50_earnings, y5_p50_earnings, y10_p50_earnings

**Approach**: Deep dive comparing nursing vs. engineering earnings distributions. Analyze variance across institutions and states. Test whether nursing earnings growth trajectories differ from engineering.

---

## 9. Community College Outcomes: The Associate's Advantage

**Question**: For which fields do Associate's degree holders earn competitive wages? Are there fields where a 2-year degree delivers better ROI than a 4-year degree?

**Data Required**:
- Earnings data for Associate's (03) and Bachelor's (05) degrees ✓
- CIP codes ✓
- Comparable fields across degree levels ✓

**Key Variables**: degree_level (03, 05), cipcode, y1_p50_earnings, y5_p50_earnings

**Approach**: Identify fields with strong Associate's earnings. Calculate implicit ROI by comparing earnings to estimated educational costs. Factor in time-to-degree differences.

---

## 10. State-Level Variation: Where Does Education Pay?

**Question**: How much does geography matter for earnings within the same field? Which states offer the highest returns to specific degrees?

**Data Required**:
- Earnings data by institution ✓
- Institution state identifiers ✓
- State-level aggregations available via API

**Key Variables**: institution_state, cipcode, y1_p50_earnings, y5_p50_earnings

**Approach**: Aggregate earnings by state and field. Control for cost of living using BLS regional price parities. Identify states with highest/lowest adjusted returns by field.

---

## Data Availability Summary

| Research Idea | Earnings | Flows | External Data |
|---------------|----------|-------|---------------|
| 1. Engineering Premium | ✓ | - | - |
| 2. Elite Premium | ✓ | - | IPEDS selectivity |
| 3. Brain Drain | ✓ | ✓ | - |
| 4. Earnings Inequality | ✓ | - | - |
| 5. Graduate Premium | ✓ | - | - |
| 6. Industry Pathways | ✓ | ✓ | - |
| 7. Cohort Effects | ✓ | - | - |
| 8. Nursing Outlier | ✓ | - | - |
| 9. Community College | ✓ | - | - |
| 10. State Variation | ✓ | - | BLS price parities |

**All 10 projects can be conducted with PSEO data.** Projects 2 and 10 would benefit from supplemental data sources (IPEDS, BLS) that are also publicly available.

---

## Recommended Starting Point

**Project 1 (Engineering Premium)** or **Project 4 (Earnings Inequality)** are recommended as first analyses because they:
- Use only earnings data (already downloaded and complete)
- Require no external data sources
- Produce visually compelling results
- Address questions with broad audience appeal
