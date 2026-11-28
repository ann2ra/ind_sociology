# Religion in France, 2036

Individual homework for the KSE Sociology course: **what will religion in France look like in 2036 — who will believe, who will belong, and who will practise?**

Author: Anna Pelekh

## What is in this repository

| File | What it is |
|---|---|
| `MY_FORECAST.md` | My own forecast, written before using any AI tool or the data (first commit, not edited afterwards) |
| `analysis.R` | All code: data cleaning, cohort × wave tables, forecast to 2036, back-cast test, charts |
| `scenario.md` | One-page scenario for a general reader |
| `AI_log.md` | AI-use log (tool, prompts, what I did with the output, a checked claim) |
| `results/` | Tables (`.csv`) and charts (`.png`) produced by `analysis.R` |
| `data/` | UN World Population Prospects 2024 population file for France |

Key results: `results/forecast_2036_final.csv`, `results/backcast_2018_final.csv`, `results/forecast_2036.png`, `results/cohorts_by_wave.png`.

## Data

- **EVS/WVS joint dataset** (ZA7503, v3.0.0), free after registration at [GESIS](https://search.gesis.org/research_data/ZA7503). France (`S003 == 250`), five waves: 1981, 1990, 1999, 2008, 2018. **The data file is not included** because its licence does not allow redistribution. Download the SPSS file (`ZA7503_v3-0-0.sav`) and put it in `~/Downloads/` or change the path in `analysis.R`.
- **UN World Population Prospects 2024** (medium variant): population of France by single year of age, 2018–2036, used for the size of each generation in 2036.

## Indicators

| Dimension | Variable | Coded as 1 if |
|---|---|---|
| Believing | F050 Believe in God | yes |
| Belonging | F025 Religious denomination | any denomination (not "none") |
| Practice: attendance | F028 Attend religious services | at least monthly (`attend_month`) / at least once a year (`attend_year`) |
| Practice: prayer | F066 Pray outside religious services | at least monthly (`pray_month`) / at least several times a year (`pray_year`); asked only from 1999 |

All shares are weighted with `S017`. Missing codes (negative values) are treated as missing. Birth cohorts are 10-year groups; cells with fewer than 50 respondents are not used.

## Method in brief

1. **Cohort × wave tables:** share of each indicator by birth decade and survey year.
2. **Forecast to 2036 (cohort replacement):** each generation keeps its 2018 level; UN projections give how many adults each generation will have in 2036. Generations born after 2000 are assumed to resemble those born in the 1990s.
3. **Drift:** a common within-cohort trend (regression `share ~ year + cohort`) is added only for **belonging**, where it is stable and improved the back-cast; for other indicators drift is 0 in the central forecast.
4. **Range:** lowest and highest of 9 scenarios (drift at its lower/central/upper estimate × new generations like the 1990s / continuing the decline between cohorts / like the 1980s), the "no change since 2018" value as a possible upper end, plus ±95% survey sampling error (effective sample size from the weights, Kish formula).
5. **Back-cast (self-test):** the same model using data up to 2008 predicts 2018; all six real 2018 values fall inside the predicted ranges.

## How to reproduce

Open the project in RStudio (R 4.6), install the packages `haven`, `foreign`, `dplyr`, `tidyr`, `ggplot2`, set the data paths at the top of `analysis.R` and run the whole script (**Source**). All outputs are written to `results/`.
