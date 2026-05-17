# NYC Taxi Data Quality Analysis and Cleaning

This folder contains the full set of R scripts I used to assess data quality, investigate specific anomalies, and clean the NYC Yellow Taxi trip dataset (January–March 2025). All scripts are included for reproducibility and transparency, even those whose outputs did not appear directly in the final report.

## Script Groups

### 1. Data Quality Assessment (`scripts/data_quality_assessment/`)

These scripts perform the initial exploratory analysis of the raw data. They print findings to the console and are intended to be run sequentially (01 through 05), though each is self-contained.

| Script | Purpose |
|--------|---------|
| `01_dataset_overview.R` | Dimensions, column types, monthly volume, vendor distribution, date ranges |
| `02_missing_values.R` | NA counts per variable, vendor-level NA rates, monthly NA trends, systemic block overlap |
| `03_duplicates.R` | Exact and near-duplicate detection, duplicate pair classification (categories A–H) |
| `04_irregularities.R` | Timestamp issues, distance/duration mismatches, coded field violations, financial anomalies, airport fee checks, policy compliance |
| `05_outliers.R` | Multi-threshold outlier comparison (IQR, percentiles), domain boundary checks |

### 2. Deep-Dive Studies (`scripts/deep_dive_studies/`)

These scripts investigate specific phenomena in greater depth and write structured markdown reports to `Output/`.

| Script | Output | Focus |
|--------|--------|-------|
| `discrepancy_analysis.R` | `Output/discrepancy_report.md` | Why `total_amount` does not equal the sum of its components, broken down by payment type, vendor, location, hour, and rate code |
| `negative_rows_study.R` | `Output/negative_rows_report.md` | Negative monetary values — refund pair identification, orphan negatives, vendor/payment breakdown |
| `timestamp_duration_study.R` | `Output/timestamp_duration_report.md` | Out-of-range timestamps, negative duration, zero-duration trips — all per vendor |

### 3. Data Cleaning (`scripts/data_cleaning/`)

| Script | Output | Purpose |
|--------|--------|---------|
| `data_preprocessing_pipeline.R` | `Data/cleaned_data_final.parquet` | 14-step cleaning pipeline: removes refund pairs, filters date window, drops extreme distances/durations/fares, imputes Airport_fee, recomputes total_amount |

## How to Run

All scripts are designed to be run from the project root directory (`FiveStars_EDA/`), where `Data/` and `Output/` are located.

```r
setwd("path/to/FiveStars_EDA")

# Data quality assessment (console output)
source("Analysis/scripts/data_quality_assessment/01_dataset_overview.R")
source("Analysis/scripts/data_quality_assessment/02_missing_values.R")
source("Analysis/scripts/data_quality_assessment/03_duplicates.R")
source("Analysis/scripts/data_quality_assessment/04_irregularities.R")
source("Analysis/scripts/data_quality_assessment/05_outliers.R")

# Deep-dive studies (writes reports to Output/)
source("Analysis/scripts/deep_dive_studies/discrepancy_analysis.R")
source("Analysis/scripts/deep_dive_studies/negative_rows_study.R")
source("Analysis/scripts/deep_dive_studies/timestamp_duration_study.R")

# Cleaning pipeline (writes cleaned data to Data/)
source("Analysis/scripts/data_cleaning/data_preprocessing_pipeline.R")
```

## Dependencies

| Package | Used For |
|---------|----------|
| `arrow` | Reading/writing Parquet files |
| `dplyr` | Data manipulation throughout |
| `tidyr` | Pivoting and reshaping (02, 04, discrepancy, negative rows) |
| `knitr` | `kable()` for markdown table generation (deep-dive studies) |
| `ggplot2` | Referenced in discrepancy_analysis.R (commented out; not required to run) |
