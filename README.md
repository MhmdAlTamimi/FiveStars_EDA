# FiveStars EDA — NYC Yellow Taxi Data (Jan–Mar 2025)

This repository contains all scripts, data, and outputs for the exploratory data analysis, cleaning, and visualization of the NYC Yellow Taxi trip dataset covering January through March 2025.

## Project Structure

```
FiveStars_EDA/
├── Data/                          <- All input data files
│   ├── original.parquet           <- Raw TLC dataset
│   ├── cleaned_data_final.parquet <- Output of the cleaning pipeline
│   ├── NYC_Taxi_Zones.csv         <- Zone boundary polygons (WKT)
│   └── taxi_zone_lookup.csv       <- Zone name and borough lookup
│
├── Analysis/                      <- Data quality assessment, deep-dive studies, cleaning pipeline
│   ├── scripts/
│   │   ├── data_quality_assessment/   (01–05: overview, NAs, duplicates, irregularities, outliers)
│   │   ├── deep_dive_studies/         (discrepancy, negative rows, timestamp studies)
│   │   └── data_cleaning/            (14-step preprocessing pipeline)
│   └── README.md
│
├── InteractiveDashboard/          <- Shiny heatmap dashboard (revenue and trip counts by zone)
│   ├── prep_dashboard_data.R      <- Aggregates cleaned data for the dashboard
│   ├── dashboard_map.R            <- Shiny app with Leaflet map
│   └── README.md
│
├── Visualization/                 <- Static visualizations (charts, plots)
├── Output/                        <- Generated reports and intermediate files
└── README.md
```

## Quick Start

All scripts are designed to be run from this directory (`FiveStars_EDA/`) as the working directory.

**Prerequisites:** R (>= 4.1). The InteractiveDashboard scripts auto-install missing packages. For the Analysis scripts, install dependencies with:

```r
install.packages(c("arrow", "dplyr", "tidyr", "knitr"))
```

**IMPORTANT NOTICE**
- **YOU MUST ADD THE ORIGINAL DATA AS `original.parquet` under `Data/`**

### Run the analysis

```r
setwd("path/to/FiveStars_EDA")
source("Analysis/scripts/data_quality_assessment/01_dataset_overview.R")
```

### Run the cleaning pipeline

```r
source("Analysis/scripts/data_cleaning/data_preprocessing_pipeline.R")
```

### Launch the interactive dashboard

```r
source("InteractiveDashboard/prep_dashboard_data.R")   # first run only
shiny::runApp("InteractiveDashboard/dashboard_map.R")
```

See each subfolder's `README.md` for full details.
