# Visualization — Static Charts and Business Dashboard

This folder contains the scripts that generate all EDA figures, summary CSVs, and the final HTML business dashboard. They must be run in order because later scripts depend on outputs from earlier ones.

## Execution Order

All scripts run from the project root (`FiveStars_EDA/`). Missing packages are installed automatically.

```r
setwd("path/to/FiveStars_EDA")

# Step 1 — Generate all EDA figures and summary CSVs
source("Visualization/01_generate_eda_figures.R")

# Step 2 — Generate the revenue zone map (downloads TLC shapefile on first run)
source("Visualization/02_revenue_zone_map.R")

# Step 3 — Assemble the HTML business dashboard from figures and KPIs
source("Visualization/03_build_business_dashboard.R")
```

## What Each Script Produces

| Script | Outputs to | Description |
|--------|-----------|-------------|
| `01_generate_eda_figures.R` | `Output/figures/` (22 PNGs), `Output/eda_summaries/` (20 CSVs) | Univariate, bivariate, and multivariate EDA charts plus KPI summary |
| `02_revenue_zone_map.R` | `Output/figures/G01_*.png`, `Output/eda_summaries/G01_*.csv` | Revenue-by-zone choropleth map using TLC shapefiles |
| `03_build_business_dashboard.R` | `Output/NYC_Yellow_Taxi_Final_Business_Dashboard.html` | Self-contained HTML dashboard assembling all figures with KPI cards |

## Dependencies

| Package | Scripts |
|---------|---------|
| `arrow` | 01, 02 |
| `dplyr` | 01, 02, 03 |
| `ggplot2` | 01, 02, 03 |
| `lubridate` | 01 |
| `readr` | 01, 02, 03 |
| `tidyr` | 01 |
| `scales` | 01, 02, 03 |
| `forcats` | 01 |
| `stringr` | 01 |
| `sf` | 02 |
| `htmltools` | 03 |
| `base64enc` | 03 |

## Notes

- `02_revenue_zone_map.R` downloads the official TLC taxi zone shapefile (~5 MB) on the first run. It is cached in `Output/geo/` for subsequent runs.
- The HTML dashboard (`03`) references figures via relative paths (`figures/*.png`). It must stay in `Output/` alongside the `figures/` folder to display correctly.
- The logo in the dashboard header is generated programmatically and embedded as a base64 image — no external logo file is needed.
