# =============================================================================
# DASHBOARD DATA PREP — NYC Taxi Zone Heatmap
#
# Reads the cleaned dataset produced by the preprocessing pipeline,
# then aggregates revenue and trip counts by pickup and dropoff zone.
#
# Output: Output/dashboard_agg.parquet  (used by dashboard_map.R at startup)
#
# Run from project root (FiveStars_EDA/):
#   source("InteractiveDashboard/prep_dashboard_data.R")
# =============================================================================

required_packages <- c("arrow", "dplyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(arrow)
library(dplyr)

fmt <- function(n) format(n, big.mark = ",")

df <- read_parquet("Data/cleaned_data_final.parquet")

cat(sprintf("Loaded %s cleaned rows\n", fmt(nrow(df))))

# =============================================================================
# Aggregate by pickup zone
# =============================================================================

agg_pickup <- df |>
  group_by(LocationID = PULocationID) |>
  summarise(
    trips    = n(),
    revenue  = round(sum(total_amount, na.rm = TRUE), 2),
    avg_fare = round(mean(fare_amount,  na.rm = TRUE), 2),
    avg_tip  = round(mean(tip_amount,   na.rm = TRUE), 2),
    .groups  = "drop"
  )

# =============================================================================
# Aggregate by dropoff zone
# =============================================================================

agg_dropoff <- df |>
  group_by(LocationID = DOLocationID) |>
  summarise(
    trips    = n(),
    revenue  = round(sum(total_amount, na.rm = TRUE), 2),
    avg_fare = round(mean(fare_amount,  na.rm = TRUE), 2),
    avg_tip  = round(mean(tip_amount,   na.rm = TRUE), 2),
    .groups  = "drop"
  )

# =============================================================================
# Save
# =============================================================================

write_parquet(
  bind_rows(
    mutate(agg_pickup,  zone_type = "pickup"),
    mutate(agg_dropoff, zone_type = "dropoff")
  ),
  "Output/dashboard_agg.parquet"
)

cat(sprintf(
  "Saved Output/dashboard_agg.parquet\n  pickup  zones: %s\n  dropoff zones: %s\n",
  fmt(nrow(agg_pickup)), fmt(nrow(agg_dropoff))
))
