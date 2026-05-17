# ============================================================
# 01_generate_eda_figures.R
# Full EDA + Business Visualizations
# Covers: 6.1 Univariate, 6.2 Bivariate, 6.3 Multivariate
#
# Run from project root (FiveStars_EDA/):
#   source("Visualization/01_generate_eda_figures.R")
# ============================================================

# --- Packages (auto-install) ---

packages <- c(
  "arrow", "dplyr", "ggplot2", "lubridate", "readr",
  "tidyr", "scales", "forcats", "stringr"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(scales)
library(forcats)
library(stringr)

# --- Paths ---

input_file  <- "Data/cleaned_data_final.parquet"
summary_dir <- "Output/eda_summaries"
figure_dir  <- "Output/figures"

if (!file.exists(input_file)) stop("Data/cleaned_data_final.parquet not found. Set working directory to FiveStars_EDA/.")

dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# --- Business theme and colors ---

main_blue  <- "#1F4E79"
gold       <- "#F2B705"
sky        <- "#4EA3D9"
dark_gray  <- "#404040"
light_gray <- "#F2F2F2"
green      <- "#2E8B57"
red        <- "#C0392B"

theme_business <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = main_blue),
      plot.subtitle = element_text(size = 11, color = dark_gray),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 9, color = "#666666")
    )
}

save_plot <- function(plot, filename, width = 9, height = 5) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# --- Load data ---

df <- read_parquet(input_file)

cat("Loaded:", input_file, "\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# --- Derived analytical fields ---

df <- df %>%
  mutate(
    pickup_datetime = as_datetime(tpep_pickup_datetime),
    dropoff_datetime = as_datetime(tpep_dropoff_datetime),
    pickup_date = as.Date(pickup_datetime),
    pickup_month = floor_date(pickup_datetime, "month"),
    pickup_month_label = format(pickup_month, "%Y-%m"),
    pickup_hour = hour(pickup_datetime),
    pickup_weekday = wday(pickup_datetime, label = TRUE, abbr = FALSE),
    duration_min = as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "mins")),
    is_post_cbd = pickup_date >= as.Date("2025-01-05"),

    payment_type_label = case_when(
      payment_type == 0 ~ "Flex Fare",
      payment_type == 1 ~ "Credit Card",
      payment_type == 2 ~ "Cash",
      payment_type == 3 ~ "No Charge",
      payment_type == 4 ~ "Dispute",
      payment_type == 5 ~ "Unknown",
      payment_type == 6 ~ "Voided",
      TRUE ~ "Missing/Invalid"
    ),

    ratecode_label = case_when(
      RatecodeID == 1 ~ "Standard",
      RatecodeID == 2 ~ "JFK",
      RatecodeID == 3 ~ "Newark",
      RatecodeID == 4 ~ "Nassau/Westchester",
      RatecodeID == 5 ~ "Negotiated",
      RatecodeID == 6 ~ "Group Ride",
      RatecodeID == 99 ~ "Unknown",
      TRUE ~ "Missing/Invalid"
    )
  )

gc()

# ============================================================
# A) EXECUTIVE KPI SUMMARY
# ============================================================

kpi_summary <- df %>%
  summarise(
    total_trips = n(),
    total_revenue = sum(total_amount, na.rm = TRUE),
    avg_total_amount = mean(total_amount, na.rm = TRUE),
    median_total_amount = median(total_amount, na.rm = TRUE),
    avg_fare_amount = mean(fare_amount, na.rm = TRUE),
    avg_trip_distance = mean(trip_distance, na.rm = TRUE),
    avg_duration_min = mean(duration_min, na.rm = TRUE),
    avg_tip_amount = mean(tip_amount, na.rm = TRUE),
    credit_card_share = mean(payment_type == 1, na.rm = TRUE),
    cash_share = mean(payment_type == 2, na.rm = TRUE),
    airport_pickup_share = mean(PULocationID %in% c(132, 138), na.rm = TRUE),
    cbd_positive_share = mean(cbd_congestion_fee > 0, na.rm = TRUE)
  )

write_csv(kpi_summary, file.path(summary_dir, "00_kpi_summary.csv"))

# ============================================================
# B) 6.1 UNIVARIATE EDA
# ============================================================

# 1. Trip volume over time
trips_by_date <- df %>%
  count(pickup_date, name = "trips") %>%
  arrange(pickup_date)

write_csv(trips_by_date, file.path(summary_dir, "U01_trips_by_date.csv"))

p_u1 <- ggplot(trips_by_date, aes(x = pickup_date, y = trips)) +
  geom_line(color = main_blue, linewidth = 0.8) +
  labs(
    title = "Univariate: Daily Trip Volume",
    subtitle = "Trip volume over time after cleaning",
    x = "Pickup Date",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u1, "U01_daily_trip_volume.png", 10, 5)

# 2. Monthly volume
trips_by_month <- df %>%
  count(pickup_month_label, name = "trips") %>%
  arrange(pickup_month_label)

write_csv(trips_by_month, file.path(summary_dir, "U02_trips_by_month.csv"))

p_u2 <- ggplot(trips_by_month, aes(x = pickup_month_label, y = trips)) +
  geom_col(fill = main_blue) +
  labs(
    title = "Univariate: Trip Volume by Month",
    x = "Month",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u2, "U02_trips_by_month.png", 8, 5)

# 3. Trip distance distribution
distance_cutoff <- quantile(df$trip_distance[df$trip_distance >= 0], 0.99, na.rm = TRUE)

distance_data <- df %>%
  select(trip_distance) %>%
  filter(
    !is.na(trip_distance),
    trip_distance >= 0,
    trip_distance <= distance_cutoff
  )

p_u3 <- ggplot(distance_data, aes(x = trip_distance)) +
  geom_histogram(bins = 60, fill = main_blue, color = "white") +
  labs(
    title = "Univariate: Trip Distance Distribution",
    subtitle = "Displayed up to P99 to improve readability",
    x = "Trip Distance (miles)",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u3, "U03_trip_distance_distribution.png", 8, 5)

rm(distance_data)
gc()

# 4. Trip duration distribution
duration_cutoff <- quantile(df$duration_min[df$duration_min >= 0], 0.99, na.rm = TRUE)

duration_data <- df %>%
  select(duration_min) %>%
  filter(
    !is.na(duration_min),
    duration_min >= 0,
    duration_min <= duration_cutoff
  )

p_u4 <- ggplot(duration_data, aes(x = duration_min)) +
  geom_histogram(bins = 60, fill = sky, color = "white") +
  labs(
    title = "Univariate: Trip Duration Distribution",
    subtitle = "Displayed up to P99 to improve readability",
    x = "Duration (minutes)",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u4, "U04_trip_duration_distribution.png", 8, 5)

rm(duration_data)
gc()

# 5. Fare amount distribution
fare_cutoff <- quantile(df$fare_amount[df$fare_amount >= 0], 0.99, na.rm = TRUE)

fare_data <- df %>%
  select(fare_amount) %>%
  filter(
    !is.na(fare_amount),
    fare_amount >= 0,
    fare_amount <= fare_cutoff
  )

p_u5 <- ggplot(fare_data, aes(x = fare_amount)) +
  geom_histogram(bins = 60, fill = gold, color = "white") +
  labs(
    title = "Univariate: Fare Amount Distribution",
    subtitle = "Displayed up to P99 to improve readability",
    x = "Fare Amount ($)",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u5, "U05_fare_amount_distribution.png", 8, 5)

rm(fare_data)
gc()

# 6. Total amount distribution
total_cutoff <- quantile(df$total_amount[df$total_amount >= 0], 0.99, na.rm = TRUE)

total_data <- df %>%
  select(total_amount) %>%
  filter(
    !is.na(total_amount),
    total_amount >= 0,
    total_amount <= total_cutoff
  )

p_u6 <- ggplot(total_data, aes(x = total_amount)) +
  geom_histogram(bins = 60, fill = green, color = "white") +
  labs(
    title = "Univariate: Total Amount Distribution",
    subtitle = "Displayed up to P99 to improve readability",
    x = "Total Amount ($)",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u6, "U06_total_amount_distribution.png", 8, 5)

rm(total_data)
gc()

# 7. Passenger count frequency
passenger_summary <- df %>%
  count(passenger_count, name = "trips") %>%
  filter(!is.na(passenger_count)) %>%
  arrange(passenger_count)

write_csv(passenger_summary, file.path(summary_dir, "U07_passenger_count_summary.csv"))

p_u7 <- ggplot(passenger_summary, aes(x = factor(passenger_count), y = trips)) +
  geom_col(fill = main_blue) +
  labs(
    title = "Univariate: Passenger Count Frequency",
    x = "Passenger Count",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u7, "U07_passenger_count_frequency.png", 8, 5)

# 8. Payment type frequency
payment_summary <- df %>%
  count(payment_type_label, name = "trips") %>%
  mutate(share = trips / sum(trips)) %>%
  arrange(desc(trips))

write_csv(payment_summary, file.path(summary_dir, "U08_payment_type_summary.csv"))

p_u8 <- ggplot(payment_summary, aes(x = fct_reorder(payment_type_label, trips), y = trips)) +
  geom_col(fill = main_blue) +
  coord_flip() +
  labs(
    title = "Univariate: Payment Type Frequency",
    x = "Payment Type",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u8, "U08_payment_type_frequency.png", 8, 5)

# 9. Rate code frequency
ratecode_summary <- df %>%
  count(ratecode_label, name = "trips") %>%
  mutate(share = trips / sum(trips)) %>%
  arrange(desc(trips))

write_csv(ratecode_summary, file.path(summary_dir, "U09_ratecode_summary.csv"))

p_u9 <- ggplot(ratecode_summary, aes(x = fct_reorder(ratecode_label, trips), y = trips)) +
  geom_col(fill = gold) +
  coord_flip() +
  labs(
    title = "Univariate: Rate Code Frequency",
    x = "Rate Code",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_u9, "U09_ratecode_frequency.png", 8, 5)

gc()

# ============================================================
# C) 6.2 BIVARIATE EDA
# ============================================================

# 1. Hour of day vs trip volume
trips_by_hour <- df %>%
  count(pickup_hour, name = "trips") %>%
  arrange(pickup_hour)

write_csv(trips_by_hour, file.path(summary_dir, "B01_trips_by_hour.csv"))

p_b1 <- ggplot(trips_by_hour, aes(x = pickup_hour, y = trips)) +
  geom_col(fill = main_blue) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Bivariate: Hour of Day vs Trip Volume",
    x = "Hour of Day",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_b1, "B01_hour_vs_trip_volume.png", 10, 5)

# 2. Weekday vs demand
weekday_demand <- df %>%
  count(pickup_weekday, name = "trips")

write_csv(weekday_demand, file.path(summary_dir, "B02_weekday_demand.csv"))

p_b2 <- ggplot(weekday_demand, aes(x = pickup_weekday, y = trips)) +
  geom_col(fill = sky) +
  labs(
    title = "Bivariate: Weekday vs Demand",
    x = "Weekday",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_b2, "B02_weekday_vs_demand.png", 9, 5)

# 3. Payment type vs tip / total amount
payment_behavior <- df %>%
  group_by(payment_type_label) %>%
  summarise(
    trips = n(),
    avg_tip = mean(tip_amount, na.rm = TRUE),
    median_tip = median(tip_amount, na.rm = TRUE),
    avg_total = mean(total_amount, na.rm = TRUE),
    median_total = median(total_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(trips))

write_csv(payment_behavior, file.path(summary_dir, "B03_payment_behavior.csv"))

p_b3 <- ggplot(payment_behavior, aes(x = fct_reorder(payment_type_label, avg_tip), y = avg_tip)) +
  geom_col(fill = gold) +
  coord_flip() +
  labs(
    title = "Bivariate: Payment Type vs Average Tip",
    subtitle = "Cash tips are not recorded, so interpretation should be cautious",
    x = "Payment Type",
    y = "Average Tip ($)"
  ) +
  theme_business()

save_plot(p_b3, "B03_payment_vs_avg_tip.png", 8, 5)

p_b4 <- ggplot(payment_behavior, aes(x = fct_reorder(payment_type_label, avg_total), y = avg_total)) +
  geom_col(fill = green) +
  coord_flip() +
  labs(
    title = "Bivariate: Payment Type vs Average Total Amount",
    x = "Payment Type",
    y = "Average Total Amount ($)"
  ) +
  theme_business()

save_plot(p_b4, "B04_payment_vs_avg_total.png", 8, 5)

# 4. Distance vs total amount (sampled)
set.seed(123)

scatter_sample <- df %>%
  select(trip_distance, total_amount) %>%
  filter(
    !is.na(trip_distance),
    !is.na(total_amount),
    trip_distance >= 0,
    total_amount >= 0,
    trip_distance <= distance_cutoff,
    total_amount <= total_cutoff
  ) %>%
  sample_n(size = min(100000, n()))

p_b5 <- ggplot(scatter_sample, aes(x = trip_distance, y = total_amount)) +
  geom_point(alpha = 0.15, color = main_blue) +
  geom_smooth(method = "loess", se = FALSE, color = red) +
  labs(
    title = "Bivariate: Distance vs Total Amount",
    x = "Trip Distance (miles)",
    y = "Total Amount ($)"
  ) +
  theme_business()

save_plot(p_b5, "B05_distance_vs_total_amount.png", 8, 5)

rm(scatter_sample)
gc()

# 5. Geography vs trip count
top_pickup_zones <- df %>%
  select(PULocationID) %>%
  filter(!is.na(PULocationID), !(PULocationID %in% c(264, 265))) %>%
  count(PULocationID, name = "trips") %>%
  arrange(desc(trips)) %>%
  slice_head(n = 10)

write_csv(top_pickup_zones, file.path(summary_dir, "B06_top_pickup_zones_by_trips.csv"))

p_b6 <- ggplot(top_pickup_zones, aes(x = fct_reorder(as.factor(PULocationID), trips), y = trips)) +
  geom_col(fill = main_blue) +
  coord_flip() +
  labs(
    title = "Bivariate: Geography vs Trip Count",
    subtitle = "Top 10 pickup zones excluding unknown zones 264/265",
    x = "Pickup Location ID",
    y = "Number of Trips"
  ) +
  scale_y_continuous(labels = comma) +
  theme_business()

save_plot(p_b6, "B06_geography_vs_trip_count.png", 8, 5)

# 6. Geography vs revenue
top_revenue_zones <- df %>%
  select(PULocationID, total_amount) %>%
  filter(!is.na(PULocationID), !(PULocationID %in% c(264, 265))) %>%
  group_by(PULocationID) %>%
  summarise(
    trips = n(),
    total_revenue = sum(total_amount, na.rm = TRUE),
    avg_total = mean(total_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 10)

write_csv(top_revenue_zones, file.path(summary_dir, "B07_top_pickup_zones_by_revenue.csv"))

p_b7 <- ggplot(top_revenue_zones, aes(x = fct_reorder(as.factor(PULocationID), total_revenue), y = total_revenue)) +
  geom_col(fill = green) +
  coord_flip() +
  labs(
    title = "Bivariate: Geography vs Revenue",
    subtitle = "Top 10 pickup zones by total revenue",
    x = "Pickup Location ID",
    y = "Total Revenue ($)"
  ) +
  scale_y_continuous(labels = dollar) +
  theme_business()

save_plot(p_b7, "B07_geography_vs_revenue.png", 8, 5)

gc()

# ============================================================
# D) 6.3 MULTIVARIATE EDA
# ============================================================

# 1. Month x hour x demand
month_hour_demand <- df %>%
  count(pickup_month_label, pickup_hour, name = "trips")

write_csv(month_hour_demand, file.path(summary_dir, "M01_month_hour_demand.csv"))

p_m1 <- ggplot(month_hour_demand, aes(x = pickup_hour, y = pickup_month_label, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = light_gray, high = main_blue, labels = comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Multivariate: Month x Hour x Demand",
    x = "Hour of Day",
    y = "Month",
    fill = "Trips"
  ) +
  theme_business()

save_plot(p_m1, "M01_month_hour_demand_heatmap.png", 10, 5)

# 2. Weekday x hour x demand
weekday_hour_demand <- df %>%
  count(pickup_weekday, pickup_hour, name = "trips")

write_csv(weekday_hour_demand, file.path(summary_dir, "M02_weekday_hour_demand.csv"))

p_m2 <- ggplot(weekday_hour_demand, aes(x = pickup_hour, y = pickup_weekday, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = light_gray, high = main_blue, labels = comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Multivariate: Weekday x Hour x Demand",
    x = "Hour of Day",
    y = "Weekday",
    fill = "Trips"
  ) +
  theme_business()

save_plot(p_m2, "M02_weekday_hour_demand_heatmap.png", 10, 6)

# 3. Time x geography x demand
top_zone_ids <- top_pickup_zones$PULocationID

zone_hour_demand <- df %>%
  select(PULocationID, pickup_hour) %>%
  filter(PULocationID %in% top_zone_ids) %>%
  count(PULocationID, pickup_hour, name = "trips")

write_csv(zone_hour_demand, file.path(summary_dir, "M03_zone_hour_demand.csv"))

p_m3 <- ggplot(zone_hour_demand, aes(x = pickup_hour, y = as.factor(PULocationID), fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = light_gray, high = main_blue, labels = comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Multivariate: Time x Geography x Demand",
    subtitle = "Top pickup zones by hour of day",
    x = "Hour of Day",
    y = "Pickup Location ID",
    fill = "Trips"
  ) +
  theme_business()

save_plot(p_m3, "M03_time_geography_demand_heatmap.png", 10, 6)

# 4. Geography x payment type x revenue
geo_payment_revenue <- df %>%
  select(PULocationID, payment_type_label, total_amount) %>%
  filter(PULocationID %in% top_zone_ids) %>%
  group_by(PULocationID, payment_type_label) %>%
  summarise(
    trips = n(),
    total_revenue = sum(total_amount, na.rm = TRUE),
    avg_total = mean(total_amount, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(geo_payment_revenue, file.path(summary_dir, "M04_geo_payment_revenue.csv"))

p_m4 <- ggplot(geo_payment_revenue, aes(x = as.factor(PULocationID), y = total_revenue, fill = payment_type_label)) +
  geom_col() +
  labs(
    title = "Multivariate: Geography x Payment Type x Revenue",
    x = "Pickup Location ID",
    y = "Total Revenue ($)",
    fill = "Payment Type"
  ) +
  scale_y_continuous(labels = dollar) +
  theme_business()

save_plot(p_m4, "M04_geography_payment_revenue.png", 10, 6)

# 5. Distance x duration x total amount
distance_duration_total <- df %>%
  select(trip_distance, duration_min, total_amount) %>%
  filter(
    !is.na(trip_distance),
    !is.na(duration_min),
    !is.na(total_amount),
    trip_distance >= 0,
    duration_min >= 0,
    total_amount >= 0
  ) %>%
  mutate(
    distance_band = cut(
      trip_distance,
      breaks = c(0, 1, 3, 5, 10, 20, Inf),
      include.lowest = TRUE
    ),
    duration_band = cut(
      duration_min,
      breaks = c(0, 5, 10, 20, 40, 60, Inf),
      include.lowest = TRUE
    )
  ) %>%
  group_by(distance_band, duration_band) %>%
  summarise(
    trips = n(),
    avg_total_amount = mean(total_amount, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(distance_duration_total, file.path(summary_dir, "M05_distance_duration_total.csv"))

p_m5 <- ggplot(distance_duration_total, aes(x = distance_band, y = duration_band, fill = avg_total_amount)) +
  geom_tile() +
  scale_fill_gradient(low = light_gray, high = green, labels = dollar) +
  labs(
    title = "Multivariate: Distance x Duration x Total Amount",
    x = "Distance Band (miles)",
    y = "Duration Band (minutes)",
    fill = "Avg Total"
  ) +
  theme_business() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_m5, "M05_distance_duration_total_heatmap.png", 9, 6)

rm(distance_duration_total)
gc()

# 6. Pre/Post Jan 5 CBD patterns
cbd_pre_post_summary <- df %>%
  mutate(cbd_period = if_else(is_post_cbd, "Post Jan 5", "Pre Jan 5")) %>%
  group_by(cbd_period) %>%
  summarise(
    trips = n(),
    avg_total_amount = mean(total_amount, na.rm = TRUE),
    total_cbd_fee = sum(cbd_congestion_fee, na.rm = TRUE),
    avg_cbd_fee = mean(cbd_congestion_fee, na.rm = TRUE),
    cbd_positive_share = mean(cbd_congestion_fee > 0, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(cbd_pre_post_summary, file.path(summary_dir, "M06_cbd_pre_post_summary.csv"))

p_m6 <- ggplot(cbd_pre_post_summary, aes(x = cbd_period, y = cbd_positive_share)) +
  geom_col(fill = gold) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Multivariate: Pre/Post Jan 5 CBD Fee Pattern",
    x = "CBD Policy Period",
    y = "Share of Trips with Positive CBD Fee"
  ) +
  theme_business()

save_plot(p_m6, "M06_cbd_positive_share_pre_post.png", 8, 5)

p_m7 <- ggplot(cbd_pre_post_summary, aes(x = cbd_period, y = avg_cbd_fee)) +
  geom_col(fill = main_blue) +
  labs(
    title = "Multivariate: Average CBD Fee Pre vs Post Jan 5",
    x = "CBD Policy Period",
    y = "Average CBD Fee ($)"
  ) +
  theme_business()

save_plot(p_m7, "M07_avg_cbd_fee_pre_post.png", 8, 5)

# ============================================================
# F) Coverage Map
# ============================================================

coverage_map <- tibble::tribble(
  ~assignment_requirement, ~output_file, ~where_to_use,
  "6.1 Univariate - Trip volume over time", "U01_daily_trip_volume.png", "Executive Report + Dashboard",
  "6.1 Univariate - Trip distance distribution", "U03_trip_distance_distribution.png", "Technical Report",
  "6.1 Univariate - Trip duration distribution", "U04_trip_duration_distribution.png", "Technical Report",
  "6.1 Univariate - Fare amount distribution", "U05_fare_amount_distribution.png", "Technical Report",
  "6.1 Univariate - Total amount distribution", "U06_total_amount_distribution.png", "Technical Report",
  "6.1 Univariate - Passenger count", "U07_passenger_count_frequency.png", "Technical Report",
  "6.1 Univariate - Payment type frequency", "U08_payment_type_frequency.png", "Executive Report + Dashboard",
  "6.1 Univariate - Rate code frequency", "U09_ratecode_frequency.png", "Technical Report",
  "6.2 Bivariate - Hour vs trip volume", "B01_hour_vs_trip_volume.png", "Executive Report + Dashboard",
  "6.2 Bivariate - Geography vs trip count", "B06_geography_vs_trip_count.png", "Executive Report + Dashboard",
  "6.2 Bivariate - Geography vs revenue", "B07_geography_vs_revenue.png", "Executive Report + Dashboard",
  "6.2 Bivariate - Payment vs tip", "B03_payment_vs_avg_tip.png", "Executive Report",
  "6.2 Bivariate - Payment vs total amount", "B04_payment_vs_avg_total.png", "Executive Report",
  "6.2 Bivariate - Distance vs total amount", "B05_distance_vs_total_amount.png", "Technical + Executive Report",
  "6.2 Bivariate - Weekday vs demand", "B02_weekday_vs_demand.png", "Executive Report + Dashboard",
  "6.3 Multivariate - Month x hour x demand", "M01_month_hour_demand_heatmap.png", "Dashboard",
  "6.3 Multivariate - Weekday x hour x demand", "M02_weekday_hour_demand_heatmap.png", "Executive Report + Dashboard",
  "6.3 Multivariate - Time x geography x demand", "M03_time_geography_demand_heatmap.png", "Dashboard",
  "6.3 Multivariate - Geography x payment x revenue", "M04_geography_payment_revenue.png", "Dashboard",
  "6.3 Multivariate - Distance x duration x total amount", "M05_distance_duration_total_heatmap.png", "Technical Report",
  "6.3 Multivariate - Pre/Post Jan 5 CBD", "M06_cbd_positive_share_pre_post.png", "Executive Report + Dashboard",
)

write_csv(coverage_map, file.path(summary_dir, "Z01_assignment_coverage_map.csv"))

cat("\nEDA COMPLETE.\n")
cat("Summaries saved to:", summary_dir, "\n")
cat("Figures saved to:", figure_dir, "\n")
