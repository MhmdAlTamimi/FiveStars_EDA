library(arrow)
library(dplyr)
library(tidyr)

fmt <- function(n) format(n, big.mark = ",")

report_step <- function(step, label, before, after) {
  dropped <- before - after
  cat(sprintf(
    "Step %-2s | %-45s | dropped: %s  | remaining: %s\n",
    step, label, fmt(dropped), fmt(after)
  ))
}

# =============================================================================
# Load data
# =============================================================================

df <- read_parquet("Data/original.parquet")
df_clean <- df

cat("=================================================================\n")
cat("           NYC TAXI DATA CLEANING REPORT\n")
cat("=================================================================\n")
cat(sprintf("Original rows loaded : %s\n\n", fmt(nrow(df))))

sdate <- as.POSIXct("2025-01-01 00:00:00")
edate <- as.POSIXct("2025-03-31 23:59:59")


# =============================================================================
# Step 1 — Remove Refund trips (matching pickup, dropoff, and PU zone)
# =============================================================================

before <- nrow(df_clean)
to_remove <- df |>
  filter(
    tpep_pickup_datetime  == lead(tpep_pickup_datetime) |
      tpep_pickup_datetime  == lag(tpep_pickup_datetime)
  ) |>
  filter(
    tpep_dropoff_datetime == lead(tpep_dropoff_datetime) |
      tpep_dropoff_datetime == lag(tpep_dropoff_datetime)
  ) |>
  filter(
    PULocationID == lead(PULocationID) | PULocationID == lag(PULocationID)
  )
df_clean <- anti_join(df_clean, to_remove, by = "trip_id")
remove(to_remove)
report_step(1, "Refund trips (time + zone match)", before, nrow(df_clean))


# =============================================================================
# Step 2 — Remove payment_type=4 (dispute) refund trips
# =============================================================================

before <- nrow(df_clean)
df_refund <- df_clean |>
  filter(payment_type == 4) |>
  filter(
    tpep_pickup_datetime == lead(tpep_pickup_datetime) |
      tpep_pickup_datetime == lag(tpep_pickup_datetime)
  )
df_clean <- anti_join(df_clean, df_refund, by = "trip_id")
remove(df_refund)
report_step(2, "Dispute refund trips (payment_type=4)", before, nrow(df_clean))


# =============================================================================
# Step 3 — Remove pickups outside the 2025-01-01 to 2025-03-31 window
# =============================================================================

before <- nrow(df_clean)
df_clean <- df_clean |>
  filter(!(tpep_pickup_datetime < sdate | tpep_pickup_datetime > edate))
report_step(3, "Pickups outside Jan-Mar 2025 window", before, nrow(df_clean))


# =============================================================================
# Step 4 — Remove dropoffs before 2025-01-01
# =============================================================================

before <- nrow(df_clean)
df_clean <- df_clean |>
  filter(!(tpep_dropoff_datetime < sdate))
report_step(4, "Dropoffs before 2025-01-01", before, nrow(df_clean))


# =============================================================================
# Step 5 — Flag duration issues (no rows dropped)
#   zero_duration          : pickup == dropoff timestamp
#   negative_duration_v6_v7: dropoff < pickup for Vendor 6 or 7
# =============================================================================

df_clean <- df_clean |>
  mutate(
    duration_mins = as.numeric(
      difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")
    ),
    duration_flag =
      duration_mins == 0 |
      (VendorID %in% c(6L, 7L) & duration_mins < 0)
  )

n_flagged <- sum(df_clean$duration_flag, na.rm = TRUE)
cat(sprintf(
  "Step 5  | %-45s | flagged: %s  | remaining: %s\n",
  "Duration issues flagged (not dropped)",
  fmt(n_flagged), fmt(nrow(df_clean))
))


# =============================================================================
# Step 6 — Remove non-positive or extreme trip distance
# =============================================================================

before <- nrow(df_clean)
df_clean <- df_clean |>
  filter(!(trip_distance <= 0 | trip_distance >= 100))
report_step(6, "Trip distance <=0 or >=100 miles", before, nrow(df_clean))


# =============================================================================
# Step 7 — Remove trips longer than 3 hours
# =============================================================================

before <- nrow(df_clean)
wrong_trip_duration <- df_clean |>
  mutate(
    duration_h = as.numeric(
      difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "hours")
    )
  ) |>
  filter(duration_h > 3)
df_clean <- anti_join(df_clean, wrong_trip_duration, by = "trip_id")
remove(wrong_trip_duration)
report_step(7, "Trip duration > 3 hours", before, nrow(df_clean))


# =============================================================================
# Step 8 — Remove out-of-NYC-zone trips (LocationID > 263)
# =============================================================================

before <- nrow(df_clean)
wrong_out_of_zone <- df_clean |>
  filter(PULocationID > 263 | DOLocationID > 263)
df_clean <- anti_join(df_clean, wrong_out_of_zone, by = "trip_id")
remove(wrong_out_of_zone)
report_step(8, "PU or DO zone outside NYC (>263)", before, nrow(df_clean))


# =============================================================================
# Step 9 — Remove invalid fare amounts (first pass)
# =============================================================================

before <- nrow(df_clean)
wrong_fare_amount <- df_clean |>
  filter(
    fare_amount < 3.00 | fare_amount > 500 |
      (RatecodeID == 1 & fare_amount < 4.00) |
      (fare_amount > total_amount)
  )
df_clean <- anti_join(df_clean, wrong_fare_amount, by = "trip_id")
remove(wrong_fare_amount)
report_step(9, "Invalid fare amount (first pass)", before, nrow(df_clean))


# =============================================================================
# Step 10 — Fare / distance consistency filter (second pass)
# =============================================================================

before <- nrow(df_clean)
df_clean <- df_clean |>
  filter(
    fare_amount >= 3.00 & fare_amount <= 250,
    !(RatecodeID == 1 & fare_amount < 4.00),
    fare_amount <= total_amount,
    trip_distance > 0 & trip_distance <= 100
  )
report_step(
  10, "Fare/distance consistency (second pass)", before, nrow(df_clean)
)

cat(sprintf(
  "         Fare ~ distance correlation after step 10 : %.4f\n",
  cor(df_clean$trip_distance, df_clean$fare_amount)
))


# =============================================================================
# Step 11 — Reset negative extra fields to 0 (no rows dropped)
# =============================================================================

n_neg_extra <- sum(df_clean$extra < 0 & df_clean$fare_amount >= 0, na.rm = TRUE)
df_clean <- df_clean |>
  mutate(extra = ifelse(extra < 0 & fare_amount >= 0, 0, extra))
cat(sprintf(
  "Step 11 | %-45s | modified: %s  | remaining: %s\n",
  "Negative extra reset to 0",
  fmt(n_neg_extra), fmt(nrow(df_clean))
))


# =============================================================================
# Step 12 — Remove invalid total amounts
# =============================================================================

before <- nrow(df_clean)
df_clean <- df_clean |>
  filter(!(total_amount < 5 | total_amount > 500))
report_step(12, "Total amount <$5 or >$500", before, nrow(df_clean))


# =============================================================================
# Step 13 — Impute Airport_fee based on pickup zone (no rows dropped)
# =============================================================================

airport_zones <- c(132, 138, 1)
df_clean <- df_clean |>
  mutate(Airport_fee = ifelse(PULocationID %in% airport_zones, 1.75, 0.00))
cat(sprintf(
  "Step 13 | %-45s | modified: %s  | remaining: %s\n",
  "Airport_fee imputed from pickup zone",
  fmt(sum(df_clean$PULocationID %in% airport_zones)),
  fmt(nrow(df_clean))
))


# =============================================================================
# Step 14 — Coalesce NAs to 0 and recompute total_amount (no rows dropped)
# =============================================================================

df_clean <- df_clean |>
  mutate(
    fare_amount           = coalesce(fare_amount, 0),
    extra                 = coalesce(extra, 0),
    mta_tax               = coalesce(mta_tax, 0),
    tip_amount            = coalesce(tip_amount, 0),
    tolls_amount          = coalesce(tolls_amount, 0),
    improvement_surcharge = coalesce(improvement_surcharge, 0),
    congestion_surcharge  = coalesce(congestion_surcharge, 0),
    Airport_fee           = coalesce(Airport_fee, 0),
    cbd_congestion_fee    = coalesce(cbd_congestion_fee, 0),
    total_amount =
      fare_amount + extra + mta_tax + tip_amount +
      tolls_amount + improvement_surcharge +
      congestion_surcharge + Airport_fee + cbd_congestion_fee
  )
cat(sprintf(
  "Step 14 | %-45s | modified: %s  | remaining: %s\n",
  "NAs coalesced to 0; total_amount recomputed",
  fmt(nrow(df_clean)), fmt(nrow(df_clean))
))


# =============================================================================
# Revenue check — clean vs original
# =============================================================================

total_rev <- df_clean |>
  group_by(VendorID) |>
  summarise(total_rev = sum(total_amount), .groups = "drop")

total_rev1 <- df |>
  group_by(VendorID) |>
  summarise(total_rev = sum(total_amount), .groups = "drop")


# =============================================================================
# Cleaning Summary
# =============================================================================

total_dropped <- nrow(df) - nrow(df_clean)
cat("\n=================================================================\n")
cat(sprintf("FINAL dataset rows   : %s\n", fmt(nrow(df_clean))))
cat(sprintf(
  "Total rows dropped   : %s  (%.2f%% of original)\n",
  fmt(total_dropped), 100 * total_dropped / nrow(df)
))
cat(sprintf(
  "Duration issues flagged (kept) : %s\n",
  fmt(sum(df_clean$duration_flag, na.rm = TRUE))
))
cat(sprintf(
  "Final fare ~ distance correlation : %.4f\n",
  cor(df_clean$trip_distance, df_clean$fare_amount)
))
cat("=================================================================\n")

# =============================================================================
# Validating Cleaned Dataset
# =============================================================================

# Adjusted R-squared before cleaning

model <- lm(trip_distance ~ fare_amount, data = df)

adj_r2 <- summary(model)$adj.r.squared
cat(sprintf(
  "Adjusted R-squared before cleaning: %.4f\n", adj_r2
))

# Adjusted R-squared after cleaning

model <- lm(trip_distance ~ fare_amount, data = df_clean)

adj_r2 <- summary(model)$adj.r.squared

cat(sprintf(
  "Adjusted R-squared After cleaning: %.4f\n", adj_r2
))

cat("=================================================================\n")

# =============================================================================
# Save cleaned dataset
# =============================================================================

write_parquet(df_clean, "Data/cleaned_data_final.parquet")
cat(sprintf("Cleaned dataset saved to: data/cleaned_data_final.parquet\n"))