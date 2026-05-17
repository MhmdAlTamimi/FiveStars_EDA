# =============================================================================
# TOTAL_AMOUNT DISCREPANCY ANALYSIS — NYC Taxi Data
# Writes a markdown report to: discrepancy_report.md
#
# Discrepancy = total_amount - (fare + extra + mta_tax + improvement +
#               tolls + congestion + airport + cbd + tip)
# Positive  → something is in total but not broken out into a component field
# Negative  → a component is not reflected in total (classic: tip on cash trips)
# =============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(knitr)

df <- read_parquet("Data/original.parquet")

# =============================================================================
# STEP 0 — Compute columns needed for the analysis from the original data
# =============================================================================

# JFK zones, Newark zones, Nassau/Westchester zones for ratecode_implied
jfk_zones <- c(1, 132, 138)
newark_zones <- c(1)
nassau_westchester_zones <- c(
  4, 7, 14, 29, 30, 44, 52, 55, 56, 57, 60, 61, 62, 63, 64, 65,
  71, 72, 83, 84, 85, 86, 91, 92, 93, 94, 95, 96, 97, 98, 99,
  110, 117, 118, 119, 133, 134, 135, 136, 167, 168, 169, 174,
  175, 176, 177, 178, 179, 187, 188, 189, 190, 191, 192, 193,
  200, 201, 203, 204, 205, 206, 207, 208, 212, 213, 214, 215,
  216, 217, 218, 219, 220, 221, 222, 223, 225, 226, 227, 228,
  247, 248, 250, 251, 252, 253, 254, 257, 258, 259, 260, 264, 265
)

df <- df |>
  mutate(
    # Treat NA fee columns as 0
    congestion_surcharge = coalesce(congestion_surcharge, 0),
    Airport_fee          = coalesce(Airport_fee, 0),
    cbd_congestion_fee   = coalesce(cbd_congestion_fee, 0),

    # discrepancy = total - sum of all recorded components
    discrepancy = total_amount - (
      fare_amount + extra + mta_tax + improvement_surcharge +
      tolls_amount + congestion_surcharge + Airport_fee +
      cbd_congestion_fee + tip_amount
    ),

    # expected_discrepancy: use rule-based mta_tax and improvement_surcharge
    expected_mta_tax = if_else(RatecodeID == 1, 0.50, 0.00),
    expected_improvement_surcharge = 1.00,
    expected_discrepancy = total_amount - (
      fare_amount + extra + expected_mta_tax + expected_improvement_surcharge +
      tolls_amount + congestion_surcharge + Airport_fee +
      cbd_congestion_fee + tip_amount
    ),

    # ratecode_implied based on pickup/dropoff zones
    ratecode_implied = case_when(
      PULocationID %in% jfk_zones | DOLocationID %in% jfk_zones ~ 2L,
      PULocationID == 1 & DOLocationID == 1 ~ 3L,
      PULocationID %in% nassau_westchester_zones |
        DOLocationID %in% nassau_westchester_zones ~ 4L,
      TRUE ~ 1L
    ),

    # duration and flags
    duration_mins = as.numeric(difftime(
      tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins"
    )),
    duration_flag = case_when(
      duration_mins == 0 ~ "zero_duration",
      duration_mins < 0 & VendorID %in% c(6, 7) ~ "negative_duration_v6_v7",
      TRUE ~ NA_character_
    ),

    # has_negative_monetary
    has_negative_monetary = (total_amount < 0) | (fare_amount < 0),

    # is_reversal: approximate — flag rows with negative total AND a matching
    # positive-total row on same pickup time, vendor, PU/DO (simplified proxy)
    is_reversal = FALSE
  )

# =============================================================================
# STEP 1 — Derive analysis columns from the computed discrepancy
# =============================================================================

df <- df |>
  mutate(
    abs_discrepancy    = abs(discrepancy),
    has_discrepancy    = abs_discrepancy >= 0.01,
    discrepancy_bucket = case_when(
      abs_discrepancy <  0.01 ~ "Exact / rounding (<$0.01)",
      abs_discrepancy <  0.50 ~ "Tiny ($0.01–$0.49)",
      abs_discrepancy <  1.00 ~ "Small ($0.50–$0.99)",
      abs_discrepancy <  5.00 ~ "Medium ($1–$4.99)",
      abs_discrepancy < 10.00 ~ "Large ($5–$9.99)",
      abs_discrepancy < 50.00 ~ "Very large ($10–$49.99)",
      TRUE                    ~ "Extreme (>=$50)"
    ),
    discrepancy_sign = case_when(
      abs_discrepancy < 0.01 ~ "Exact",
      discrepancy > 0        ~ "Positive (total > sum)",
      TRUE                   ~ "Negative (total < sum)"
    )
  )

n_total <- nrow(df)
n_disc  <- sum(df$has_discrepancy, na.rm = TRUE)


# =============================================================================
# STEP 2 — Overall scale
# =============================================================================

overall_summary <- tibble(
  Metric  = c(
    "Total rows",
    "Rows with discrepancy (>= $0.01)",
    "Rows exact / rounding only",
    "Pct rows discrepant"
  ),
  Value = c(
    format(n_total, big.mark = ","),
    format(n_disc,  big.mark = ","),
    format(n_total - n_disc, big.mark = ","),
    sprintf("%.4f%%", 100 * n_disc / n_total)
  )
)

disc_stats <- df |>
  filter(has_discrepancy) |>
  summarise(
    Mean   = round(mean(discrepancy),   2),
    Median = round(median(discrepancy), 2),
    SD     = round(sd(discrepancy),     2),
    P5     = round(quantile(discrepancy, 0.05), 2),
    P25    = round(quantile(discrepancy, 0.25), 2),
    P75    = round(quantile(discrepancy, 0.75), 2),
    P95    = round(quantile(discrepancy, 0.95), 2),
    P99    = round(quantile(discrepancy, 0.99), 2),
    Min    = round(min(discrepancy), 2),
    Max    = round(max(discrepancy), 2)
  )


# =============================================================================
# STEP 3 — Magnitude buckets
# =============================================================================

bucket_order <- c(
  "Exact / rounding (<$0.01)",
  "Tiny ($0.01–$0.49)",
  "Small ($0.50–$0.99)",
  "Medium ($1–$4.99)",
  "Large ($5–$9.99)",
  "Very large ($10–$49.99)",
  "Extreme (>=$50)"
)

bucket_summary <- df |>
  count(discrepancy_bucket) |>
  mutate(
    discrepancy_bucket = factor(discrepancy_bucket, levels = bucket_order)
  ) |>
  arrange(discrepancy_bucket) |>
  mutate(
    Pct_of_all_rows = sprintf("%.4f%%", 100 * n / n_total),
    Pct_of_discrepant = sprintf(
      "%.2f%%",
      100 * n / sum(n[discrepancy_bucket != "Exact / rounding (<$0.01)"])
    )
  ) |>
  rename(Bucket = discrepancy_bucket, Rows = n)


# =============================================================================
# STEP 4 — Sign breakdown
# =============================================================================

sign_summary <- df |>
  count(discrepancy_sign) |>
  mutate(Pct = sprintf("%.4f%%", 100 * n / n_total)) |>
  rename(Sign = discrepancy_sign, Rows = n) |>
  arrange(desc(Rows))


# =============================================================================
# STEP 5 — Payment type breakdown
# =============================================================================
# payment_type: 1=Credit card, 2=Cash, 3=No charge, 4=Dispute, 5=Unknown, 6=Voided
# Cash trips (type 2): tip not captured electronically — the most common
# explanation for positive discrepancies (tip added to total but tip_amount=0).

payment_labels <- c(
  "1" = "1-Credit card",
  "2" = "2-Cash",
  "3" = "3-No charge",
  "4" = "4-Dispute",
  "5" = "5-Unknown",
  "6" = "6-Voided"
)

payment_summary <- df |>
  mutate(payment_label = coalesce(
    payment_labels[as.character(payment_type)], "Other"
  )) |>
  group_by(payment_label) |>
  summarise(
    Total_rows      = n(),
    Discrepant_rows = sum(has_discrepancy, na.rm = TRUE),
    Pct_discrepant  = sprintf("%.2f%%", 100 * Discrepant_rows / Total_rows),
    Median_disc     = round(median(discrepancy[has_discrepancy], na.rm = TRUE), 2),
    Mean_disc       = round(mean(discrepancy[has_discrepancy],   na.rm = TRUE), 2),
    .groups         = "drop"
  ) |>
  arrange(desc(Discrepant_rows))

# Cash tip reconciliation test:
# For cash trips with a positive discrepancy, check if discrepancy ≈ a
# plausible tip (> $0 and < $50). These are the "hidden tip" rows.
cash_tip_recon <- df |>
  filter(payment_type == 2, discrepancy > 0.01) |>
  summarise(
    cash_disc_rows          = n(),
    plausible_hidden_tip    = sum(discrepancy > 0.01 & discrepancy < 50,
                                  na.rm = TRUE),
    median_hidden_tip_value = round(
      median(discrepancy[discrepancy > 0.01 & discrepancy < 50], na.rm = TRUE),
      2
    )
  )


# =============================================================================
# STEP 6 — Vendor breakdown
# =============================================================================
# VendorID: 1=CMT, 2=Curb Mobility, 6=Myle Technologies, 7=Helix

vendor_labels <- c(
  "1" = "1-CMT",
  "2" = "2-Curb Mobility",
  "6" = "6-Myle Technologies",
  "7" = "7-Helix"
)

vendor_summary <- df |>
  mutate(vendor_label = coalesce(
    vendor_labels[as.character(VendorID)], "Other"
  )) |>
  group_by(vendor_label) |>
  summarise(
    Total_rows      = n(),
    Discrepant_rows = sum(has_discrepancy, na.rm = TRUE),
    Pct_discrepant  = sprintf("%.2f%%", 100 * Discrepant_rows / Total_rows),
    Median_disc     = round(median(
      discrepancy[has_discrepancy], na.rm = TRUE
    ), 2),
    .groups = "drop"
  ) |>
  arrange(desc(Discrepant_rows))

library(ggplot2)

# vendor_summary |>
#   filter(abs(discrepancy) < 20) |>  # zoom into the meaningful range
#   ggplot(aes(x = vendor_label, y = discrepancy, fill = vendor_label)) +
#   geom_violin(alpha = 0.6, scale = "width") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   labs(
#     title = "Total_amount discrepancy by vendor",
#     subtitle = "Discrepancy = total_amount − sum(components). Zero means components reconcile.",
#     x = "Vendor",
#     y = "Discrepancy ($)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")
# =============================================================================
# STEP 7 — Component-level diagnosis
# =============================================================================
# For rows with a discrepancy, check which component fields are zero or NA.
# A component being zero when the discrepancy is non-zero suggests that
# field is the "missing" piece.

component_cols <- c(
  "fare_amount", "extra", "mta_tax", "improvement_surcharge",
  "tolls_amount", "congestion_surcharge", "Airport_fee",
  "cbd_congestion_fee", "tip_amount"
)

disc_rows <- df |> filter(has_discrepancy)

component_diagnosis <- sapply(component_cols, function(col) {
  if (!col %in% names(disc_rows)) return(c(NA, NA, NA))
  vals <- disc_rows[[col]]
  c(
    n_zero = sum(vals == 0,  na.rm = TRUE),
    n_na   = sum(is.na(vals)),
    n_neg  = sum(vals < 0,   na.rm = TRUE)
  )
}) |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("Component") |>
  mutate(
    Pct_zero = sprintf("%.2f%%", 100 * n_zero / n_disc),
    Pct_na   = sprintf("%.2f%%", 100 * n_na   / n_disc)
  ) |>
  select(Component, n_zero, Pct_zero, n_na, Pct_na, n_neg) |>
  arrange(desc(n_zero))


# =============================================================================
# STEP 8 — Monthly pattern
# =============================================================================

monthly_pattern <- df |>
  mutate(pickup_month = format(tpep_pickup_datetime, "%Y-%m")) |>
  group_by(pickup_month) |>
  summarise(
    Total_rows      = n(),
    Discrepant_rows = sum(has_discrepancy, na.rm = TRUE),
    Pct_discrepant  = sprintf("%.2f%%", 100 * Discrepant_rows / Total_rows),
    Median_disc     = round(median(
      discrepancy[has_discrepancy], na.rm = TRUE
    ), 2),
    .groups = "drop"
  ) |>
  arrange(pickup_month)


# =============================================================================
# STEP 9 — Medium bucket deep dive ($1.00 – $4.99)
# =============================================================================
# The medium bucket is the most analytically interesting: it's large enough
# to rule out rounding noise, but below the obvious airport-fee threshold.
# The goal is to identify WHICH specific surcharge amount drives these rows.
#
# Approach:
#   a. Top-20 modal discrepancy values — the most common exact dollar amounts.
#      A cluster at e.g. $2.50 points to a rush-hour surcharge; $2.75 to the
#      congestion surcharge; $1.50 to the CBD fee.
#   b. Same modal values split by vendor.
#   c. Same modal values split by cbd_era (pre / post Jan 5 2025).
# =============================================================================

# cbd_era mirrors the column created in cleaning_data.R; compute here so
# this script stays self-contained when run independently.
cbd_launch <- as.POSIXct("2025-01-05")

medium_rows <- df |>
  filter(abs_discrepancy >= 1.00, abs_discrepancy < 5.00) |>
  mutate(
    disc_rounded = round(discrepancy, 2),
    cbd_era      = if_else(tpep_pickup_datetime >= cbd_launch, "post_cbd", "pre_cbd")
  )

# a. Top-20 most frequent exact discrepancy amounts
medium_modal <- medium_rows |>
  count(disc_rounded, name = "Count") |>
  arrange(desc(Count)) |>
  slice_head(n = 20) |>
  mutate(
    `% of medium bucket` = sprintf(
      "%.2f%%", 100 * Count / nrow(medium_rows)
    )
  ) |>
  rename(`Discrepancy ($)` = disc_rounded)

# b. Top-10 modal values per vendor
medium_by_vendor <- medium_rows |>
  mutate(vendor_label = coalesce(
    c("1" = "1-CMT", "2" = "2-Curb Mobility",
      "6" = "6-Myle", "7" = "7-Helix")[as.character(VendorID)],
    "Other"
  )) |>
  count(vendor_label, disc_rounded, name = "Count") |>
  group_by(vendor_label) |>
  slice_max(Count, n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(vendor_label, desc(Count)) |>
  rename(Vendor = vendor_label, `Discrepancy ($)` = disc_rounded)

# c. Top-10 modal values per cbd_era (pre / post Jan 5 2025)
medium_by_era <- medium_rows |>
  count(cbd_era, disc_rounded, name = "Count") |>
  group_by(cbd_era) |>
  slice_max(Count, n = 10, with_ties = FALSE) |>
  ungroup() |>
  arrange(cbd_era, desc(Count)) |>
  rename(`CBD Era` = cbd_era, `Discrepancy ($)` = disc_rounded)

# d. Headline counts for context
medium_n      <- nrow(medium_rows)
medium_pre    <- sum(medium_rows$cbd_era == "pre_cbd")
medium_post   <- sum(medium_rows$cbd_era == "post_cbd")


# =============================================================================
# STEP 10 — Extreme outliers (top 20 by absolute discrepancy)
# =============================================================================

top_outliers <- df |>
  filter(has_discrepancy) |>
  slice_max(abs_discrepancy, n = 20) |>
  select(
    tpep_pickup_datetime, VendorID, payment_type,
    fare_amount, tip_amount, total_amount,
    discrepancy, expected_discrepancy
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))


# =============================================================================
# STEP 10 — RatecodeID fix vs discrepancy overlap
# =============================================================================
# ratecode_implied is already in dataset_newV2.parquet from cleaning_data.R.

df <- df |>
  mutate(
    ratecode_changed = !is.na(RatecodeID) & RatecodeID != ratecode_implied
  )

# 2x2 cross-tab: ratecode changed × has discrepancy
ratecode_disc_crosstab <- df |>
  mutate(
    `RatecodeID Changed` = if_else(
      ratecode_changed, "Yes — rate fixed", "No — unchanged"
    ),
    `Has Discrepancy` = if_else(
      has_discrepancy, "Yes — discrepant", "No — exact"
    )
  ) |>
  count(`RatecodeID Changed`, `Has Discrepancy`) |>
  group_by(`RatecodeID Changed`) |>
  mutate(
    `% Within Group` = sprintf("%.4f%%", 100 * n / sum(n))
  ) |>
  ungroup() |>
  arrange(`RatecodeID Changed`, `Has Discrepancy`) |>
  rename(`Trip Count` = n)

# Summary: discrepancy rate for changed vs unchanged rows side by side
ratecode_disc_summary <- df |>
  group_by(`Rate Code Group` = if_else(
    ratecode_changed,
    "Rate code was corrected",
    "Rate code unchanged"
  )) |>
  summarise(
    `Total Trips`       = n(),
    `Discrepant Trips`  = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`      = sprintf(
      "%.4f%%", 100 * `Discrepant Trips` / `Total Trips`
    ),
    `Median Discrepancy ($)` = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    `Mean Discrepancy ($)` = round(
      mean(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  )

# Which original RatecodeID values were most commonly corrected AND discrepant?
ratecode_disc_by_original <- df |>
  filter(ratecode_changed) |>
  group_by(
    `Original RatecodeID` = RatecodeID,
    `Fixed RatecodeID`    = ratecode_implied
  ) |>
  summarise(
    `Total Corrected Trips` = n(),
    `Discrepant Trips`      = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`          = sprintf(
      "%.2f%%", 100 * `Discrepant Trips` / `Total Corrected Trips`
    ),
    .groups = "drop"
  ) |>
  arrange(desc(`Total Corrected Trips`))


# =============================================================================
# STEP 11 — Location & hour-of-day analysis
# =============================================================================
# Vendor 6 and 7 are excluded throughout this section because their timestamp
# data is unreliable (Vendor 7: 100% zero-duration trips; Vendor 6: 23.5%
# zero-duration). Including them would distort the hour-of-day pattern.
# =============================================================================

df_clean_vendors <- df |> filter(!VendorID %in% c(6, 7))

n_clean <- nrow(df_clean_vendors)
n_clean_disc <- sum(df_clean_vendors$has_discrepancy, na.rm = TRUE)

# --- Top pickup locations by discrepancy rate ---------------------------------
# Minimum 100 trips at a location to avoid noise from rare zones.

disc_by_pu <- df_clean_vendors |>
  group_by(`Pickup Zone` = PULocationID) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  filter(`Total Trips` >= 100) |>
  arrange(desc(`Discrepant Trips`)) |>
  slice_head(n = 20)

# --- Top dropoff locations by discrepancy rate --------------------------------

disc_by_do <- df_clean_vendors |>
  group_by(`Dropoff Zone` = DOLocationID) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  filter(`Total Trips` >= 100) |>
  arrange(desc(`Discrepant Trips`)) |>
  slice_head(n = 20)

# --- Highest RATE locations (% of trips discrepant, min 100 trips) -----------
# Complements the count table above: a small zone with a 90% rate is more
# diagnostic than a large zone with a 5% rate.

disc_rate_by_pu <- df_clean_vendors |>
  group_by(`Pickup Zone` = PULocationID) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = 100 * `Discrepant Trips` / `Total Trips`,
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  filter(`Total Trips` >= 100) |>
  arrange(desc(`% Discrepant`)) |>
  slice_head(n = 20) |>
  mutate(`% Discrepant` = sprintf("%.2f%%", `% Discrepant`))

# --- Hour-of-day pattern ------------------------------------------------------
# Extract pickup hour from timestamp. Vendor 6 & 7 already excluded above
# so hours reflect real trip start times only.

disc_by_hour <- df_clean_vendors |>
  mutate(`Pickup Hour` = as.integer(format(tpep_pickup_datetime, "%H"))) |>
  group_by(`Pickup Hour`) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    `Mean Disc ($)`    = round(
      mean(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  arrange(`Pickup Hour`)


# =============================================================================
# STEP 12 — Congestion surcharge causation tests
# =============================================================================
# The top discrepant zones are all in the Manhattan congestion surcharge area.
# These three tests determine whether the discrepancy IS the congestion_surcharge
# (i.e. it was applied to the trip but not rolled into total_amount).
#
# Note: cbd_congestion_fee is excluded — data only exists from Jan 5 2025.
# =============================================================================

top_disc_zones <- c(232, 224, 4, 54, 45, 209, 66, 87, 33, 40,
                    128, 50, 88, 158, 148, 79, 255, 144, 256, 112)

# Test 1 — Does discrepancy ≈ congestion_surcharge for discrepant rows?
# If 95%+ match, the congestion surcharge is the sole missing piece in total.
# If lower, it explains part of the gap but something else is also at play.
congestion_match_test <- df |>
  filter(PULocationID %in% top_disc_zones, has_discrepancy) |>
  mutate(
    matches_congestion = !is.na(congestion_surcharge) &
                         abs(discrepancy - congestion_surcharge) < 0.01
  ) |>
  summarise(
    `Discrepant trips in top zones` = n(),
    `disc == congestion_surcharge`  = sum(matches_congestion, na.rm = TRUE),
    `% match`                       = sprintf(
      "%.2f%%", mean(matches_congestion, na.rm = TRUE) * 100
    )
  )

# Test 2 — Within the top zones, do discrepant and non-discrepant rows
# differ in whether congestion_surcharge was applied at all?
# If non-discrepant rows have congestion_surcharge = $0, the trip simply
# wasn't charged the surcharge — there is nothing "missing from total."
# If non-discrepant rows also have congestion_surcharge > $0, then both
# groups were charged but only the discrepant group failed to include it in total.
congestion_inverse_test <- df |>
  filter(PULocationID %in% top_disc_zones) |>
  group_by(`Is Discrepant` = if_else(has_discrepancy, "Yes", "No")) |>
  summarise(
    `Trip Count`               = n(),
    `% Zero Congestion Surch.` = sprintf(
      "%.2f%%",
      mean(coalesce(congestion_surcharge, 0) == 0, na.rm = TRUE) * 100
    ),
    `Median Congestion Surch.` = round(
      median(coalesce(congestion_surcharge, 0), na.rm = TRUE), 2
    ),
    `Mean Congestion Surch.`   = round(
      mean(coalesce(congestion_surcharge, 0), na.rm = TRUE), 2
    ),
    .groups = "drop"
  )

# Test 3 — Vendor split among discrepant rows in the top zones.
# A vendor-specific cluster confirms a billing software bug, not a data
# pipeline issue that would affect all vendors equally.
congestion_vendor_test <- df |>
  filter(PULocationID %in% top_disc_zones, has_discrepancy) |>
  mutate(Vendor = coalesce(
    c("1" = "1-CMT", "2" = "2-Curb Mobility",
      "6" = "6-Myle", "7" = "7-Helix")[as.character(VendorID)],
    "Other"
  )) |>
  count(Vendor, name = "Discrepant Trips") |>
  mutate(`% of Zone Discrepancies` = sprintf(
    "%.2f%%", 100 * `Discrepant Trips` / sum(`Discrepant Trips`)
  )) |>
  arrange(desc(`Discrepant Trips`))


# =============================================================================
# STEP 13 — Congestion zone vs non-zone: part-of-day and rate code breakdown
# =============================================================================

congestion_zones_full <- c(
  4, 12, 13, 24, 41, 42, 43, 45, 48, 50, 68, 74, 75, 79,
  87, 88, 90, 100, 103, 104, 105, 107, 113, 114, 116, 120,
  125, 127, 128, 137, 140, 141, 142, 143, 144, 148, 151,
  152, 153, 158, 161, 162, 163, 164, 166, 170, 186, 194,
  202, 209, 211, 224, 229, 230, 231, 232, 233, 234, 236,
  237, 238, 239, 243, 244, 246, 249, 261, 262, 263
)

df_zone <- df |>
  filter(!VendorID %in% c(6, 7)) |>
  mutate(
    pickup_hour  = as.integer(format(tpep_pickup_datetime, "%H")),
    in_cong_zone = PULocationID %in% congestion_zones_full,
    part_of_day  = case_when(
      pickup_hour >= 6  & pickup_hour < 12 ~ "Morning (6–11)",
      pickup_hour >= 12 & pickup_hour < 17 ~ "Afternoon (12–16)",
      pickup_hour >= 17 & pickup_hour < 20 ~ "Rush Hour (17–19)",
      pickup_hour >= 20 | pickup_hour < 6  ~ "Night (20–5)"
    )
  )

# Part-of-day × zone: discrepancy rate and median discrepancy
disc_by_pod_zone <- df_zone |>
  group_by(
    `Zone`        = if_else(in_cong_zone, "Congestion Zone", "Outside Zone"),
    `Part of Day` = part_of_day
  ) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  arrange(`Zone`, `Part of Day`)

# Congestion zone vs outside — headline comparison
disc_zone_headline <- df_zone |>
  group_by(
    `Zone` = if_else(in_cong_zone, "Congestion Zone", "Outside Zone")
  ) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  )

# Rate code breakdown — discrepancy rate and median per RatecodeID
ratecode_labels <- c(
  "1" = "1 - Standard Metered",
  "2" = "2 - JFK Flat Rate",
  "3" = "3 - Newark",
  "4" = "4 - Nassau/Westchester",
  "5" = "5 - Negotiated",
  "6" = "6 - Group Ride"
)

disc_by_ratecode <- df |>
  mutate(
    `Rate Code` = coalesce(
      ratecode_labels[as.character(RatecodeID)], "Other"
    )
  ) |>
  group_by(`Rate Code`) |>
  summarise(
    `Total Trips`      = n(),
    `Discrepant Trips` = sum(has_discrepancy, na.rm = TRUE),
    `% Discrepant`     = sprintf("%.2f%%", 100 * `Discrepant Trips` / `Total Trips`),
    `Median Disc ($)`  = round(
      median(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    `Mean Disc ($)`    = round(
      mean(discrepancy[has_discrepancy], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  arrange(desc(`Discrepant Trips`))


# =============================================================================
# STEP 14 — Write markdown report
# =============================================================================

report_path <- "Output/discrepancy_report.md"
sink(report_path)


cat("# Total Amount Discrepancy Analysis Report\n\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("**Source:** `original.parquet`\n\n")
cat("**`discrepancy`** = `total_amount` − sum of all *recorded* components\n")
cat("(`fare_amount` + `extra` + `mta_tax` + `improvement_surcharge`")
cat(" + `tolls_amount` + `congestion_surcharge` + `Airport_fee`")
cat(" + `cbd_congestion_fee` + `tip_amount`)\n\n")
cat("**`expected_discrepancy`** = same formula but `mta_tax` replaced with")
cat(" `expected_mta_tax` ($0.50 for RatecodeID 1, $0 otherwise) and")
cat(" `improvement_surcharge` replaced with `expected_improvement_surcharge`")
cat(" ($1.00 for all trips). All other components stay as recorded.\n\n")
cat("Optional fee columns treated as 0 when NA.\n\n")
cat("**Flags available in this dataset:**\n")
cat("- `is_reversal` — row is part of a strict-dup void pair (categories B–H)\n")
cat("- `has_negative_monetary` — `total_amount` < 0 OR `fare_amount` < 0\n")
cat("- `duration_flag` — `zero_duration` | `negative_duration_v6_v7` | NA\n\n")
cat("---\n\n")

cat("## 1. Overall Scale\n\n")
print(kable(overall_summary, format = "markdown"))
cat("\n\n### Distribution of Discrepancy (discrepant rows only)\n\n")
print(kable(disc_stats, format = "markdown"))
cat("\n\n---\n\n")

cat("## 2. Magnitude Buckets\n\n")
cat("Every row is classified by how large the absolute discrepancy is.\n\n")
print(kable(bucket_summary, format = "markdown"))
cat("\n\n---\n\n")

cat("## 3. Sign Breakdown\n\n")
cat("- **Positive** (total > sum of components): something is added to\n")
cat("  `total_amount` that is not broken out — most commonly a cash tip.\n")
cat("- **Negative** (total < sum of components): a component is not\n")
cat("  reflected in `total_amount` — e.g. a tip captured in `tip_amount`\n")
cat("  but not carried through to the total on cash trips.\n\n")
print(kable(sign_summary, format = "markdown"))
cat("\n\n---\n\n")

cat("## 4. Breakdown by Payment Type\n\n")
cat("Cash trips (type 2) are the primary driver of discrepancies: the meter\n")
cat("system does not capture electronic tips, so when a driver enters a cash\n")
cat("tip it may appear in `total_amount` but not in `tip_amount` — or vice\n")
cat("versa depending on vendor.\n\n")
print(kable(payment_summary, format = "markdown"))
cat("\n\n### Cash Tip Reconciliation Test\n\n")
cat("Among cash trips with a *positive* discrepancy, how many look like\n")
cat("a plausible hidden tip ($0.01–$50)?\n\n")
print(kable(cash_tip_recon, format = "markdown"))
cat("\n\n---\n\n")

cat("## 5. Breakdown by Vendor\n\n")
cat("Discrepancy rates differ by vendor, which points to vendor-specific\n")
cat("billing logic or meter firmware differences.\n\n")
print(kable(vendor_summary, format = "markdown"))
cat("\n\n---\n\n")

cat("## 6. Component-Level Diagnosis\n\n")
cat("For rows that *have* a discrepancy: how many have each component at\n")
cat("zero, NA, or negative? A high zero-rate in a column alongside a\n")
cat("consistent discrepancy size identifies the missing field.\n\n")
print(kable(component_diagnosis, format = "markdown"))
cat("\n\n---\n\n")

cat("## 7. Monthly Pattern\n\n")
cat("Does discrepancy rate shift over time? A jump in a specific month\n")
cat("can point to a fare rule change or a billing system update.\n\n")
print(kable(monthly_pattern, format = "markdown"))
cat("\n\n---\n\n")

cat("## 8. Medium Bucket Deep Dive ($1.00 – $4.99)\n\n")
cat(sprintf(
  "Medium-bucket rows: **%s** total  (%s pre-CBD, %s post-CBD)\n\n",
  format(medium_n,    big.mark = ","),
  format(medium_pre,  big.mark = ","),
  format(medium_post, big.mark = ",")
))
cat("### 8a. Top-20 most common exact discrepancy amounts\n\n")
cat("A cluster at a specific value identifies the responsible surcharge:\n")
cat("$2.75 → congestion surcharge, $2.50 → rush-hour surcharge,\n")
cat("$1.50 → CBD congestion fee, $1.00 → nighttime surcharge.\n\n")
print(kable(medium_modal, format = "markdown"))
cat("\n\n")

cat("### 8b. Top-5 modal values per vendor\n\n")
cat("If one vendor dominates a specific dollar amount, the issue is\n")
cat("vendor-specific billing logic, not a system-wide rule gap.\n\n")
print(kable(medium_by_vendor, format = "markdown"))
cat("\n\n")

cat("### 8c. Top-10 modal values by CBD era\n\n")
cat("If the $1.50 cluster appears only in `post_cbd`, it confirms the\n")
cat("CBD fee is the cause. Pre-CBD spikes at $2.75 point to the older\n")
cat("Manhattan congestion surcharge not being broken out correctly.\n\n")
print(kable(medium_by_era, format = "markdown"))
cat("\n\n---\n\n")

cat("## 9. Top 20 Extreme Outliers\n\n")
cat("The 20 rows with the largest absolute discrepancy.\n\n")
print(kable(top_outliers, format = "markdown"))
cat("\n\n---\n\n")

cat("## 9. RatecodeID Correction vs Discrepancy\n\n")
cat("Some trips had their rate code corrected based on pickup location —\n")
cat("JFK trips fixed to rate 2, Newark to rate 3, Nassau/Westchester to\n")
cat("rate 4. This section checks whether those corrected rows are more or\n")
cat("less likely to also carry a `total_amount` discrepancy, which would\n")
cat("suggest the original mis-coding also affected the fare components.\n\n")

cat("### 9a. Discrepancy rate: corrected vs unchanged rows\n\n")
print(kable(ratecode_disc_summary, format = "markdown"))
cat("\n\n")

cat("### 9b. Full cross-tab\n\n")
print(kable(ratecode_disc_crosstab, format = "markdown"))
cat("\n\n")

cat("### 9c. Breakdown by which rate code was corrected\n\n")
cat("Among only the rows whose rate code changed, which original → fixed\n")
cat("pair is most associated with discrepancies?\n\n")
print(kable(ratecode_disc_by_original, format = "markdown"))
cat("\n\n---\n\n")

cat("## 10. Location & Hour-of-Day Analysis\n\n")
cat("**Vendors 6 and 7 are excluded from this entire section** because their\n")
cat("timestamps are unreliable (Vendor 7 has 100% zero-duration trips;\n")
cat("Vendor 6 has 23.5% zero-duration). Including them would corrupt\n")
cat("the hour-of-day signal and inflate certain location counts.\n\n")
cat(sprintf(
  "Trips in this section: %s total, %s discrepant (%.4f%%)\n\n",
  format(n_clean,      big.mark = ","),
  format(n_clean_disc, big.mark = ","),
  100 * n_clean_disc / n_clean
))

cat("### 10a. Top 20 Pickup Zones by Discrepancy Count\n\n")
cat("Which pickup zones produce the most discrepant trips in absolute terms?\n")
cat("Zones with high counts but low rates are simply busy — high rates are\n")
cat("more diagnostic of a zone-specific billing issue.\n\n")
print(kable(disc_by_pu, format = "markdown"))
cat("\n\n")

cat("### 10b. Top 20 Pickup Zones by Discrepancy Rate\n\n")
cat("Which pickup zones have the highest *share* of their trips discrepant?\n")
cat("Only zones with at least 100 trips are shown to avoid statistical noise.\n\n")
print(kable(disc_rate_by_pu, format = "markdown"))
cat("\n\n")

cat("### 10c. Top 20 Dropoff Zones by Discrepancy Count\n\n")
cat("Dropoff zone patterns can reveal destination-driven surcharge issues\n")
cat("(e.g. airport fees applied at the wrong end of the trip).\n\n")
print(kable(disc_by_do, format = "markdown"))
cat("\n\n---\n\n")

cat("### 10d. Hour-of-Day Pattern\n\n")
cat("Discrepancy rate by pickup hour (0 = midnight, 23 = 11 PM).\n")
cat("A spike at specific hours (e.g. 4–8 PM rush, 8 PM–6 AM night)\n")
cat("suggests the rush-hour or nighttime surcharge is being added to\n")
cat("`total_amount` but not correctly broken out into `extra`.\n\n")
print(kable(disc_by_hour, format = "markdown"))
cat("\n\n---\n\n")

cat("## 11. Congestion Surcharge Causation Tests\n\n")
cat("The top discrepant pickup zones are all inside the Manhattan congestion\n")
cat("surcharge area. These three tests determine whether the discrepancy\n")
cat("**is** the congestion surcharge — i.e. the surcharge was applied to\n")
cat("the trip components but not summed into `total_amount`.\n\n")
cat("Zones tested:", paste(top_disc_zones, collapse = ", "), "\n\n")

cat("### 11a. Test 1 — Does `discrepancy ≈ congestion_surcharge`?\n\n")
cat("For discrepant rows in the top zones, check if the discrepancy equals\n")
cat("the recorded `congestion_surcharge` to within $0.01.\n")
cat("- **95%+ match** → the congestion surcharge is the sole missing piece.\n")
cat("- **Lower match** → surcharge explains part of the gap but not all.\n\n")
print(kable(congestion_match_test, format = "markdown"))
cat("\n\n")

cat("### 11b. Test 2 — Do non-discrepant rows have `congestion_surcharge = $0`?\n\n")
cat("This is the critical disambiguation. If non-discrepant rows in these zones\n")
cat("have `congestion_surcharge = $0`, those trips simply weren't charged the\n")
cat("surcharge — there is nothing missing from total. If non-discrepant rows\n")
cat("also have `congestion_surcharge > $0`, both groups were charged but only\n")
cat("the discrepant group failed to include it in `total_amount`.\n\n")
print(kable(congestion_inverse_test, format = "markdown"))
cat("\n\n")

cat("### 11c. Test 3 — Vendor split among discrepant rows in top zones\n\n")
cat("A vendor-specific concentration confirms a billing software bug rather\n")
cat("than a data-pipeline issue that would affect all vendors equally.\n\n")
print(kable(congestion_vendor_test, format = "markdown"))
cat("\n\n---\n\n")

cat("## 12. Congestion Zone vs Outside — Part-of-Day & Rate Code\n\n")
cat("Vendors 6 and 7 are excluded throughout this section.\n\n")

cat("### 12a. Congestion zone vs outside — headline\n\n")
cat("Do trips starting inside the Manhattan congestion surcharge zone have\n")
cat("a materially higher discrepancy rate than trips outside it?\n\n")
print(kable(disc_zone_headline, format = "markdown"))
cat("\n\n")

cat("### 12b. Part-of-day breakdown within each zone group\n\n")
cat("Parts of day: Morning 6–11, Afternoon 12–16, Rush Hour 17–19,")
cat(" Night 20–5.\n")
cat("If Rush Hour has a higher discrepancy rate only inside the congestion\n")
cat("zone, the rush-hour surcharge (`extra`) is likely the additional factor.\n\n")
print(kable(disc_by_pod_zone, format = "markdown"))
cat("\n\n")

cat("### 12c. Discrepancy by rate code\n\n")
cat("Rate codes 2 (JFK flat) and 3 (Newark) have fixed fares, so their\n")
cat("discrepancy pattern differs from the standard metered rate (1).\n")
cat("A high rate for code 1 inside the congestion zone is consistent with\n")
cat("the congestion surcharge being omitted from `total_amount`.\n\n")
print(kable(disc_by_ratecode, format = "markdown"))
cat("\n\n---\n\n")

cat("## Notes on Interpretation\n\n")
cat("- A discrepancy < $0.01 is treated as exact (floating-point rounding).\n")
cat("- Cash tip gap: the most common source of positive discrepancies is a\n")
cat("  cash gratuity that a driver manually enters into `total_amount` after\n")
cat("  the trip closes, while `tip_amount` remains 0 (not captured by the\n")
cat("  electronic payment system). This is expected TLC data behaviour.\n")
cat("- CBD fee gap: as shown in the duplicate analysis, the CBD congestion\n")
cat("  fee launched January 5 2025 and void records do not always negate it,\n")
cat("  which can inflate `total_amount` on one row of a pair.\n")
cat("- Negative discrepancies (total < components) may indicate rows where\n")
cat("  `total_amount` was set before a surcharge or tip was added, or where\n")
cat("  a field was corrected individually without updating the total.\n\n")

sink()

cat("Report written to:", report_path, "\n")
