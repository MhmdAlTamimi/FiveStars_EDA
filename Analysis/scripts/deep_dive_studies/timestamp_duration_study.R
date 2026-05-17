# =============================================================================
# TIMESTAMP & DURATION ISSUES — NYC Taxi Data (Jan–Mar 2025)
# Writes a markdown report to: timestamp_duration_report.md
#
# Issues covered:
#   1. Out-of-range timestamps  (pickup or dropoff outside Jan–Mar 2025)
#   2. Negative duration        (dropoff recorded before pickup)
#   3. Zero duration            (dropoff recorded at the exact same second)
#
# All findings are broken down by vendor to identify the source.
# =============================================================================

library(arrow)
library(dplyr)
library(knitr)

df <- read_parquet("Data/original.parquet")

# Vendor lookup — used throughout to make tables human-readable
vendor_names <- c(
  "1" = "CMT",
  "2" = "Curb Mobility",
  "6" = "Myle Technologies",
  "7" = "Helix"
)

df <- df |>
  mutate(
    duration_mins = as.numeric(
      difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")
    ),
    vendor_name = coalesce(vendor_names[as.character(VendorID)], "Unknown")
  )

n_total <- nrow(df)
window_start <- as.POSIXct("2025-01-01", tz = "UTC")
window_end   <- as.POSIXct("2025-04-01", tz = "UTC")


# =============================================================================
# SECTION 1 — Out-of-range timestamps
# =============================================================================

# Flag each row for each specific timestamp issue
df <- df |>
  mutate(
    pickup_before_window  = tpep_pickup_datetime  <  window_start,
    pickup_after_window   = tpep_pickup_datetime  >= window_end,
    dropoff_before_window = tpep_dropoff_datetime <  window_start,
    dropoff_after_window  = tpep_dropoff_datetime >= window_end,
    any_ts_issue          = pickup_before_window | pickup_after_window |
                            dropoff_before_window | dropoff_after_window
  )

# Overall headline counts
ts_headline <- tibble(
  Issue = c(
    "Pickup before Jan 1 2025",
    "Pickup on or after Apr 1 2025",
    "Dropoff before Jan 1 2025",
    "Dropoff on or after Apr 1 2025",
    "Any out-of-range timestamp"
  ),
  `Trip Count` = c(
    sum(df$pickup_before_window),
    sum(df$pickup_after_window),
    sum(df$dropoff_before_window),
    sum(df$dropoff_after_window),
    sum(df$any_ts_issue)
  )
) |>
  mutate(`% of Dataset` = sprintf("%.4f%%", 100 * `Trip Count` / n_total))

# Per-vendor breakdown of out-of-range pickups and dropoffs
ts_by_vendor <- df |>
  group_by(`Vendor` = vendor_name) |>
  summarise(
    `Total Trips`                   = n(),
    `Pickup Before 2025`            = sum(pickup_before_window),
    `Pickup Apr 2025+`              = sum(pickup_after_window),
    `Dropoff Before 2025`           = sum(dropoff_before_window),
    `Dropoff Apr 2025+`             = sum(dropoff_after_window),
    `Any Timestamp Issue`           = sum(any_ts_issue),
    `% Trips With Timestamp Issue`  = sprintf(
      "%.4f%%", 100 * `Any Timestamp Issue` / `Total Trips`
    ),
    .groups = "drop"
  ) |>
  arrange(desc(`Any Timestamp Issue`))

# Earliest and latest timestamps per vendor (to show the actual extent)
ts_range_by_vendor <- df |>
  group_by(`Vendor` = vendor_name) |>
  summarise(
    `Earliest Pickup`  = format(min(tpep_pickup_datetime,  na.rm = TRUE),
                                "%Y-%m-%d %H:%M"),
    `Latest Pickup`    = format(max(tpep_pickup_datetime,  na.rm = TRUE),
                                "%Y-%m-%d %H:%M"),
    `Earliest Dropoff` = format(min(tpep_dropoff_datetime, na.rm = TRUE),
                                "%Y-%m-%d %H:%M"),
    `Latest Dropoff`   = format(max(tpep_dropoff_datetime, na.rm = TRUE),
                                "%Y-%m-%d %H:%M"),
    .groups = "drop"
  )


# =============================================================================
# SECTION 2 — Negative duration
# =============================================================================

neg_dur <- df |> filter(duration_mins < 0)

neg_dur_headline <- tibble(
  Metric  = c(
    "Total negative-duration trips",
    "% of all trips",
    "Shortest (most negative) duration",
    "Median duration among these trips"
  ),
  Value = c(
    format(nrow(neg_dur), big.mark = ","),
    sprintf("%.4f%%", 100 * nrow(neg_dur) / n_total),
    sprintf("%.1f mins", min(neg_dur$duration_mins)),
    sprintf("%.1f mins", median(neg_dur$duration_mins))
  )
)

neg_dur_by_vendor <- df |>
  group_by(`Vendor` = vendor_name) |>
  summarise(
    `Total Trips`              = n(),
    `Negative Duration Trips`  = sum(duration_mins < 0, na.rm = TRUE),
    `% of Vendor Trips`        = sprintf(
      "%.4f%%", 100 * `Negative Duration Trips` / `Total Trips`
    ),
    `Most Negative (mins)`     = if (any(duration_mins < 0, na.rm = TRUE))
                                   round(min(duration_mins[duration_mins < 0],
                                             na.rm = TRUE), 1)
                                 else NA_real_,
    `Median Duration (mins)`   = if (any(duration_mins < 0, na.rm = TRUE))
                                   round(median(duration_mins[duration_mins < 0],
                                                na.rm = TRUE), 1)
                                 else NA_real_,
    `Avg Fare ($)`             = if (any(duration_mins < 0, na.rm = TRUE))
                                   round(mean(fare_amount[duration_mins < 0],
                                              na.rm = TRUE), 2)
                                 else NA_real_,
    .groups = "drop"
  ) |>
  arrange(desc(`Negative Duration Trips`))

# Five most extreme examples
neg_dur_examples <- neg_dur |>
  slice_min(duration_mins, n = 5) |>
  select(
    `Vendor`       = vendor_name,
    `Pickup Time`  = tpep_pickup_datetime,
    `Dropoff Time` = tpep_dropoff_datetime,
    `Duration (mins)` = duration_mins,
    `Fare ($)`     = fare_amount,
    `Total ($)`    = total_amount,
    `Payment Type` = payment_type
  ) |>
  mutate(
    `Pickup Time`     = format(`Pickup Time`,  "%Y-%m-%d %H:%M:%S"),
    `Dropoff Time`    = format(`Dropoff Time`, "%Y-%m-%d %H:%M:%S"),
    `Duration (mins)` = round(`Duration (mins)`, 1),
    `Fare ($)`        = round(`Fare ($)`, 2),
    `Total ($)`       = round(`Total ($)`, 2)
  )


# =============================================================================
# SECTION 3 — Zero duration
# =============================================================================

zero_dur <- df |> filter(duration_mins == 0)

zero_dur_headline <- tibble(
  Metric = c(
    "Total zero-duration trips",
    "% of all trips",
    "Of those — have a non-zero fare",
    "Of those — have a non-zero distance",
    "Average fare on trips that have one",
    "Average distance on trips that have one"
  ),
  Value = c(
    format(nrow(zero_dur), big.mark = ","),
    sprintf("%.4f%%", 100 * nrow(zero_dur) / n_total),
    sprintf("%s  (%.1f%%)",
            format(sum(zero_dur$fare_amount != 0, na.rm = TRUE), big.mark = ","),
            100 * mean(zero_dur$fare_amount != 0, na.rm = TRUE)),
    sprintf("%s  (%.1f%%)",
            format(sum(zero_dur$trip_distance != 0, na.rm = TRUE), big.mark = ","),
            100 * mean(zero_dur$trip_distance != 0, na.rm = TRUE)),
    sprintf("$%.2f",
            mean(zero_dur$fare_amount[zero_dur$fare_amount != 0], na.rm = TRUE)),
    sprintf("%.2f miles",
            mean(zero_dur$trip_distance[zero_dur$trip_distance != 0],
                 na.rm = TRUE))
  )
)

zero_dur_by_vendor <- df |>
  group_by(`Vendor` = vendor_name) |>
  summarise(
    `Total Trips`            = n(),
    `Zero Duration Trips`    = sum(duration_mins == 0, na.rm = TRUE),
    `% of Vendor Trips`      = sprintf(
      "%.2f%%", 100 * `Zero Duration Trips` / `Total Trips`
    ),
    `Has Non-Zero Fare`      = sum(
      duration_mins == 0 & fare_amount != 0, na.rm = TRUE
    ),
    `Has Non-Zero Distance`  = sum(
      duration_mins == 0 & trip_distance != 0, na.rm = TRUE
    ),
    `Avg Fare on Zero Trips ($)` = round(
      mean(fare_amount[duration_mins == 0], na.rm = TRUE), 2
    ),
    .groups = "drop"
  ) |>
  arrange(desc(`Zero Duration Trips`))

# Payment type mix for zero-duration trips vs. the rest
zero_vs_rest_payment <- df |>
  mutate(
    `Trip Group`   = if_else(duration_mins == 0, "Zero Duration", "Normal Trip"),
    `Payment Type` = case_when(
      payment_type == 1 ~ "1 - Credit Card",
      payment_type == 2 ~ "2 - Cash",
      payment_type == 3 ~ "3 - No Charge",
      payment_type == 4 ~ "4 - Dispute",
      payment_type == 5 ~ "5 - Unknown",
      payment_type == 6 ~ "6 - Voided Trip",
      TRUE              ~ "Other"
    )
  ) |>
  count(`Trip Group`, `Payment Type`) |>
  group_by(`Trip Group`) |>
  mutate(`% Within Group` = sprintf("%.1f%%", 100 * n / sum(n))) |>
  ungroup() |>
  arrange(`Trip Group`, desc(n)) |>
  rename(`Trip Count` = n)


# =============================================================================
# SECTION 4 — Combined vendor heat map (all issues in one table)
# =============================================================================

combined_by_vendor <- df |>
  group_by(`Vendor` = vendor_name) |>
  summarise(
    `Total Trips`            = n(),
    `Out-of-Range Timestamp` = sum(any_ts_issue,        na.rm = TRUE),
    `Negative Duration`      = sum(duration_mins < 0,   na.rm = TRUE),
    `Zero Duration`          = sum(duration_mins == 0,  na.rm = TRUE),
    `Any Issue`              = sum(
      any_ts_issue | duration_mins < 0 | duration_mins == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    `% Out-of-Range`   = sprintf("%.4f%%",
                           100 * `Out-of-Range Timestamp` / `Total Trips`),
    `% Negative Dur.`  = sprintf("%.4f%%",
                           100 * `Negative Duration`      / `Total Trips`),
    `% Zero Dur.`      = sprintf("%.2f%%",
                           100 * `Zero Duration`          / `Total Trips`),
    `% Any Issue`      = sprintf("%.2f%%",
                           100 * `Any Issue`              / `Total Trips`)
  ) |>
  arrange(desc(`Any Issue`))


# =============================================================================
# Write markdown report
# =============================================================================

report_path <- "Output/timestamp_duration_report.md"
sink(report_path)

cat("# Timestamp & Duration Issues Report\n\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("**Dataset window:** January 1 2025 – March 31 2025  \n")
cat("**Total trips:**", format(n_total, big.mark = ","), "\n\n")
cat("---\n\n")

# ---- Section 1 ----
cat("## 1. Out-of-Range Timestamps\n\n")
cat("Trips whose pickup or dropoff time falls outside the expected dataset")
cat(" window (Jan 1 – Mar 31 2025). These may be clock errors, data entry")
cat(" mistakes, or trips that leaked in from outside the reporting period.\n\n")

cat("### 1a. How many trips are affected?\n\n")
print(kable(ts_headline, format = "markdown", align = c("l", "r", "r")))
cat("\n\n")

cat("### 1b. Which vendors are responsible?\n\n")
print(kable(ts_by_vendor, format = "markdown", align = "lrrrrrrr"))
cat("\n\n")

cat("### 1c. Actual timestamp range per vendor\n\n")
cat("Shows the earliest and latest pickup and dropoff recorded for each")
cat(" vendor — useful for spotting how far out-of-range the outliers are.\n\n")
print(kable(ts_range_by_vendor, format = "markdown"))
cat("\n\n---\n\n")

# ---- Section 2 ----
cat("## 2. Negative Duration Trips\n\n")
cat("Trips where the dropoff time is recorded *before* the pickup time.")
cat(" This is logically impossible and indicates a data entry error,")
cat(" a system clock discrepancy between the pickup and dropoff event,")
cat(" or a record where the two timestamps were swapped.\n\n")

cat("### 2a. Overall scale\n\n")
print(kable(neg_dur_headline, format = "markdown", align = c("l", "r")))
cat("\n\n")

cat("### 2b. Breakdown by vendor\n\n")
print(kable(neg_dur_by_vendor, format = "markdown", align = "lrrrrrr"))
cat("\n\n")

cat("### 2c. Five most extreme examples\n\n")
cat("The trips with the largest time reversal, to illustrate the severity.\n\n")
print(kable(neg_dur_examples, format = "markdown"))
cat("\n\n---\n\n")

# ---- Section 3 ----
cat("## 3. Zero Duration Trips\n\n")
cat("Trips where pickup and dropoff were recorded at the exact same second.")
cat(" A small number are expected (e.g. cancelled trips, system test records),")
cat(" but a large share with non-zero fares or distances suggests meter")
cat(" firmware logging the dropoff event at the wrong time.\n\n")

cat("### 3a. Overall scale\n\n")
print(kable(zero_dur_headline, format = "markdown", align = c("l", "r")))
cat("\n\n")

cat("### 3b. Breakdown by vendor\n\n")
cat("The `Has Non-Zero Fare` and `Has Non-Zero Distance` columns show how")
cat(" many zero-duration trips still carried a real fare or distance —")
cat(" these are the most suspicious ones.\n\n")
print(kable(zero_dur_by_vendor, format = "markdown", align = "lrrrrrr"))
cat("\n\n")

cat("### 3c. Payment type mix — zero-duration vs. normal trips\n\n")
cat("A shift in payment type mix can reveal whether zero-duration trips are")
cat(" a specific type (e.g. mostly voided or no-charge) or a representative")
cat(" cross-section of all trips.\n\n")
print(kable(zero_vs_rest_payment, format = "markdown"))
cat("\n\n---\n\n")

# ---- Section 4 ----
cat("## 4. Vendor Summary — All Issues Combined\n\n")
cat("One row per vendor showing the raw count and rate for each issue type.")
cat(" Use this table to identify whether problems are vendor-specific")
cat(" (pointing to a hardware or firmware issue) or spread evenly")
cat(" (pointing to a systemic data pipeline problem).\n\n")
print(kable(combined_by_vendor, format = "markdown", align = "lrrrrrrrrrr"))
cat("\n\n---\n\n")

cat("## Notes\n\n")
cat("- **Out-of-range timestamps** are the rarest issue and usually trace")
cat(" to a specific vendor whose meter software uses an incorrect base date.\n")
cat("- **Negative duration** trips are almost always caused by a clock")
cat(" that drifts or resets between the pickup and dropoff event on the")
cat(" same device — look for the vendor with the highest rate.\n")
cat("- **Zero duration** trips with a real fare are the most actionable:")
cat(" the meter ran but the dropoff timestamp was never updated, leaving")
cat(" both events stamped to the pickup second.\n")
cat("- Trips may be counted in more than one issue category")
cat(" (e.g. a zero-duration trip could also have an out-of-range timestamp).\n\n")

sink()

cat("Report written to:", report_path, "\n")
