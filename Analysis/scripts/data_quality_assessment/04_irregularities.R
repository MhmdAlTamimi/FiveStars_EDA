# =============================================================================
# Section 4: Main Irregularities Found - Refer to section 4 in the Report
# =============================================================================
# Covers:
#   4.1  Timestamp and duration issues
#   4.2  Distance and duration logical mismatches
#   4.3  Coded field violations (RatecodeID, store_and_fwd_flag, payment_type)
#   4.4  Financial irregularities (negatives, mismatch, fare formula, tips, MTA tax)
#   4.5  Airport fee anomalies
#   4.6  Policy and regulatory compliance
# =============================================================================

library(arrow)
library(dplyr)
library(tidyr)

df <- read_parquet("Data/original.parquet")

df <- df %>%
  mutate(duration_min = as.numeric(
    difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")
  ))

total <- nrow(df)

# ========== ZERO DURATION PER VENDOR ==============
payment_compare <- df %>%
  mutate(group = if_else(duration_min == 0, "zero_duration", "rest")) %>%
  count(group, payment_type) %>%
  group_by(group) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = group, values_from = pct, values_fill = 0) %>%
  mutate(difference_pp = round(zero_duration - rest, 2),
         zero_duration = round(zero_duration, 2),
         rest = round(rest, 2)) %>%
  arrange(payment_type)

print(payment_compare)

vendor_compare <- df %>%
  mutate(group = if_else(duration_min == 0, "zero_duration", "rest")) %>%
  count(group, VendorID) %>%
  group_by(group) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = group, values_from = pct, values_fill = 0) %>%
  mutate(difference_pp = round(zero_duration - rest, 2),
         zero_duration = round(zero_duration, 2),
         rest = round(rest, 2)) %>%
  arrange(VendorID)

print(vendor_compare)

# ================================================================
# 4.1  Timestamp and Duration Issues
# ================================================================

cat("=== 4.1 Timestamp and Duration Issues ===\n")

pre_2025_pickup <- df %>% filter(tpep_pickup_datetime < as.POSIXct("2025-01-01"))
cat("Pickups before 2025                :", nrow(pre_2025_pickup), "\n")

april_pickup <- df %>% filter(tpep_pickup_datetime >= as.POSIXct("2025-04-01"))
cat("Pickups in April 2025+             :", nrow(april_pickup), "\n")

pre_2025_dropoff <- df %>% filter(tpep_dropoff_datetime < as.POSIXct("2025-01-01"))
cat("Dropoffs before 2025               :", nrow(pre_2025_dropoff), "\n")

april_dropoff <- df %>% filter(tpep_dropoff_datetime >= as.POSIXct("2025-04-01"))
cat("Dropoffs in April 2025+            :", nrow(april_dropoff), "\n")

negative_dur <- df %>% filter(duration_min < 0)
cat("Negative duration (dropoff < pickup) :", nrow(negative_dur), "\n")

zero_dur <- df %>% filter(duration_min == 0)
cat("Zero-duration trips                :", nrow(zero_dur), "\n")
cat("  With nonzero fare_amount :", sum(zero_dur$fare_amount != 0, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(zero_dur$fare_amount != 0, na.rm = TRUE)))
cat("  With nonzero trip_distance:", sum(zero_dur$trip_distance != 0, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(zero_dur$trip_distance != 0, na.rm = TRUE)))
cat("  Mean fare (nonzero-fare group):",
    round(mean(zero_dur$fare_amount[zero_dur$fare_amount != 0], na.rm = TRUE), 2), "\n")
cat("  Mean distance (nonzero-distance group):",
    round(mean(zero_dur$trip_distance[zero_dur$trip_distance != 0], na.rm = TRUE), 2), "miles\n")
cat("  Payment type breakdown:\n")
print(table(zero_dur$payment_type))


# ===========================================================
# 4.2  Distance and Duration Logical Mismatches
# ===========================================================

cat("\n=== 4.2 Distance and Duration Logical Mismatches ===\n")

neg_distance <- df %>% filter(trip_distance < 0)
cat("Negative trip_distance             :", nrow(neg_distance), "\n")

zero_dist_nonzero_dur <- df %>% filter(trip_distance == 0 & duration_min > 0)
cat("Zero distance with nonzero duration:", nrow(zero_dist_nonzero_dur), "\n")

zero_dur_nonzero_dist <- df %>% filter(duration_min == 0 & trip_distance > 0)
cat("Zero duration with nonzero distance:", nrow(zero_dur_nonzero_dist), "\n")


# =============================================================================
# 4.3  Coded Field Violations
# =============================================================================

cat("\n=== 4.3 Coded Field Violations ===\n")

# RatecodeID — valid codes: 1, 2, 3, 4, 5, 6, 99
oor_ratecode <- df %>%
  filter(!is.na(RatecodeID) & !(RatecodeID %in% c(1, 2, 3, 4, 5, 6, 99)))
cat("RatecodeID invalid                 :", nrow(oor_ratecode),
    sprintf("(%.3f%%)\n", 100 * nrow(oor_ratecode) / total))
cat("  Invalid value breakdown:\n")
print(table(oor_ratecode$RatecodeID))
cat("RatecodeID = 99 (valid TLC Null/Unknown):", sum(df$RatecodeID == 99, na.rm = TRUE), "\n")

cat("\n RatecodeID full distribution:\n")
print(table(df$RatecodeID))
# store_and_fwd_flag — valid values: Y, N
oor_flag <- df %>%
  filter(!is.na(store_and_fwd_flag) & !(store_and_fwd_flag %in% c("Y", "N")))
cat("\nstore_and_fwd_flag invalid         :", nrow(oor_flag),
    sprintf("(%.3f%%)\n", 100 * nrow(oor_flag) / total))
cat("  Invalid value breakdown:\n")
print(table(oor_flag$store_and_fwd_flag))

cat("\n store_and_fwd_flag full distribution:\n")
print(table(df$store_and_fwd_flag))

# payment_type — valid codes: 0, 1, 2, 3, 4, 5, 6
oor_payment <- df %>%
  filter(!is.na(payment_type) & !(payment_type %in% 0:6))
cat("\npayment_type invalid               :", nrow(oor_payment),
    sprintf("(%.3f%%)\n", 100 * nrow(oor_payment) / total))
cat("  Invalid value breakdown:\n")
print(table(oor_payment$payment_type))

cat("\npayment_type full distribution:\n")
print(table(df$payment_type))


# =============================================================================
# 4.4  Financial Irregularities
# =============================================================================

cat("\n=== 4.4A Negative Monetary Values ===\n")

monetary_cols <- c("fare_amount", "total_amount", "improvement_surcharge",
                   "mta_tax", "congestion_surcharge", "extra",
                   "cbd_congestion_fee", "Airport_fee", "tolls_amount", "tip_amount")
for (col in monetary_cols) {
  n <- sum(df[[col]] < 0, na.rm = TRUE)
  cat(sprintf("  %-28s %7d  (%.2f%%)\n", col, n, 100 * n / total))
}

# ---

cat("\n=== 4.4B Fare Component Sum vs total_amount ===\n")
# Expected: total_amount = fare + extra + mta + tip + tolls + improvement +
#           congestion (NA treated as 0) + Airport_fee (NA treated as 0) + cbd
df_sum <- df %>%
  mutate(
    computed_total = fare_amount + extra + mta_tax + tip_amount + tolls_amount +
      improvement_surcharge +
      coalesce(congestion_surcharge, 0) +
      coalesce(Airport_fee, 0) +
      cbd_congestion_fee,
    discrepancy = total_amount - computed_total
  )
mismatch <- df_sum %>% filter(abs(discrepancy) > 0.01)
cat("Rows with mismatch > $0.01:", nrow(mismatch),
    sprintf("(%.1f%% of all records)\n", 100 * nrow(mismatch) / total))
cat("Top discrepancy values:\n")
mismatch %>%
  mutate(disc_rounded = round(discrepancy, 2)) %>%
  count(disc_rounded, sort = TRUE) %>%
  head(5) %>%
  print()

# ---

cat("\n=== 4.4C Fare Formula Violations (Standard Rate) ===\n")
# TLC meter formula: fare = $3.00 flag drop + $3.50/mile (>12 mph) or $0.70/min (<= 12 mph)
# Theoretical maximum: fare_max = $3.00 + $3.50 * distance + $0.70 * duration
# Filter requires duration > 0 AND distance > 0 to avoid zero-duration inflation

df_formula <- df %>%
  filter(!is.na(fare_amount), !is.na(trip_distance), !is.na(duration_min),
         duration_min > 0, trip_distance > 0) %>%
  mutate(
    fare_max   = 3.00 + 3.50 * trip_distance + 0.70 * duration_min,
    fare_floor = 3.00 + 3.50 * trip_distance,
    flag_std_above_max    = RatecodeID == 1 & fare_amount > fare_max,
    flag_std_severe_under = RatecodeID == 1 & fare_floor > 3.00 &
                            fare_amount < fare_floor * 0.80,
    flag_jfk_not_flat     = RatecodeID == 2 & fare_amount > 0 &
                            abs(fare_amount - 70.00) > 0.50,
    flag_newark_below_min = RatecodeID == 3 & fare_amount > 0 & fare_amount < 23.00
  )

cat("Standard rate above theoretical max        :",
    sum(df_formula$flag_std_above_max, na.rm = TRUE), "\n")
cat("Standard rate severe undercharge (<80% dist):",
    sum(df_formula$flag_std_severe_under, na.rm = TRUE), "\n")
cat("JFK flat fare not $70 (reversals excluded) :",
    sum(df_formula$flag_jfk_not_flat, na.rm = TRUE), "\n")
cat("Newark below $23 minimum (reversals excl.) :",
    sum(df_formula$flag_newark_below_min, na.rm = TRUE), "\n")

# ---

cat("\n=== 4.4D Cash Payments with Tip > $0 ===\n")
# TLC: tip_amount is auto-captured for card/Flex Fare only; cash tips are not recorded
# Flex Fare (payment_type=0) legitimately captures tips — only payment_type=2 is flagged
cash_tips <- df %>% filter(payment_type == 2 & tip_amount > 0)
cat("Cash payments (type=2) with tip > $0:", nrow(cash_tips), "\n")

# --- Flex Fare tip verification (payment_type = 0) ---
# The report does NOT flag Flex Fare trips with tip > $0.
# Reason: Flex Fare auto-captures tips like credit card, not via manual
# entry. This is verified by comparing mean tip amounts — similar values
# confirm the tip field is populated automatically, not an error.
flex_tips <- df %>% filter(payment_type == 0 & tip_amount > 0)
cat("Flex Fare (type=0) trips with tip > $0:", nrow(flex_tips), "\n")
cat("  Mean tip — Flex Fare  :",
    round(mean(flex_tips$tip_amount, na.rm = TRUE), 2), "\n")
cat("  Mean tip — Credit card:",
    round(mean(
      df$tip_amount[df$payment_type == 1 & df$tip_amount > 0],
      na.rm = TRUE
    ), 2), "\n")

# ---

cat("\n=== 4.4E mta_tax = $0 on Standard Rate (RatecodeID=1) ===\n")
# Standard metered trips should always carry mta_tax = $0.50
mta_zero_std <- df %>%
  filter(mta_tax == 0, RatecodeID == 1) %>%
  mutate(
    subgroup = case_when(
      fare_amount == 0                         ~ "fare_zero_likely_voided",
      fare_amount < 0                          ~ "fare_negative_reversal",
      payment_type == 3                        ~ "no_charge_trip",
      payment_type == 4                        ~ "dispute_trip",
      fare_amount > 0 & payment_type %in% 0:2 ~ "genuine_anomaly",
      TRUE                                     ~ "other"
    )
  )
cat("Total mta_tax = $0 on standard rate:", nrow(mta_zero_std), "\n")
cat("Subgroup breakdown:\n")
print(count(mta_zero_std, subgroup))


# =============================================================================
# 4.5  Airport Fee Anomalies
# =============================================================================

cat("\n=== 4.5 Airport Fee Anomalies ===\n")
# Airport_fee should be non-zero ONLY for:
#   PULocationID = 132 (JFK, $1.75) or PULocationID = 138 (LGA, $1.25)

airport_wrong_zone <- df %>%
  filter(!is.na(Airport_fee) & Airport_fee > 0 & !(PULocationID %in% c(132, 138)))
cat("Airport fee on non-airport PU zone :", nrow(airport_wrong_zone), "\n")
cat("  Top PULocationIDs with wrong zone fee:\n")
airport_wrong_zone %>% count(PULocationID, sort = TRUE) %>% head(5) %>% print()

airport_wrong_amt <- df %>%
  filter(!is.na(Airport_fee) & Airport_fee > 0 &
         !(round(Airport_fee, 2) %in% c(1.25, 1.75)))
cat("\nAirport fee with invalid amount    :", nrow(airport_wrong_amt), "\n")
cat("  Amount breakdown:\n")
airport_wrong_amt %>%
  mutate(af = round(Airport_fee, 2)) %>%
  count(af, sort = TRUE) %>%
  print()

# Zone 70 vendor breakdown
# Zone 70 = Lincoln Square East (Manhattan) — not an airport.
# The report notes 78% of zone 70 anomalies come from VendorID 2,
# pointing to a meter zone misconfiguration on that vendor's system.
cat("  Zone 70 VendorID breakdown:\n")
airport_wrong_zone %>%
  filter(PULocationID == 70) %>%
  count(VendorID, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Combined total (union of both anomaly types)
# Some records satisfy BOTH conditions (wrong zone AND wrong amount),
# so the combined count is less than 26,597 + 830.
airport_combined <- df %>%
  filter(!is.na(Airport_fee)) %>%
  filter(
    (Airport_fee > 0 & !(PULocationID %in% c(132, 138))) |
    (Airport_fee > 0 & !(round(Airport_fee, 2) %in% c(1.25, 1.75)))
  )
cat("\nCombined airport fee anomalies (union of both flags):",
    nrow(airport_combined), "\n")


# =============================================================================
# 4.6  Policy and Regulatory Compliance
# =============================================================================

cat("\n=== 4.6 Policy and Regulatory Compliance ===\n")

# CBD Congestion Fee — only valid from Jan 5, 2025 onward
policy_start <- as.POSIXct("2025-01-05 00:00:00")
cbd_pre <- df %>%
  filter(tpep_pickup_datetime < policy_start &
         !is.na(cbd_congestion_fee) & cbd_congestion_fee != 0)
cat("CBD fee charged before Jan 5, 2025 :", nrow(cbd_pre), "\n")

# Improvement surcharge — $1.00 rate effective Jan 2023 ($0.30 is obsolete)
old_impr <- df %>% filter(improvement_surcharge == 0.30)
cat("Old improvement surcharge ($0.30)  :", nrow(old_impr), "\n")

cat("\nImprovement surcharge value distribution:\n")
cat("  $1.00 (current correct rate) :", sum(df$improvement_surcharge == 1.00, na.rm = TRUE), "\n")
cat("  $0.30 (old rate, pre-2023)   :", sum(df$improvement_surcharge == 0.30, na.rm = TRUE), "\n")
cat("  -$1.00 (reversal pairs)      :", sum(df$improvement_surcharge == -1.00, na.rm = TRUE), "\n")

# PULocationID / DOLocationID unknown zones (264 = Unknown, 265 = Not Available)
oor_pu <- df %>% filter(PULocationID %in% c(264, 265))
oor_do <- df %>% filter(DOLocationID %in% c(264, 265))
cat("\nPULocationID unknown (264/265):", nrow(oor_pu),
    sprintf("(%.3f%%)\n", 100 * nrow(oor_pu) / total))
cat("DOLocationID unknown (264/265):", nrow(oor_do),
    sprintf("(%.3f%%)\n", 100 * nrow(oor_do) / total))



# ==== Monetary values follow RateCodeID =========================

# ── 1. JFK Flat Rate (RatecodeID = 2) — strictest check ──────────────
# fare_amount should be exactly $70
jfk_fare_check <- df %>%
  filter(RatecodeID == 2) %>%
  group_by(VendorID) %>%
  summarise(
    total_jfk_trips              = n(),
    trips_fare_equals_70         = sum(fare_amount == 70, na.rm = TRUE),
    trips_fare_not_70            = sum(fare_amount != 70, na.rm = TRUE),
    pct_correct_fare             = round(trips_fare_equals_70 / total_jfk_trips * 100, 2),
    avg_fare_when_not_70         = round(mean(fare_amount[fare_amount != 70], na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("=== JFK Flat Rate Check (RatecodeID = 2, expected fare = $70) ===\n")
print(jfk_fare_check, n = Inf, width = Inf)


# ── 2. Standard Rate (RatecodeID = 1) — structural checks ────────────
# Can't verify exact meter math, but can check:
#   - fare >= $3.00 (initial charge)
#   - surcharges in expected values
standard_rate_check <- df %>%
  filter(RatecodeID == 1) %>%
  group_by(VendorID) %>%
  summarise(
    total_standard_trips         = n(),
    trips_fare_below_initial_3   = sum(fare_amount < 3.00, na.rm = TRUE),
    pct_fare_below_initial_3     = round(trips_fare_below_initial_3 / total_standard_trips * 100, 2),
    trips_fare_negative          = sum(fare_amount < 0, na.rm = TRUE),
    pct_fare_negative            = round(trips_fare_negative / total_standard_trips * 100, 2),
    trips_mta_tax_not_0.5        = sum(mta_tax != 0.50, na.rm = TRUE),
    pct_mta_tax_not_0.5          = round(trips_mta_tax_not_0.5 / total_standard_trips * 100, 2),
    trips_improvement_sur_not_1  = sum(improvement_surcharge != 1.0, na.rm = TRUE),
    pct_improvement_sur_not_1    = round(trips_improvement_sur_not_1 / total_standard_trips * 100, 2),
    .groups = "drop"
  )

cat("\n=== Standard Rate Check (RatecodeID = 1) ===\n")
print(standard_rate_check, n = Inf, width = Inf)


# ── 3. Newark (RatecodeID = 3) — metered + $20 surcharge ─────────────
# fare should be metered (>= $3 initial), and there should be tolls
newark_check <- df %>%
  filter(RatecodeID == 3) %>%
  group_by(VendorID) %>%
  summarise(
    total_newark_trips           = n(),
    trips_fare_below_initial_3   = sum(fare_amount < 3.00, na.rm = TRUE),
    pct_fare_below_initial_3     = round(trips_fare_below_initial_3 / total_newark_trips * 100, 2),
    trips_with_zero_tolls        = sum(tolls_amount == 0, na.rm = TRUE),
    pct_with_zero_tolls          = round(trips_with_zero_tolls / total_newark_trips * 100, 2),
    avg_fare_amount              = round(mean(fare_amount, na.rm = TRUE), 2),
    avg_tolls_amount             = round(mean(tolls_amount, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n=== Newark Check (RatecodeID = 3, metered + tolls expected) ===\n")
print(newark_check, n = Inf, width = Inf)


# ── 4. Negotiated Fare (RatecodeID = 5) — sanity checks only ─────────
# No fixed rule, but fare should be positive and reasonable
negotiated_check <- df %>%
  filter(RatecodeID == 5) %>%
  group_by(VendorID) %>%
  summarise(
    total_negotiated_trips       = n(),
    trips_fare_zero_or_negative  = sum(fare_amount <= 0, na.rm = TRUE),
    pct_fare_zero_or_negative    = round(trips_fare_zero_or_negative / total_negotiated_trips * 100, 2),
    avg_fare_amount              = round(mean(fare_amount, na.rm = TRUE), 2),
    median_fare_amount           = round(median(fare_amount, na.rm = TRUE), 2),
    max_fare_amount              = max(fare_amount, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Negotiated Fare Check (RatecodeID = 5) ===\n")
print(negotiated_check, n = Inf, width = Inf)


# ── 5. Total Amount Consistency (all ratecodes) ──────────────────────
# total_amount should ≈ fare + extras + mta_tax + tip + tolls +
#                        improvement_surcharge + congestion_surcharge
total_amount_check <- df %>%
  filter(!is.na(RatecodeID)) %>%
  mutate(
    calculated_total = fare_amount + extra + mta_tax + tip_amount +
                       tolls_amount + improvement_surcharge +
                       congestion_surcharge + Airport_fee,
    total_difference = round(abs(total_amount - calculated_total), 2),
    totals_match     = total_difference < 0.01
  ) %>%
  group_by(VendorID, RatecodeID) %>%
  summarise(
    total_trips                  = n(),
    trips_where_totals_mismatch  = sum(!totals_match, na.rm = TRUE),
    pct_totals_mismatch          = round(trips_where_totals_mismatch / total_trips * 100, 2),
    avg_difference_when_mismatch = round(mean(total_difference[!totals_match], na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n=== Total Amount Consistency Check (all ratecodes) ===\n")
print(total_amount_check, n = Inf, width = Inf)



# =============== Payment Type and Negative Values ===============
# ── 1. Payment Types 3, 4, 6, 0 and Negative Monetary Values ─────────
# Payment types: 3=No charge, 4=Dispute, 5=Unknown, 6=Voided, 0=Unknown

monetary_vars <- c("fare_amount", "extra", "mta_tax", "tip_amount", 
                   "tolls_amount", "improvement_surcharge", 
                   "congestion_surcharge", "Airport_fee", "total_amount")

df %>%
  filter(payment_type %in% c(0, 3, 4, 6)) %>%
  group_by(payment_type) %>%
  summarise(
    total_trips_with_this_payment   = n(),
    across(all_of(monetary_vars), list(
      negative_count = ~ sum(.x < 0, na.rm = TRUE),
      pct_negative   = ~ round(sum(.x < 0, na.rm = TRUE) / n() * 100, 2),
      avg_when_negative = ~ round(mean(.x[.x < 0], na.rm = TRUE), 2)
    ), .names = "{.col}__{.fn}"),
    .groups = "drop"
  ) %>%
  print(n = Inf, width = Inf)
