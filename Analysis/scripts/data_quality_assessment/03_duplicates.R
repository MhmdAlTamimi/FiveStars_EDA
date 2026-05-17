# =============================================================================
# Section 3: Duplicate and Near-Duplicate Records - 
# Refers to section on the report
# =============================================================================
# Covers:
#   - Exact row duplicates (all columns identical)
#   - Duplicate trip_ids
#   - Exact duplicates when trip_id is excluded from comparison
#   - Near-duplicates under two composite key definitions
#   - Classification of near-duplicate pair groups (categories A–F)
# =============================================================================

library(arrow)
library(dplyr)

df <- read_parquet("Data/original.parquet")


# --- 3.1 Exact Duplicates -----------------------------------------------------

# cat("Exact duplicate rows (all columns identical):", sum(duplicated(df)), "\n")
# cat("Duplicate trip_ids                          :", sum(duplicated(df$trip_id)), "\n")

# Exact duplicates excluding trip_id (trip_id is always unique, but the trip
# content itself may be repeated)
check_cols <- setdiff(names(df), "trip_id")
exact_dups_excl_id <- df %>%
  group_by(across(all_of(check_cols))) %>%
  filter(n() > 1) %>%
  ungroup()
# --- 3.2 Near-Duplicates: Two Key Definitions ---------------------------------

# Simple key: same pickup+dropoff timestamps AND same PU/DO zone
simple_dups <- df %>%
  group_by(tpep_pickup_datetime, tpep_dropoff_datetime, PULocationID, DOLocationID) %>%
  filter(n() > 1) %>%
  mutate(simple_dup_group_id = cur_group_id()) %>%
  ungroup()

cat("\nSimple key near-duplicates (datetime + PU + DO):\n")
cat("  Rows  :", nrow(simple_dups), "\n")
cat("  Groups:", n_distinct(simple_dups$simple_dup_group_id), "\n")

# Strict key: simple key + trip_distance + VendorID + RatecodeID
strict_dups <- df %>%
  group_by(tpep_pickup_datetime, tpep_dropoff_datetime,
           PULocationID, DOLocationID, trip_distance, VendorID, RatecodeID, payment_type) %>%
  filter(n() > 1) %>%
  mutate(dup_group_id = cur_group_id()) %>%
  ungroup()

cat("\nStrict key near-duplicates (simple key + distance + vendor + rate):\n")
cat("  Rows  :", nrow(strict_dups), "\n")
cat("  Groups:", n_distinct(strict_dups$dup_group_id), "\n")


# --- 3.3 Category Classification of Pair Groups (size == 2) ------------------
#
# The NYC taxi billing system generates a mirror record whenever a completed
# trip is voided. The original and voided records share identical trip metadata
# but have all monetary values negated, so they sum to approximately zero.
#
# Category A: Same-sign fares — true double entries (no cancellation)
# Category B: All monetary fields cancel perfectly — exact mirror
# Category C: Mirror except tip_amount differs between the pair
# Category D: Mirror except surcharge fields (congestion/airport/CBD)
# Category E: Other / unclassified pairs
# Category F: Groups with 3+ rows (not classifiable as pairs)
# =============================================================================
# DUPLICATE PAIR ANALYSIS — NYC Taxi Data
# =============================================================================
# Goal: understand what kinds of duplicates exist in strict_dups.
#
# Background:
#   strict_dups groups rows by trip identity (pickup/dropoff time, locations,
#   distance, vendor, ratecode). Anything outside those 7 fields — including
#   ALL monetary values — is free to vary within a group.
#
# A "pair" = a duplicate group containing exactly 2 rows.
# We classify each pair by how its money fields relate:
#   - Do the fares have opposite signs (one void of the other)?
#   - Do the core charges cancel out?
#   - Do tips cancel? Do surcharges cancel?
# =============================================================================


# --- STEP 1: Build per-pair summary of monetary behavior ---------------------
# For each pair, we compute:
#   * sign counts: how many rows have positive vs negative fares
#   * sums of every money field: ~0 means the pair cancels on that field
#   * total_amount on each row, to compare against the sum of components later

pair_class <- strict_dups %>%
  group_by(dup_group_id) %>%
  filter(n() == 2) %>%
  summarise(
    # Sign counts on fare_amount — tells us if this looks like a void pair
    n_pos_fare = sum(fare_amount > 0, na.rm = TRUE),
    n_neg_fare = sum(fare_amount < 0, na.rm = TRUE),

    # Sums of each money field. If ~0, the two rows offset each other.
    sum_fare        = sum(fare_amount),
    sum_extra       = sum(extra),
    sum_mta_tax     = sum(mta_tax),
    sum_improvement = sum(improvement_surcharge),
    sum_tolls       = sum(tolls_amount),
    sum_congestion  = sum(congestion_surcharge,  na.rm = TRUE),
    sum_airport     = sum(Airport_fee,           na.rm = TRUE),
    sum_cbd         = sum(cbd_congestion_fee,    na.rm = TRUE),
    sum_tip         = sum(tip_amount),
    sum_total       = sum(total_amount),

    # Keep raw row-level totals so we can check the "components vs total" mismatch
    row1_total = first(total_amount),
    row2_total = nth(total_amount, 2),
    row1_fare  = first(fare_amount),
    row2_fare  = nth(fare_amount, 2),

    .groups = "drop"
  ) %>%
  mutate(
    # --- Pair-shape flags ---------------------------------------------------
    is_opposite_sign = n_pos_fare == 1 & n_neg_fare == 1,  # likely a void
    is_same_sign     = n_pos_fare == 2 | n_neg_fare == 2,  # both live OR both void

    # --- Cancellation flags (within $0.01 tolerance for float safety) -------
    core_charges_cancel = abs(sum_fare)        < 0.01 &
                          abs(sum_extra)       < 0.01 &
                          abs(sum_mta_tax)     < 0.01 &
                          abs(sum_improvement) < 0.01,

    tip_cancels = abs(sum_tip) < 0.01,

    surcharges_cancel = abs(sum_congestion) < 0.01 &
                        abs(sum_airport)    < 0.01 &
                        abs(sum_cbd)        < 0.01 &
                        abs(sum_tolls)      < 0.01,

    # --- Reconciliation check ----------------------------------------------
    # In a clean record: total_amount should equal the sum of all components.
    # We compute what total *should* be and compare it to what's recorded.
    expected_sum_total = sum_fare + sum_extra + sum_mta_tax + sum_improvement +
                         sum_tolls + sum_congestion + sum_airport + sum_cbd +
                         sum_tip,
    total_reconciles   = abs(sum_total - expected_sum_total) < 0.01,
    total_discrepancy  = sum_total - expected_sum_total,

    # --- Category assignment ------------------------------------------------
    category = case_when(
      is_same_sign                                                              ~ "A_same_sign_double_charge",
      is_opposite_sign & core_charges_cancel &  tip_cancels &  surcharges_cancel ~ "B_exact_mirror_void",
      is_opposite_sign & core_charges_cancel & !tip_cancels &  surcharges_cancel ~ "C_mirror_except_tip",
      is_opposite_sign & core_charges_cancel &  tip_cancels & !surcharges_cancel ~ "D_mirror_except_surcharge",
      is_opposite_sign & core_charges_cancel & !tip_cancels & !surcharges_cancel ~ "E_mirror_except_tip_and_surcharge",
      is_opposite_sign & !core_charges_cancel                                    ~ "F_partial_void_core_differs",
      TRUE                                                                        ~ "G_other_unclassified"
    )
  )


# --- STEP 2: Attach categories back to row-level data ------------------------
# Groups with 3+ rows weren't classified above; they become category H.

strict_dups_classified <- strict_dups %>%
  left_join(pair_class %>% select(dup_group_id, category), by = "dup_group_id") %>%
  mutate(category = if_else(is.na(category), "H_group_size_gt_2", category))


# --- STEP 3: Headline breakdown ----------------------------------------------

category_labels <- c(
  A_same_sign_double_charge          = "A. Same-sign (both live OR both voided)",
  B_exact_mirror_void                = "B. Exact mirror — perfect cancellation",
  C_mirror_except_tip                = "C. Mirror except tip differs",
  D_mirror_except_surcharge          = "D. Mirror except surcharge differs",
  E_mirror_except_tip_and_surcharge  = "E. Mirror except BOTH tip and surcharge differ",
  F_partial_void_core_differs        = "F. Opposite-sign but core charges don't cancel",
  G_other_unclassified               = "G. Other / unclassified",
  H_group_size_gt_2                  = "H. Groups with 3+ rows (not a pair)"
)

cat("\n========================================================\n")
cat(" DUPLICATE PAIR CLASSIFICATION — HEADLINE\n")
cat("========================================================\n")

cat_summary <- strict_dups_classified %>%
  count(category) %>%
  mutate(
    Category    = category_labels[category],
    Pairs       = if_else(category == "H_group_size_gt_2",
                          NA_integer_, as.integer(n / 2)),
    Share_pct   = sprintf("%.2f%%", 100 * n / sum(n))
  ) %>%
  select(Category, Rows = n, Pairs, Share_pct) %>%
  arrange(desc(Rows))

print(cat_summary, n = Inf)


# --- STEP 4: Negative-fare diagnostics ---------------------------------------
# Where do the negative fares actually live? In a clean void system, every
# negative fare should be paired with a positive one (categories B–F).
# Negatives showing up in A or H are surprising and worth flagging.

cat("\n========================================================\n")
cat(" WHERE DO NEGATIVE FARES LIVE?\n")
cat("========================================================\n")

negative_fare_by_category <- strict_dups_classified %>%
  group_by(category) %>%
  summarise(
    total_rows      = n(),
    rows_neg_fare   = sum(fare_amount < 0, na.rm = TRUE),
    rows_zero_fare  = sum(fare_amount == 0, na.rm = TRUE),
    min_fare        = min(fare_amount, na.rm = TRUE),
    max_fare        = max(fare_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Category = category_labels[category]) %>%
  select(Category, total_rows, rows_neg_fare, rows_zero_fare, min_fare, max_fare) %>%
  arrange(desc(rows_neg_fare))

print(negative_fare_by_category, n = Inf)


# --- STEP 5: Total-vs-components reconciliation ------------------------------
# Does total_amount actually equal the sum of components within each pair?
# If not, the dataset has rounding, undocumented fees, or data-entry errors.

cat("\n========================================================\n")
cat(" DOES total_amount RECONCILE WITH ITS COMPONENTS?\n")
cat("========================================================\n")
cat("Check per pair: sum(total_amount) vs sum(all component fields)\n")
cat("A mismatch means total_amount on at least one row doesn't equal\n")
cat("fare + extra + mta + improvement + tolls + congestion + airport + cbd + tip.\n\n")

reconciliation_summary <- pair_class %>%
  group_by(category) %>%
  summarise(
    pairs               = n(),
    pairs_reconciling   = sum(total_reconciles),
    pairs_mismatched    = sum(!total_reconciles),
    median_discrepancy  = round(median(total_discrepancy), 4),
    max_abs_discrepancy = round(max(abs(total_discrepancy)), 4),
    .groups = "drop"
  ) %>%
  mutate(
    Category      = category_labels[category],
    pct_mismatched = sprintf("%.1f%%", 100 * pairs_mismatched / pairs)
  ) %>%
  select(Category, pairs, pairs_mismatched, pct_mismatched,
         median_discrepancy, max_abs_discrepancy) %>%
  arrange(desc(pairs_mismatched))

print(reconciliation_summary, n = Inf)


# --- STEP 6: Show concrete examples ------------------------------------------
# Print a couple of pairs from each category so you can SEE what's going on.

cat("\n========================================================\n")
cat(" EXAMPLE PAIRS FROM EACH CATEGORY (up to 2 each)\n")
cat("========================================================\n")

example_cols <- c("dup_group_id", "category", "fare_amount", "tip_amount",
                  "tolls_amount", "congestion_surcharge", "Airport_fee",
                  "cbd_congestion_fee", "total_amount")

examples <- strict_dups_classified %>%
  select(any_of(example_cols)) %>%
  group_by(category) %>%
  filter(dup_group_id %in% head(unique(dup_group_id), 2)) %>%
  arrange(category, dup_group_id) %>%
  ungroup()

print(examples, n = Inf)
