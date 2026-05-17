# =============================================================================
# Section 2: Missing Values (NA Assessment) Refer to section 2 in Report
# =============================================================================
# Covers:
#   - NA count and % for every variable
#   - Which vendor drives passenger_count NAs (share of all NAs)
#   - NA rate per vendor's own trips — Report Table 2.3
#   - Monthly NA trend with rates for both key fields — Report Table 2.4
#   - Systemic block overlap (are the four NA fields from the same rows?)
#   - passenger_count NAs beyond the systemic block (isolated 3,359)
# =============================================================================

library(arrow)
library(dplyr)

df <- read_parquet("Data/original.parquet")
total_rows <- nrow(df)


# --- 2.1 NA Summary for All Variables -----------------------------------------

cat("NA count and percentage for every variable:\n")
na_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(na_pct = round(na_count / total_rows * 100, 2)) %>%
  arrange(desc(na_count))
print(na_summary, n = Inf)


# --- 2.2 Vendor Share of passenger_count NAs ----------------------------------
# pct_of_pc_nas = what % of ALL passenger_count NAs come from this vendor.
# This is different from the per-vendor NA rate in 2.3 below.
# Report finding: VendorID 2 contributes ~85.3% of all passenger_count NAs.

cat("\npassenger_count NAs by VendorID (share of all missing values):\n")
df %>%
  filter(is.na(passenger_count)) %>%
  count(VendorID) %>%
  mutate(pct_of_pc_nas = round(n / sum(n) * 100, 1)) %>%
  print()


library(dplyr)

na_vars <- c("passenger_count", "RatecodeID", "store_and_fwd_flag",
             "congestion_surcharge", "Airport_fee")

df %>%
  group_by(VendorID) %>%
  summarise(
    total_trips    = n(),
    pct_of_dataset = round(n() / nrow(df) * 100, 2),
    across(all_of(na_vars), ~ round(mean(is.na(.x)) * 100, 2),
           .names = "na_pct_{.col}"),
    .groups = "drop"
  ) %>%
  print(n = Inf, width = Inf)

# --- 2.3 NA Rate per Vendor (Report Table 2.3) --------------------------------
# na_rate_passenger / na_rate_ratecode = % of THAT VENDOR'S OWN TRIPS that are NA.
# This is different from 2.2 above which shows share of total NAs.
# Key finding: VendorID 6 (Myle Tech) has 100% NA on both fields across all 1,153 trips.
# na_rate_passenger and na_rate_ratecode are nearly identical for each vendor
# because the same systemic pipeline block drives both fields simultaneously.

cat("\nNA rate per vendor (% of each vendor's own trips that are missing):\n")
df %>%
  group_by(VendorID) %>%
  summarise(
    total_trips       = n(),
    pct_of_dataset    = round(n() / total_rows * 100, 2),
    na_rate_passenger = round(mean(is.na(passenger_count)) * 100, 2),
    na_rate_ratecode  = round(mean(is.na(RatecodeID))      * 100, 2),
    .groups = "drop"
  ) %>%
  print()


# --- 2.4 Monthly NA Trend (Report Table 2.4) ----------------------------------
# Missingness escalates month-over-month — Jan: 15.6%, Feb: 22.6%, Mar: 22.1%
# for passenger_count. Consistent with an unresolved pipeline configuration gap
# rather than a one-time event.

cat("\nNA trend by source_month:\n")
df %>%
  group_by(source_month) %>%
  summarise(
    total_trips       = n(),
    na_passenger      = sum(is.na(passenger_count)),
    na_rate_passenger = round(mean(is.na(passenger_count)) * 100, 2),
    na_ratecode       = sum(is.na(RatecodeID)),
    .groups = "drop"
  ) %>%
  print()


# --- 2.5 Systemic Block Overlap -----------------------------------------------
# The key question: do the NAs in RatecodeID, store_and_fwd_flag, and
# congestion_surcharge all come from the SAME rows?

df_flags <- df %>%
  mutate(
    na_ratecode = is.na(RatecodeID),
    na_flag     = is.na(store_and_fwd_flag),
    na_cong     = is.na(congestion_surcharge),
    na_airport  = is.na(Airport_fee),
    na_passg    = is.na(passenger_count)
  )

# Three-field overlap (core systemic block)
all_three_overlap <- df_flags %>%
  filter(na_ratecode & na_flag & na_cong) %>%
  nrow()

# Four-field overlap (does Airport_fee also follow the same pattern?)
all_four_overlap <- df_flags %>%
  filter(na_ratecode & na_flag & na_cong & na_airport) %>%
  nrow()

all_five_overlap <- df_flags %>%
  filter(na_ratecode & na_flag & na_cong & na_airport & na_passg) %>%
  nrow()


cat("Rows all five overlap: ", all_five_overlap)
# Rows where Airport_fee is present but the other three are NA
airport_with_others_missing <- df_flags %>%
  filter(na_ratecode & na_flag & na_cong & !na_airport) %>%
  nrow()

cat("\nRows where RatecodeID + store_and_fwd_flag + congestion_surcharge are all NA:",
    all_three_overlap, "\n")


# passenger_count NAs beyond the systemic block
# 2,267,572 total - 2,264,213 block = 3,359 isolated rows missing
# passenger_count for a different, unrelated reason — not part of the
# systemic vendor pipeline failure.
pc_na_beyond_block <- sum(is.na(df$passenger_count)) - all_three_overlap
cat("passenger_count NAs beyond the systemic block:",
    pc_na_beyond_block, "\n")

cat("Of those, rows where Airport_fee is also NA:", all_four_overlap, "\n")
cat("Rows with Airport_fee present but the other 3 NA:", airport_with_others_missing, "\n")


# --- 2.6 Vendor Breakdown of the Systemic Block --------------------------------

cat("\nVendorID breakdown of the systemic NA block:\n")
df_flags %>%
  filter(na_ratecode & na_flag & na_cong) %>%
  count(VendorID) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

