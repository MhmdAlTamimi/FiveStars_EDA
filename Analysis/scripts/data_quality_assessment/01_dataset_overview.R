# =============================================================================
# Section 1: Dataset Overview and Structure
# =============================================================================
# Run from project root: source("scripts/data_quality_assessment/01_dataset_overview.R")
# =============================================================================

library(arrow)
library(dplyr)

df <- read_parquet("Data/original.parquet")


# --- 1.1 Dimensions -----------------------------------------------------------

cat("Total records  :", nrow(df), "\n")
cat("Total variables:", ncol(df), "\n")


# --- 1.2 Column Names and Types -----------------------------------------------

cat("\nVariable inventory:\n")
type_summary <- data.frame(
  variable = names(df),
  r_type   = sapply(df, function(x) class(x)[1])
)
print(type_summary, row.names = FALSE)


# --- 1.3 Monthly Volume -------------------------------------------------------
# source_month is a file-level label (may not match actual pickup date)

cat("\nTrips by source_month:\n")
print(table(df$source_month))


# --- 1.4 Vendor Distribution --------------------------------------------------
# VendorID codes: 1=CMT, 2=Curb Mobility, 6=Myle Technologies, 7=Helix

cat("\nVendorID distribution:\n")
print(table(df$VendorID, useNA = "ifany"))


# --- 1.4 RateCodeID Distribution --------------------------------------------------
# RateCodeID codes: 1=CMT, 2=Curb Mobility, 6=Myle Technologies, 7=Helix
cat("\RateCodeID distribution:\n")
print(table(df$RatecodeID, useNA = "ifany"))


# --- 1.4 RateCodeID Distribution --------------------------------------------------
# RateCodeID codes: 1=CMT, 2=Curb Mobility, 6=Myle Technologies, 7=Helix


# --- 1.5 Date Range -----------------------------------------------------------

cat("\nPickup datetime range:\n")
cat("  Min:", format(min(df$tpep_pickup_datetime, na.rm = TRUE)), "\n")
cat("  Max:", format(max(df$tpep_pickup_datetime, na.rm = TRUE)), "\n")

cat("\nDropoff datetime range:\n")
cat("  Min:", format(min(df$tpep_dropoff_datetime, na.rm = TRUE)), "\n")
cat("  Max:", format(max(df$tpep_dropoff_datetime, na.rm = TRUE)), "\n")
