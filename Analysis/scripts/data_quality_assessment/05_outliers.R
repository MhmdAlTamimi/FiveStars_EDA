# =============================================================================
# Section 5: Outlier Analysis - Refer to section 5 in the report
# =============================================================================
# Covers:
#   5.1  Multi-threshold comparison for trip_distance, duration_min, total_amount
#        Methods: IQR 1.5x, IQR 3x, P99, P99.9, P99.99
#   5.2  Domain threshold checks (absolute business rules)
# =============================================================================

library(arrow)
library(dplyr)

df <- read_parquet("Data/original.parquet")

df <- df %>%
  mutate(duration_min = as.numeric(
    difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")
  ))

total <- nrow(df)
fmt   <- function(n) format(round(n), big.mark = ",")


# =============================================================================
# 5.1  Multi-Threshold Comparison (Upper Tail Only; Valid Non-Negative Values)
# =============================================================================

cat("=== 5.1 Outlier Thresholds ===\n")
cat(sprintf("%-15s  %10s %12s  %10s %12s  %10s %12s  %10s %12s  %10s %12s\n",
    "Variable", "IQR1.5x", "count", "IQR3x", "count", "P99", "count",
    "P99.9", "count", "P99.99", "count"))

for (vname in c("trip_distance", "duration_min", "total_amount")) {
  x_raw <- df[[vname]]
  # duration_min: exclude negatives and zeros; others: exclude negatives only
  x <- x_raw[!is.na(x_raw) & (if (vname == "duration_min") x_raw > 0 else x_raw >= 0)]

  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- q3 - q1
  hi15   <- q3 + 1.5 * iqr
  hi3    <- q3 + 3.0 * iqr
  p99    <- quantile(x, 0.99)
  p999   <- quantile(x, 0.999)
  p9999  <- quantile(x, 0.9999)
  p9995 <-  quantile(x, 0.9995)

  cat(sprintf("\n--- %s ---\n", vname))
  cat(sprintf("  IQR 1.5x  upper cutoff: %8.2f   flagged: %s\n",
      hi15,  fmt(sum(x > hi15))))
  cat(sprintf("  IQR 3x    upper cutoff: %8.2f   flagged: %s\n",
      hi3,   fmt(sum(x > hi3))))
  cat(sprintf("  P99       cutoff      : %8.4f  flagged: %s\n",
      p99,   fmt(sum(x > p99))))
  cat(sprintf("  P99.9     cutoff      : %8.4f  flagged: %s\n",
      p999,  fmt(sum(x > p999))))
  cat(sprintf("  P99.99    cutoff      : %8.4f  flagged: %s\n",
      p9999, fmt(sum(x > p9999))))
    cat(sprintf("  P99.95    cutoff      : %8.4f  flagged: %s\n",
      p9995, fmt(sum(x > p9995))))
}



# =============================================================================
# 5.2  Domain Threshold Checks
# =============================================================================

cat("\n\n=== 5.2 Domain Threshold Checks ===\n")

cat("trip_distance:\n")
cat("  > 100 miles        :", fmt(sum(df$trip_distance > 100,  na.rm = TRUE)), "\n")
cat("  > 200 miles        :", fmt(sum(df$trip_distance > 200,  na.rm = TRUE)), "\n")
cat("  max value          :", max(df$trip_distance, na.rm = TRUE), "miles\n")

cat("\nfare_amount:\n")
cat("  > $200             :", fmt(sum(df$fare_amount > 200, na.rm = TRUE)), "\n")
cat("  > $500             :", fmt(sum(df$fare_amount > 500, na.rm = TRUE)), "\n")
cat("  max value          :", max(df$fare_amount, na.rm = TRUE), "\n")

cat("\ntotal_amount:\n")
cat("  > $200             :", fmt(sum(df$total_amount > 200, na.rm = TRUE)), "\n")
cat("  > $500             :", fmt(sum(df$total_amount > 500, na.rm = TRUE)), "\n")
cat("  max value          :", max(df$total_amount, na.rm = TRUE), "\n")

cat("\nduration_min:\n")
cat("  > 8 hours (480 min):", fmt(sum(df$duration_min > 480, na.rm = TRUE)), "\n")
cat("  max value          :", max(df$duration_min, na.rm = TRUE),
    "min =", round(max(df$duration_min, na.rm = TRUE) / 60 / 24, 1), "days\n")
