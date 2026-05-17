# ============================================================
# 02_revenue_zone_map.R
# NYC Yellow Taxi Revenue by Zone Map
#
# Run from project root (FiveStars_EDA/):
#   source("Visualization/02_revenue_zone_map.R")
# ============================================================

# --- Packages (auto-install) ---

packages <- c("arrow", "dplyr", "ggplot2", "sf", "scales", "readr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(arrow)
library(dplyr)
library(ggplot2)
library(sf)
library(scales)
library(readr)

# --- Paths ---

input_file  <- "Data/cleaned_data_final.parquet"
figure_dir  <- "Output/figures"
summary_dir <- "Output/eda_summaries"
geo_dir     <- "Output/geo"

if (!file.exists(input_file)) stop("Data/cleaned_data_final.parquet not found. Set working directory to FiveStars_EDA/.")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(geo_dir, recursive = TRUE, showWarnings = FALSE)

# --- Download / Read TLC Taxi Zones ---

zip_file <- file.path(geo_dir, "taxi_zones.zip")
unzip_dir <- file.path(geo_dir, "taxi_zones")

if (!file.exists(zip_file)) {
  download.file(
    url = "https://d37ci6vzurychx.cloudfront.net/misc/taxi_zones.zip",
    destfile = zip_file,
    mode = "wb"
  )
}

if (!dir.exists(unzip_dir)) {
  unzip(zip_file, exdir = unzip_dir)
}

shp_file <- list.files(
  unzip_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(shp_file) == 0) {
  stop("No shapefile found after unzip.")
}

taxi_zones <- st_read(shp_file[1], quiet = TRUE)
taxi_zones <- st_transform(taxi_zones, 4326)

# --- Load cleaned taxi data ---

df <- read_parquet(input_file)

# --- Revenue by Pickup Zone ---

zone_revenue <- df %>%
  filter(
    !is.na(PULocationID),
    !(PULocationID %in% c(264, 265)),
    !is.na(total_amount),
    total_amount >= 0
  ) %>%
  group_by(PULocationID) %>%
  summarise(
    trips = n(),
    total_revenue = sum(total_amount, na.rm = TRUE),
    avg_revenue = mean(total_amount, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  zone_revenue,
  file.path(summary_dir, "G01_revenue_by_zone_summary.csv")
)

# --- Join Revenue with Zone Geometry ---

map_data <- taxi_zones %>%
  left_join(
    zone_revenue,
    by = c("LocationID" = "PULocationID")
  )

# --- Create Revenue Groups ---

map_data <- map_data %>%
  mutate(
    revenue_group = case_when(
      is.na(total_revenue) ~ "No data",
      total_revenue < 10000 ~ "0–10k",
      total_revenue < 50000 ~ "10k–50k",
      total_revenue < 100000 ~ "50k–100k",
      total_revenue < 500000 ~ "100k–500k",
      total_revenue >= 500000 ~ "500k+"
    ),
    revenue_group = factor(
      revenue_group,
      levels = c("0–10k", "10k–50k", "50k–100k", "100k–500k", "500k+", "No data")
    )
  )

# --- Plot Revenue Map ---

p_map <- ggplot(map_data) +
  geom_sf(
    aes(fill = revenue_group),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "0–10k" = "#1B1464",
      "10k–50k" = "#8E24AA",
      "50k–100k" = "#D45087",
      "100k–500k" = "#F9843B",
      "500k+" = "#F4F000",
      "No data" = "#E6E6E6"
    ),
    drop = FALSE
  ) +
  labs(
    title = "NYC Yellow Taxi Revenue by Zone",
    subtitle = "Total revenue aggregated by pickup taxi zone",
    fill = "Revenue",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#1F4E79"
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "#555555"
    ),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    axis.text = element_text(color = "#555555")
  )

ggsave(
  filename = file.path(figure_dir, "G01_nyc_yellow_taxi_revenue_by_zone.png"),
  plot = p_map,
  width = 10,
  height = 8,
  dpi = 300
)

cat("\nMap created successfully:\n")
cat(file.path(figure_dir, "G01_nyc_yellow_taxi_revenue_by_zone.png"))
cat("\n")
