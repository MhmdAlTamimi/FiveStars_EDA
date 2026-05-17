# ============================================================
# 03_build_business_dashboard.R
# Final Business Dashboard - NYC Yellow Taxi
# Uses existing EDA figures and summaries
# Includes Five Stars Data Analysis logo in header (top-right)
#
# Run from project root (FiveStars_EDA/):
#   source("Visualization/03_build_business_dashboard.R")
#
# Prerequisite: run 01 and 02 first to generate figures and summaries.
# ============================================================

packages <- c("readr", "dplyr", "htmltools", "scales", "ggplot2", "base64enc")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(readr)
library(dplyr)
library(htmltools)
library(scales)
library(ggplot2)
library(base64enc)

# --- Paths ---

summary_dir <- "Output/eda_summaries"
figure_dir  <- "Output/figures"
report_dir  <- "Output"

if (!dir.exists(summary_dir)) stop("Output/eda_summaries/ not found. Run 03_full_eda_business_visuals.R first.")

# --- Generate logo and encode as base64 ---

make_star_points <- function(cx, cy, r_out = 0.22, r_in = 0.09, points = 7, start_angle = pi/2) {
  angles <- seq(start_angle, start_angle - 2 * pi, length.out = 2 * points + 1)
  radii <- rep(c(r_out, r_in), points + 1)[1:(2 * points + 1)]
  data.frame(
    x = cx + radii * cos(angles),
    y = cy + radii * sin(angles)
  )
}

n_stars <- 5
circle_radius <- 0.95
star_angles <- seq(pi/2, pi/2 - 2*pi, length.out = n_stars + 1)[1:n_stars]

stars_data <- data.frame()
for (i in 1:n_stars) {
  cx <- circle_radius * cos(star_angles[i])
  cy <- circle_radius * sin(star_angles[i])
  star_pts <- make_star_points(cx, cy, start_angle = pi/2)
  star_pts$group <- i
  stars_data <- rbind(stars_data, star_pts)
}

ring_data <- data.frame()
for(i in 1:n_stars) {
  start_arc <- star_angles[i] - (2*pi/5) * 0.32
  end_arc   <- star_angles[i] - (2*pi/5) * 0.68
  arc_angles <- seq(start_arc, end_arc, length.out = 30)
  segment <- data.frame(
    x = circle_radius * cos(arc_angles),
    y = circle_radius * sin(arc_angles),
    group = i
  )
  ring_data <- rbind(ring_data, segment)
}

logo_plot <- ggplot() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#03234d", color = NA),
    plot.background  = element_rect(fill = "#03234d", color = NA),
    plot.margin      = margin(20, 20, 20, 20)
  ) +
  coord_fixed(xlim = c(-2.2, 2.2), ylim = c(-1.9, 1.4)) +
  geom_path(data = ring_data, aes(x = x, y = y, group = group),
            color = "white", linewidth = 2.5, lineend = "round") +
  geom_polygon(data = stars_data, aes(x = x, y = y, group = group),
               fill = "white", color = "white", linewidth = 0.5) +
  annotate("text", x = 0, y = -1.3, label = "Five Stars Data Analysis",
           color = "white", size = 7, fontface = "bold", family = "sans")

logo_tmp <- tempfile(fileext = ".png")
ggsave(logo_tmp, plot = logo_plot, width = 5, height = 4.2, dpi = 200, bg = "#03234d")

logo_b64 <- base64encode(logo_tmp)
logo_src  <- paste0("data:image/png;base64,", logo_b64)

# --- Load KPI summary ---

kpi <- read_csv(
  file.path(summary_dir, "00_kpi_summary.csv"),
  show_col_types = FALSE
)

# --- Helper functions ---

img_tag <- function(file_name, width = "100%") {
  img_path <- file.path("figures", file_name)
  tags$img(
    src = img_path,
    style = paste0(
      "width:", width, ";",
      "border-radius:14px;",
      "box-shadow:0 4px 14px rgba(0,0,0,0.15);",
      "background:white;",
      "padding:8px;"
    )
  )
}

chart_card <- function(title, file_name, note = "") {
  tags$div(
    class = "chart-card",
    tags$h3(title),
    img_tag(file_name),
    if (note != "") tags$p(class = "note", note)
  )
}

kpi_card <- function(title, value, subtitle = "", color = "#1F4E79") {
  tags$div(
    class = "kpi-card",
    style = paste0("border-top:6px solid ", color, ";"),
    tags$div(class = "kpi-title", title),
    tags$div(class = "kpi-value", value),
    tags$div(class = "kpi-subtitle", subtitle)
  )
}

section_title <- function(title, subtitle = "") {
  tags$div(
    class = "section-title",
    tags$h2(title),
    if (subtitle != "") tags$p(subtitle)
  )
}

file_exists_fig <- function(file_name) {
  file.exists(file.path(figure_dir, file_name))
}

# --- Optional map check ---

map_file <- "G01_nyc_yellow_taxi_revenue_by_zone.png"
has_map  <- file_exists_fig(map_file)

# --- Dashboard ---

dashboard <- tags$html(
  tags$head(
    tags$title("NYC Yellow Taxi Business Dashboard"),
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background: #F4F7FA;
        margin: 0;
        padding: 0;
        color: #222;
      }

      .header {
        background: linear-gradient(135deg, #1F4E79, #163B5C);
        color: white;
        padding: 34px 48px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }

      .header-text h1 {
        font-size: 42px;
        margin: 0;
        font-weight: 800;
      }

      .header-text p {
        font-size: 18px;
        margin-top: 8px;
        color: #DDEAF6;
      }

      .header-logo {
        flex-shrink: 0;
        margin-left: 32px;
      }

      .header-logo img {
        height: 160px;
        width: auto;
        border-radius: 14px;
        display: block;
      }

      .container {
        padding: 32px 48px;
      }

      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 18px;
        margin-bottom: 28px;
      }

      .kpi-card {
        background: white;
        border-radius: 16px;
        padding: 20px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.10);
      }

      .kpi-title {
        font-size: 14px;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 0.8px;
      }

      .kpi-value {
        font-size: 32px;
        font-weight: bold;
        color: #1F4E79;
        margin-top: 8px;
      }

      .kpi-subtitle {
        font-size: 13px;
        color: #777;
        margin-top: 6px;
      }

      .section-title {
        margin-top: 38px;
        margin-bottom: 18px;
        border-left: 8px solid #F2B705;
        padding-left: 16px;
      }

      .section-title h2 {
        color: #1F4E79;
        margin: 0;
        font-size: 28px;
      }

      .section-title p {
        margin: 6px 0 0 0;
        color: #555;
        font-size: 15px;
      }

      .grid-2 {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 22px;
      }

      .grid-1 {
        display: grid;
        grid-template-columns: 1fr;
        gap: 22px;
      }

      .chart-card {
        background: white;
        border-radius: 18px;
        padding: 18px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.10);
      }

      .chart-card h3 {
        margin-top: 0;
        color: #1F4E79;
        font-size: 20px;
      }

      .note {
        color: #555;
        font-size: 14px;
        line-height: 1.6;
      }

      .insight-box {
        background: white;
        border-radius: 18px;
        padding: 24px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.10);
        font-size: 16px;
        line-height: 1.8;
      }

      .footer {
        margin-top: 40px;
        padding: 20px;
        text-align: center;
        color: #777;
        font-size: 13px;
      }

      @media (max-width: 1000px) {
        .header {
          flex-direction: column;
          align-items: flex-start;
          gap: 20px;
        }
        .header-logo {
          margin-left: 0;
        }
        .kpi-grid, .grid-2 {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),

  tags$body(
    tags$div(
      class = "header",

      tags$div(
        class = "header-text",
        tags$h1("NYC Yellow Taxi Business Dashboard"),
        tags$p("January–March 2025 | Demand, Revenue, Payment Behavior, Geography, and CBD Policy Analysis")
      ),

      tags$div(
        class = "header-logo",
        tags$img(src = logo_src, alt = "Five Stars Data Analysis Logo")
      )
    ),

    tags$div(
      class = "container",

      # KPI Cards
      tags$div(
        class = "kpi-grid",
        kpi_card("Total Trips",          comma(kpi$total_trips),               "Cleaned analysis-ready trips",          "#1F4E79"),
        kpi_card("Total Revenue",        dollar(kpi$total_revenue),            "Total passenger charges",               "#F2B705"),
        kpi_card("Average Total Amount", dollar(kpi$avg_total_amount),         "Average amount per trip",               "#2E8B57"),
        kpi_card("Credit Card Share",    percent(kpi$credit_card_share),       "Dominant recorded payment method",      "#4EA3D9"),
        kpi_card("Average Distance",     round(kpi$avg_trip_distance, 2),      "Miles per trip",                        "#1F4E79"),
        kpi_card("Average Duration",     round(kpi$avg_duration_min, 2),       "Minutes per trip",                      "#4EA3D9"),
        kpi_card("Average Tip",          dollar(kpi$avg_tip_amount),           "Recorded tips only",                    "#F2B705"),
        kpi_card("CBD Positive Share",   percent(kpi$cbd_positive_share),      "Trips with CBD fee > 0",                "#2E8B57")
      ),

      section_title(
        "Executive Overview",
        "A high-level business view of taxi activity, demand concentration, revenue behavior, and policy-sensitive patterns."
      ),
      tags$div(
        class = "insight-box",
        HTML("
          <b>Dashboard purpose:</b> This dashboard translates the cleaned taxi trip dataset into business-friendly visual insights.
          It focuses on when demand occurs, where revenue is concentrated, how payment behavior affects recorded tips,
          and how CBD congestion fee patterns changed after January 5, 2025.
        ")
      ),

      # 1. Temporal
      section_title("1. Temporal Demand", "Understanding when trips occur and how demand changes over time."),
      tags$div(class = "grid-2",
        chart_card("Daily Trip Volume",       "U01_daily_trip_volume.png",   "Daily demand shows repeated temporal fluctuations across the study period."),
        chart_card("Trip Volume by Month",    "U02_trips_by_month.png",      "Monthly volume increased from January to March, with March recording the highest demand.")
      ),
      tags$div(class = "grid-2",
        chart_card("Hour of Day vs Trip Volume", "B01_hour_vs_trip_volume.png", "Demand is lowest in early morning hours and strongest during evening/night periods."),
        chart_card("Weekday vs Demand",          "B02_weekday_vs_demand.png",   "Weekend activity, especially Saturday, shows stronger demand than early weekdays.")
      ),

      # 2. Geography
      section_title("2. Geographic Revenue and Demand", "Identifying high-value and high-demand taxi zones."),
      if (has_map) {
        tags$div(class = "grid-1",
          chart_card("NYC Yellow Taxi Revenue by Zone", map_file, "The map highlights spatial concentration of revenue across NYC taxi zones.")
        )
      },
      tags$div(class = "grid-2",
        chart_card("Top Pickup Zones by Trip Count", "B06_geography_vs_trip_count.png", "A small number of pickup zones dominate overall taxi demand."),
        chart_card("Top Pickup Zones by Revenue",    "B07_geography_vs_revenue.png",    "Airport and central zones generate disproportionately high revenue.")
      ),

      # 3. Payment
      section_title("3. Payment and Passenger Behavior", "Understanding how passengers pay and how payments relate to recorded tips."),
      tags$div(class = "grid-2",
        chart_card("Payment Type Frequency",    "U08_payment_type_frequency.png",    "Credit card is the dominant payment method in the dataset."),
        chart_card("Passenger Count Frequency", "U07_passenger_count_frequency.png", "Single-passenger trips dominate taxi activity.")
      ),
      tags$div(class = "grid-2",
        chart_card("Payment Type vs Average Tip",          "B03_payment_vs_avg_tip.png",   "Credit card trips show much higher recorded tips because cash tips are not captured."),
        chart_card("Payment Type vs Average Total Amount", "B04_payment_vs_avg_total.png", "Credit card trips generally show higher average total amounts.")
      ),

      # 4. Trip Characteristics
      section_title("4. Trip Characteristics", "Exploring distributions of distance, duration, fare, and total amount."),
      tags$div(class = "grid-2",
        chart_card("Trip Distance Distribution", "U03_trip_distance_distribution.png", "Most trips are short-distance urban rides with a right-skewed distribution."),
        chart_card("Trip Duration Distribution", "U04_trip_duration_distribution.png", "Most trips are short to medium duration, with a long right tail.")
      ),
      tags$div(class = "grid-2",
        chart_card("Fare Amount Distribution",  "U05_fare_amount_distribution.png",  "Fare amounts are concentrated in low-to-medium ranges."),
        chart_card("Total Amount Distribution", "U06_total_amount_distribution.png", "Total amounts include fare, taxes, tolls, surcharges, and recorded tips.")
      ),
      tags$div(class = "grid-2",
        chart_card("Distance vs Total Amount",          "B05_distance_vs_total_amount.png",       "Total amount generally increases with trip distance."),
        chart_card("Distance × Duration × Total Amount", "M05_distance_duration_total_heatmap.png", "Longer and slower trips generate higher average total amounts.")
      ),

      # 5. Multivariate
      section_title("5. Multivariate Demand Patterns", "Combining time, geography, demand, payment, and revenue."),
      tags$div(class = "grid-2",
        chart_card("Month × Hour × Demand",        "M01_month_hour_demand_heatmap.png",    "Demand peaks remain visible across months, especially during evening hours."),
        chart_card("Weekday × Hour × Demand",      "M02_weekday_hour_demand_heatmap.png",  "Weekend nighttime demand is especially strong.")
      ),
      tags$div(class = "grid-2",
        chart_card("Time × Geography × Demand",    "M03_time_geography_demand_heatmap.png","Certain zones remain active across multiple time periods."),
        chart_card("Geography × Payment Type × Revenue", "M04_geography_payment_revenue.png",    "Credit card revenue dominates most high-revenue zones.")
      ),

      # 6. CBD
      section_title("6. CBD Policy Analysis", "Pre/post January 5, 2025 congestion pricing behavior."),
      tags$div(class = "grid-2",
        chart_card("CBD Positive Share: Pre vs Post Jan 5", "M06_cbd_positive_share_pre_post.png", "CBD charges became active after the policy start date."),
        chart_card("Average CBD Fee: Pre vs Post Jan 5",    "M07_avg_cbd_fee_pre_post.png",        "Average CBD fee increased after the policy implementation period.")
      ),

      # 7. Caveats
      section_title("7. Interpretation Caveats", "Important analytical limitations."),
      tags$div(
        class = "insight-box",
        HTML("
          <ul>
            <li><b>Cash tips:</b> Cash tips are not recorded, so tip analysis is more reliable for card-based transactions.</li>
            <li><b>Unknown zones:</b> Zones 264 and 265 were excluded from geography-specific charts.</li>
            <li><b>Outliers:</b> Some distributions were displayed up to P99 for readability, not because all records were removed.</li>
            <li><b>CBD policy:</b> January 5, 2025 was used as the policy split date for CBD congestion fee analysis.</li>
          </ul>
        ")
      ),

      tags$div(
        class = "footer",
        "Generated as part of the NYC Yellow Taxi Data Visualization Project by Five Stars Data Analysis"
      )
    )
  )
)

# --- Save dashboard ---

output_file <- file.path(report_dir, "NYC_Yellow_Taxi_Final_Business_Dashboard.html")

save_html(dashboard, file = output_file)

cat("\nDashboard created successfully:\n")
cat(output_file, "\n")
