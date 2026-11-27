# ==============================================================================
# GPS and Line Length Visualization Examples
# ==============================================================================
# Visualize the new GPS position and fishing line sensors
# ==============================================================================

library(tidyverse)
library(ggplot2)

source("05_mock_data_generator.R")

# Generate sample data
cat("Generating mock data with GPS and line length sensors...\n")
mock_data <- generate_mock_poseidat_data(
  n_trips = 10,
  vessel_ids = c("NLD123456", "NLD789012"),
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# ==============================================================================
# EXAMPLE 1: GPS Track for a Single Trip
# ==============================================================================

cat("\nExample 1: Plotting GPS track for one trip...\n")

# Get one trip
single_trip <- mock_data$trips[1, ]

# Get GPS data for that trip
gps_data <- mock_data$sensors |>
  filter(trip_id == single_trip$trip_id,
         sensor_type %in% c("gps_latitude", "gps_longitude")) |>
  select(trip_id, timestamp, sensor_type, value) |>
  pivot_wider(names_from = sensor_type, 
              values_from = value,
              values_fn = first) |>  # Take first value if duplicates
  filter(!is.na(gps_latitude), !is.na(gps_longitude))  # Remove any NAs

# Ensure columns are numeric (not lists)
gps_data <- gps_data |>
  mutate(
    gps_latitude = as.numeric(gps_latitude),
    gps_longitude = as.numeric(gps_longitude)
  )

# Plot GPS track
gps_plot <- ggplot(gps_data, aes(x = gps_longitude, y = gps_latitude)) +
  geom_path(color = "steelblue", size = 0.8) +
  geom_point(data = gps_data[1, ], aes(x = gps_longitude, y = gps_latitude),
             color = "green", size = 4, shape = 17) +  # Start point
  geom_point(data = gps_data[nrow(gps_data), ], aes(x = gps_longitude, y = gps_latitude),
             color = "red", size = 4, shape = 15) +    # End point
  labs(
    title = glue::glue("GPS Track - {single_trip$vessel_id} - {single_trip$trip_id}"),
    subtitle = glue::glue("{single_trip$departure_date} to {single_trip$arrival_date}"),
    x = "Longitude (degrees)",
    y = "Latitude (degrees)",
    caption = "Green triangle = start, Red square = end"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1.3)  # Adjust aspect ratio for map-like view

print(gps_plot)

# ==============================================================================
# EXAMPLE 2: Line Length Over Time
# ==============================================================================

cat("\nExample 2: Plotting fishing line length over time...\n")

# Get line length data for the same trip
line_data <- mock_data$sensors |>
  filter(trip_id == single_trip$trip_id,
         sensor_type == "line_length")

# Plot line length
line_plot <- ggplot(line_data, aes(x = timestamp, y = value)) +
  geom_line(color = "darkgreen", size = 0.6) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = min(line_data$timestamp), y = 55, 
           label = "Fishing threshold (50m)", hjust = 0, color = "red") +
  labs(
    title = glue::glue("Fishing Line Length - {single_trip$vessel_id}"),
    subtitle = glue::glue("{single_trip$departure_date} to {single_trip$arrival_date}"),
    x = "Time",
    y = "Line Length (meters)"
  ) +
  theme_minimal()

print(line_plot)

# ==============================================================================
# EXAMPLE 3: GPS Track Colored by Line Length (Fishing Activity)
# ==============================================================================

cat("\nExample 3: GPS track colored by fishing activity...\n")

# Combine GPS and line length
combined_data <- gps_data |>
  left_join(
    mock_data$sensors |>
      filter(trip_id == single_trip$trip_id, sensor_type == "line_length") |>
      select(timestamp, line_length = value),
    by = "timestamp"
  ) |>
  filter(!is.na(line_length)) |>
  mutate(
    fishing = line_length > 50,
    gps_latitude = as.numeric(gps_latitude),
    gps_longitude = as.numeric(gps_longitude),
    line_length = as.numeric(line_length)
  )

# Plot GPS track colored by fishing activity
fishing_track_plot <- ggplot(combined_data, 
                              aes(x = gps_longitude, y = gps_latitude, color = line_length)) +
  geom_path(size = 1.5) +
  scale_color_gradient2(
    low = "blue", 
    mid = "yellow", 
    high = "red",
    midpoint = 150,
    name = "Line Length (m)"
  ) +
  geom_point(data = combined_data[1, ], 
             aes(x = gps_longitude, y = gps_latitude),
             color = "black", size = 4, shape = 17) +
  labs(
    title = glue::glue("GPS Track Colored by Line Length - {single_trip$vessel_id}"),
    subtitle = "Blue = line retracted, Red = line deployed",
    x = "Longitude (degrees)",
    y = "Latitude (degrees)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1.3)

print(fishing_track_plot)

# ==============================================================================
# EXAMPLE 4: Multiple Trips on One Map
# ==============================================================================

cat("\nExample 4: Multiple trips on one map...\n")

# Get first 3 trips
trips_to_plot <- mock_data$trips[1:3, ]

# Get GPS data for these trips
multi_gps <- mock_data$sensors |>
  filter(trip_id %in% trips_to_plot$trip_id,
         sensor_type %in% c("gps_latitude", "gps_longitude")) |>
  select(trip_id, timestamp, sensor_type, value) |>
  pivot_wider(names_from = sensor_type, 
              values_from = value,
              values_fn = first) |>
  filter(!is.na(gps_latitude), !is.na(gps_longitude)) |>
  mutate(
    gps_latitude = as.numeric(gps_latitude),
    gps_longitude = as.numeric(gps_longitude)
  )

# Plot multiple tracks
multi_plot <- ggplot(multi_gps, aes(x = gps_longitude, y = gps_latitude, color = trip_id)) +
  geom_path(size = 1) +
  labs(
    title = "Multiple Fishing Trips",
    x = "Longitude (degrees)",
    y = "Latitude (degrees)",
    color = "Trip ID"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1.3)

print(multi_plot)

# ==============================================================================
# EXAMPLE 5: Summary Statistics
# ==============================================================================

cat("\nExample 5: Summary statistics for GPS and line data...\n\n")

# GPS coverage statistics
gps_stats <- mock_data$sensors |>
  filter(sensor_type %in% c("gps_latitude", "gps_longitude")) |>
  group_by(trip_id, sensor_type) |>
  summarise(
    n_readings = n(),
    first_reading = min(timestamp),
    last_reading = max(timestamp),
    duration_hours = as.numeric(difftime(max(timestamp), min(timestamp), units = "hours")),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = sensor_type, values_from = c(n_readings, duration_hours))

cat("GPS Statistics per trip:\n")
print(gps_stats)

# Line length statistics
line_stats <- mock_data$sensors |>
  filter(sensor_type == "line_length") |>
  group_by(trip_id) |>
  summarise(
    n_readings = n(),
    mean_length = mean(value),
    max_length = max(value),
    time_fishing = sum(value > 50),  # Minutes with line deployed
    pct_fishing = 100 * sum(value > 50) / n(),
    .groups = "drop"
  )

cat("\n\nLine Length Statistics per trip:\n")
print(line_stats)

# ==============================================================================
# EXAMPLE 6: Fishing Intensity Map
# ==============================================================================

cat("\nExample 6: Fishing intensity heatmap...\n")

# Calculate fishing intensity by location
fishing_intensity <- mock_data$sensors |>
  filter(sensor_type %in% c("gps_latitude", "gps_longitude", "line_length")) |>
  select(trip_id, timestamp, sensor_type, value) |>
  pivot_wider(names_from = sensor_type, 
              values_from = value,
              values_fn = first) |>
  filter(!is.na(gps_latitude), !is.na(gps_longitude), !is.na(line_length)) |>
  mutate(
    gps_latitude = as.numeric(gps_latitude),
    gps_longitude = as.numeric(gps_longitude),
    line_length = as.numeric(line_length),
    fishing = line_length > 50,
    lat_bin = round(gps_latitude, 1),
    lon_bin = round(gps_longitude, 1)
  ) |>
  filter(fishing) |>
  group_by(lat_bin, lon_bin) |>
  summarise(
    fishing_minutes = n(),
    .groups = "drop"
  )

# Plot heatmap
heatmap_plot <- ggplot(fishing_intensity, aes(x = lon_bin, y = lat_bin, fill = fishing_minutes)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", name = "Fishing\nMinutes") +
  labs(
    title = "Fishing Intensity Heatmap",
    subtitle = "Where vessels spent time fishing",
    x = "Longitude (degrees)",
    y = "Latitude (degrees)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1.3)

print(heatmap_plot)

# ==============================================================================
# EXAMPLE 7: Time Series of All Sensors for One Trip
# ==============================================================================

cat("\nExample 7: All sensor types over time...\n")

# Get all sensors for one trip
all_sensors <- mock_data$sensors |>
  filter(trip_id == single_trip$trip_id)

# Plot all sensor types
sensor_plot <- ggplot(all_sensors, aes(x = timestamp, y = value, color = sensor_type)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~sensor_type, scales = "free_y", ncol = 2) +
  labs(
    title = glue::glue("All Sensors - {single_trip$trip_id}"),
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(sensor_plot)

# ==============================================================================
# SAVE PLOTS
# ==============================================================================

cat("\n\nSaving plots...\n")

# Save plots if output folder exists
if (exists("config") && !is.null(config$outputs_path)) {
  ggsave(file.path(config$outputs_path, "figures", "gps_track.png"), 
         gps_plot, width = 10, height = 8, dpi = 300)
  ggsave(file.path(config$outputs_path, "figures", "line_length.png"), 
         line_plot, width = 10, height = 6, dpi = 300)
  ggsave(file.path(config$outputs_path, "figures", "fishing_track.png"), 
         fishing_track_plot, width = 10, height = 8, dpi = 300)
  ggsave(file.path(config$outputs_path, "figures", "fishing_intensity.png"), 
         heatmap_plot, width = 10, height = 8, dpi = 300)
  cat("✓ Plots saved to outputs/figures/\n")
}

cat("\n✓ GPS and line length visualization examples complete!\n\n")
