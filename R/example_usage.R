# =====================================================
# Poseidat Reader - Working Example
#
# This script demonstrates how to use the Poseidat reader
# =====================================================

# Clean workspace
rm(list = ls())

# Load required libraries
library(tidyverse)
library(jsonlite)

# Set your data directory
mydir <- "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data"  # CHANGE THIS to your data folder

# =====================================================
# STEP 1: Load the Poseidat Reader (ONE LINE!)
# =====================================================

source("R/load_poseidat_reader.R")

# =====================================================
# STEP 2: Extract Trip Data
# =====================================================

cat("\n=== EXTRACTING TRIP DATA ===\n")

trip_data <- read_and_extract_poseidat(
  file.path(mydir, "poseidat_complete_trip_example.json")
)

# What you get:
# - trip_data$trip_info ...... All entries with trip numbers
# - trip_data$trips_summary ... Summary of each trip
# - trip_data$gear ............ Gear used in fishing activities
# - trip_data$catches ......... All catches with discard_reason column
# - trip_data$timeline ........ Timeline of first trip events

cat("\nTrip Summary:\n")
print(trip_data$trips_summary)

cat("\nCatches (with discard reasons):\n")
print(trip_data$catches)

# =====================================================
# STEP 3: Extract Sensor Data
# =====================================================

cat("\n=== EXTRACTING SENSOR DATA ===\n")

sensor_data <- read_and_extract_poseidat(
  file.path(mydir, "poseidat_sensor_data_example.json")
)

# What you get:
# - sensor_data$gps ............. GPS positions (lat, lon, speed, course)
# - sensor_data$trawl_tension ... Line length and towing force
# - sensor_data$ctd ............. Temperature & salinity profiles
# - sensor_data$temperature ..... Simple temperature readings
# - sensor_data$weather ......... Weather observations
# - sensor_data$speedlog ........ Speed through water vs over ground
# - sensor_data$fuel ............ Fuel consumption

cat("\nGPS Data (first 3 rows):\n")
print(head(sensor_data$gps, 3))

cat("\nCTD Data (first 5 rows):\n")
print(head(sensor_data$ctd, 5))

# =====================================================
# STEP 4: Example Analyses
# =====================================================

# Example 1: Discard rates
cat("\n=== DISCARD ANALYSIS ===\n")
if (nrow(trip_data$catches) > 0) {
  discard_summary <- trip_data$catches %>%
    group_by(catch_type) %>%
    summarise(
      total_weight = sum(total_weight),
      n_species = n()
    )
  print(discard_summary)
}

# Example 2: GPS track summary
cat("\n=== GPS TRACK SUMMARY ===\n")
if (nrow(sensor_data$gps) > 0) {
  gps_summary <- sensor_data$gps %>%
    summarise(
      n_points = n(),
      start_lat = first(latitude),
      start_lon = first(longitude),
      end_lat = last(latitude),
      end_lon = last(longitude),
      avg_speed = mean(speed_over_ground, na.rm = TRUE)
    )
  print(gps_summary)
}

# Example 3: CTD temperature profile by depth
cat("\n=== CTD TEMPERATURE PROFILE ===\n")
if (nrow(sensor_data$ctd) > 0) {
  ctd_profile <- sensor_data$ctd %>%
    group_by(depth) %>%
    summarise(
      avg_temp = mean(temperature, na.rm = TRUE),
      avg_salinity = mean(salinity, na.rm = TRUE),
      n_samples = n()
    ) %>%
    arrange(depth)
  print(ctd_profile)
}

# =====================================================
# STEP 5: Save Results (Optional)
# =====================================================

# Uncomment to save results to CSV files:
# write_csv(trip_data$catches, "catches_with_discard_reasons.csv")
# write_csv(sensor_data$gps, "gps_track.csv")
# write_csv(sensor_data$ctd, "ctd_profile.csv")

cat("\n✓ All data extracted successfully!\n")

