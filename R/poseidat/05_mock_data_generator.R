# ==============================================================================
# Mock Poseidat Data Generator for Testing
# ==============================================================================
# Use this to test your Poseidat functions before real data is available
# ==============================================================================

library(tidyverse)
library(lubridate)

# Generate mock trip data ----
generate_mock_trips <- function(n_trips = 50, 
                                vessel_ids = c("NLD123456", "NLD789012", "NLD345678"),
                                start_date = "2024-01-01",
                                end_date = "2024-12-31") {
  
  tibble(
    trip_id = paste0("TRIP_", str_pad(1:n_trips, 6, pad = "0")),
    vessel_id = sample(vessel_ids, n_trips, replace = TRUE),
    departure_date = sample(seq(as.Date(start_date), as.Date(end_date), by = "day"), n_trips, replace = TRUE),
    departure_port = sample(c("IJmuiden", "Scheveningen", "Stellendam", "Urk"), n_trips, replace = TRUE),
    arrival_port = sample(c("IJmuiden", "Scheveningen", "Stellendam", "Urk"), n_trips, replace = TRUE),
    fishing_days = sample(1:14, n_trips, replace = TRUE)
  ) |>
    mutate(
      arrival_date = departure_date + days(fishing_days),
      trip_duration_hours = fishing_days * 24,
      status = sample(c("completed", "in_progress", "cancelled"), n_trips, replace = TRUE, prob = c(0.85, 0.1, 0.05))
    ) |>
    arrange(departure_date)
}

# Generate mock catch data ----
generate_mock_catches <- function(trips_data, 
                                  catches_per_trip = 3:10,
                                  species = c("HER", "COD", "HAD", "MAC", "PLE", "SOL", "WHG")) {
  
  map_dfr(1:nrow(trips_data), function(i) {
    trip <- trips_data[i, ]
    n_catches <- sample(catches_per_trip, 1)
    
    tibble(
      catch_id = paste0("CATCH_", trip$trip_id, "_", str_pad(1:n_catches, 3, pad = "0")),
      trip_id = trip$trip_id,
      vessel_id = trip$vessel_id,
      catch_date = trip$departure_date + days(sample(0:trip$fishing_days, n_catches, replace = TRUE)),
      catch_time = paste0(str_pad(sample(0:23, n_catches, replace = TRUE), 2, pad = "0"), ":", 
                         str_pad(sample(0:59, n_catches, replace = TRUE), 2, pad = "0")),
      species = sample(species, n_catches, replace = TRUE),
      weight_kg = round(rnorm(n_catches, mean = 500, sd = 200), 1),
      latitude = round(runif(n_catches, 51, 56), 4),
      longitude = round(runif(n_catches, 2, 8), 4),
      fishing_zone = sample(c("IVa", "IVb", "IVc", "VIId"), n_catches, replace = TRUE),
      gear_type = sample(c("OTB", "OTM", "PTM", "TBB"), n_catches, replace = TRUE)
    ) |>
      mutate(
        weight_kg = pmax(weight_kg, 10)  # Ensure positive weights
      )
  })
}

# Generate mock sensor data ----
generate_mock_sensors <- function(trips_data,
                                  other_sensors_per_trip = 20:50) {
  
  map_dfr(1:nrow(trips_data), function(i) {
    trip <- trips_data[i, ]
    
    # Calculate trip duration in minutes
    trip_start <- as.POSIXct(paste(trip$departure_date, "08:00:00"))
    trip_end <- as.POSIXct(paste(trip$arrival_date, "18:00:00"))
    trip_duration_minutes <- as.numeric(difftime(trip_end, trip_start, units = "mins"))
    
    # Generate minute-by-minute timestamps (with slight irregularity)
    n_gps_readings <- round(trip_duration_minutes)
    gps_timestamps <- trip_start + minutes(0:(n_gps_readings - 1)) + 
                      seconds(sample(-30:30, n_gps_readings, replace = TRUE))  # Add jitter
    
    # GPS POSITION readings (every minute)
    # Simulate vessel moving in North Sea area (roughly)
    base_lat <- runif(1, 51.5, 55.5)  # North Sea latitude range
    base_lon <- runif(1, 2.0, 6.0)    # North Sea longitude range
    
    gps_latitude <- tibble(
      sensor_id = paste0("SENSOR_GPS_LAT_", trip$trip_id, "_", str_pad(1:n_gps_readings, 5, pad = "0")),
      trip_id = trip$trip_id,
      vessel_id = trip$vessel_id,
      timestamp = gps_timestamps,
      sensor_type = "gps_latitude",
      value = base_lat + cumsum(rnorm(n_gps_readings, mean = 0, sd = 0.01)),  # Random walk
      unit = "degrees",
      date = as.Date(gps_timestamps)
    )
    
    gps_longitude <- tibble(
      sensor_id = paste0("SENSOR_GPS_LON_", trip$trip_id, "_", str_pad(1:n_gps_readings, 5, pad = "0")),
      trip_id = trip$trip_id,
      vessel_id = trip$vessel_id,
      timestamp = gps_timestamps,
      sensor_type = "gps_longitude",
      value = base_lon + cumsum(rnorm(n_gps_readings, mean = 0, sd = 0.01)),  # Random walk
      unit = "degrees",
      date = as.Date(gps_timestamps)
    )
    
    # FISHING LINE LENGTH readings (every minute)
    # Simulate line being deployed and retrieved
    fishing_active <- rep(FALSE, n_gps_readings)
    # Randomly select periods when fishing (30-70% of trip)
    n_fishing_periods <- round(n_gps_readings * runif(1, 0.3, 0.7))
    if (n_fishing_periods > 0 && n_fishing_periods <= n_gps_readings) {
      fishing_periods <- sample(1:n_gps_readings, n_fishing_periods)
      fishing_active[fishing_periods] <- TRUE
    }
    
    line_length_values <- numeric(n_gps_readings)
    for (j in 1:n_gps_readings) {
      if (fishing_active[j]) {
        # When fishing: line length varies between 50-500 meters
        line_length_values[j] <- abs(rnorm(1, mean = 250, sd = 100))
      } else {
        # When not fishing: line is retracted (0-20 meters)
        line_length_values[j] <- abs(rnorm(1, mean = 5, sd = 5))
      }
    }
    
    line_length <- tibble(
      sensor_id = paste0("SENSOR_LINE_", trip$trip_id, "_", str_pad(1:n_gps_readings, 5, pad = "0")),
      trip_id = trip$trip_id,
      vessel_id = trip$vessel_id,
      timestamp = gps_timestamps,
      sensor_type = "line_length",
      value = round(pmax(line_length_values, 0), 1),  # Ensure non-negative
      unit = "meters",
      date = as.Date(gps_timestamps)
    )
    
    # OTHER SENSORS (less frequent - every 15-30 minutes)
    # Make sure we don't try to sample more than available timestamps
    max_other_sensors <- min(max(other_sensors_per_trip), n_gps_readings)
    n_other_sensors <- sample(min(other_sensors_per_trip):max_other_sensors, 1)
    
    # Sample timestamps for other sensors
    other_timestamps <- sort(sample(gps_timestamps, size = n_other_sensors, replace = FALSE))
    other_sensor_types <- c("temperature", "speed", "fuel_consumption", "engine_rpm")
    
    other_sensors <- tibble(
      sensor_id = paste0("SENSOR_OTHER_", trip$trip_id, "_", str_pad(1:n_other_sensors, 4, pad = "0")),
      trip_id = trip$trip_id,
      vessel_id = trip$vessel_id,
      timestamp = other_timestamps,
      sensor_type = sample(other_sensor_types, n_other_sensors, replace = TRUE),
      date = as.Date(other_timestamps)
    ) |>
      mutate(
        value = case_when(
          sensor_type == "temperature" ~ round(rnorm(n(), mean = 8, sd = 2), 1),
          sensor_type == "speed" ~ round(abs(rnorm(n(), mean = 6, sd = 2)), 1),
          sensor_type == "fuel_consumption" ~ round(abs(rnorm(n(), mean = 45, sd = 10)), 1),
          sensor_type == "engine_rpm" ~ round(abs(rnorm(n(), mean = 1200, sd = 200)), 0),
          TRUE ~ 0
        ),
        unit = case_when(
          sensor_type == "temperature" ~ "celsius",
          sensor_type == "speed" ~ "knots",
          sensor_type == "fuel_consumption" ~ "liters/hour",
          sensor_type == "engine_rpm" ~ "rpm",
          TRUE ~ "unknown"
        )
      )
    
    # Combine all sensors
    bind_rows(gps_latitude, gps_longitude, line_length, other_sensors)
  })
}

# Main function to generate all mock data ----
generate_mock_poseidat_data <- function(n_trips = 50,
                                        vessel_ids = c("NLD123456", "NLD789012", "NLD345678"),
                                        start_date = "2024-01-01",
                                        end_date = "2024-12-31") {
  
  cat("Generating mock Poseidat data...\n")
  
  # Generate trips
  cat("  Creating", n_trips, "trips...\n")
  trips <- generate_mock_trips(n_trips, vessel_ids, start_date, end_date)
  
  # Generate catches
  cat("  Creating catch data...\n")
  catches <- generate_mock_catches(trips)
  
  # Generate sensor data
  cat("  Creating sensor data...\n")
  sensors <- generate_mock_sensors(trips)
  
  cat("✓ Mock data generation complete!\n")
  cat("  Trips:", nrow(trips), "\n")
  cat("  Catches:", nrow(catches), "\n")
  cat("  Sensor readings:", nrow(sensors), "\n")
  cat("    - GPS latitude: ~", round(nrow(sensors[sensors$sensor_type == "gps_latitude",]) / nrow(trips)), "per trip (every minute)\n")
  cat("    - GPS longitude: ~", round(nrow(sensors[sensors$sensor_type == "gps_longitude",]) / nrow(trips)), "per trip (every minute)\n")
  cat("    - Line length: ~", round(nrow(sensors[sensors$sensor_type == "line_length",]) / nrow(trips)), "per trip (every minute)\n")
  cat("    - Other sensors: ~", round(nrow(sensors[!sensors$sensor_type %in% c("gps_latitude", "gps_longitude", "line_length"),]) / nrow(trips)), "per trip\n")
  cat("\n")
  
  list(
    trips = trips,
    catches = catches,
    sensors = sensors
  )
}

# Quick test function ----
test_mock_data <- function() {
  
  cat("\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
  cat("TESTING MOCK DATA GENERATION\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  # Generate small dataset for testing
  mock_data <- generate_mock_poseidat_data(
    n_trips = 10,
    vessel_ids = c("NLD123456", "NLD789012"),
    start_date = "2024-01-01",
    end_date = "2024-03-31"
  )
  
  # Show samples
  cat("Sample trips:\n")
  print(head(mock_data$trips, 3))
  
  cat("\n\nSample catches:\n")
  print(head(mock_data$catches, 5))
  
  cat("\n\nSample sensors:\n")
  print(head(mock_data$sensors, 5))
  
  cat("\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
  cat("Mock data looks good! Ready to test your functions.\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  invisible(mock_data)
}

# Print usage instructions
cat("\n✓ Mock data generator loaded!\n\n")
cat("Generate test data:\n")
cat("  mock_data <- generate_mock_poseidat_data(n_trips = 50)\n\n")
cat("Quick test:\n")
cat("  test_mock_data()\n\n")
cat("Access data:\n")
cat("  mock_data$trips\n")
cat("  mock_data$catches\n")
cat("  mock_data$sensors\n\n")
cat("Sensor types included:\n")
cat("  - gps_latitude (every minute)\n")
cat("  - gps_longitude (every minute)\n")
cat("  - line_length (fishing line length, every minute)\n")
cat("  - temperature, speed, fuel_consumption, engine_rpm (periodic)\n\n")
