# ==================================================
# Test Script for Unified Poseidat Reader V2.0
#
# v2.0 12/11/2025
# ==================================================

rm(list=ls())

library(jsonlite)
library(tidyverse)

# Load the unified reader (includes all parsing functions)
source("R/poseidat_reader_v2.R")  # Complete reader with all functions
source("R/poseidat_reader_unified.R")  # Unified interface

# Test directory
mydir <- "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data"

# =====================================================
# METHOD 1: Simple one-line extraction
# =====================================================

cat("=== METHOD 1: One-line extraction ===\n\n")

# Trip data
trip_data <- read_and_extract_poseidat(
  file.path(mydir, "poseidat_complete_trip_example.json")
)

# Sensor data
sensor_data <- read_and_extract_poseidat(
  file.path(mydir, "poseidat_sensor_data_example.json")
)

# =====================================================
# METHOD 2: Two-step with inspection
# =====================================================

cat("\n=== METHOD 2: Two-step process ===\n\n")

# Step 1: Read and inspect
poseidat <- read_poseidat(
  file.path(mydir, "poseidat_complete_trip_example.json")
)

print(poseidat)

# Step 2: Extract
data <- extract_poseidat(poseidat, what = "auto")

# =====================================================
# METHOD 3: Extract specific components
# =====================================================

cat("\n=== METHOD 3: Extract specific components ===\n\n")

# Read once
poseidat_trip <- read_poseidat(
  file.path(mydir, "poseidat_complete_trip_example.json")
)

poseidat_sensor <- read_poseidat(
  file.path(mydir, "poseidat_sensor_data_example.json")
)

# Extract specific things
trips <- extract_poseidat(poseidat_trip, what = "trips")
catches <- extract_poseidat(poseidat_trip, what = "catches")
gps <- extract_poseidat(poseidat_sensor, what = "gps")
trawl <- extract_poseidat(poseidat_sensor, what = "trawl")
ctd <- extract_poseidat(poseidat_sensor, what = "ctd")

cat("Trips summary:\n")
print(trips)

cat("\nGPS records:\n")
print(head(gps))

# =====================================================
# METHOD 4: Process multiple files in a loop
# =====================================================

cat("\n=== METHOD 4: Process multiple files ===\n\n")

json_files <- c(
  "poseidat_complete_trip_example.json", 
  "poseidat_sensor_data_example.json"
)

all_trip_info <- data.frame()
all_sensor_data <- list(gps = data.frame(), trawl = data.frame(), ctd = data.frame())

for (json_file in json_files) {
  
  cat("\nProcessing:", json_file, "\n")
  
  # Read
  poseidat <- read_poseidat(file.path(mydir, json_file))
  
  # Route based on type
  if (poseidat$type == "trip") {
    trip_data <- extract_poseidat(poseidat, what = "trip")
    all_trip_info <- bind_rows(all_trip_info, trip_data$trip_info)
    
  } else if (poseidat$type == "sensor") {
    sensor_data <- extract_poseidat(poseidat, what = "sensor")
    all_sensor_data$gps <- bind_rows(all_sensor_data$gps, sensor_data$gps)
    all_sensor_data$trawl <- bind_rows(all_sensor_data$trawl, sensor_data$trawl_tension)
    all_sensor_data$ctd <- bind_rows(all_sensor_data$ctd, sensor_data$ctd)
    
  } else if (poseidat$type == "mixed") {
    mixed_data <- extract_poseidat(poseidat, what = "auto")
    all_trip_info <- bind_rows(all_trip_info, mixed_data$trip_data$trip_info)
    all_sensor_data$gps <- bind_rows(all_sensor_data$gps, mixed_data$sensor_data$gps)
  }
}

cat("\n=== COMBINED RESULTS ===\n")
cat("Total trip entries:", nrow(all_trip_info), "\n")
cat("Total GPS records:", nrow(all_sensor_data$gps), "\n")
cat("Total trawl records:", nrow(all_sensor_data$trawl), "\n")
cat("Total CTD samples:", nrow(all_sensor_data$ctd), "\n")

# =====================================================
# SUMMARY
# =====================================================

cat("\n\n=== SUMMARY OF UNIFIED READER ===\n\n")
cat("Benefits:\n")
cat("  ✓ Automatic type detection\n")
cat("  ✓ Single interface for all data types\n")
cat("  ✓ Easy to process multiple files\n")
cat("  ✓ Flexible extraction (auto or manual)\n")
cat("  ✓ S3 class with print method\n\n")

cat("Recommended usage:\n")
cat('  data <- read_and_extract_poseidat("file.json")  # Simplest\n')
cat("  # OR for more control:\n")
cat('  obj <- read_poseidat("file.json")\n')
cat('  print(obj)  # Inspect\n')
cat('  data <- extract_poseidat(obj, what="auto")\n')

cat("\n✓ All tests completed!\n")
