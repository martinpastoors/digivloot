# =====================================================
# Unified Reader for Poseidat Data with Auto-Detection
#
# v2.0 12/11/2025 - Unified reader with auto-detection
# =====================================================

library(dplyr)
library(tidyr)
library(jsonlite)

# =====================================================
# CORE FUNCTION: Read and Classify Poseidat Data
# =====================================================

read_poseidat <- function(file_path, detect_type = TRUE) {
  
  # Read JSON file
  cat("Reading Poseidat file:", basename(file_path), "\n")
  
  tryCatch({
    raw_data <- fromJSON(file_path, simplifyVector = TRUE)
  }, error = function(e) {
    stop("Failed to read JSON file: ", e$message)
  })
  
  if (!"items" %in% names(raw_data) || !is.data.frame(raw_data$items)) {
    stop("Invalid Poseidat file format: 'items' not found or not a data frame")
  }
  
  data <- raw_data$items
  
  # Detect data type
  if (detect_type) {
    data_type <- detect_poseidat_type(data)
    cat("Detected data type:", data_type, "\n\n")
  } else {
    data_type <- "mixed"
  }
  
  # Return structured result
  result <- list(
    data = data,
    type = data_type,
    metadata = list(
      file = basename(file_path),
      n_entries = nrow(data),
      date_range = if (nrow(data) > 0 && "entry_datetime" %in% names(data)) {
        c(min(data$entry_datetime, na.rm = TRUE), 
          max(data$entry_datetime, na.rm = TRUE))
      } else NULL
    )
  )
  
  class(result) <- c("poseidat", class(result))
  return(result)
}

# =====================================================
# TYPE DETECTION FUNCTION
# =====================================================

detect_poseidat_type <- function(data) {
  
  if (!is.data.frame(data) || nrow(data) == 0) {
    return("empty")
  }
  
  # Count entry types
  entry_types <- table(data$entry_type)
  
  # Calculate proportions
  n_device <- sum(data$entry_type == "device-measurement", na.rm = TRUE)
  n_trip <- sum(data$entry_type %in% c("departure", "arrival", "fishing-activity", 
                                         "zone-enter", "zone-exit"), na.rm = TRUE)
  
  # Decision logic
  prop_device <- n_device / nrow(data)
  prop_trip <- n_trip / nrow(data)
  
  if (prop_device > 0.8) {
    return("sensor")
  } else if (prop_trip > 0.8) {
    return("trip")
  } else if (prop_device > 0.3 && prop_trip > 0.3) {
    return("mixed")
  } else if (n_device > 0) {
    return("sensor")
  } else if (n_trip > 0) {
    return("trip")
  } else {
    return("unknown")
  }
}

# =====================================================
# SMART EXTRACTION: Routes to appropriate parser
# =====================================================

extract_poseidat <- function(poseidat_obj, what = "auto") {
  
  if (!inherits(poseidat_obj, "poseidat")) {
    stop("Input must be a poseidat object from read_poseidat()")
  }
  
  data <- poseidat_obj$data
  data_type <- poseidat_obj$type
  
  # Auto-detect what to extract based on data type
  if (what == "auto") {
    if (data_type == "sensor") {
      cat("Extracting sensor data...\n")
      return(parse_all_sensor_data(data))
      
    } else if (data_type == "trip") {
      cat("Extracting trip data...\n")
      return(extract_all_trip_data(data))
      
    } else if (data_type == "mixed") {
      cat("Extracting both trip and sensor data...\n")
      return(list(
        trip_data = extract_all_trip_data(data),
        sensor_data = parse_all_sensor_data(data)
      ))
    } else {
      warning("Unknown data type, returning raw data")
      return(data)
    }
  }
  
  # Manual specification
  if (what == "sensor") {
    return(parse_all_sensor_data(data))
  } else if (what == "trip") {
    return(extract_all_trip_data(data))
  } else if (what == "trips") {
    return(build_complete_trips(data))
  } else if (what == "timeline") {
    return(reconstruct_trip_timeline(data))
  } else if (what == "catches") {
    trips <- unique(extract_trip_info(data)$trip_nr)
    trips <- trips[!is.na(trips)]
    if (length(trips) > 0) {
      return(summarize_trip_catches(data, trips[1]))
    }
  } else if (what == "gps") {
    return(parse_gps_data(data))
  } else if (what == "trawl") {
    return(parse_trawl_tension_data(data))
  } else if (what == "ctd") {
    return(parse_ctd_data(data))
  } else {
    stop("Unknown extraction type: ", what)
  }
}

# =====================================================
# HELPER: Extract all trip-related data
# =====================================================

extract_all_trip_data <- function(data) {
  
  result <- list(
    trip_info = extract_trip_info(data),
    trips_summary = build_complete_trips(data),
    gear = link_gear_to_activities(data)
  )
  
  # Get all unique trips
  trips <- unique(result$trip_info$trip_nr)
  trips <- trips[!is.na(trips)]
  
  if (length(trips) > 0) {
    # Add catches for all trips
    catches_list <- list()
    for (trip in trips) {
      trip_catches <- summarize_trip_catches(data, trip)
      if (nrow(trip_catches) > 0) {
        catches_list[[trip]] <- trip_catches
      }
    }
    
    if (length(catches_list) > 0) {
      result$catches = bind_rows(catches_list)
    }
    
    # Add timeline for first trip
    result$timeline = reconstruct_trip_timeline(data, trips[1])
  }
  
  cat("\nTrip Data Extracted:\n")
  cat("  Entries:", nrow(result$trip_info), "\n")
  cat("  Trips:", nrow(result$trips_summary), "\n")
  cat("  Gear records:", nrow(result$gear), "\n")
  if ("catches" %in% names(result)) {
    cat("  Catch records:", nrow(result$catches), "\n")
  }
  
  return(result)
}

# =====================================================
# PRINT METHOD for poseidat objects
# =====================================================

print.poseidat <- function(x, ...) {
  cat("Poseidat Data Object\n")
  cat("====================\n")
  cat("File:", x$metadata$file, "\n")
  cat("Type:", x$type, "\n")
  cat("Entries:", x$metadata$n_entries, "\n")
  
  if (!is.null(x$metadata$date_range)) {
    cat("Date range:", format(x$metadata$date_range[1]), "to", 
        format(x$metadata$date_range[2]), "\n")
  }
  
  cat("\nEntry types:\n")
  print(table(x$data$entry_type))
  
  cat("\nUse extract_poseidat() to parse the data\n")
}

# =====================================================
# CONVENIENCE WRAPPER: Read and extract in one go
# =====================================================

read_and_extract_poseidat <- function(file_path) {
  poseidat_obj <- read_poseidat(file_path)
  return(extract_poseidat(poseidat_obj, what = "auto"))
}

cat("✓ Unified Poseidat Reader Loaded Successfully!\n\n")
cat("Main Functions:\n")
cat("  read_poseidat(file_path) - Read and detect data type\n")
cat("  extract_poseidat(obj, what='auto') - Extract data\n")
cat("  read_and_extract_poseidat(file_path) - One-step read & extract\n\n")
cat("Example:\n")
cat('  poseidat <- read_poseidat("myfile.json")\n')
cat('  data <- extract_poseidat(poseidat)  # Auto-detects type\n')
cat('  # OR in one step:\n')
cat('  data <- read_and_extract_poseidat("myfile.json")\n\n')
