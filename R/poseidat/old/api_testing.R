library(httr2)
library(dplyr)
library(purrr)
library(lubridate)

#' Get All Journals (Vessels)
#'
#' @param base_url Character. Base URL for the API (e.g., "https://your-server.com/api/v1")
#' @param api_token Character. Your API authentication token
#'
#' @return A data frame with journal information
#' @export
get_journals <- function(base_url, api_token) {
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) |>
    req_perform()
  
  journals_data <- resp_body_json(response)
  
  if (length(journals_data) == 0) {
    return(tibble(
      journal_id = character(),
      vessel_name = character()
    ))
  }
  
  # Convert to data frame
  journals_df <- map_dfr(journals_data, function(journal) {
    tibble(
      journal_id = journal$journal_id %||% NA_character_,
      vessel_name = journal$vessel$name %||% NA_character_,
      flag_state = journal$vessel$flag_state %||% NA_character_,
      cfr = journal$vessel$cfr %||% NA_character_,
      hull_number = journal$vessel$hull_number %||% NA_character_
    )
  })
  
  return(journals_df)
}


#' Get Equipment Inventory with Devices (Flattened)
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param flatten_devices Logical. If TRUE, creates one row per device (default FALSE)
#'
#' @return A data frame with equipment and optionally device information
#' @export
get_equipment_inventory <- function(base_url, api_token, journal_id, 
                                    flatten_devices = FALSE) {
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("equipment_inventory") |>
    req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) |>
    req_perform()
  
  equipment_data <- resp_body_json(response)
  
  # Extract the equipment list
  equipment_list <- equipment_data$equipment
  
  if (is.null(equipment_list) || length(equipment_list) == 0) {
    return(tibble(
      equipment_id = character(),
      equipment_name = character(),
      equipment_type = character()
    ))
  }
  
  if (!flatten_devices) {
    # Keep devices as list column
    equipment_df <- map_dfr(equipment_list, function(equip) {
      tibble(
        equipment_id = equip$equipment_id %||% NA_character_,
        equipment_name = equip$name %||% NA_character_,
        equipment_type = equip$type %||% NA_character_,
        num_devices = length(equip$devices %||% list()),
        devices = list(equip$devices)
      )
    })
  } else {
    # Flatten: one row per device
    equipment_df <- map_dfr(equipment_list, function(equip) {
      if (is.null(equip$devices) || length(equip$devices) == 0) {
        # Equipment with no devices
        return(tibble(
          equipment_id = equip$equipment_id %||% NA_character_,
          equipment_name = equip$name %||% NA_character_,
          equipment_type = equip$type %||% NA_character_,
          device_id = NA_character_,
          device_name = NA_character_,
          device_type = NA_character_,
          device_brand = NA_character_,
          device_product_no = NA_character_,
          device_serial_no = NA_character_,
          device_complete_up_to = as.POSIXct(NA)
        ))
      }
      
      # One row per device
      map_dfr(equip$devices, function(device) {
        tibble(
          equipment_id = equip$equipment_id %||% NA_character_,
          equipment_name = equip$name %||% NA_character_,
          equipment_type = equip$type %||% NA_character_,
          device_id = device$device_id %||% NA_character_,
          device_name = device$name %||% NA_character_,
          device_type = device$type %||% NA_character_,
          device_brand = device$brand %||% NA_character_,
          device_product_no = device$product_no %||% NA_character_,
          device_serial_no = device$serial_no %||% NA_character_,
          device_complete_up_to = if (!is.null(device$complete_up_to)) {
            as.POSIXct(device$complete_up_to, format = "%Y-%m-%dT%H:%M:%S")
          } else {
            as.POSIXct(NA)
          }
        )
      })
    })
  }
  
  return(equipment_df)
}


# Get equipment (devices as list column)
equipment <- get_equipment_inventory(base_url, api_token, journal_id)
print(equipment)

# Get equipment with flattened devices (one row per device)
equipment_flat <- get_equipment_inventory(base_url, api_token, journal_id, 
                                          flatten_devices = TRUE)
print(equipment_flat)

# Access devices for specific equipment
equipment$devices[[1]]  # Devices for first equipment








#' Get Measurements for Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param equipment_id Character. The equipment ID
#' @param start_time POSIXct or Character. Start time (e.g., "2024-12-17 10:00:00" or POSIXct object)
#' @param stop_time POSIXct or Character. Stop time (e.g., "2024-12-18 10:00:00" or POSIXct object)
#'
#' @return A list with measurement data from the API
#' @export
get_measurements <- function(base_url, api_token, journal_id, equipment_id,
                             start_time, stop_time) {
  
  # Convert character to POSIXct if needed
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time)
  }
  if (is.character(stop_time)) {
    stop_time <- as.POSIXct(stop_time)
  }
  
  # Convert to Unix timestamps
  start_unix <- as.integer(as.numeric(start_time))
  stop_unix <- as.integer(as.numeric(stop_time))
  
  message(sprintf("Fetching measurements from %s to %s", 
                  format(start_time, "%Y-%m-%d %H:%M:%S"),
                  format(stop_time, "%Y-%m-%d %H:%M:%S")))
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("measurements") |>
    req_url_path_append(equipment_id) |>
    req_url_query(start = start_unix, stop = stop_unix) |>
    req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) |>
    req_perform()
  
  measurements <- resp_body_json(response)
  
  return(measurements)
}

#' Debug measurement query
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param equipment_id Character. The equipment ID
#' @param start_time POSIXct or Character. Start time
#' @param stop_time POSIXct or Character. Stop time
#'
#' @return Debug information about the query
debug_measurements <- function(base_url, api_token, journal_id, equipment_id,
                               start_time, stop_time) {
  
  # Convert character to POSIXct if needed
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time)
  }
  if (is.character(stop_time)) {
    stop_time <- as.POSIXct(stop_time)
  }
  
  # Convert to Unix timestamps
  start_unix <- as.integer(as.numeric(start_time))
  stop_unix <- as.integer(as.numeric(stop_time))
  
  cat("=== MEASUREMENT QUERY DEBUG ===\n")
  cat(sprintf("Journal ID: %s\n", journal_id))
  cat(sprintf("Equipment ID: %s\n", equipment_id))
  cat(sprintf("Start time: %s (Unix: %d)\n", 
              format(start_time, "%Y-%m-%d %H:%M:%S"), start_unix))
  cat(sprintf("Stop time: %s (Unix: %d)\n", 
              format(stop_time, "%Y-%m-%d %H:%M:%S"), stop_unix))
  cat(sprintf("Time span: %.2f days\n", 
              as.numeric(difftime(stop_time, start_time, units = "days"))))
  
  # Build URL
  url <- sprintf("%s/journals/%s/measurements/%s?start=%d&stop=%d",
                 base_url, journal_id, equipment_id, start_unix, stop_unix)
  cat(sprintf("\nFull URL:\n%s\n\n", url))
  
  # Make request
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("measurements") |>
    req_url_path_append(equipment_id) |>
    req_url_query(start = start_unix, stop = stop_unix) |>
    req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) |>
    req_perform()
  
  cat(sprintf("Status code: %d\n", resp_status(response)))
  cat(sprintf("Content type: %s\n", resp_content_type(response)))
  
  # Get raw body
  raw_body <- resp_body_raw(response)
  cat(sprintf("Raw body length: %d bytes\n", length(raw_body)))
  cat(sprintf("Raw body (hex): %s\n", paste(as.character(raw_body), collapse = " ")))
  cat(sprintf("Raw body (string): %s\n", rawToChar(raw_body)))
  
  # Parse JSON
  measurements <- resp_body_json(response)
  cat(sprintf("\nParsed JSON length: %d\n", length(measurements)))
  
  if (length(measurements) == 0) {
    cat("\n⚠ NO DATA RETURNED ⚠\n")
    cat("This means:\n")
    cat("  1. The API query worked correctly\n")
    cat("  2. But there's no data for this equipment in this time range\n")
    cat("\nTroubleshooting:\n")
    cat("  - Check if 'complete_up_to' date from equipment_inventory is within your query range\n")
    cat("  - Try a wider time range\n")
    cat("  - Verify this equipment_id actually has measurements\n")
  } else {
    cat(sprintf("\n✓ Found %d measurements\n", length(measurements)))
  }
  
  return(invisible(list(
    response = response,
    measurements = measurements,
    url = url
  )))
}

# First, check what equipment you have and their complete_up_to dates
equipment_dates <- get_equipment_with_dates(base_url, api_token, journal_id)
print(equipment_dates)

# Pick an equipment and check its latest data time
equipment_id <- equipment_dates$equipment_id[1]
latest_time <- equipment_dates$complete_up_to[
  equipment_dates$equipment_id == equipment_id & 
    equipment_dates$device_id == equipment_dates$device_id[equipment_dates$equipment_id == equipment_id][1]
]

cat(sprintf("Latest data available: %s\n", latest_time))

# Query BEFORE that time
stop_time <- latest_time
start_time <- stop_time - (24 * 60 * 60)  # 24 hours before

# Debug the query
debug_measurements(base_url, api_token, journal_id, equipment_id,
                   start_time, stop_time)

# Query the last 30 days
debug_measurements(
  base_url, api_token, journal_id, equipment_id,
  start_time = Sys.time() - (3000 * 24 * 60 * 60),
  stop_time = Sys.time()
)

#' Get Measurements as Data Frame
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param equipment_id Character. The equipment ID
#' @param start_time POSIXct or Character. Start time (e.g., "2024-12-17 10:00:00")
#' @param stop_time POSIXct or Character. Stop time (e.g., "2024-12-18 10:00:00")
#'
#' @return A data frame with measurements
#' @export
get_measurements_df <- function(base_url, api_token, journal_id, equipment_id,
                                start_time, stop_time) {
  
  measurements <- get_measurements(base_url, api_token, journal_id, equipment_id,
                                   start_time, stop_time)
  
  if (length(measurements) == 0 || is.null(measurements)) {
    return(tibble())
  }
  
  # The API might return measurements directly or nested
  # Check if there's a 'measurements' or 'data' field
  if (is.list(measurements) && !is.null(measurements$measurements)) {
    measurement_list <- measurements$measurements
  } else if (is.list(measurements) && !is.null(measurements$data)) {
    measurement_list <- measurements$data
  } else {
    measurement_list <- measurements
  }
  
  # Try to convert to data frame
  tryCatch({
    if (is.list(measurement_list) && length(measurement_list) > 0) {
      # Convert each measurement to a row
      measurements_df <- map_dfr(measurement_list, function(m) {
        # Base fields
        row <- tibble(
          timestamp = m$timestamp %||% m$ts %||% m$time %||% NA_real_
        )
        
        # Add datetime if timestamp exists
        if (!is.na(row$timestamp)) {
          row$datetime <- as_datetime(row$timestamp)
        }
        
        # Add all other fields
        other_fields <- m[!names(m) %in% c("timestamp", "ts", "time")]
        if (length(other_fields) > 0) {
          # Handle nested structures
          other_fields <- map(other_fields, function(x) {
            if (is.list(x) && length(x) > 0) {
              # If it's a named list, convert to string
              if (!is.null(names(x))) {
                paste(names(x), x, sep = "=", collapse = "; ")
              } else {
                toString(x)
              }
            } else {
              x
            }
          })
          row <- bind_cols(row, as_tibble(other_fields))
        }
        
        row
      })
      
      return(measurements_df)
    }
    
    return(tibble())
    
  }, error = function(e) {
    warning(sprintf("Could not convert measurements to data frame: %s", e$message))
    return(tibble())
  })
}


#' Get Date Range for Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param equipment_id Character. The equipment ID
#'
#' @return A list with first_datetime and last_datetime, or NULL if endpoint doesn't exist
#' @export
get_equipment_date_range <- function(base_url, api_token, journal_id, equipment_id) {
  
  # Try multiple possible endpoint patterns
  possible_paths <- list(
    c("journals", journal_id, "measurements", equipment_id, "date_range"),
    c("journals", journal_id, "measurements", equipment_id, "range"),
    c("journals", journal_id, "equipment", equipment_id, "date_range"),
    c("journals", journal_id, "equipment", equipment_id, "range")
  )
  
  for (path in possible_paths) {
    tryCatch({
      req <- request(base_url)
      for (p in path) {
        req <- req_url_path_append(req, p)
      }
      
      response <- req |>
        req_headers("Authorization" = paste("Bearer", api_token)) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()
      
      if (resp_status(response) == 200) {
        date_range <- resp_body_json(response)
        
        # Extract timestamps (try different field names)
        first_ts <- date_range$first %||% date_range$start %||% 
          date_range$min %||% date_range$first_timestamp
        last_ts <- date_range$last %||% date_range$stop %||% 
          date_range$max %||% date_range$last_timestamp
        
        return(list(
          first_datetime = if (!is.null(first_ts)) as_datetime(first_ts) else NA,
          last_datetime = if (!is.null(last_ts)) as_datetime(last_ts) else NA,
          endpoint = paste(path, collapse = "/")
        ))
      }
      
    }, error = function(e) {
      # Continue to next pattern
    })
  }
  
  # If no endpoint found, return NULL
  message(sprintf("  No date_range endpoint found for equipment %s", equipment_id))
  return(NULL)
}


#' Get All Equipment with Date Ranges
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param show_progress Logical. Show progress messages? (default TRUE)
#'
#' @return A data frame with equipment and their date ranges (if available)
#' @export
get_all_equipment_with_dates <- function(base_url, api_token, journal_id,
                                         show_progress = TRUE) {
  
  # Get equipment inventory
  if (show_progress) message("Fetching equipment inventory...")
  equipment <- get_equipment_inventory(base_url, api_token, journal_id)
  
  if (nrow(equipment) == 0) {
    warning("No equipment found for this vessel")
    return(equipment)
  }
  
  if (show_progress) message(sprintf("Found %d equipment items", nrow(equipment)))
  
  # Try to get date ranges
  if (show_progress) message("Attempting to fetch date ranges for each equipment...")
  
  date_ranges <- map(seq_len(nrow(equipment)), function(i) {
    equipment_id <- equipment$equipment_id[i]
    equipment_name <- equipment$equipment_name[i]
    
    if (show_progress) {
      message(sprintf("  [%d/%d] %s (%s)", 
                      i, nrow(equipment), equipment_name, equipment_id))
    }
    
    date_range <- get_equipment_date_range(base_url, api_token, journal_id, equipment_id)
    
    if (!is.null(date_range)) {
      tibble(
        equipment_id = equipment_id,
        first_datetime = date_range$first_datetime,
        last_datetime = date_range$last_datetime,
        days_available = if (!is.na(date_range$first_datetime) && !is.na(date_range$last_datetime)) {
          as.numeric(difftime(date_range$last_datetime, date_range$first_datetime, units = "days"))
        } else {
          NA_real_
        }
      )
    } else {
      tibble(
        equipment_id = equipment_id,
        first_datetime = as.POSIXct(NA),
        last_datetime = as.POSIXct(NA),
        days_available = NA_real_
      )
    }
  }) |>
    bind_rows()
  
  # Combine with equipment data
  result <- equipment |>
    left_join(date_ranges, by = "equipment_id")
  
  if (show_progress) {
    # Show summary
    with_dates <- sum(!is.na(result$first_datetime))
    message(sprintf("\nSummary: %d/%d equipment have date range information", 
                    with_dates, nrow(result)))
  }
  
  return(result)
}


#' Fetch All Measurements for All Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param start_time POSIXct or Character. Start time (e.g., "2024-12-17 10:00:00")
#' @param stop_time POSIXct or Character. Stop time (e.g., "2024-12-18 10:00:00")
#' @param show_progress Logical. Show progress messages? (default TRUE)
#'
#' @return A list with equipment info and all measurements
#' @export
fetch_all_measurements <- function(base_url, api_token, journal_id,
                                   start_time, stop_time,
                                   show_progress = TRUE) {
  
  # Convert character to POSIXct if needed
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time)
  }
  if (is.character(stop_time)) {
    stop_time <- as.POSIXct(stop_time)
  }
  
  # Get equipment
  equipment <- get_equipment_inventory(base_url, api_token, journal_id)
  
  if (nrow(equipment) == 0) {
    warning("No equipment found for this vessel")
    return(list(equipment = equipment, measurements = list()))
  }
  
  if (show_progress) {
    message(sprintf("\nFetching measurements for %d equipment items", nrow(equipment)))
    message(sprintf("Time range: %s to %s", 
                    format(start_time, "%Y-%m-%d %H:%M:%S"),
                    format(stop_time, "%Y-%m-%d %H:%M:%S")))
    message("")
  }
  
  # Fetch measurements for each equipment
  all_measurements <- map(seq_len(nrow(equipment)), function(i) {
    equipment_id <- equipment$equipment_id[i]
    equipment_name <- equipment$equipment_name[i]
    
    if (show_progress) {
      message(sprintf("  [%d/%d] %s (%s)", 
                      i, nrow(equipment), equipment_name, equipment_id))
    }
    
    tryCatch({
      measurements <- get_measurements(
        base_url, api_token, journal_id, equipment_id,
        start_time, stop_time
      )
      
      # Try to count measurements
      count <- if (is.list(measurements)) {
        if (!is.null(measurements$measurements)) {
          length(measurements$measurements)
        } else if (!is.null(measurements$data)) {
          length(measurements$data)
        } else {
          length(measurements)
        }
      } else {
        0
      }
      
      if (show_progress && count > 0) {
        message(sprintf("    → Found %d measurements", count))
      }
      
      list(
        equipment_id = equipment_id,
        equipment_name = equipment_name,
        measurements = measurements,
        count = count
      )
      
    }, error = function(e) {
      if (show_progress) {
        warning(sprintf("    → Failed: %s", e$message))
      }
      list(
        equipment_id = equipment_id,
        equipment_name = equipment_name,
        measurements = list(),
        count = 0,
        error = e$message
      )
    })
    
    Sys.sleep(0.1)  # Be nice to the API
  })
  
  names(all_measurements) <- equipment$equipment_id
  
  # Summary
  if (show_progress) {
    total_measurements <- sum(map_dbl(all_measurements, ~ .x$count))
    successful <- sum(map_lgl(all_measurements, ~ is.null(.x$error)))
    message(sprintf("\nComplete: %d/%d equipment successful, %d total measurements", 
                    successful, length(all_measurements), total_measurements))
  }
  
  return(list(
    equipment = equipment,
    measurements = all_measurements,
    time_range = list(
      start = start_time,
      stop = stop_time,
      start_unix = as.integer(as.numeric(start_time)),
      stop_unix = as.integer(as.numeric(stop_time))
    )
  ))
}


#' Get Equipment Inventory with Device Date Info
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#'
#' @return A data frame with equipment and device complete_up_to dates
#' @export
get_equipment_with_dates <- function(base_url, api_token, journal_id) {
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("equipment_inventory") |>
    req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) |>
    req_perform()
  
  equipment_data <- resp_body_json(response)
  equipment_list <- equipment_data$equipment
  
  if (is.null(equipment_list) || length(equipment_list) == 0) {
    return(tibble(
      equipment_id = character(),
      equipment_name = character(),
      equipment_type = character(),
      device_id = character(),
      device_name = character(),
      complete_up_to = as.POSIXct(character())
    ))
  }
  
  # Flatten: one row per device with complete_up_to
  equipment_df <- map_dfr(equipment_list, function(equip) {
    if (is.null(equip$devices) || length(equip$devices) == 0) {
      return(tibble(
        equipment_id = equip$equipment_id %||% NA_character_,
        equipment_name = equip$name %||% NA_character_,
        equipment_type = equip$type %||% NA_character_,
        device_id = NA_character_,
        device_name = NA_character_,
        device_type = NA_character_,
        complete_up_to = as.POSIXct(NA)
      ))
    }
    
    # One row per device
    map_dfr(equip$devices, function(device) {
      tibble(
        equipment_id = equip$equipment_id %||% NA_character_,
        equipment_name = equip$name %||% NA_character_,
        equipment_type = equip$type %||% NA_character_,
        device_id = device$device_id %||% NA_character_,
        device_name = device$name %||% NA_character_,
        device_type = device$type %||% NA_character_,
        complete_up_to = if (!is.null(device$complete_up_to)) {
          as.POSIXct(device$complete_up_to, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        } else {
          as.POSIXct(NA)
        }
      )
    })
  })
  
  return(equipment_df)
}


#' Get Latest Available Data Time for Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#'
#' @return A data frame with equipment and their latest available datetime
#' @export
get_equipment_latest_data <- function(base_url, api_token, journal_id) {
  
  equipment_devices <- get_equipment_with_dates(base_url, api_token, journal_id)
  
  # Get the latest complete_up_to for each equipment
  equipment_summary <- equipment_devices |>
    group_by(equipment_id, equipment_name, equipment_type) |>
    summarise(
      num_devices = n(),
      latest_complete_up_to = max(complete_up_to, na.rm = TRUE),
      oldest_complete_up_to = min(complete_up_to, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      latest_complete_up_to = if_else(
        is.infinite(latest_complete_up_to), 
        as.POSIXct(NA), 
        as.POSIXct(latest_complete_up_to, origin = "1970-01-01", tz = "UTC")
      ),
      oldest_complete_up_to = if_else(
        is.infinite(oldest_complete_up_to), 
        as.POSIXct(NA), 
        as.POSIXct(oldest_complete_up_to, origin = "1970-01-01", tz = "UTC")
      )
    )
  
  return(equipment_summary)
}

#' Find First Available Measurement for Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param equipment_id Character. The equipment ID
#' @param search_days_back Integer. How many days back to search (default 365)
#'
#' @return A POSIXct datetime of first available measurement, or NA
#' @export
find_first_measurement <- function(base_url, api_token, journal_id, equipment_id,
                                   search_days_back = 365) {
  
  # Start from now and go back
  stop_time <- Sys.time()
  start_time <- stop_time - (search_days_back * 24 * 60 * 60)
  
  message(sprintf("Searching for first measurement between %s and %s",
                  format(start_time, "%Y-%m-%d"),
                  format(stop_time, "%Y-%m-%d")))
  
  tryCatch({
    measurements <- get_measurements(
      base_url, api_token, journal_id, equipment_id,
      start_time, stop_time
    )
    
    # Extract timestamps from measurements
    if (is.list(measurements)) {
      if (!is.null(measurements$measurements)) {
        timestamps <- map_dbl(measurements$measurements, ~ .x$timestamp %||% NA_real_)
      } else if (!is.null(measurements$data)) {
        timestamps <- map_dbl(measurements$data, ~ .x$timestamp %||% NA_real_)
      } else if (length(measurements) > 0) {
        timestamps <- map_dbl(measurements, ~ .x$timestamp %||% NA_real_)
      } else {
        return(as.POSIXct(NA))
      }
      
      timestamps <- timestamps[!is.na(timestamps)]
      
      if (length(timestamps) > 0) {
        first_ts <- min(timestamps)
        first_datetime <- as_datetime(first_ts)
        message(sprintf("  First measurement found: %s", 
                        format(first_datetime, "%Y-%m-%d %H:%M:%S")))
        return(first_datetime)
      }
    }
    
    return(as.POSIXct(NA))
    
  }, error = function(e) {
    warning(sprintf("Error finding first measurement: %s", e$message))
    return(as.POSIXct(NA))
  })
}


#' Get Complete Date Range for All Equipment
#'
#' @param base_url Character. Base URL for the API
#' @param api_token Character. Your API authentication token
#' @param journal_id Character. The journal/vessel ID
#' @param search_days_back Integer. Days to search back for first measurement (default 365)
#' @param show_progress Logical. Show progress? (default TRUE)
#'
#' @return A data frame with equipment and full date ranges
#' @export
get_complete_date_ranges <- function(base_url, api_token, journal_id,
                                     search_days_back = 365,
                                     show_progress = TRUE) {
  
  # Get equipment with latest dates from inventory
  if (show_progress) message("Fetching equipment inventory...")
  equipment_dates <- get_equipment_latest_data(base_url, api_token, journal_id)
  
  if (nrow(equipment_dates) == 0) {
    warning("No equipment found")
    return(equipment_dates)
  }
  
  if (show_progress) {
    message(sprintf("Found %d equipment items", nrow(equipment_dates)))
    message(sprintf("Searching for first measurements (last %d days)...\n", search_days_back))
  }
  
  # Find first measurement for each equipment
  first_measurements <- map(seq_len(nrow(equipment_dates)), function(i) {
    equipment_id <- equipment_dates$equipment_id[i]
    equipment_name <- equipment_dates$equipment_name[i]
    
    if (show_progress) {
      message(sprintf("[%d/%d] %s (%s)", 
                      i, nrow(equipment_dates), equipment_name, equipment_id))
    }
    
    first_datetime <- find_first_measurement(
      base_url, api_token, journal_id, equipment_id, search_days_back
    )
    
    tibble(
      equipment_id = equipment_id,
      first_measurement = first_datetime
    )
  }) |>
    bind_rows()
  
  # Combine with equipment data
  result <- equipment_dates |>
    left_join(first_measurements, by = "equipment_id") |>
    mutate(
      days_available = if_else(
        !is.na(first_measurement) & !is.na(latest_complete_up_to),
        as.numeric(difftime(latest_complete_up_to, first_measurement, units = "days")),
        NA_real_
      )
    ) |>
    select(equipment_id, equipment_name, equipment_type, num_devices,
           first_measurement, latest_complete_up_to, days_available)
  
  return(result)
}


# Get equipment with device complete_up_to dates
equipment_dates <- get_equipment_with_dates(base_url, api_token, journal_id)
print(equipment_dates)

# Get summary: latest available data per equipment
equipment_summary <- get_equipment_latest_data(base_url, api_token, journal_id)
print(equipment_summary)

# Find first measurement for specific equipment (searches last 365 days)
first_date <- find_first_measurement(
  base_url, api_token, journal_id, equipment_id,
  search_days_back = 365
)

# Get complete date ranges for all equipment (this takes longer)
complete_ranges <- get_complete_date_ranges(
  base_url, api_token, journal_id,
  search_days_back = 365  # Adjust based on how far back you want to search
)
print(complete_ranges)

# Use the dates to fetch measurements
# For example, get last 7 days up to latest available:
latest <- equipment_summary$latest_complete_up_to[1]
start <- latest - (7 * 24 * 60 * 60)

measurements <- get_measurements_df(
  base_url, api_token, journal_id, equipment_id,
  start_time = start,
  stop_time = latest
)



equipment_id <- equipment$equipment_id
get_equipment_date_range(base_url, api_token, journal_id, equipment_id)
  
# Much easier to read and use!
measurements <- get_measurements(
  base_url, api_token, journal_id, equipment_id,
  start_time = "2024-12-17 10:00:00",
  stop_time = "2024-12-18 10:00:00"
)

# Or with POSIXct
measurements <- get_measurements(
  base_url, api_token, journal_id, equipment_id,
  start_time = as.POSIXct("2024-12-17 10:00:00"),
  stop_time = Sys.time()
)

# Get last 24 hours
measurements_df <- get_measurements_df(
  base_url, api_token, journal_id, equipment_id,
  start_time = Sys.time() - (24 * 60 * 60),
  stop_time = Sys.time()
)

# Get all measurements for last week
all_data <- fetch_all_measurements(
  base_url, api_token, journal_id,
  start_time = Sys.time() - (7 * 24 * 60 * 60),
  stop_time = Sys.time()
)