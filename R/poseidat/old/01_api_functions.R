# ==============================================================================
# Poseidat API Functions
# ==============================================================================

library(tidyverse)
library(httr2)
library(glue)
library(jsonlite)

# Load configuration
if (file.exists(file.path(here(),"config.json"))) {
  config <- read_json("config.json")
} else {
  stop("Configuration file not found. Please run 00_setup.R first.")
}

# Get bearer token
poseidat_token <- Sys.getenv(config$token_env_var)

if (poseidat_token == "") {
  stop("Bearer token not found. Please configure POSEIDAT_TOKEN in .Renviron")
}

# vessel_ids <- c("NLD123456", "NLD789012", "NLD345678")
# date_from = "2025-01-01"
# date_to = "2025-12-31"
# params <- list(vessel_id = vessel_ids, date_from = date_from, date_to = date_to) |> compact()
# base = config$base_url
# endpoint = "trips"
# token = poseidat_token

# Core API function
get_poseidat <- function(endpoint, params = list(), token = poseidat_token, base = config$base_url) {
  resp <- request(glue("{base}/{endpoint}")) |>
    req_auth_bearer_token(token) |>
    req_url_query(!!!params, .multi = "comma") |>
    req_error(is_error = \(resp) FALSE) |>
    req_retry(max_tries = config$max_retries) |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    error_body <- resp_body_string(resp)
    stop(glue("API request failed:\nStatus: {resp_status(resp)}\nEndpoint: {endpoint}\nResponse: {error_body}"))
  }
  
  resp |> resp_body_json(simplifyVector = TRUE) |> as_tibble()
}

# Get trip data
get_trips <- function(vessel_ids = NULL, date_from = NULL, date_to = NULL, ...) {
  params <- list(vessel_id = vessel_ids, date_from = date_from, date_to = date_to, ...) |> compact()
  message("Fetching trips data...")
  result <- get_poseidat("trips", params = params)
  message(glue("✓ Retrieved {nrow(result)} trip records"))
  result
}

# Get catch data
get_catches <- function(vessel_ids = NULL, date_from = NULL, date_to = NULL, species = NULL, ...) {
  params <- list(vessel_id = vessel_ids, date_from = date_from, date_to = date_to, species = species, ...) |> compact()
  message("Fetching catches data...")
  result <- get_poseidat("catches", params = params)
  message(glue("✓ Retrieved {nrow(result)} catch records"))
  result
}

# Get sensor data
get_sensors <- function(vessel_ids = NULL, date_from = NULL, date_to = NULL, sensor_type = NULL, ...) {
  params <- list(vessel_id = vessel_ids, date_from = date_from, date_to = date_to, sensor_type = sensor_type, ...) |> compact()
  message("Fetching sensors data...")
  result <- get_poseidat("sensors", params = params)
  message(glue("✓ Retrieved {nrow(result)} sensor records"))
  result
}

# Get all data types at once
get_all_vessel_data <- function(vessel_ids, date_from, date_to) {
  message(glue("Fetching all data for {length(vessel_ids)} vessel(s)..."))
  list(
    trips = get_trips(vessel_ids, date_from, date_to),
    catches = get_catches(vessel_ids, date_from, date_to),
    sensors = get_sensors(vessel_ids, date_from, date_to)
  )
}

# Get data for multiple vessels iteratively
get_data_by_vessels <- function(vessel_ids, date_from, date_to, data_type = c("trips", "catches", "sensors")) {
  data_type <- match.arg(data_type)
  get_func <- switch(data_type, trips = get_trips, catches = get_catches, sensors = get_sensors)
  message(glue("Fetching {data_type} for {length(vessel_ids)} vessels..."))
  map_dfr(vessel_ids, \(vessel) {
    Sys.sleep(config$rate_limit_delay)
    get_func(vessel_ids = vessel, date_from = date_from, date_to = date_to) |> mutate(vessel_id = vessel, .before = 1)
  })
}

# Helper: Get recent data
get_recent_data <- function(vessel_ids, days = 30, data_type = "all") {
  date_to <- today()
  date_from <- date_to - days(days)
  if (data_type == "all") {
    get_all_vessel_data(vessel_ids, date_from, date_to)
  } else {
    get_func <- switch(data_type, trips = get_trips, catches = get_catches, sensors = get_sensors)
    get_func(vessel_ids, date_from, date_to)
  }
}

# Helper: Get specific month
get_month_data <- function(vessel_ids, year, month, data_type = "all") {
  date_from <- make_date(year, month, 1)
  date_to <- date_from + months(1) - days(1)
  if (data_type == "all") {
    get_all_vessel_data(vessel_ids, date_from, date_to)
  } else {
    get_func <- switch(data_type, trips = get_trips, catches = get_catches, sensors = get_sensors)
    get_func(vessel_ids, date_from, date_to)
  }
}

message("\n✓ Poseidat API functions loaded")






get_poseidat_journals <- function(base_url, api_key, auth_type = "Bearer") {
  
  # Make the GET request
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_headers(
      "Authorization" = paste(auth_type, api_key),
      "Content-Type" = "application/json"
    ) |>
    req_error(is_error = function(resp) FALSE) |>  # Don't error immediately
    req_perform()
  
  # Check if request was successful
  if (resp_status(response) != 200) {
    stop(sprintf(
      "API request failed with status %d: %s",
      resp_status(response),
      resp_status_desc(response)
    ))
  }
  
  # Parse the JSON response
  journals_data <- response |>
    resp_body_json()
  
  # Handle empty response
  if (length(journals_data) == 0) {
    warning("No journals found")
    return(tibble(
      journal_id = character(),
      vessel_name = character(),
      flag_state = character(),
      cfr = character(),
      hull_number = character()
    ))
  }
  
  # Convert to data frame
  journals_df <- map_dfr(journals_data, function(journal) {
    tibble(
      journal_id = journal$journal_id,
      vessel_name = journal$vessel$name,
      flag_state = journal$vessel$flag_state,
      cfr = ifelse(is.null(journal$vessel$cfr), NA_character_, journal$vessel$cfr),
      hull_number = ifelse(is.null(journal$vessel$hull_number), NA_character_, journal$vessel$hull_number)
    )
  })
  
  return(journals_df)
}

# Set your credentials
my_base_url <- config$base_url
my_api_key <- Sys.getenv(config$token_env_var)

# Call the function
journals <- get_poseidat_journals(
  base_url = my_base_url,
  api_key = my_api_key
)

# View results
print(journals)















library(httr2)
library(dplyr)
library(purrr)
library(lubridate)

#' Get All Devices for a Vessel/Journal
#'
#' @param base_url Character. Base URL for the API
#' @param api_key Character. Your API authentication key
#' @param journal_id Character. The journal/vessel ID
#' @param auth_type Character. Authentication type (default "Bearer")
#'
#' @return A data frame with device information
get_poseidat_devices <- function(base_url, api_key, journal_id, auth_type = "Bearer") {
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("devices") |>
    req_headers(
      "Authorization" = paste(auth_type, api_key),
      "Content-Type" = "application/json"
    ) |>
    req_perform()
  
  devices_data <- resp_body_json(response)
  
  if (length(devices_data) == 0) {
    return(tibble(device_id = character(), device_name = character()))
  }
  
  # Convert to data frame (structure depends on API response)
  devices_df <- map_dfr(devices_data, as_tibble)
  
  return(devices_df)
}

get_poseidat_devices(base_url, api_key, journal_id, auth_type = "Bearer")

# Modified function to show the URL being called
get_poseidat_devices_debug <- function(base_url, api_key, journal_id, auth_type = "Bearer") {
  
  # Build the request to see the URL
  req <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("equipment_inventory") |>
    req_headers(
      "Authorization" = paste(auth_type, api_key),
      "Content-Type" = "application/json"
    )
  
  # Print the URL
  print(paste("Attempting URL:", req$url))
  
  # Try the request
  response <- req |>
    req_error(is_error = function(resp) FALSE) |>  # Don't error immediately
    req_perform()
  
  print(paste("Status code:", resp_status(response)))
  print(paste("Status description:", resp_status_desc(response)))
  
  if (resp_status(response) != 200) {
    # Try to get error details
    tryCatch({
      error_body <- resp_body_json(response)
      print("Error response:")
      print(error_body)
    }, error = function(e) {
      print("Could not parse error response")
    })
  }
  
  return(response)
}

# Test it
response <- get_poseidat_devices_debug(base_url, api_key, journal_id)



#' Get Measurements for a Device
#'
#' @param base_url Character. Base URL for the API
#' @param api_key Character. Your API authentication key
#' @param journal_id Character. The journal/vessel ID
#' @param device_id Character. The device ID
#' @param start_time Integer. Unix timestamp for start time
#' @param stop_time Integer. Unix timestamp for stop time
#' @param auth_type Character. Authentication type (default "Bearer")
#'
#' @return A list with raw and normalized measurements
get_device_measurements <- function(base_url, api_key, journal_id, device_id, 
                                    start_time, stop_time, auth_type = "Bearer") {
  
  response <- request(base_url) |>
    req_url_path_append("journals") |>
    req_url_path_append(journal_id) |>
    req_url_path_append("equipment inventory") |>
    req_url_path_append(device_id) |>
    req_url_path_append("measurements") |>
    req_url_query(start = start_time, stop = stop_time) |>
    req_headers(
      "Authorization" = paste(auth_type, api_key),
      "Content-Type" = "application/json"
    ) |>
    req_perform()
  
  measurements <- resp_body_json(response)
  
  return(measurements)
}


#' Fetch All Measurements for a Vessel
#'
#' @param base_url Character. Base URL for the API
#' @param api_key Character. Your API authentication key
#' @param journal_id Character. The journal/vessel ID
#' @param start_time POSIXct or Integer. Start time (if POSIXct, converted to Unix timestamp)
#' @param stop_time POSIXct or Integer. Stop time (if POSIXct, converted to Unix timestamp)
#' @param auth_type Character. Authentication type (default "Bearer")
#' @param show_progress Logical. Show progress bar? (default TRUE)
#'
#' @return A list with 'devices' and 'measurements' (all normalized data combined)
#' @export
fetch_all_measurements_for_vessel <- function(base_url, api_key, journal_id,
                                              start_time, stop_time,
                                              auth_type = "Bearer",
                                              show_progress = TRUE) {
  
  # Convert times to Unix timestamps if needed
  if (inherits(start_time, "POSIXct")) {
    start_time <- as.integer(as.numeric(start_time))
  }
  if (inherits(stop_time, "POSIXct")) {
    stop_time <- as.integer(as.numeric(stop_time))
  }
  
  # 1) Get all devices for this vessel
  message(sprintf("Fetching devices for journal %s...", journal_id))
  devices <- get_poseidat_devices(base_url, api_key, journal_id, auth_type)
  
  if (nrow(devices) == 0) {
    warning("No devices found for this vessel")
    return(list(devices = devices, measurements = list()))
  }
  
  message(sprintf("Found %d devices", nrow(devices)))
  
  # 2) Fetch measurements for each device
  if (show_progress) {
    message(sprintf("Fetching measurements from %s to %s...", 
                    as_datetime(start_time), as_datetime(stop_time)))
  }
  
  all_measurements <- list()
  
  for (i in seq_len(nrow(devices))) {
    device_id <- devices$device_id[i]
    
    if (show_progress) {
      message(sprintf("  [%d/%d] Device: %s", i, nrow(devices), device_id))
    }
    
    tryCatch({
      measurements <- get_device_measurements(
        base_url, api_key, journal_id, device_id,
        start_time, stop_time, auth_type
      )
      
      all_measurements[[device_id]] <- measurements
      
    }, error = function(e) {
      warning(sprintf("Failed to fetch measurements for device %s: %s", 
                      device_id, e$message))
    })
  }
  
  # 3) Extract and combine all normalized measurements
  normalized_measurements <- map(all_measurements, function(m) {
    if (!is.null(m$normalized)) {
      return(m$normalized)
    }
    return(list())
  }) |>
    flatten()
  
  return(list(
    devices = devices,
    measurements = normalized_measurements,
    raw_data = all_measurements
  ))
}


#' Get All Data for a Vessel (Complete Example)
#'
#' @param base_url Character. Base URL for the API
#' @param api_key Character. Your API authentication key
#' @param journal_id Character. Optional. If NULL, uses first available vessel
#' @param time_window_minutes Integer. Number of minutes to look back (default 15)
#' @param auth_type Character. Authentication type (default "Bearer")
#'
#' @return A list with devices, measurements, and summary statistics
#' @export
get_vessel_data_complete <- function(base_url, api_key, journal_id = NULL,
                                     time_window_minutes = 15,
                                     auth_type = "Bearer") {
  
  # 1) Get vessel/journal if not provided
  if (is.null(journal_id)) {
    message("No journal_id provided, fetching available vessels...")
    journals <- get_poseidat_journals(base_url, api_key, auth_type)
    
    if (nrow(journals) == 0) {
      stop("No vessels available")
    }
    
    journal_id <- journals$journal_id[1]
    message(sprintf("Using vessel: %s (%s)", 
                    journals$vessel_name[1], journal_id))
  }
  
  # 2) Define time window
  stop_time <- as.integer(as.numeric(Sys.time()))
  start_time <- stop_time - (time_window_minutes * 60)
  
  # 3) Fetch all data
  result <- fetch_all_measurements_for_vessel(
    base_url, api_key, journal_id,
    start_time, stop_time, auth_type
  )
  
  # 4) Create summary by measurement kind
  if (length(result$measurements) > 0) {
    kinds_summary <- map_chr(result$measurements, ~ .x$kind) |>
      table() |>
      as.data.frame() |>
      rename(kind = Var1, count = Freq) |>
      arrange(desc(count))
    
    message("\nMeasurements by kind:")
    print(kinds_summary)
  } else {
    kinds_summary <- tibble(kind = character(), count = integer())
    message("\nNo measurements found in time window")
  }
  
  return(list(
    devices = result$devices,
    measurements = result$measurements,
    summary = kinds_summary,
    raw_data = result$raw_data
  ))
}


#' Filter Measurements by Type
#'
#' @param measurements List. The measurements from fetch_all_measurements_for_vessel
#' @param kind Character. The kind to filter (e.g., "NUMERIC", "POSITION", "WEATHER")
#' @param raw_type Character. Optional. Further filter by rawType (e.g., "RPM")
#'
#' @return A data frame with filtered measurements
#' @export
filter_measurements <- function(measurements, kind, raw_type = NULL) {
  
  filtered <- measurements |>
    keep(~ .x$kind == kind)
  
  if (!is.null(raw_type)) {
    filtered <- filtered |>
      keep(~ !is.null(.x$payload$rawType) && .x$payload$rawType == raw_type)
  }
  
  # Convert to data frame
  if (length(filtered) > 0) {
    df <- map_dfr(filtered, function(m) {
      base_data <- tibble(
        timestamp = m$timestamp,
        kind = m$kind
      )
      
      # Add payload fields if they exist
      if (!is.null(m$payload)) {
        payload_df <- as_tibble(m$payload)
        base_data <- bind_cols(base_data, payload_df)
      }
      
      return(base_data)
    })
    
    return(df)
  }
  
  return(tibble())
}

#' Get Date Range for a Vessel
#'
#' @param base_url Character. Base URL for the API
#' @param api_key Character. Your API authentication key
#' @param journal_id Character. The journal/vessel ID
#' @param auth_type Character. Authentication type (default "Bearer")
#'
#' @return A list with first_datetime and last_datetime
#' @export
get_vessel_date_range <- function(base_url, api_key, journal_id, auth_type = "Bearer") {
  
  # Try common endpoint patterns
  possible_endpoints <- c(
    paste0("journals/", journal_id, "/date-range"),
    paste0("journals/", journal_id, "/info"),
    paste0("journals/", journal_id, "/metadata")
  )
  
  for (endpoint in possible_endpoints) {
    tryCatch({
      response <- request(base_url) |>
        req_url_path(endpoint) |>
        req_headers(
          "Authorization" = paste(auth_type, api_key),
          "Content-Type" = "application/json"
        ) |>
        req_perform()
      
      data <- resp_body_json(response)
      
      # Try to extract date range information
      if (!is.null(data$first_datetime) || !is.null(data$start_date)) {
        return(list(
          first_datetime = data$first_datetime %||% data$start_date %||% data$min_date,
          last_datetime = data$last_datetime %||% data$end_date %||% data$max_date,
          endpoint_used = endpoint
        ))
      }
      
    }, error = function(e) {
      # Continue to next endpoint
    })
  }
  
  stop("Could not find date range endpoint. Please check API documentation or use binary search method.")
}

base_url <- config$base_url
api_key <- Sys.getenv(config$token_env_var)


  
# Try the API endpoint approach first
date_range <- tryCatch({
  get_vessel_date_range(base_url, api_key, journal_id)
}, error = function(e) {
  # Fall back to binary search
  message("No date-range endpoint found, using binary search...")
  find_vessel_date_range(base_url, api_key, journal_id)
})

print(date_range)

# Get all data for a vessel (last 15 minutes)
vessel_data <- get_vessel_data_complete(
  base_url = base_url,
  api_key = api_key,
  time_window_minutes = 15
)

# Access the results
print(vessel_data$devices)
print(vessel_data$summary)

# Filter for specific measurement types
rpm_data <- filter_measurements(
  vessel_data$measurements,
  kind = "NUMERIC",
  raw_type = "RPM"
)

print(rpm_data)

# Filter for positions
positions <- filter_measurements(
  vessel_data$measurements,
  kind = "POSITION"
)

print(positions)

