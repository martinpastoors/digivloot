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
# date_from = "2024-01-01"
# date_to = "2024-12-31"
# params <- list(vessel_id = vessel_ids, date_from = date_from, date_to = date_to) |> compact()
# base = config$base_url
# endpoint = "trips"

# Core API function
get_poseidat <- function(endpoint, params = list(), token = poseidat_token, base = config$base_url) {
  resp <- request(glue("{base}/{endpoint}")) |>
    req_auth_bearer_token(token) |>
    req_url_query(!!!params) |>
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
