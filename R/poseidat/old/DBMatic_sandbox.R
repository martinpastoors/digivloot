library(httr2)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

#' Set up API configuration
#'
#' Call this first to set up your API credentials
#'
#' @param base_url Character. Base URL of the API
#' @param api_token Character. Your Bearer token
#' @export
#' @examples
#' setup_api(
#'   base_url = "https://api.dbmatic.eu/api/v1",
#'   api_token = "your-token-here"
#' )
setup_api <- function(base_url, api_token) {
  options(
    dbmatic_base_url = base_url,
    dbmatic_api_token = api_token
  )
  
  message("✓ API configuration saved")
  message(sprintf("  Base URL: %s", base_url))
}


# ============================================================================
# CORE API FUNCTION
# ============================================================================

#' Make API request (internal function)
#'
#' @param endpoint Character. API endpoint path
#' @param params List. Query parameters
#' @return Parsed JSON response
api_request <- function(endpoint, params = list()) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  if (is.null(base_url) || is.null(api_token)) {
    stop("API not configured. Please run setup_api() first.")
  }
  
  # Remove NULL values from params
  params <- compact(params)
  
  # Make request
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(!!!params, .multi = "comma") |>
    req_error(is_error = \(resp) FALSE) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  # Check status
  if (resp_status(resp) >= 400) {
    error_body <- resp_body_string(resp)
    stop(glue(
      "API request failed:\n",
      "Status: {resp_status(resp)} {resp_status_desc(resp)}\n",
      "Endpoint: {endpoint}\n",
      "Response: {error_body}"
    ))
  }
  
  # Parse and return JSON
  resp_body_json(resp)
}


# ============================================================================
# DATA RETRIEVAL FUNCTIONS
# ============================================================================

#' Get all available vessels (journals)
#'
#' Returns a list of all vessels your API token has access to.
#' Each vessel is identified by a journal_id.
#'
#' @return Tibble with vessel information
#' @export
#' @examples
#' vessels <- get_vessels()
#' print(vessels)
get_vessels <- function() {
  
  message("Fetching vessels...")
  
  data <- api_request("journals")
  
  if (length(data) == 0) {
    warning("⚠ No vessels found. Your API token may not have access to any vessels.")
    return(tibble(
      journal_id = character(),
      vessel_name = character(),
      flag_state = character()
    ))
  }
  
  vessels <- map_dfr(data, function(j) {
    tibble(
      journal_id = j$journal_id %||% NA_character_,
      vessel_name = j$vessel$name %||% NA_character_,
      flag_state = j$vessel$flag_state %||% NA_character_,
      cfr = j$vessel$cfr %||% NA_character_,
      hull_number = j$vessel$hull_number %||% NA_character_
    )
  })
  
  message(sprintf("✓ Found %d vessel(s)", nrow(vessels)))
  
  return(vessels)
}


#' Get equipment for a vessel
#'
#' Returns all equipment installed on a specific vessel.
#' Each equipment item (e.g., GPS, Engine sensors) can have multiple devices.
#'
#' @param journal_id Character. Vessel ID from get_vessels()
#' @param include_devices Logical. If TRUE, returns one row per device (default FALSE)
#' @return Tibble with equipment information
#' @export
#' @examples
#' equipment <- get_equipment(journal_id)
#' equipment_detailed <- get_equipment(journal_id, include_devices = TRUE)
get_equipment <- function(journal_id, include_devices = FALSE) {
  
  message(sprintf("Fetching equipment for vessel %s...", journal_id))
  
  endpoint <- glue("journals/{journal_id}/equipment_inventory")
  data <- api_request(endpoint)
  
  equipment_list <- data$equipment
  
  if (is.null(equipment_list) || length(equipment_list) == 0) {
    warning("⚠ No equipment found for this vessel")
    return(tibble(
      equipment_id = character(),
      equipment_name = character(),
      equipment_type = character()
    ))
  }
  
  if (!include_devices) {
    # Summary: One row per equipment
    result <- map_dfr(equipment_list, function(equip) {
      tibble(
        equipment_id = equip$equipment_id %||% NA_character_,
        equipment_name = equip$name %||% NA_character_,
        equipment_type = equip$type %||% NA_character_,
        num_devices = length(equip$devices %||% list())
      )
    })
    
    message(sprintf("✓ Found %d equipment item(s)", nrow(result)))
    
  } else {
    # Detailed: One row per device
    result <- map_dfr(equipment_list, function(equip) {
      
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
    
    message(sprintf("✓ Found %d equipment item(s) with %d total device(s)", 
                    n_distinct(result$equipment_id), 
                    nrow(result)))
  }
  
  return(result)
}


#' Get measurements for specific equipment
#'
#' Retrieves measurement data (GPS positions, sensor readings, etc.)
#' for a specific equipment item within a time range.
#'
#' @param journal_id Character. Vessel ID
#' @param equipment_id Character. Equipment ID from get_equipment()
#' @param start_time POSIXct or Character. Start time (e.g., "2025-01-01 00:00:00")
#' @param stop_time POSIXct or Character. Stop time (e.g., "2025-01-02 00:00:00")
#' @return Tibble with measurements
#' @export
#' @examples
#' measurements <- get_measurements(
#'   journal_id, 
#'   equipment_id,
#'   start_time = "2025-01-01 00:00:00",
#'   stop_time = "2025-01-02 00:00:00"
#' )
#' 

# journal_id = "21eacd08-fcd3-4665-9dba-9c23c325bfa3"
# equipment_id = "6a737577-6670-3277-0000-000100000001" 
# start_time="2025-07-01 00:00:00" 
# stop_time="2025-07-02 00:00:00"

get_measurements <- function(journal_id, equipment_id, start_time, stop_time) {
  
  # Convert to POSIXct if character
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(stop_time)) stop_time <- as.POSIXct(stop_time)
  
  # Convert to Unix timestamps
  start_unix <- as.integer(as.numeric(start_time))
  stop_unix <- as.integer(as.numeric(stop_time))
  
  message(sprintf("Fetching measurements..."))
  message(sprintf("  Equipment: %s", equipment_id))
  message(sprintf("  From: %s", format(start_time, "%Y-%m-%d %H:%M:%S")))
  message(sprintf("  To:   %s", format(stop_time, "%Y-%m-%d %H:%M:%S")))
  
  endpoint <- glue("journals/{journal_id}/measurements/{equipment_id}")
  params <- list(start = start_unix, stop = stop_unix)
  
  data <- api_request(endpoint, params)
  
  if (length(data) == 0) {
    warning("⚠ No measurements found for this time range")
    return(tibble())
  }
  
  # Convert to tibble
  result <- tryCatch({
    map_dfr(data, function(m) {
      row <- tibble(timestamp = m$timestamp %||% NA_real_)
      
      if (!is.na(row$timestamp)) {
        row$datetime <- as_datetime(row$timestamp)
      }
      
      other_fields <- m[names(m) != "timestamp"]
      if (length(other_fields) > 0) {
        other_fields <- map(other_fields, function(x) {
          if (is.list(x) && length(x) > 0) {
            toString(x)
          } else {
            x
          }
        })
        row <- bind_cols(row, as_tibble(other_fields))
      }
      
      row
    })
  }, error = function(e) {
    warning(sprintf("Could not parse measurements: %s", e$message))
    return(tibble())
  })
  
  if (nrow(result) > 0) {
    message(sprintf("✓ Found %d measurement(s)", nrow(result)))
  }
  
  return(result)
}


# ============================================================================
# DATA AVAILABILITY DIAGNOSTICS
# ============================================================================

#' Check data availability for a vessel
#'
#' This function checks what data is actually available in the database.
#' It's useful for diagnosing issues and sharing with data providers.
#'
#' @param journal_id Character. Vessel ID (if NULL, checks all vessels)
#' @param test_time_ranges Logical. Test multiple time ranges for measurements? (default TRUE)
#' @return List with diagnostic information
#' @export
#' @examples
#' # Check specific vessel
#' diagnostics <- check_data_availability(journal_id)
#' 
#' # Check all vessels
#' diagnostics_all <- check_data_availability()
check_data_availability <- function(journal_id = NULL, test_time_ranges = TRUE) {
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    DATA AVAILABILITY DIAGNOSTIC REPORT\n")
  cat("================================================================================\n")
  cat(sprintf("Report generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  cat(sprintf("API Base URL: %s\n", getOption("dbmatic_base_url")))
  cat("\n")
  
  # Get vessels
  cat("STEP 1: CHECKING VESSELS\n")
  cat("--------------------------------------------------------------------------------\n")
  
  vessels <- get_vessels()
  
  if (nrow(vessels) == 0) {
    cat("\n❌ ISSUE: No vessels found\n")
    cat("   ACTION NEEDED: Verify API token has access to vessels\n\n")
    return(invisible(list(vessels = vessels, issues = "No vessels found")))
  }
  
  cat(sprintf("✓ Found %d vessel(s):\n", nrow(vessels)))
  for (i in seq_len(nrow(vessels))) {
    cat(sprintf("  - %s (ID: %s)\n", vessels$vessel_name[i], vessels$journal_id[i]))
  }
  cat("\n")
  
  # Determine which vessels to check
  if (is.null(journal_id)) {
    vessels_to_check <- vessels$journal_id
  } else {
    vessels_to_check <- journal_id
  }
  
  # Check each vessel
  all_results <- list()
  
  for (j_id in vessels_to_check) {
    vessel_name <- vessels$vessel_name[vessels$journal_id == j_id]
    if (length(vessel_name) == 0) vessel_name <- "Unknown"
    
    cat("================================================================================\n")
    cat(sprintf("VESSEL: %s\n", vessel_name))
    cat(sprintf("ID: %s\n", j_id))
    cat("================================================================================\n\n")
    
    # Get equipment
    cat("STEP 2: CHECKING EQUIPMENT\n")
    cat("--------------------------------------------------------------------------------\n")
    
    equipment <- get_equipment(j_id, include_devices = TRUE)
    
    if (nrow(equipment) == 0) {
      cat("\n❌ ISSUE: No equipment found for this vessel\n")
      cat("   ACTION NEEDED: Configure equipment for this vessel\n\n")
      
      all_results[[j_id]] <- list(
        vessel_id = j_id,
        vessel_name = vessel_name,
        equipment_count = 0,
        issues = "No equipment configured"
      )
      next
    }
    
    cat(sprintf("✓ Found %d equipment item(s) with %d device(s):\n\n", 
                n_distinct(equipment$equipment_id),
                nrow(equipment)))
    
    print(equipment |> select(equipment_name, equipment_type, device_name, complete_up_to))
    cat("\n")
    
    # Check measurements if requested
    if (test_time_ranges) {
      cat("STEP 3: CHECKING MEASUREMENT DATA\n")
      cat("--------------------------------------------------------------------------------\n")
      
      measurement_results <- list()
      
      for (i in seq_len(nrow(equipment))) {
        eq_id <- equipment$equipment_id[i]
        eq_name <- equipment$equipment_name[i]
        complete_up_to <- equipment$complete_up_to[i]
        
        cat(sprintf("\nEquipment: %s (ID: %s)\n", eq_name, eq_id))
        cat(sprintf("  complete_up_to: %s\n", 
                    if (!is.na(complete_up_to)) format(complete_up_to, "%Y-%m-%d %H:%M:%S") else "Not set"))
        
        # Test different time ranges
        test_ranges <- list(
          "Last 24 hours" = list(
            start = Sys.time() - (1 * 24 * 60 * 60),
            stop = Sys.time()
          ),
          "Last 7 days" = list(
            start = Sys.time() - (7 * 24 * 60 * 60),
            stop = Sys.time()
          ),
          "Last 30 days" = list(
            start = Sys.time() - (30 * 24 * 60 * 60),
            stop = Sys.time()
          ),
          "Last 365 days" = list(
            start = Sys.time() - (365 * 24 * 60 * 60),
            stop = Sys.time()
          )
        )
        
        # If complete_up_to is set, test around that date
        if (!is.na(complete_up_to)) {
          test_ranges[["Around complete_up_to"]] <- list(
            start = complete_up_to - (7 * 24 * 60 * 60),
            stop = complete_up_to
          )
        }
        
        found_data <- FALSE
        
        for (range_name in names(test_ranges)) {
          start <- test_ranges[[range_name]]$start
          stop <- test_ranges[[range_name]]$stop
          
          suppressMessages({
            suppressWarnings({
              measurements <- tryCatch({
                get_measurements(j_id, eq_id, start, stop)
              }, error = function(e) {
                tibble()
              })
            })
          })
          
          if (nrow(measurements) > 0) {
            found_data <- TRUE
            cat(sprintf("  ✓ %s: %d measurements\n", range_name, nrow(measurements)))
            
            # Show date range of actual data
            if ("datetime" %in% names(measurements)) {
              cat(sprintf("    First: %s\n", format(min(measurements$datetime), "%Y-%m-%d %H:%M:%S")))
              cat(sprintf("    Last:  %s\n", format(max(measurements$datetime), "%Y-%m-%d %H:%M:%S")))
            }
            break  # Found data, no need to test more ranges
          }
        }
        
        if (!found_data) {
          cat("  ❌ No measurements found in any time range tested\n")
          cat("     Tested ranges: Last 24h, 7d, 30d, 365d")
          if (!is.na(complete_up_to)) {
            cat(", around complete_up_to date")
          }
          cat("\n")
        }
        
        measurement_results[[eq_id]] <- list(
          equipment_name = eq_name,
          has_data = found_data
        )
      }
      
      # Summary
      cat("\n")
      cat("MEASUREMENT DATA SUMMARY\n")
      cat("--------------------------------------------------------------------------------\n")
      
      equipment_with_data <- sum(map_lgl(measurement_results, ~ .x$has_data))
      total_equipment <- length(measurement_results)
      
      if (equipment_with_data == 0) {
        cat("❌ ISSUE: No measurement data found for any equipment\n")
        cat("   ACTION NEEDED: \n")
        cat("   1. Verify that measurement data has been uploaded to the API\n")
        cat("   2. Check if 'complete_up_to' dates are accurate\n")
        cat("   3. Confirm data is being sent to the correct equipment IDs\n")
        cat("   4. Check if data exists in a different time range than tested\n")
      } else {
        cat(sprintf("✓ %d/%d equipment item(s) have measurement data\n", 
                    equipment_with_data, total_equipment))
        
        if (equipment_with_data < total_equipment) {
          cat(sprintf("\n⚠ WARNING: %d equipment item(s) have no measurement data\n",
                      total_equipment - equipment_with_data))
          
          missing_data <- measurement_results[!map_lgl(measurement_results, ~ .x$has_data)]
          cat("   Equipment without data:\n")
          for (eq in missing_data) {
            cat(sprintf("   - %s\n", eq$equipment_name))
          }
        }
      }
      
      all_results[[j_id]] <- list(
        vessel_id = j_id,
        vessel_name = vessel_name,
        equipment_count = nrow(equipment),
        equipment_with_data = equipment_with_data,
        measurement_results = measurement_results
      )
    }
    
    cat("\n")
  }
  
  cat("================================================================================\n")
  cat("                         END OF DIAGNOSTIC REPORT\n")
  cat("================================================================================\n")
  cat("\n")
  cat("RECOMMENDATIONS:\n")
  cat("--------------------------------------------------------------------------------\n")
  
  # Generate recommendations based on findings
  total_vessels <- length(all_results)
  vessels_with_data <- sum(map_lgl(all_results, function(r) {
    if (is.null(r$equipment_with_data)) return(FALSE)
    r$equipment_with_data > 0
  }))
  
  if (vessels_with_data == 0) {
    cat("⚠ CRITICAL: No measurement data found for any vessel\n")
    cat("\n")
    cat("Please verify with your data provider:\n")
    cat("1. Has measurement data been uploaded to the test database?\n")
    cat("2. Are the equipment IDs correct?\n")
    cat("3. What time range should contain data?\n")
    cat("4. Is the data being sent to the correct API endpoint?\n")
  } else if (vessels_with_data < total_vessels) {
    cat("⚠ Some vessels have data, others don't\n")
    cat("Please check equipment configuration for vessels without data\n")
  } else {
    cat("✓ All vessels have measurement data\n")
    cat("You can proceed with data analysis\n")
  }
  
  cat("\n")
  cat("Share this report with your data provider if you need assistance.\n")
  cat("================================================================================\n\n")
  
  return(invisible(all_results))
}


# ============================================================================
# QUICK REFERENCE GUIDE
# ============================================================================

#' Show usage examples
#'
#' Prints example code for using these functions
#'
#' @export
show_examples <- function() {
  cat("\n")
  cat("================================================================================\n")
  cat("                           QUICK REFERENCE GUIDE\n")
  cat("================================================================================\n\n")
  
  cat("1. SETUP (run once)\n")
  cat("   -------------------------------\n")
  cat('   setup_api(\n')
  cat('     base_url = "https://api.dbmatic.eu/api/v1",\n')
  cat('     api_token = "your-token-here"\n')
  cat('   )\n\n')
  
  cat("2. CHECK DATA AVAILABILITY\n")
  cat("   -------------------------------\n")
  cat('   # Check all vessels\n')
  cat('   check_data_availability()\n\n')
  cat('   # Check specific vessel\n')
  cat('   check_data_availability(journal_id = "abc-123")\n\n')
  
  cat("3. GET VESSELS\n")
  cat("   -------------------------------\n")
  cat('   vessels <- get_vessels()\n')
  cat('   print(vessels)\n\n')
  
  cat("4. GET EQUIPMENT\n")
  cat("   -------------------------------\n")
  cat('   journal_id <- vessels$journal_id[1]\n')
  cat('   equipment <- get_equipment(journal_id)\n')
  cat('   equipment_detailed <- get_equipment(journal_id, include_devices = TRUE)\n\n')
  
  cat("5. GET MEASUREMENTS\n")
  cat("   -------------------------------\n")
  cat('   equipment_id <- equipment$equipment_id[1]\n')
  cat('   measurements <- get_measurements(\n')
  cat('     journal_id,\n')
  cat('     equipment_id,\n')
  cat('     start_time = "2025-01-01 00:00:00",\n')
  cat('     stop_time = "2025-01-02 00:00:00"\n')
  cat('   )\n\n')
  
  cat("================================================================================\n\n")
}





# Step 1: Setup
setup_api(
  base_url = "https://api.dbmatic.eu/api/v1",
  api_token = poseidat_token
)

# Step 2: Run comprehensive diagnostic
# This will generate a detailed report you can share
diagnostics <- check_data_availability()

# Or check just one vessel
diagnostics <- check_data_availability(journal_id = "21eacd08-fcd3-4665-9dba-9c23c325bfa3")

# Step 3: If you want to see example code
show_examples()






# 1. get journals

endpoint <- glue("journals")
journal_id = api_request(endpoint)[[1]]$journal_id
cat(paste("Journal_id:", journal_id))

# 2. get equipment
endpoint <- glue("journals/{journal_id}/equipment_inventory")
data <- api_request(endpoint)
equipment_id <- data$equipment[[1]]$equipment_id
device_id <- data$equipment[[1]]$devices[[1]]$device_id

cat(paste("Equipment_id:", equipment_id))
cat(paste("Device_id:", device_id))

#3 get devices
endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
start_time = as.POSIXct("2026-01-01 00:00:00") 
stop_time  = as.POSIXct("2026-01-02 00:00:00")  
start_unix <- as.integer(as.numeric(start_time))
stop_unix  <- as.integer(as.numeric(stop_time))
params <- list(start = start_unix, stop = stop_unix)

resp <- 
  request(glue("{base_url}/{endpoint}")) |>
  req_auth_bearer_token(api_token) |>
  req_url_query(!!!params, .multi = "comma") |>
  req_error(is_error = \(resp) FALSE) |>
  req_retry(max_tries = 3) |>
  req_perform()

print(resp)

resp_body_json(resp)




# 15:42
#' Test both equipment_id and device_id with multiple time ranges
#'
#' @param journal_id Character. Journal ID
#' @param equipment_id Character. Equipment ID
#' @param device_id Character. Device ID
test_measurement_endpoints <- function(journal_id, equipment_id, device_id) {
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    TESTING MEASUREMENT ENDPOINTS\n")
  cat("================================================================================\n\n")
  
  cat("Journal ID:   ", journal_id, "\n")
  cat("Equipment ID: ", equipment_id, "\n")
  cat("Device ID:    ", device_id, "\n\n")
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  # Define test time ranges (including past dates)
  time_ranges <- list(
    "Past week (Dec 29 - Jan 5)" = list(
      start = as.POSIXct("2025-12-29 00:00:00"),
      stop = as.POSIXct("2026-01-05 23:59:59")
    ),
    "Early 2025 (Jan 1-7)" = list(
      start = as.POSIXct("2025-01-01 00:00:00"),
      stop = as.POSIXct("2025-01-07 23:59:59")
    ),
    "Dec 2024" = list(
      start = as.POSIXct("2024-12-01 00:00:00"),
      stop = as.POSIXct("2024-12-31 23:59:59")
    ),
    "Nov 2024" = list(
      start = as.POSIXct("2024-11-01 00:00:00"),
      stop = as.POSIXct("2024-11-30 23:59:59")
    ),
    "July 2024 (not 2025!)" = list(
      start = as.POSIXct("2024-07-01 00:00:00"),
      stop = as.POSIXct("2024-07-31 23:59:59")
    ),
    "All of 2024" = list(
      start = as.POSIXct("2024-01-01 00:00:00"),
      stop = as.POSIXct("2024-12-31 23:59:59")
    ),
    "All of 2025 so far" = list(
      start = as.POSIXct("2025-01-01 00:00:00"),
      stop = Sys.time()
    )
  )
  
  # Test both equipment_id and device_id
  endpoint_patterns <- list(
    "equipment_id" = glue("journals/{journal_id}/measurements/{equipment_id}"),
    "device_id" = glue("journals/{journal_id}/measurements/{device_id}")
  )
  
  results <- list()
  
  for (endpoint_type in names(endpoint_patterns)) {
    endpoint <- endpoint_patterns[[endpoint_type]]
    
    cat("================================================================================\n")
    cat(sprintf("TESTING WITH: %s\n", toupper(endpoint_type)))
    cat(sprintf("Endpoint: %s\n", endpoint))
    cat("================================================================================\n\n")
    
    for (range_name in names(time_ranges)) {
      start_time <- time_ranges[[range_name]]$start
      stop_time <- time_ranges[[range_name]]$stop
      
      start_unix <- as.integer(as.numeric(start_time))
      stop_unix <- as.integer(as.numeric(stop_time))
      
      cat(sprintf("Testing: %s\n", range_name))
      cat(sprintf("  From: %s (Unix: %d)\n", 
                  format(start_time, "%Y-%m-%d %H:%M:%S"), start_unix))
      cat(sprintf("  To:   %s (Unix: %d)\n", 
                  format(stop_time, "%Y-%m-%d %H:%M:%S"), stop_unix))
      
      tryCatch({
        # Make request
        resp <- request(glue("{base_url}/{endpoint}")) |>
          req_auth_bearer_token(api_token) |>
          req_url_query(start = start_unix, stop = stop_unix) |>
          req_error(is_error = \(resp) FALSE) |>
          req_perform()
        
        status <- resp_status(resp)
        body_size <- length(resp_body_raw(resp))
        
        cat(sprintf("  Status: %d %s\n", status, resp_status_desc(resp)))
        cat(sprintf("  Body size: %d bytes\n", body_size))
        
        if (status == 200) {
          data <- resp_body_json(resp)
          count <- length(data)
          
          cat(sprintf("  Result: %d measurements\n", count))
          
          if (count > 0) {
            cat("  ✓✓✓ DATA FOUND! ✓✓✓\n")
            
            # Extract timestamps
            timestamps <- map_dbl(data, ~ .x$timestamp %||% NA_real_)
            timestamps <- timestamps[!is.na(timestamps)]
            
            if (length(timestamps) > 0) {
              first_dt <- as_datetime(min(timestamps))
              last_dt <- as_datetime(max(timestamps))
              
              cat(sprintf("  First measurement: %s\n", format(first_dt, "%Y-%m-%d %H:%M:%S")))
              cat(sprintf("  Last measurement:  %s\n", format(last_dt, "%Y-%m-%d %H:%M:%S")))
              cat(sprintf("  Time span: %.2f days\n", 
                          as.numeric(difftime(last_dt, first_dt, units = "days"))))
              
              cat("\n  Sample measurement structure:\n")
              cat("  Fields: ", paste(names(data[[1]]), collapse = ", "), "\n")
              
              cat("\n  First measurement:\n")
              print(str(data[[1]], max.level = 2))
            }
            
            # Store successful result
            results[[paste(endpoint_type, range_name, sep = " - ")]] <- list(
              endpoint_type = endpoint_type,
              range = range_name,
              count = count,
              data = data
            )
            
            cat("\n  *** THIS COMBINATION WORKS! ***\n")
          } else {
            cat("  - Empty\n")
          }
        }
        
      }, error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
      })
      
      cat("\n")
    }
  }
  
  cat("================================================================================\n")
  cat("                              SUMMARY\n")
  cat("================================================================================\n\n")
  
  if (length(results) > 0) {
    cat("✓ FOUND DATA in the following combinations:\n\n")
    
    for (result_name in names(results)) {
      result <- results[[result_name]]
      cat(sprintf("  - %s: %d measurements\n", 
                  result_name, result$count))
    }
    
    cat("\n")
    cat("RECOMMENDATION:\n")
    cat("Use the endpoint and time range combination that returned data.\n")
    
    # Show example code
    best_result <- results[[1]]
    cat("\nExample code:\n")
    cat("```r\n")
    cat(sprintf('measurements <- get_measurements(\n'))
    cat(sprintf('  journal_id = "%s",\n', journal_id))
    
    if (best_result$endpoint_type == "equipment_id") {
      cat(sprintf('  equipment_id = "%s",  # Use equipment_id\n', equipment_id))
    } else {
      cat(sprintf('  device_id = "%s",     # Use device_id (NOT equipment_id!)\n', device_id))
    }
    
    cat(sprintf('  start_time = "%s",\n', format(time_ranges[[best_result$range]]$start, "%Y-%m-%d %H:%M:%S")))
    cat(sprintf('  stop_time = "%s"\n', format(time_ranges[[best_result$range]]$stop, "%Y-%m-%d %H:%M:%S")))
    cat(')\n```\n')
    
  } else {
    cat("❌ NO DATA FOUND in any combination tested\n\n")
    cat("Tested:\n")
    cat("  - Both equipment_id and device_id endpoints\n")
    cat("  - Multiple time ranges from July 2024 to present\n\n")
    cat("ACTION NEEDED:\n")
    cat("  1. Ask data provider what time range has data\n")
    cat("  2. Ask data provider which ID to use (equipment_id or device_id)\n")
    cat("  3. Verify this is the correct journal_id with GPS data\n")
  }
  
  cat("\n================================================================================\n\n")
  
  return(invisible(results))
}


# Your current IDs
journal_id <- "21eacd08-fcd3-4665-9dba-9c23c325bfa3"
equipment_id <- "6a737577-6670-3277-0000-000100000001"
device_id <- "6a737577-6670-3277-0000-000200000001"

# Run comprehensive test
results <- test_measurement_endpoints(journal_id, equipment_id, device_id)

# If data was found, access it
if (length(results) > 0) {
  # Get the first successful result
  first_result <- results[[1]]
  cat("\nData sample:\n")
  print(head(first_result$data))
}


# 15:55

#' Inspect raw API response in detail
#'
#' @param journal_id Character. Journal ID
#' @param equipment_id Character. Equipment ID  
#' @param device_id Character. Device ID
#' @param start_time POSIXct. Start time
#' @param stop_time POSIXct. Stop time
inspect_raw_response <- function(journal_id, equipment_id, device_id, 
                                 start_time, stop_time) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  start_unix <- as.integer(as.numeric(start_time))
  stop_unix <- as.integer(as.numeric(stop_time))
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    RAW RESPONSE INSPECTION\n")
  cat("================================================================================\n\n")
  
  cat("Request parameters:\n")
  cat(sprintf("  Journal ID: %s\n", journal_id))
  cat(sprintf("  Equipment ID: %s\n", equipment_id))
  cat(sprintf("  Device ID: %s\n", device_id))
  cat(sprintf("  Start: %s (Unix: %d)\n", format(start_time, "%Y-%m-%d %H:%M:%S"), start_unix))
  cat(sprintf("  Stop: %s (Unix: %d)\n", format(stop_time, "%Y-%m-%d %H:%M:%S"), stop_unix))
  cat("\n")
  
  # Test both equipment_id and device_id
  test_ids <- list(
    "equipment_id" = equipment_id,
    "device_id" = device_id
  )
  
  for (id_type in names(test_ids)) {
    id_value <- test_ids[[id_type]]
    
    cat("================================================================================\n")
    cat(sprintf("TESTING: %s = %s\n", toupper(id_type), id_value))
    cat("================================================================================\n\n")
    
    endpoint <- glue("journals/{journal_id}/measurements/{id_value}")
    full_url <- glue("{base_url}/{endpoint}")
    
    cat("Full URL:\n")
    cat(sprintf("  %s?start=%d&stop=%d\n\n", full_url, start_unix, stop_unix))
    
    # Make the request
    resp <- request(full_url) |>
      req_auth_bearer_token(api_token) |>
      req_url_query(start = start_unix, stop = stop_unix) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    # 1. Basic response info
    cat("1. RESPONSE STATUS:\n")
    cat("   ----------------\n")
    cat(sprintf("   Status code: %d\n", resp_status(resp)))
    cat(sprintf("   Status description: %s\n", resp_status_desc(resp)))
    cat("\n")
    
    # 2. Headers
    cat("2. RESPONSE HEADERS:\n")
    cat("   ----------------\n")
    headers <- resp_headers(resp)
    for (header_name in names(headers)) {
      cat(sprintf("   %s: %s\n", header_name, headers[[header_name]]))
    }
    cat("\n")
    
    # 3. Content type
    cat("3. CONTENT TYPE:\n")
    cat("   ----------------\n")
    cat(sprintf("   %s\n\n", resp_content_type(resp)))
    
    # 4. Body size
    cat("4. BODY SIZE:\n")
    cat("   ----------------\n")
    raw_body <- resp_body_raw(resp)
    cat(sprintf("   %d bytes\n\n", length(raw_body)))
    
    # 5. Raw bytes (hex)
    cat("5. RAW BODY (HEX):\n")
    cat("   ----------------\n")
    if (length(raw_body) > 0) {
      # Show first 100 bytes in hex
      bytes_to_show <- min(100, length(raw_body))
      hex_values <- sprintf("%02x", as.integer(raw_body[1:bytes_to_show]))
      
      # Print in groups of 16
      for (i in seq(1, bytes_to_show, by = 16)) {
        end_idx <- min(i + 15, bytes_to_show)
        cat(sprintf("   %s\n", paste(hex_values[i:end_idx], collapse = " ")))
      }
      
      if (length(raw_body) > 100) {
        cat(sprintf("   ... (%d more bytes)\n", length(raw_body) - 100))
      }
    } else {
      cat("   (empty)\n")
    }
    cat("\n")
    
    # 6. Raw body as string
    cat("6. RAW BODY (STRING):\n")
    cat("   ----------------\n")
    body_string <- resp_body_string(resp)
    cat(sprintf("   Length: %d characters\n", nchar(body_string)))
    cat(sprintf("   Content: %s\n", body_string))
    cat("\n")
    
    # 7. ASCII representation
    cat("7. RAW BODY (ASCII/CHARACTERS):\n")
    cat("   ----------------\n")
    if (length(raw_body) > 0) {
      # Show ASCII characters
      ascii_chars <- rawToChar(raw_body)
      cat(sprintf("   '%s'\n", ascii_chars))
      
      # Show individual bytes as characters
      cat("\n   Byte-by-byte:\n")
      for (i in seq_len(min(50, length(raw_body)))) {
        byte_val <- as.integer(raw_body[i])
        char_repr <- if (byte_val >= 32 && byte_val <= 126) {
          rawToChar(raw_body[i])
        } else {
          sprintf("\\x%02x", byte_val)
        }
        cat(sprintf("   [%d] = %3d = '%s'\n", i, byte_val, char_repr))
      }
      
      if (length(raw_body) > 50) {
        cat(sprintf("   ... (%d more bytes)\n", length(raw_body) - 50))
      }
    } else {
      cat("   (empty)\n")
    }
    cat("\n")
    
    # 8. Try to parse as JSON
    cat("8. JSON PARSING:\n")
    cat("   ----------------\n")
    tryCatch({
      parsed <- resp_body_json(resp)
      cat(sprintf("   Type: %s\n", class(parsed)[1]))
      cat(sprintf("   Length: %d\n", length(parsed)))
      
      if (length(parsed) > 0) {
        cat("\n   ✓✓✓ JSON CONTAINS DATA! ✓✓✓\n")
        cat("\n   Structure:\n")
        print(str(parsed, max.level = 3))
        
        cat("\n   First item:\n")
        print(parsed[[1]])
      } else {
        cat("\n   ⚠ JSON parsed successfully but is EMPTY\n")
        cat("   This means: API returned '[]' (empty array)\n")
      }
      
    }, error = function(e) {
      cat(sprintf("   ✗ JSON parsing failed: %s\n", e$message))
    })
    
    cat("\n")
  }
  
  cat("================================================================================\n")
  cat("                            END INSPECTION\n")
  cat("================================================================================\n\n")
}

# Your current values
journal_id <- "21eacd08-fcd3-4665-9dba-9c23c325bfa3"
equipment_id <- "6a737577-6670-3277-0000-000100000001"
device_id <- "6a737577-6670-3277-0000-000200000001"

# Try different time ranges
cat("TEST 1: July 2024 (not 2025!)\n")
inspect_raw_response(
  journal_id, equipment_id, device_id,
  start_time = as.POSIXct("2024-07-01 00:00:00"),
  stop_time = as.POSIXct("2024-07-31 23:59:59")
)

cat("\n\nTEST 2: December 2024\n")
inspect_raw_response(
  journal_id, equipment_id, device_id,
  start_time = as.POSIXct("2024-12-01 00:00:00"),
  stop_time = as.POSIXct("2024-12-31 23:59:59")
)

cat("\n\nTEST 3: All of 2024\n")
inspect_raw_response(
  journal_id, equipment_id, device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  stop_time = as.POSIXct("2024-12-31 23:59:59")
)

cat("\n\nTEST 4: All of 2025\n")
inspect_raw_response(
  journal_id, equipment_id, device_id,
  start_time = as.POSIXct("2025-01-01 00:00:00"),
  stop_time = as.POSIXct("2025-12-31 23:59:59")
)

cat("\n\nTEST 4: before 2024\n")
inspect_raw_response(
  journal_id, equipment_id, device_id,
  start_time = as.POSIXct("2020-01-01 00:00:00"),
  stop_time = as.POSIXct("2023-12-31 23:59:59")
)



# 16:28 na input van Frans Wakker

#' Test een specifiek endpoint pad (gegeven door data provider)
#'
#' @param journal_id Character. Journal ID
#' @param equipment_id Character. Equipment ID
#' @param device_id Character. Device ID
#' @param endpoint_template Character. Endpoint template van data provider
#' @param start_time POSIXct. Start tijd
#' @param stop_time POSIXct. Stop tijd
#' @export
test_specific_endpoint <- function(journal_id, equipment_id, device_id,
                                   endpoint_template,
                                   start_time, stop_time) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  start_unix <- as.integer(as.numeric(start_time))
  stop_unix <- as.integer(as.numeric(stop_time))
  
  cat("\n")
  cat("================================================================================\n")
  cat("              TESTING SPECIFIC ENDPOINT FROM DATA PROVIDER\n")
  cat("================================================================================\n\n")
  
  cat("Input parameters:\n")
  cat(sprintf("  journal_id:   %s\n", journal_id))
  cat(sprintf("  equipment_id: %s\n", equipment_id))
  cat(sprintf("  device_id:    %s\n", device_id))
  cat(sprintf("  Start: %s (Unix: %d)\n", format(start_time, "%Y-%m-%d %H:%M:%S"), start_unix))
  cat(sprintf("  Stop:  %s (Unix: %d)\n\n", format(stop_time, "%Y-%m-%d %H:%M:%S"), stop_unix))
  
  # Vervang placeholders in het endpoint template
  endpoint <- endpoint_template
  endpoint <- gsub("\\{journal_id\\}", journal_id, endpoint)
  endpoint <- gsub("\\{equipment_id\\}", equipment_id, endpoint)
  endpoint <- gsub("\\{device_id\\}", device_id, endpoint)
  
  cat("Endpoint template: ", endpoint_template, "\n")
  cat("Resolved endpoint: ", endpoint, "\n\n")
  
  full_url <- glue("{base_url}/{endpoint}")
  
  cat("Full URL:\n")
  cat(sprintf("  %s?start=%d&stop=%d\n\n", full_url, start_unix, stop_unix))
  
  # Maak request
  resp <- request(full_url) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, stop = stop_unix) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  # Inspecteer response
  status <- resp_status(resp)
  cat("Response:\n")
  cat(sprintf("  Status: %d %s\n", status, resp_status_desc(resp)))
  
  if (status == 200) {
    body_string <- resp_body_string(resp)
    body_size <- nchar(body_string)
    
    cat(sprintf("  Body size: %d characters\n", body_size))
    cat(sprintf("  Body: %s\n", 
                if (body_size > 200) paste0(substr(body_string, 1, 200), "...") else body_string))
    
    data <- resp_body_json(resp)
    count <- length(data)
    
    cat(sprintf("  Parsed: %d items\n\n", count))
    
    if (count > 0) {
      cat("✓✓✓ DATA GEVONDEN! ✓✓✓\n\n")
      
      # Toon details
      timestamps <- map_dbl(data, ~ .x$timestamp %||% NA_real_)
      timestamps <- timestamps[!is.na(timestamps)]
      
      if (length(timestamps) > 0) {
        first_dt <- as_datetime(min(timestamps))
        last_dt <- as_datetime(max(timestamps))
        
        cat(sprintf("Eerste meting: %s\n", format(first_dt, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Laatste meting: %s\n", format(last_dt, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Aantal metingen: %d\n", count))
        cat(sprintf("Tijdspanne: %.2f dagen\n\n", 
                    as.numeric(difftime(last_dt, first_dt, units = "days"))))
        
        cat("Structuur eerste meting:\n")
        print(str(data[[1]], max.level = 2))
        
        cat("\n\nVoorbeeld (eerste 3 metingen):\n")
        for (i in seq_len(min(3, count))) {
          cat(sprintf("\nMeting %d:\n", i))
          print(data[[i]])
        }
      }
      
      return(invisible(data))
      
    } else {
      cat("❌ Endpoint werkt maar geen data gevonden\n")
      cat("   De API geeft een lege array [] terug\n\n")
      return(invisible(NULL))
    }
    
  } else if (status == 404) {
    cat("  ❌ Endpoint bestaat niet (404 Not Found)\n\n")
    return(invisible(NULL))
    
  } else {
    cat(sprintf("  ❌ Onverwachte status: %d\n\n", status))
    error_body <- resp_body_string(resp)
    cat(sprintf("  Error body: %s\n\n", error_body))
    return(invisible(NULL))
  }
  
  cat("================================================================================\n\n")
}

test_specific_endpoint(
  journal_id = "21eacd08-fcd3-4665-9dba-9c23c325bfa3",
  equipment_id = "6a737577-6670-3277-0000-000100000001",
  device_id = "6a737577-6670-3277-0000-000200000001",
  endpoint_template = "journals/{journal_id}/equipment/{equipment_id}/devices/{device_id}/measurements",
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  stop_time = as.POSIXct("2024-12-31 23:59:59")
)

# 16:56
# 
#  What is in the measurements endpoint?
#
#' Exploreer het measurements endpoint zonder filters
#'
#' @param journal_id Character. Journal ID
#' @param device_id Character. Device ID
explore_measurements_endpoint <- function(journal_id, device_id) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  cat("\n")
  cat("================================================================================\n")
  cat("              EXPLORING MEASUREMENTS ENDPOINT\n")
  cat("================================================================================\n\n")
  
  cat("Parameters:\n")
  cat(sprintf("  journal_id: %s\n", journal_id))
  cat(sprintf("  device_id:  %s\n\n", device_id))
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  full_url <- glue("{base_url}/{endpoint}")
  
  # ============================================================================
  # TEST 1: Zonder parameters
  # ============================================================================
  cat("TEST 1: Zonder parameters (geen start/stop)\n")
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("URL: %s\n", full_url))
  
  tryCatch({
    resp <- request(full_url) |>
      req_auth_bearer_token(api_token) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    cat(sprintf("Status: %d %s\n", resp_status(resp), resp_status_desc(resp)))
    
    if (resp_status(resp) == 200) {
      body_string <- resp_body_string(resp)
      cat(sprintf("Body: %s\n", body_string))
      
      data <- resp_body_json(resp)
      cat(sprintf("Items: %d\n", length(data)))
      
      if (length(data) > 0) {
        cat("\n✓✓✓ DATA GEVONDEN ZONDER PARAMETERS! ✓✓✓\n")
        print(str(data, max.level = 2))
        return(invisible(data))
      }
    }
  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })
  
  cat("\n\n")
  
  # ============================================================================
  # TEST 2: Met limit parameter
  # ============================================================================
  cat("TEST 2: Met limit parameter (laatste N metingen)\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (limit in c(1, 10, 100, 1000)) {
    cat(sprintf("Limit=%d: ", limit))
    
    tryCatch({
      resp <- request(full_url) |>
        req_auth_bearer_token(api_token) |>
        req_url_query(limit = limit) |>
        req_error(is_error = \(resp) FALSE) |>
        req_perform()
      
      if (resp_status(resp) == 200) {
        data <- resp_body_json(resp)
        cat(sprintf("%d items\n", length(data)))
        
        if (length(data) > 0) {
          cat("\n✓✓✓ DATA GEVONDEN MET LIMIT PARAMETER! ✓✓✓\n")
          print(str(data[[1]], max.level = 2))
          return(invisible(data))
        }
      } else {
        cat(sprintf("Status %d\n", resp_status(resp)))
      }
    }, error = function(e) {
      cat(sprintf("Error: %s\n", e$message))
    })
  }
  
  cat("\n\n")
  
  # ============================================================================
  # TEST 3: Zeer ruime tijd range (vanaf Unix epoch)
  # ============================================================================
  cat("TEST 3: Vanaf Unix epoch (1970) tot nu\n")
  cat("--------------------------------------------------------------------------------\n")
  
  tryCatch({
    resp <- request(full_url) |>
      req_auth_bearer_token(api_token) |>
      req_url_query(start = 0, stop = as.integer(as.numeric(Sys.time()))) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    cat(sprintf("Status: %d\n", resp_status(resp)))
    
    if (resp_status(resp) == 200) {
      data <- resp_body_json(resp)
      cat(sprintf("Items: %d\n", length(data)))
      
      if (length(data) > 0) {
        cat("\n✓✓✓ DATA GEVONDEN! ✓✓✓\n")
        print(str(data, max.level = 2))
        return(invisible(data))
      }
    }
  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })
  
  cat("\n\n")
  
  # ============================================================================
  # TEST 4: OPTIONS request (welke methoden zijn beschikbaar?)
  # ============================================================================
  cat("TEST 4: OPTIONS request (welke HTTP methoden?)\n")
  cat("--------------------------------------------------------------------------------\n")
  
  tryCatch({
    resp <- request(full_url) |>
      req_method("OPTIONS") |>
      req_auth_bearer_token(api_token) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    cat(sprintf("Status: %d\n", resp_status(resp)))
    
    headers <- resp_headers(resp)
    if ("allow" %in% names(headers)) {
      cat(sprintf("Allowed methods: %s\n", headers$allow))
    }
    
    # Toon alle headers
    cat("\nHeaders:\n")
    for (header_name in names(headers)) {
      cat(sprintf("  %s: %s\n", header_name, headers[[header_name]]))
    }
  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })
  
  cat("\n\n")
  
  # ============================================================================
  # TEST 5: Alternatieve endpoints
  # ============================================================================
  cat("TEST 5: Alternatieve endpoints\n")
  cat("--------------------------------------------------------------------------------\n")
  
  alternative_endpoints <- c(
    glue("journals/{journal_id}/data/{device_id}"),
    glue("journals/{journal_id}/streams/{device_id}"),
    glue("journals/{journal_id}/readings/{device_id}"),
    glue("journals/{journal_id}/values/{device_id}"),
    glue("devices/{device_id}/measurements"),
    glue("devices/{device_id}/data"),
    glue("measurements/{device_id}")
  )
  
  for (alt_endpoint in alternative_endpoints) {
    alt_url <- glue("{base_url}/{alt_endpoint}")
    cat(sprintf("\n%s\n", alt_endpoint))
    
    tryCatch({
      resp <- request(alt_url) |>
        req_auth_bearer_token(api_token) |>
        req_error(is_error = \(resp) FALSE) |>
        req_perform()
      
      status <- resp_status(resp)
      cat(sprintf("  Status: %d", status))
      
      if (status == 200) {
        data <- resp_body_json(resp)
        cat(sprintf(" - %d items", length(data)))
        
        if (length(data) > 0) {
          cat(" ✓✓✓ DATA GEVONDEN!\n")
          print(str(data, max.level = 2))
          return(invisible(data))
        }
      }
      cat("\n")
      
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
    })
  }
  
  cat("\n\n")
  
  # ============================================================================
  # TEST 6: Bekijk journal metadata
  # ============================================================================
  cat("TEST 6: Journal metadata (misschien verwijzingen naar data?)\n")
  cat("--------------------------------------------------------------------------------\n")
  
  tryCatch({
    journal_url <- glue("{base_url}/journals/{journal_id}")
    
    resp <- request(journal_url) |>
      req_auth_bearer_token(api_token) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    cat(sprintf("Status: %d\n", resp_status(resp)))
    
    if (resp_status(resp) == 200) {
      data <- resp_body_json(resp)
      cat("\nJournal metadata:\n")
      print(str(data, max.level = 3))
      
      # Zoek naar data-gerelateerde velden
      if (!is.null(data$data_available)) {
        cat(sprintf("\n⚠ Found 'data_available' field: %s\n", data$data_available))
      }
      if (!is.null(data$measurement_count)) {
        cat(sprintf("\n⚠ Found 'measurement_count' field: %s\n", data$measurement_count))
      }
    }
  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })
  
  cat("\n")
  cat("================================================================================\n")
  cat("                              CONCLUSIE\n")
  cat("================================================================================\n\n")
  
  cat("Alle tests zijn uitgevoerd maar geen data gevonden.\n\n")
  
  cat("Dit betekent:\n")
  cat("  1. De API endpoints werken (200 OK responses)\n")
  cat("  2. Maar de database bevat geen measurement data\n")
  cat("  3. Of de data zit in een ander endpoint dan we getest hebben\n\n")
  
  cat("AANBEVELING:\n")
  cat("Vraag de data eigenaar:\n")
  cat("  1. Is er daadwerkelijk GPS data geüpload naar de test database?\n")
  cat("  2. Zo ja, kun je een SQL query uitvoeren om te bevestigen dat data bestaat?\n")
  cat("  3. Kun je een werkend curl commando geven dat data retourneert?\n")
  cat("  4. Welke datum/tijd range bevat gegarandeerd data?\n\n")
  
  cat("================================================================================\n\n")
  
  return(invisible(NULL))
}


result <- explore_measurements_endpoint(journal_id, device_id)




# 7 januari 8:44
# http://{base-url}/api/v1/journals/{journal_id}/measurements/{device_id}?start={unix_start}&end={unix_end)

#' Test met de CORRECTE parameter naam: 'end' in plaats van 'stop'
#'
#' @param journal_id Character. Journal ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct. Start tijd
#' @param end_time POSIXct. Eind tijd
test_with_end_parameter <- function(journal_id, device_id, start_time, end_time) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  cat("\n")
  cat("================================================================================\n")
  cat("              TESTING WITH CORRECT PARAMETER: 'end' NOT 'stop'\n")
  cat("================================================================================\n\n")
  
  cat("Parameters:\n")
  cat(sprintf("  journal_id: %s\n", journal_id))
  cat(sprintf("  device_id:  %s\n", device_id))
  cat(sprintf("  start: %s (Unix: %d)\n", format(start_time, "%Y-%m-%d %H:%M:%S"), start_unix))
  cat(sprintf("  end:   %s (Unix: %d)\n\n", format(end_time, "%Y-%m-%d %H:%M:%S"), end_unix))
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  full_url <- glue("{base_url}/{endpoint}")
  
  cat("Full URL:\n")
  cat(sprintf("  %s?start=%d&end=%d\n\n", full_url, start_unix, end_unix))
  
  # Maak request met 'end' parameter (NIET 'stop')
  resp <- request(full_url) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>  # 'end' niet 'stop'!
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  cat("Response:\n")
  cat(sprintf("  Status: %d %s\n", resp_status(resp), resp_status_desc(resp)))
  
  if (resp_status(resp) == 200) {
    body_string <- resp_body_string(resp)
    body_size <- nchar(body_string)
    
    cat(sprintf("  Body size: %d characters\n", body_size))
    cat(sprintf("  Body: %s\n\n", 
                if (body_size > 200) paste0(substr(body_string, 1, 200), "...") else body_string))
    
    data <- resp_body_json(resp)
    count <- length(data)
    
    cat(sprintf("  Parsed: %d items\n\n", count))
    
    if (count > 0) {
      cat("✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓\n")
      cat("✓✓✓ PROBLEEM GEVONDEN EN OPGELOST! ✓✓✓\n")
      cat("✓✓✓ De parameter heet 'end' NIET 'stop'! ✓✓✓\n")
      cat("✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓✓\n\n")
      
      # Toon details
      # timestamps <- map_dbl(data, ~ .x$timestamp %||% NA_real_)
      timestamps <- map_dbl(data, ~ .x$revision %||% NA_real_)
      timestamps <- timestamps[!is.na(timestamps)]
      
      if (length(timestamps) > 0) {
        first_dt <- as_datetime(min(timestamps))
        last_dt <- as_datetime(max(timestamps))
        
        cat(sprintf("Eerste meting: %s\n", format(first_dt, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Laatste meting: %s\n", format(last_dt, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Aantal metingen: %d\n", count))
        cat(sprintf("Tijdspanne: %.2f dagen\n\n", 
                    as.numeric(difftime(last_dt, first_dt, units = "days"))))
        
        cat("Structuur eerste meting:\n")
        print(str(data[[1]], max.level = 2))
        
        cat("\n\nVoorbeeld data (eerste 5 metingen):\n")
        print(head(data, 5))
      }
      
      return(invisible(data))
      
    } else {
      cat("❌ Nog steeds geen data - mogelijk is database leeg\n\n")
      return(invisible(NULL))
    }
    
  } else {
    cat(sprintf("  ❌ Onverwachte status: %d\n\n", status))
    return(invisible(NULL))
  }
  
  cat("================================================================================\n\n")
}

# Probeer verschillende time ranges
result <- test_with_end_parameter(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)





# =========================================================================================================
# 7 januari 8:55

#' Inspecteer de structuur van measurements
#'
#' @param journal_id Character. Journal ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct. Start tijd
#' @param end_time POSIXct. Eind tijd
inspect_measurement_structure <- function(journal_id, device_id, start_time, end_time) {
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    MEASUREMENT DATA STRUCTURE\n")
  cat("================================================================================\n\n")
  
  cat(sprintf("Total measurements: %d\n\n", length(data)))
  
  if (length(data) > 0) {
    cat("Fields in first measurement:\n")
    cat("----------------------------\n")
    print(names(data[[1]]))
    
    cat("\n\nFirst measurement (full structure):\n")
    cat("-----------------------------------\n")
    print(str(data[[1]], max.level = 3))
    
    cat("\n\nFirst measurement (values):\n")
    cat("---------------------------\n")
    print(data[[1]])
    
    cat("\n\nFirst 3 measurements:\n")
    cat("---------------------\n")
    print(data[1:min(3, length(data))])
    
    # Check for nested structures
    if ("payload" %in% names(data[[1]])) {
      cat("\n\nPayload structure:\n")
      cat("------------------\n")
      print(str(data[[1]]$payload, max.level = 3))
    }
    
    if ("position" %in% names(data[[1]])) {
      cat("\n\nPosition structure:\n")
      cat("-------------------\n")
      print(str(data[[1]]$position, max.level = 3))
    }
  }
  
  cat("\n================================================================================\n\n")
  
  return(invisible(data))
}


#' Get measurements en converteer naar Poseidat format
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Tibble met measurements in Poseidat format
#' @export
get_measurements <- function(journal_id, device_id, start_time, end_time) {
  
  # Convert to POSIXct if character
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  # Convert to Unix timestamps
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message(sprintf("Fetching measurements..."))
  message(sprintf("  Device: %s", device_id))
  message(sprintf("  From: %s", format(start_time, "%Y-%m-%d %H:%M:%S")))
  message(sprintf("  To:   %s", format(end_time, "%Y-%m-%d %H:%M:%S")))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    stop(glue("API request failed: {resp_status(resp)} {resp_status_desc(resp)}"))
  }
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    warning("⚠ No measurements found for this time range")
    return(tibble())
  }
  
  message(sprintf("✓ Found %d measurement(s)", length(data)))
  
  # Convert to tibble with Poseidat naming conventions
  result <- map_dfr(data, function(m) {
    
    # Start with base fields - zoek naar tijd veld
    # Mogelijke veldnamen: timestamp, revision, time, datetime, ts
    time_value <- m$timestamp %||% m$revision %||% m$time %||% m$ts %||% m$datetime
    
    # Parse tijd - kan Unix timestamp of ISO string zijn
    if (!is.null(time_value)) {
      if (is.numeric(time_value)) {
        # Unix timestamp
        datetime <- as_datetime(time_value)
      } else if (is.character(time_value)) {
        # ISO string
        datetime <- as.POSIXct(time_value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        if (is.na(datetime)) {
          # Probeer andere formaten
          datetime <- as.POSIXct(time_value)
        }
      } else {
        datetime <- as.POSIXct(NA)
      }
    } else {
      datetime <- as.POSIXct(NA)
    }
    
    # Maak basis row met Poseidat veldnamen
    row <- tibble(
      datetime = datetime,
      timestamp_utc = if (!is.na(datetime)) format(datetime, "%Y-%m-%dT%H:%M:%OSZ") else NA_character_
    )
    
    # Verwerk positie data (als aanwezig)
    if (!is.null(m$position)) {
      row$latitude <- m$position$latitude %||% m$position$lat %||% NA_real_
      row$longitude <- m$position$longitude %||% m$position$lon %||% m$position$lng %||% NA_real_
      row$altitude <- m$position$altitude %||% m$position$alt %||% NA_real_
    } else {
      # Directe velden
      row$latitude <- m$latitude %||% m$lat %||% NA_real_
      row$longitude <- m$longitude %||% m$lon %||% m$lng %||% NA_real_
      row$altitude <- m$altitude %||% m$alt %||% NA_real_
    }
    
    # Verwerk speed/course
    row$speed_over_ground <- m$speed %||% m$sog %||% m$speed_over_ground %||% NA_real_
    row$course_over_ground <- m$course %||% m$cog %||% m$course_over_ground %||% m$heading %||% NA_real_
    
    # Verwerk payload data (als aanwezig)
    if (!is.null(m$payload)) {
      # Voeg alle payload velden toe met prefix
      payload_fields <- m$payload
      for (field_name in names(payload_fields)) {
        field_value <- payload_fields[[field_name]]
        # Converteer complexe types naar string
        if (is.list(field_value)) {
          field_value <- toJSON(field_value, auto_unbox = TRUE)
        }
        row[[paste0("payload_", field_name)]] <- field_value
      }
    }
    
    # Voeg overige velden toe (behalve die al verwerkt zijn)
    processed_fields <- c("timestamp", "revision", "time", "ts", "datetime", 
                          "position", "latitude", "longitude", "lat", "lon", "lng",
                          "altitude", "alt", "speed", "sog", "course", "cog", "heading",
                          "payload")
    
    remaining_fields <- setdiff(names(m), processed_fields)
    
    for (field_name in remaining_fields) {
      field_value <- m[[field_name]]
      # Converteer complexe types naar string
      if (is.list(field_value) && length(field_value) > 0) {
        field_value <- toJSON(field_value, auto_unbox = TRUE)
      }
      row[[field_name]] <- field_value
    }
    
    return(row)
  })
  
  # Sorteer op datetime
  if ("datetime" %in% names(result) && any(!is.na(result$datetime))) {
    result <- result |> arrange(datetime)
  }
  
  message(sprintf("✓ Converted to tibble with %d rows and %d columns", 
                  nrow(result), ncol(result)))
  
  return(result)
}


#' Get measurements (raw) - zonder conversie
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return List met raw measurements data
#' @export
get_measurements_raw <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  return(data)
}

# Stap 1: Inspecteer eerst de structuur van de data
journal_id <- "21eacd08-fcd3-4665-9dba-9c23c325bfa3"
device_id <- "6a737577-6670-3277-0000-000200000001"

raw_data <- inspect_measurement_structure(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)

# Stap 2: Haal measurements op in Poseidat format
measurements <- get_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)

# Bekijk het resultaat
print(measurements)
glimpse(measurements)

# Als je de raw data wilt zonder conversie:
raw_measurements <- get_measurements_raw(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)



# ====================================================================================================
# 8:59

#' Get measurements en converteer naar Poseidat format (FIXED)
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Tibble met measurements in Poseidat format
#' @export
get_measurements <- function(journal_id, device_id, start_time, end_time) {
  
  # Convert to POSIXct if character
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  # Convert to Unix timestamps
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message(sprintf("Fetching measurements..."))
  message(sprintf("  Device: %s", device_id))
  message(sprintf("  From: %s", format(start_time, "%Y-%m-%d %H:%M:%S")))
  message(sprintf("  To:   %s", format(end_time, "%Y-%m-%d %H:%M:%S")))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    stop(glue("API request failed: {resp_status(resp)} {resp_status_desc(resp)}"))
  }
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    warning("⚠ No measurements found for this time range")
    return(tibble())
  }
  
  message(sprintf("✓ Found %d measurement(s)", length(data)))
  message("Converting to Poseidat format...")
  
  # Eerst: vind alle unieke veldnamen in de dataset
  all_fields <- unique(unlist(map(data, names)))
  
  # Convert to tibble met consistente types
  result <- map_dfr(data, function(m) {
    
    # Helper functie om waarde te converteren naar consistent type
    convert_value <- function(val) {
      if (is.null(val)) {
        return(NA_character_)
      } else if (is.list(val)) {
        # Converteer lists/objecten naar JSON string
        return(toJSON(val, auto_unbox = TRUE))
      } else if (is.numeric(val)) {
        return(as.character(val))
      } else {
        return(as.character(val))
      }
    }
    
    # Start met basis tijd veld
    time_value <- m$timestamp %||% m$revision %||% m$time %||% m$ts %||% m$datetime
    
    # Parse tijd
    if (!is.null(time_value)) {
      if (is.numeric(time_value)) {
        datetime <- as_datetime(time_value)
      } else if (is.character(time_value)) {
        datetime <- as.POSIXct(time_value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        if (is.na(datetime)) {
          datetime <- as.POSIXct(time_value)
        }
      } else {
        datetime <- as.POSIXct(NA)
      }
    } else {
      datetime <- as.POSIXct(NA)
    }
    
    # Maak row - start met lege tibble met alle velden
    row <- tibble(
      datetime = datetime,
      timestamp_utc = if (!is.na(datetime)) format(datetime, "%Y-%m-%dT%H:%M:%OSZ") else NA_character_
    )
    
    # Poseidat GPS velden - probeer verschillende veldnamen
    if (!is.null(m$position)) {
      row$latitude <- as.numeric(m$position$latitude %||% m$position$lat %||% NA)
      row$longitude <- as.numeric(m$position$longitude %||% m$position$lon %||% m$position$lng %||% NA)
      row$altitude <- as.numeric(m$position$altitude %||% m$position$alt %||% NA)
    } else {
      row$latitude <- as.numeric(m$latitude %||% m$lat %||% NA)
      row$longitude <- as.numeric(m$longitude %||% m$lon %||% m$lng %||% NA)
      row$altitude <- as.numeric(m$altitude %||% m$alt %||% NA)
    }
    
    # Speed en course
    row$speed_over_ground <- as.numeric(m$speed %||% m$sog %||% m$speed_over_ground %||% NA)
    row$course_over_ground <- as.numeric(m$course %||% m$cog %||% m$course_over_ground %||% m$heading %||% NA)
    
    # Verwerk alle andere velden als character (om type conflicten te voorkomen)
    processed_fields <- c("timestamp", "revision", "time", "ts", "datetime",
                          "position", "latitude", "longitude", "lat", "lon", "lng",
                          "altitude", "alt", "speed", "sog", "course", "cog", "heading")
    
    remaining_fields <- setdiff(names(m), processed_fields)
    
    for (field_name in remaining_fields) {
      field_value <- m[[field_name]]
      
      # Converteer naar consistent type
      if (field_name == "value") {
        # Speciale behandeling voor 'value' veld
        row[[field_name]] <- convert_value(field_value)
      } else if (is.numeric(field_value)) {
        row[[field_name]] <- field_value
      } else if (is.character(field_value)) {
        row[[field_name]] <- field_value
      } else if (is.logical(field_value)) {
        row[[field_name]] <- field_value
      } else if (is.list(field_value)) {
        row[[field_name]] <- convert_value(field_value)
      } else {
        row[[field_name]] <- as.character(field_value)
      }
    }
    
    return(row)
  })
  
  # Sorteer op datetime
  if ("datetime" %in% names(result) && any(!is.na(result$datetime))) {
    result <- result |> arrange(datetime)
  }
  
  message(sprintf("✓ Converted to tibble with %d rows and %d columns", 
                  nrow(result), ncol(result)))
  
  return(result)
}


#' Get measurements met type-specifieke kolommen
#'
#' Deze versie probeert de juiste types te behouden waar mogelijk
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @param simplify_nested Logical. Converteer geneste structuren naar JSON strings? (default TRUE)
#' @return Tibble met measurements
#' @export
get_measurements_typed <- function(journal_id, device_id, start_time, end_time,
                                   simplify_nested = TRUE) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message("Fetching measurements...")
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    return(tibble())
  }
  
  message(sprintf("✓ Found %d measurements", length(data)))
  
  # Analyseer de data structuur eerst
  message("Analyzing data structure...")
  
  # Vind het type van elk veld
  field_types <- list()
  
  for (m in data[1:min(100, length(data))]) {  # Sample eerste 100
    for (field_name in names(m)) {
      field_value <- m[[field_name]]
      
      if (is.null(field_value)) {
        field_types[[field_name]] <- c(field_types[[field_name]], "null")
      } else if (is.numeric(field_value)) {
        field_types[[field_name]] <- c(field_types[[field_name]], "numeric")
      } else if (is.character(field_value)) {
        field_types[[field_name]] <- c(field_types[[field_name]], "character")
      } else if (is.logical(field_value)) {
        field_types[[field_name]] <- c(field_types[[field_name]], "logical")
      } else if (is.list(field_value)) {
        field_types[[field_name]] <- c(field_types[[field_name]], "list")
      }
    }
  }
  
  # Bepaal dominant type voor elk veld
  dominant_types <- map_chr(field_types, function(types) {
    types <- types[types != "null"]
    if (length(types) == 0) return("character")
    if (length(unique(types)) == 1) return(types[1])
    # Als gemengde types, gebruik character
    return("character")
  })
  
  message("Converting to tibble...")
  
  # Nu convert met correcte types
  result <- map_dfr(data, function(m) {
    row <- list()
    
    for (field_name in names(dominant_types)) {
      field_value <- m[[field_name]]
      target_type <- dominant_types[[field_name]]
      
      if (is.null(field_value)) {
        row[[field_name]] <- switch(target_type,
                                    "numeric" = NA_real_,
                                    "character" = NA_character_,
                                    "logical" = NA,
                                    "list" = NA_character_,
                                    NA_character_
        )
      } else if (target_type == "list" && simplify_nested) {
        row[[field_name]] <- toJSON(field_value, auto_unbox = TRUE)
      } else if (target_type == "numeric") {
        row[[field_name]] <- as.numeric(field_value)
      } else if (target_type == "character") {
        if (is.list(field_value)) {
          row[[field_name]] <- toJSON(field_value, auto_unbox = TRUE)
        } else {
          row[[field_name]] <- as.character(field_value)
        }
      } else if (target_type == "logical") {
        row[[field_name]] <- as.logical(field_value)
      } else {
        row[[field_name]] <- field_value
      }
    }
    
    as_tibble(row)
  })
  
  # Parse datetime veld
  if ("revision" %in% names(result)) {
    result <- result |>
      mutate(
        datetime = as.POSIXct(revision, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      )
  }
  
  message(sprintf("✓ Created tibble with %d rows and %d columns", 
                  nrow(result), ncol(result)))
  
  return(result)
}

# Versie 1: Alles als character (meest robuust)
measurements <- get_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)

print(measurements)
glimpse(measurements)

# Versie 2: Behoudt types waar mogelijk
measurements_typed <- get_measurements_typed(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)

print(measurements_typed)
glimpse(measurements_typed)

# Bekijk de eerste paar rijen
head(measurements, 10)

# Bekijk de structuur
str(measurements)


# ========================================================================================
# 9:04

#' Inspecteer raw measurements data
#'
#' @param journal_id Character. Journal ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct. Start tijd
#' @param end_time POSIXct. Eind tijd
inspect_raw_measurements <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    RAW MEASUREMENTS INSPECTION\n")
  cat("================================================================================\n\n")
  
  cat(sprintf("Total measurements: %d\n\n", length(data)))
  
  if (length(data) > 0) {
    # Inspect first 5 measurements
    for (i in 1:min(5, length(data))) {
      cat(sprintf("--- MEASUREMENT %d ---\n", i))
      cat("Fields: ", paste(names(data[[i]]), collapse = ", "), "\n")
      
      # Print each field
      for (field_name in names(data[[i]])) {
        field_value <- data[[i]][[field_name]]
        cat(sprintf("  %s: ", field_name))
        
        if (is.null(field_value)) {
          cat("NULL\n")
        } else if (is.list(field_value)) {
          cat(sprintf("list with %d items\n", length(field_value)))
          cat("    Structure: ", str(field_value, max.level = 1), "\n")
        } else {
          cat(sprintf("%s (%s)\n", as.character(field_value), class(field_value)[1]))
        }
      }
      cat("\n")
    }
    
    # Check value field types across all measurements
    cat("VALUE FIELD ANALYSIS:\n")
    cat("---------------------\n")
    value_types <- map_chr(data, function(m) {
      if (is.null(m$value)) return("NULL")
      if (is.list(m$value)) return("list")
      if (is.numeric(m$value)) return("numeric")
      if (is.character(m$value)) return("character")
      return("other")
    })
    
    cat("Value field type distribution:\n")
    print(table(value_types))
    
    # Show examples of each type
    cat("\nExamples of each value type:\n")
    for (vtype in unique(value_types)) {
      idx <- which(value_types == vtype)[1]
      cat(sprintf("\n%s (measurement #%d):\n", vtype, idx))
      print(data[[idx]]$value)
    }
  }
  
  cat("\n================================================================================\n\n")
  
  return(invisible(data))
}


#' Get measurements - ROBUST versie die alle types afhandelt
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Tibble met measurements
#' @export
get_measurements_robust <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message("Fetching measurements...")
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    return(tibble())
  }
  
  message(sprintf("✓ Found %d measurements", length(data)))
  message("Converting to tibble...")
  
  # Converteer ELKE measurement individueel naar een tibble, dan combineer
  rows_list <- map(data, function(m) {
    
    # Maak een named list met alle velden
    row_list <- list()
    
    for (field_name in names(m)) {
      field_value <- m[[field_name]]
      
      # Converteer ALLES naar character om type conflicten te voorkomen
      if (is.null(field_value)) {
        row_list[[field_name]] <- NA_character_
      } else if (is.list(field_value)) {
        # Converteer list naar JSON string
        row_list[[field_name]] <- toJSON(field_value, auto_unbox = TRUE)
      } else {
        # Converteer naar character
        row_list[[field_name]] <- as.character(field_value)
      }
    }
    
    # Retourneer als tibble
    as_tibble(row_list)
  })
  
  # Combineer alle rows - nu hebben ze allemaal dezelfde types (character)
  result <- bind_rows(rows_list)
  
  # Nu converteren we specifieke kolommen naar betere types
  message("Converting column types...")
  
  # Parse datetime
  if ("revision" %in% names(result)) {
    result$datetime <- as.POSIXct(result$revision, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  }
  
  # Converteer numerieke velden
  numeric_fields <- c("latitude", "longitude", "altitude", "speed", "course", "heading")
  for (field in numeric_fields) {
    if (field %in% names(result)) {
      result[[field]] <- as.numeric(result[[field]])
    }
  }
  
  # Als 'value' een getal lijkt te zijn, probeer het te converteren
  if ("value" %in% names(result)) {
    # Probeer te converteren naar numeric, maar hou character als dat faalt
    value_numeric <- suppressWarnings(as.numeric(result$value))
    if (sum(!is.na(value_numeric)) > length(value_numeric) * 0.5) {
      # Als meer dan 50% succesvol converteert, gebruik numeric
      result$value_numeric <- value_numeric
      result$value_original <- result$value
    }
  }
  
  # Sorteer op datetime
  if ("datetime" %in% names(result) && any(!is.na(result$datetime))) {
    result <- result |> arrange(datetime)
  }
  
  message(sprintf("✓ Created tibble with %d rows and %d columns", 
                  nrow(result), ncol(result)))
  
  # Toon summary
  cat("\nColumn summary:\n")
  cat("---------------\n")
  for (col in names(result)) {
    n_na <- sum(is.na(result[[col]]))
    n_unique <- n_distinct(result[[col]], na.rm = TRUE)
    cat(sprintf("  %-20s: %d unique values, %d NAs\n", col, n_unique, n_na))
  }
  cat("\n")
  
  return(result)
}


# Kijk wat er precies in de data zit
raw_data <- inspect_raw_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)


# ===================================================================================================
# 9:07

#' Parse DBMatic measurement format naar tibble
#'
#' DBMatic gebruikt een generiek format waar value een geneste structuur is met:
#' - type: het type meting (TEMPERATURE, POSITION, etc.)
#' - numeric: lijst met value veld voor numerieke metingen
#' - position: lijst met lat/lon voor GPS metingen
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Tibble met measurements in Poseidat-achtig format
#' @export
get_measurements <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message("Fetching measurements...")
  message(sprintf("  Device: %s", device_id))
  message(sprintf("  From: %s", format(start_time, "%Y-%m-%d %H:%M:%S")))
  message(sprintf("  To:   %s", format(end_time, "%Y-%m-%d %H:%M:%S")))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    warning("⚠ No measurements found")
    return(tibble())
  }
  
  message(sprintf("✓ Found %d measurements", length(data)))
  message("Parsing DBMatic format...")
  
  # Parse elke measurement
  result <- map_dfr(data, function(m) {
    
    # Basis velden
    row <- tibble(
      entry_id = m$entry_id %||% NA_character_,
      entry_type = m$entry_type %||% NA_character_,
      journal_id = m$journal_id %||% NA_character_,
      device_id = m$device_id %||% NA_character_,
      immutable = m$immutable %||% NA,
      revision = m$revision %||% NA_character_
    )
    
    # Parse datetime van revision veld
    if (!is.na(row$revision)) {
      row$datetime <- as.POSIXct(row$revision, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    } else {
      row$datetime <- as.POSIXct(NA)
    }
    
    # Parse value veld (geneste structuur)
    if (!is.null(m$value)) {
      
      # Measurement type
      row$measurement_type <- m$value$type %||% NA_character_
      
      # Numerieke waarde (voor TEMPERATURE, SPEED, etc.)
      if (!is.null(m$value$numeric) && !is.null(m$value$numeric$value)) {
        row$value <- m$value$numeric$value
      } else {
        row$value <- NA_real_
      }
      
      # GPS position (voor POSITION type)
      if (!is.null(m$value$position)) {
        row$latitude <- m$value$position$latitude %||% m$value$position$lat %||% NA_real_
        row$longitude <- m$value$position$longitude %||% m$value$position$lon %||% NA_real_
        row$altitude <- m$value$position$altitude %||% m$value$position$alt %||% NA_real_
      } else {
        row$latitude <- NA_real_
        row$longitude <- NA_real_
        row$altitude <- NA_real_
      }
      
      # Speed/Course (voor SPEED/COURSE types of binnen POSITION)
      if (!is.null(m$value$speed)) {
        row$speed <- m$value$speed$value %||% m$value$speed %||% NA_real_
      } else {
        row$speed <- NA_real_
      }
      
      if (!is.null(m$value$course)) {
        row$course <- m$value$course$value %||% m$value$course %||% NA_real_
      } else {
        row$course <- NA_real_
      }
      
    } else {
      row$measurement_type <- NA_character_
      row$value <- NA_real_
      row$latitude <- NA_real_
      row$longitude <- NA_real_
      row$altitude <- NA_real_
      row$speed <- NA_real_
      row$course <- NA_real_
    }
    
    return(row)
  })
  
  # Sorteer op datetime
  result <- result |> arrange(datetime)
  
  message(sprintf("✓ Converted to tibble with %d rows", nrow(result)))
  
  # Toon summary van measurement types
  if ("measurement_type" %in% names(result)) {
    type_counts <- table(result$measurement_type)
    message("\nMeasurement types found:")
    for (mtype in names(type_counts)) {
      message(sprintf("  - %s: %d measurements", mtype, type_counts[mtype]))
    }
  }
  
  return(result)
}


#' Get measurements en filter op type
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @param measurement_type Character. Filter op type (bijv. "TEMPERATURE", "POSITION")
#' @return Tibble met gefilterde measurements
#' @export
get_measurements_by_type <- function(journal_id, device_id, start_time, end_time,
                                     measurement_type = NULL) {
  
  # Haal alle measurements op
  measurements <- get_measurements(journal_id, device_id, start_time, end_time)
  
  # Filter op type als gevraagd
  if (!is.null(measurement_type)) {
    measurements <- measurements |>
      filter(measurement_type == !!measurement_type)
    
    message(sprintf("\nFiltered to %d %s measurements", 
                    nrow(measurements), measurement_type))
  }
  
  return(measurements)
}


#' Verkrijg alle measurement types voor een device
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Tibble met measurement type statistieken
#' @export
get_measurement_types_summary <- function(journal_id, device_id, start_time, end_time) {
  
  measurements <- get_measurements(journal_id, device_id, start_time, end_time)
  
  if (nrow(measurements) == 0) {
    return(tibble(
      measurement_type = character(),
      count = integer(),
      first_datetime = as.POSIXct(character()),
      last_datetime = as.POSIXct(character())
    ))
  }
  
  summary <- measurements |>
    group_by(measurement_type) |>
    summarise(
      count = n(),
      first_datetime = min(datetime, na.rm = TRUE),
      last_datetime = max(datetime, na.rm = TRUE),
      mean_value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      min_value = if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      max_value = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary)
}

# Haal alle measurements op (inclusief TEMPERATURE)
measurements <- get_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2025-07-01 00:00:00"),
  end_time = as.POSIXct("2025-07-02 23:59:59")
)

# Bekijk het resultaat
print(measurements)
glimpse(measurements)
head(measurements, 20)

# Bekijk summary per type
summary <- get_measurement_types_summary(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59")
)
print(summary)

# Filter alleen TEMPERATURE metingen
temp_measurements <- get_measurements_by_type(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2024-01-01 00:00:00"),
  end_time = as.POSIXct("2024-12-31 23:59:59"),
  measurement_type = "TEMPERATURE"
)

# Plot temperatuur over tijd
library(ggplot2)
measurements %>% 
  filter(measurement_type=="POSITION") %>% 
  mutate(
    month = lubridate::month(datetime),
    yearday  = lubridate::yday(datetime)
  ) %>% 
  # View()

  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(aes(colour=as.character(yearday))) +
  geom_line(aes(colour=as.character(yearday))) +
  theme_minimal() +
  facet_wrap(~yearday)


# Statistieken
summary(temp_measurements$value)




# ============================================================================================
# 9:34

#' Krijg een schatting van het aantal measurements
#'
#' Deze functie probeert verschillende strategieën om het aantal measurements
#' te schatten zonder alle data te downloaden
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return List met schatting informatie
#' @export
estimate_measurement_count <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    MEASUREMENT COUNT ESTIMATION\n")
  cat("================================================================================\n\n")
  
  cat(sprintf("Device: %s\n", device_id))
  cat(sprintf("Period: %s to %s\n", 
              format(start_time, "%Y-%m-%d %H:%M:%S"),
              format(end_time, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Duration: %.1f days\n\n", 
              as.numeric(difftime(end_time, start_time, units = "days"))))
  
  # Strategie 1: Probeer met count parameter
  cat("Strategy 1: Trying count parameter...\n")
  tryCatch({
    resp <- request(glue("{base_url}/{endpoint}")) |>
      req_auth_bearer_token(api_token) |>
      req_url_query(start = start_unix, end = end_unix, count = TRUE) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    if (resp_status(resp) == 200) {
      result <- resp_body_json(resp)
      if (!is.null(result$count)) {
        cat(sprintf("  ✓ Found count: %d measurements\n\n", result$count))
        return(list(
          method = "count_parameter",
          total = result$count,
          exact = TRUE
        ))
      }
    }
  }, error = function(e) {})
  cat("  - Count parameter not supported\n\n")
  
  # Strategie 2: Sample de eerste 100 records en extrapoleer
  cat("Strategy 2: Sampling first 100 records...\n")
  tryCatch({
    resp <- request(glue("{base_url}/{endpoint}")) |>
      req_auth_bearer_token(api_token) |>
      req_url_query(start = start_unix, end = end_unix, limit = 100) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    if (resp_status(resp) == 200) {
      sample_data <- resp_body_json(resp)
      sample_size <- length(sample_data)
      
      cat(sprintf("  ✓ Got %d records in sample\n", sample_size))
      
      if (sample_size > 0) {
        # Parse timestamps om frequentie te berekenen
        sample_times <- map_dbl(sample_data, function(m) {
          rev_str <- m$revision %||% m$timestamp %||% m$time
          if (is.character(rev_str)) {
            dt <- as.POSIXct(rev_str, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
            if (!is.na(dt)) return(as.numeric(dt))
          }
          return(NA_real_)
        })
        
        sample_times <- sample_times[!is.na(sample_times)]
        
        if (length(sample_times) >= 2) {
          # Bereken gemiddelde interval tussen metingen
          intervals <- diff(sort(sample_times))
          avg_interval_sec <- mean(intervals)
          
          # Schat totaal aantal
          total_duration_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
          estimated_total <- round(total_duration_sec / avg_interval_sec)
          
          # Bepaal measurement type(s)
          types <- map_chr(sample_data, ~ .x$value$type %||% "Unknown")
          type_counts <- table(types)
          
          cat(sprintf("  Average interval: %.1f seconds\n", avg_interval_sec))
          cat(sprintf("  Estimated total: ~%d measurements\n", estimated_total))
          cat("\n  Sample measurement types:\n")
          for (mtype in names(type_counts)) {
            pct <- round(100 * type_counts[mtype] / sum(type_counts), 1)
            cat(sprintf("    - %s: %d (%.1f%%)\n", mtype, type_counts[mtype], pct))
          }
          cat("\n")
          
          return(list(
            method = "sampling",
            sample_size = sample_size,
            avg_interval_seconds = avg_interval_sec,
            estimated_total = estimated_total,
            exact = FALSE,
            measurement_types = type_counts,
            sample_data = sample_data
          ))
        }
      }
    }
  }, error = function(e) {
    cat(sprintf("  Error: %s\n\n", e$message))
  })
  
  # Strategie 3: Sample meerdere periodes
  cat("Strategy 3: Multi-period sampling...\n")
  tryCatch({
    # Sample begin, midden en eind
    total_duration <- as.numeric(difftime(end_time, start_time, units = "hours"))
    
    sample_periods <- list(
      list(name = "Start", start = start_time, end = start_time + (1 * 60 * 60)),
      list(name = "Middle", start = start_time + (total_duration/2 * 60 * 60), 
           end = start_time + (total_duration/2 * 60 * 60) + (1 * 60 * 60)),
      list(name = "End", start = end_time - (1 * 60 * 60), end = end_time)
    )
    
    period_counts <- list()
    
    for (period in sample_periods) {
      s <- as.integer(as.numeric(period$start))
      e <- as.integer(as.numeric(period$end))
      
      resp <- request(glue("{base_url}/{endpoint}")) |>
        req_auth_bearer_token(api_token) |>
        req_url_query(start = s, end = e) |>
        req_error(is_error = \(resp) FALSE) |>
        req_perform()
      
      if (resp_status(resp) == 200) {
        data <- resp_body_json(resp)
        count <- length(data)
        period_counts[[period$name]] <- count
        cat(sprintf("  %s: %d measurements in 1 hour\n", period$name, count))
      }
      
      Sys.sleep(0.1)
    }
    
    if (length(period_counts) > 0) {
      avg_per_hour <- mean(unlist(period_counts))
      total_hours <- as.numeric(difftime(end_time, start_time, units = "hours"))
      estimated_total <- round(avg_per_hour * total_hours)
      
      cat(sprintf("\n  Average: %.0f measurements/hour\n", avg_per_hour))
      cat(sprintf("  Estimated total: ~%d measurements\n\n", estimated_total))
      
      return(list(
        method = "multi_period_sampling",
        avg_per_hour = avg_per_hour,
        estimated_total = estimated_total,
        exact = FALSE,
        period_samples = period_counts
      ))
    }
  }, error = function(e) {
    cat(sprintf("  Error: %s\n\n", e$message))
  })
  
  cat("Could not estimate measurement count\n")
  cat("================================================================================\n\n")
  
  return(NULL)
}


#' Krijg een snel overzicht van beschikbare data
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @param sample_size Integer. Aantal records om te samplen (default 50)
#' @return Tibble met sample data
#' @export
preview_measurements <- function(journal_id, device_id, start_time, end_time,
                                 sample_size = 50) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  message(sprintf("Fetching preview (%d records)...", sample_size))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix, limit = sample_size) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  if (length(data) == 0) {
    message("No data found")
    return(tibble())
  }
  
  message(sprintf("✓ Got %d records", length(data)))
  
  # Parse zoals normale measurements
  result <- map_dfr(data, function(m) {
    row <- tibble(
      datetime = as.POSIXct(m$revision, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      measurement_type = m$value$type %||% NA_character_,
      value = m$value$numeric$value %||% NA_real_,
      latitude = m$value$position$latitude %||% NA_real_,
      longitude = m$value$position$longitude %||% NA_real_
    )
    return(row)
  })
  
  # Toon summary
  cat("\n")
  cat("Preview Summary:\n")
  cat("----------------\n")
  cat(sprintf("Period: %s to %s\n", 
              format(min(result$datetime), "%Y-%m-%d %H:%M:%S"),
              format(max(result$datetime), "%Y-%m-%d %H:%M:%S")))
  
  if ("measurement_type" %in% names(result)) {
    cat("\nMeasurement types:\n")
    print(table(result$measurement_type))
  }
  
  if (any(!is.na(result$value))) {
    cat(sprintf("\nValue range: %.2f to %.2f\n", 
                min(result$value, na.rm = TRUE),
                max(result$value, na.rm = TRUE)))
  }
  
  if (any(!is.na(result$latitude))) {
    cat(sprintf("\nGPS range:\n"))
    cat(sprintf("  Lat: %.6f to %.6f\n", 
                min(result$latitude, na.rm = TRUE),
                max(result$latitude, na.rm = TRUE)))
    cat(sprintf("  Lon: %.6f to %.6f\n", 
                min(result$longitude, na.rm = TRUE),
                max(result$longitude, na.rm = TRUE)))
  }
  
  cat("\n")
  
  return(result)
}


#' Check data availability per maand
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @return Tibble met maandelijkse statistieken
#' @export
check_monthly_availability <- function(journal_id, device_id, year = 2024) {
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("           MONTHLY DATA AVAILABILITY - YEAR %d\n", year))
  cat("================================================================================\n\n")
  
  # Check elke maand
  monthly_stats <- map_dfr(1:12, function(month) {
    
    # Bepaal begin en eind van maand
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    cat(sprintf("Checking %s %d...", month_name, year))
    
    # Haal kleine sample op
    preview <- suppressMessages(
      preview_measurements(journal_id, device_id, start_date, end_date, sample_size = 10)
    )
    
    if (nrow(preview) > 0) {
      # Schat totaal
      estimation <- suppressMessages(
        estimate_measurement_count(journal_id, device_id, start_date, end_date)
      )
      
      estimated_count <- if (!is.null(estimation)) estimation$estimated_total else NA_integer_
      
      types <- table(preview$measurement_type)
      types_str <- paste(names(types), types, sep = ":", collapse = ", ")
      
      cat(sprintf(" ✓ ~%d measurements (%s)\n", estimated_count, types_str))
      
      tibble(
        year = year,
        month = month,
        month_name = month_name,
        has_data = TRUE,
        estimated_count = estimated_count,
        measurement_types = types_str
      )
      
    } else {
      cat(" - No data\n")
      
      tibble(
        year = year,
        month = month,
        month_name = month_name,
        has_data = FALSE,
        estimated_count = 0L,
        measurement_types = NA_character_
      )
    }
  })
  
  cat("\n")
  cat("Summary:\n")
  cat("--------\n")
  cat(sprintf("Months with data: %d/12\n", sum(monthly_stats$has_data)))
  cat(sprintf("Total estimated: ~%d measurements\n", 
              sum(monthly_stats$estimated_count, na.rm = TRUE)))
  
  cat("\n================================================================================\n\n")
  
  return(monthly_stats)
}


# 1. Schat hoeveel data er is in een periode
estimate <- estimate_measurement_count(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2025-07-01 00:00:00"),
  end_time = as.POSIXct("2025-07-31 23:59:59")
)

# 2. Krijg een preview (eerste 50 records)
preview <- preview_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2025-07-01 00:00:00"),
  end_time = as.POSIXct("2025-07-31 23:59:59"),
  sample_size = 50
)

print(preview)

# 3. Check beschikbaarheid per maand
monthly_2024 <- check_monthly_availability(journal_id, device_id, year = 2024)
monthly_2025 <- check_monthly_availability(journal_id, device_id, year = 2025)

print(monthly_2024)
print(monthly_2025)

# 4. Visualiseer beschikbaarheid
library(ggplot2)

# Combineer jaren
all_months <- bind_rows(monthly_2024, monthly_2025)

ggplot(all_months, aes(x = month, y = estimated_count, fill = has_data)) +
  geom_col() +
  facet_wrap(~ year, ncol = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Data Availability by Month",
       x = "Month",
       y = "Estimated # Measurements") +
  theme_minimal()

# 5. Als je nu weet wat je wilt, download dan de volledige dataset
# Voor juli 2025 (als preview laat zien dat het GPS data is)
measurements_july <- get_measurements(
  journal_id, 
  device_id,
  start_time = as.POSIXct("2025-07-01 00:00:00"),
  end_time = as.POSIXct("2025-07-31 23:59:59")
)



# ============================================================================================
# 11:47

#' Snelle check: is er überhaupt data?
#'
#' Doet maar 1 API call om te zien of er data is
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @return Logical - TRUE als er data is
#' @export
has_data_quick <- function(journal_id, device_id, start_time, end_time) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  # Haal slechts 1 record op
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix, limit = 1) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  if (resp_status(resp) == 200) {
    data <- resp_body_json(resp)
    return(length(data) > 0)
  }
  
  return(FALSE)
}


#' Check maandelijkse beschikbaarheid (snel)
#'
#' Gebruikt alleen een snelle check (1 record per maand)
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @return Tibble met maandelijkse beschikbaarheid
#' @export
check_monthly_availability_fast <- function(journal_id, device_id, year = 2024) {
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("      MONTHLY DATA AVAILABILITY (FAST CHECK) - YEAR %d\n", year))
  cat("================================================================================\n\n")
  
  monthly_stats <- map_dfr(1:12, function(month) {
    
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    cat(sprintf("%-10s: ", month_name))
    
    has_data <- has_data_quick(journal_id, device_id, start_date, end_date)
    
    cat(if (has_data) "✓ Has data\n" else "- No data\n")
    
    tibble(
      year = year,
      month = month,
      month_name = month_name,
      has_data = has_data
    )
  })
  
  cat("\n")
  cat(sprintf("Summary: %d/12 months have data\n", sum(monthly_stats$has_data)))
  cat("================================================================================\n\n")
  
  return(monthly_stats)
}


#' Download data met progress indicator
#'
#' Voor grote datasets is het beter om gewoon te downloaden met een progress bar
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @param show_progress Logical. Toon progress? (default TRUE)
#' @return Tibble met measurements
#' @export
get_measurements_with_progress <- function(journal_id, device_id, 
                                           start_time, end_time,
                                           show_progress = TRUE) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  start_unix <- as.integer(as.numeric(start_time))
  end_unix <- as.integer(as.numeric(end_time))
  
  if (show_progress) {
    cat("\n")
    cat("Downloading measurements...\n")
    cat(sprintf("  Period: %s to %s\n", 
                format(start_time, "%Y-%m-%d"),
                format(end_time, "%Y-%m-%d")))
    cat("  Starting download... ")
  }
  
  download_start <- Sys.time()
  
  base_url <- getOption("dbmatic_base_url")
  api_token <- getOption("dbmatic_api_token")
  
  endpoint <- glue("journals/{journal_id}/measurements/{device_id}")
  
  resp <- request(glue("{base_url}/{endpoint}")) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(start = start_unix, end = end_unix) |>
    req_perform()
  
  data <- resp_body_json(resp)
  
  download_duration <- as.numeric(difftime(Sys.time(), download_start, units = "secs"))
  
  if (show_progress) {
    cat(sprintf("Done! (%.1f seconds)\n", download_duration))
    cat(sprintf("  Downloaded: %d measurements\n", length(data)))
  }
  
  if (length(data) == 0) {
    if (show_progress) cat("  No data found\n\n")
    return(tibble())
  }
  
  if (show_progress) {
    cat("  Parsing data... ")
  }
  
  parse_start <- Sys.time()
  
  # Parse zoals gebruikelijk
  result <- map_dfr(data, function(m) {
    row <- tibble(
      entry_id = m$entry_id %||% NA_character_,
      datetime = as.POSIXct(m$revision, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      measurement_type = m$value$type %||% NA_character_,
      value = m$value$numeric$value %||% NA_real_,
      latitude = m$value$position$latitude %||% NA_real_,
      longitude = m$value$position$longitude %||% NA_real_,
      altitude = m$value$position$altitude %||% NA_real_
    )
    return(row)
  })
  
  result <- result |> arrange(datetime)
  
  parse_duration <- as.numeric(difftime(Sys.time(), parse_start, units = "secs"))
  total_duration <- as.numeric(difftime(Sys.time(), download_start, units = "secs"))
  
  if (show_progress) {
    cat(sprintf("Done! (%.1f seconds)\n\n", parse_duration))
    cat("Summary:\n")
    cat(sprintf("  Total time: %.1f seconds (%.0f records/sec)\n", 
                total_duration, nrow(result) / total_duration))
    cat(sprintf("  Records: %d\n", nrow(result)))
    
    if ("measurement_type" %in% names(result)) {
      types <- table(result$measurement_type)
      cat("  Types:\n")
      for (mtype in names(types)) {
        cat(sprintf("    - %s: %d\n", mtype, types[mtype]))
      }
    }
    cat("\n")
  }
  
  return(result)
}


#' Slim download strategie: check eerst, download dan
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct or Character. Start tijd
#' @param end_time POSIXct or Character. Eind tijd
#' @param estimate_first Logical. Schat eerst aantal? (default FALSE voor snelheid)
#' @return Tibble met measurements
#' @export
download_measurements_smart <- function(journal_id, device_id, 
                                        start_time, end_time,
                                        estimate_first = FALSE) {
  
  if (is.character(start_time)) start_time <- as.POSIXct(start_time)
  if (is.character(end_time)) end_time <- as.POSIXct(end_time)
  
  cat("\n")
  cat("================================================================================\n")
  cat("                    SMART MEASUREMENT DOWNLOAD\n")
  cat("================================================================================\n\n")
  
  # Stap 1: Snelle check - is er data?
  cat("Step 1: Quick check... ")
  has_data <- has_data_quick(journal_id, device_id, start_time, end_time)
  
  if (!has_data) {
    cat("No data found\n\n")
    return(tibble())
  }
  
  cat("Data available ✓\n\n")
  
  # Stap 2: Optioneel - schat aantal
  if (estimate_first) {
    cat("Step 2: Estimating count...\n")
    estimate <- estimate_measurement_count(journal_id, device_id, start_time, end_time)
    
    if (!is.null(estimate) && !is.na(estimate$estimated_total)) {
      cat(sprintf("\nEstimated: ~%d measurements\n", estimate$estimated_total))
      
      # Waarschuwing bij grote datasets
      if (estimate$estimated_total > 100000) {
        cat("\n⚠ WARNING: Large dataset (>100k records)\n")
        cat("  This may take several minutes to download and parse\n")
        cat("  Consider downloading in smaller chunks (e.g., per month)\n\n")
        
        response <- readline(prompt = "Continue? (y/n): ")
        if (tolower(response) != "y") {
          cat("Download cancelled\n\n")
          return(tibble())
        }
      }
    }
    cat("\n")
  }
  
  # Stap 3: Download
  step_label <- if (estimate_first) "Step 3" else "Step 2"
  cat(sprintf("%s: Downloading...\n\n", step_label))
  
  result <- get_measurements_with_progress(
    journal_id, device_id, start_time, end_time, show_progress = TRUE
  )
  
  cat("================================================================================\n\n")
  
  return(result)
}

# SNELLE STRATEGIE (aanbevolen voor grote datasets):
# Gewoon downloaden zonder te schatten

# Voor één maand (snel)
measurements_july <- get_measurements_with_progress(
  journal_id, 
  device_id,
  start_time = "2025-07-01",
  end_time = "2025-07-31"
)

# SNELLE MAANDELIJKSE CHECK (gebruikt has_data_quick):
monthly_2024_fast <- check_monthly_availability_fast(journal_id, device_id, 2024)
monthly_2025_fast <- check_monthly_availability_fast(journal_id, device_id, 2025)

# SLIMME DOWNLOAD (met optionele schatting):
# Zonder schatting (sneller):
measurements <- download_measurements_smart(
  journal_id, device_id,
  start_time = "2025-07-01",
  end_time = "2025-07-31",
  estimate_first = FALSE  # Skip schatting, gewoon downloaden
)

# Met schatting (langzamer, maar waarschuwt bij grote datasets):
measurements <- download_measurements_smart(
  journal_id, device_id,
  start_time = "2025-07-01",
  end_time = "2025-07-31",
  estimate_first = TRUE  # Schat eerst, vraag bevestiging bij >100k records
)

# BATCH DOWNLOAD per maand:
download_year <- function(journal_id, device_id, year) {
  
  all_data <- list()
  
  for (month in 1:12) {
    start_date <- as.POSIXct(sprintf("%d-%02d-01", year, month))
    end_date <- if (month == 12) {
      as.POSIXct(sprintf("%d-01-01", year + 1))
    } else {
      as.POSIXct(sprintf("%d-%02d-01", year, month + 1))
    }
    
    cat(sprintf("\n=== %s %d ===\n", format(start_date, "%B"), year))
    
    data <- get_measurements_with_progress(
      journal_id, device_id, start_date, end_date, show_progress = TRUE
    )
    
    if (nrow(data) > 0) {
      all_data[[month]] <- data
    }
  }
  
  # Combineer alle maanden
  bind_rows(all_data)
}

# Download heel 2025 per maand
all_2025 <- download_year(journal_id, device_id, 2025)




# =======================================================================================
# 11:57

#' Check maandelijkse beschikbaarheid (snel) met timing
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @return Tibble met maandelijkse beschikbaarheid en timing info
#' @export
check_monthly_availability_fast <- function(journal_id, device_id, year = 2024) {
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("      MONTHLY DATA AVAILABILITY (FAST CHECK) - YEAR %d\n", year))
  cat("================================================================================\n\n")
  
  total_start <- Sys.time()
  
  monthly_stats <- map_dfr(1:12, function(month) {
    
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    cat(sprintf("%-10s: ", month_name))
    
    # Meet de tijd
    month_start <- Sys.time()
    has_data <- has_data_quick(journal_id, device_id, start_date, end_date)
    month_duration <- as.numeric(difftime(Sys.time(), month_start, units = "secs"))
    
    # Toon resultaat met timing
    status <- if (has_data) "✓ Has data" else "- No data"
    cat(sprintf("%-12s (%.2f sec)\n", status, month_duration))
    
    tibble(
      year = year,
      month = month,
      month_name = month_name,
      has_data = has_data,
      check_time_seconds = month_duration
    )
  })
  
  total_duration <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))
  
  cat("\n")
  cat("Summary:\n")
  cat("--------\n")
  cat(sprintf("Months with data: %d/12\n", sum(monthly_stats$has_data)))
  cat(sprintf("Total check time: %.2f seconds\n", total_duration))
  cat(sprintf("Average per month: %.2f seconds\n", mean(monthly_stats$check_time_seconds)))
  cat(sprintf("Slowest month: %s (%.2f sec)\n", 
              monthly_stats$month_name[which.max(monthly_stats$check_time_seconds)],
              max(monthly_stats$check_time_seconds)))
  cat(sprintf("Fastest month: %s (%.2f sec)\n", 
              monthly_stats$month_name[which.min(monthly_stats$check_time_seconds)],
              min(monthly_stats$check_time_seconds)))
  cat("================================================================================\n\n")
  
  return(monthly_stats)
}


#' Check maandelijkse beschikbaarheid met uitgebreide details
#'
#' Doet ook een preview (10 records) om type te bepalen
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @return Tibble met maandelijkse statistieken
#' @export
check_monthly_availability_detailed <- function(journal_id, device_id, year = 2024) {
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("    MONTHLY DATA AVAILABILITY (DETAILED) - YEAR %d\n", year))
  cat("================================================================================\n\n")
  
  total_start <- Sys.time()
  
  monthly_stats <- map_dfr(1:12, function(month) {
    
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    cat(sprintf("%-10s: ", month_name))
    
    month_start <- Sys.time()
    
    # Haal 10 records op om type te bepalen
    preview <- suppressMessages(
      preview_measurements(journal_id, device_id, start_date, end_date, sample_size = 10)
    )
    
    month_duration <- as.numeric(difftime(Sys.time(), month_start, units = "secs"))
    
    if (nrow(preview) > 0) {
      types <- table(preview$measurement_type)
      types_str <- paste(names(types), collapse = ", ")
      
      cat(sprintf("✓ %-30s (%.2f sec)\n", types_str, month_duration))
      
      tibble(
        year = year,
        month = month,
        month_name = month_name,
        has_data = TRUE,
        measurement_types = types_str,
        check_time_seconds = month_duration
      )
      
    } else {
      cat(sprintf("- No data%-22s (%.2f sec)\n", "", month_duration))
      
      tibble(
        year = year,
        month = month,
        month_name = month_name,
        has_data = FALSE,
        measurement_types = NA_character_,
        check_time_seconds = month_duration
      )
    }
  })
  
  total_duration <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))
  
  cat("\n")
  cat("Summary:\n")
  cat("--------\n")
  cat(sprintf("Months with data: %d/12\n", sum(monthly_stats$has_data)))
  cat(sprintf("Total check time: %.2f seconds (%.1f min)\n", 
              total_duration, total_duration / 60))
  cat(sprintf("Average per month: %.2f seconds\n", mean(monthly_stats$check_time_seconds)))
  cat("================================================================================\n\n")
  
  return(monthly_stats)
}


#' Parallelle check voor snellere uitvoering
#'
#' Checkt meerdere maanden tegelijk (vereist future package)
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @param workers Integer. Aantal parallelle workers (default 4)
#' @return Tibble met maandelijkse beschikbaarheid
#' @export
check_monthly_availability_parallel <- function(journal_id, device_id, year = 2024, 
                                                workers = 4) {
  
  if (!requireNamespace("future", quietly = TRUE) || 
      !requireNamespace("furrr", quietly = TRUE)) {
    stop("Packages 'future' en 'furrr' zijn nodig voor parallelle uitvoering.\n",
         "Installeer met: install.packages(c('future', 'furrr'))")
  }
  
  library(future)
  library(furrr)
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("  MONTHLY DATA AVAILABILITY (PARALLEL) - YEAR %d\n", year))
  cat(sprintf("  Using %d parallel workers\n", workers))
  cat("================================================================================\n\n")
  
  # Setup parallel processing
  plan(multisession, workers = workers)
  
  total_start <- Sys.time()
  
  cat("Checking all months in parallel... ")
  
  monthly_stats <- future_map_dfr(1:12, function(month) {
    
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    
    month_start <- Sys.time()
    has_data <- has_data_quick(journal_id, device_id, start_date, end_date)
    month_duration <- as.numeric(difftime(Sys.time(), month_start, units = "secs"))
    
    tibble(
      year = year,
      month = month,
      month_name = month_name,
      has_data = has_data,
      check_time_seconds = month_duration
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Reset to sequential
  plan(sequential)
  
  total_duration <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))
  
  cat(sprintf("Done! (%.2f seconds)\n\n", total_duration))
  
  # Sorteer op maand
  monthly_stats <- monthly_stats |> arrange(month)
  
  # Toon resultaten
  for (i in seq_len(nrow(monthly_stats))) {
    status <- if (monthly_stats$has_data[i]) "✓ Has data" else "- No data"
    cat(sprintf("%-10s: %-12s (%.2f sec)\n", 
                monthly_stats$month_name[i], 
                status,
                monthly_stats$check_time_seconds[i]))
  }
  
  cat("\n")
  cat("Summary:\n")
  cat("--------\n")
  cat(sprintf("Months with data: %d/12\n", sum(monthly_stats$has_data)))
  cat(sprintf("Total time: %.2f seconds (%.1fx speedup vs sequential)\n", 
              total_duration, 
              sum(monthly_stats$check_time_seconds) / total_duration))
  cat("================================================================================\n\n")
  
  return(monthly_stats)
}



# Standaard check met timing
monthly_2024 <- check_monthly_availability_fast(journal_id, device_id, 2024)
monthly_2025 <- check_monthly_availability_fast(journal_id, device_id, 2025)

# Bekijk de timing details
print(monthly_2024)
print(monthly_2025)

# Uitgebreide check (haalt ook 10 records op om type te zien)
monthly_2025_detailed <- check_monthly_availability_detailed(journal_id, device_id, 2025)

# Parallelle check (sneller voor meerdere jaren)
# Vereist: install.packages(c("future", "furrr"))
monthly_2024_parallel <- check_monthly_availability_parallel(
  journal_id, device_id, 2024, workers = 4
)

monthly_2025_parallel <- check_monthly_availability_parallel(
  journal_id, device_id, 2025, workers = 4
)


# ==========================================================================================
# 13:11

#' Parallelle check voor snellere uitvoering (FIXED)
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param year Integer. Jaar om te checken
#' @param workers Integer. Aantal parallelle workers (default 4)
#' @return Tibble met maandelijkse beschikbaarheid
#' @export
check_monthly_availability_parallel <- function(journal_id, device_id, year = 2024, 
                                                workers = 4) {
  
  if (!requireNamespace("future", quietly = TRUE) || 
      !requireNamespace("furrr", quietly = TRUE)) {
    stop("Packages 'future' en 'furrr' zijn nodig voor parallelle uitvoering.\n",
         "Installeer met: install.packages(c('future', 'furrr'))")
  }
  
  library(future)
  library(furrr)
  
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("  MONTHLY DATA AVAILABILITY (PARALLEL) - YEAR %d\n", year))
  cat(sprintf("  Using %d parallel workers\n", workers))
  cat("================================================================================\n\n")
  
  # Setup parallel processing
  plan(multisession, workers = workers)
  
  total_start <- Sys.time()
  
  cat("Checking all months in parallel... ")
  
  # Converteer alle parameters naar basic types (geen glue/complex objects)
  journal_id_char <- as.character(journal_id)
  device_id_char <- as.character(device_id)
  base_url <- as.character(getOption("dbmatic_base_url"))
  api_token <- as.character(getOption("dbmatic_api_token"))
  
  monthly_stats <- future_map_dfr(1:12, function(month) {
    
    # Load libraries in worker
    library(httr2)
    library(dplyr)
    library(lubridate)
    
    start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month))
    
    if (month == 12) {
      end_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1))
    } else {
      end_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", year, month + 1))
    }
    
    month_name <- format(start_date, "%B")
    
    month_start <- Sys.time()
    
    # Inline API call (zonder glue)
    start_unix <- as.integer(as.numeric(start_date))
    end_unix <- as.integer(as.numeric(end_date))
    
    endpoint <- paste0("journals/", journal_id_char, "/measurements/", device_id_char)
    full_url <- paste0(base_url, "/", endpoint)
    
    resp <- request(full_url) |>
      req_auth_bearer_token(api_token) |>
      req_url_query(start = start_unix, end = end_unix, limit = 1) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    has_data <- FALSE
    if (resp_status(resp) == 200) {
      data <- resp_body_json(resp)
      has_data <- length(data) > 0
    }
    
    month_duration <- as.numeric(difftime(Sys.time(), month_start, units = "secs"))
    
    tibble(
      year = year,
      month = month,
      month_name = month_name,
      has_data = has_data,
      check_time_seconds = month_duration
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Reset to sequential
  plan(sequential)
  
  total_duration <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))
  
  cat(sprintf("Done! (%.2f seconds)\n\n", total_duration))
  
  # Sorteer op maand
  monthly_stats <- monthly_stats |> arrange(month)
  
  # Toon resultaten
  for (i in seq_len(nrow(monthly_stats))) {
    status <- if (monthly_stats$has_data[i]) "✓ Has data" else "- No data"
    cat(sprintf("%-10s: %-12s (%.2f sec)\n", 
                monthly_stats$month_name[i], 
                status,
                monthly_stats$check_time_seconds[i]))
  }
  
  cat("\n")
  cat("Summary:\n")
  cat("--------\n")
  cat(sprintf("Months with data: %d/12\n", sum(monthly_stats$has_data)))
  
  # Bereken speedup
  sequential_time <- sum(monthly_stats$check_time_seconds)
  speedup <- sequential_time / total_duration
  
  cat(sprintf("Total time: %.2f seconds\n", total_duration))
  cat(sprintf("Sequential would take: %.2f seconds\n", sequential_time))
  cat(sprintf("Speedup: %.1fx\n", speedup))
  cat("================================================================================\n\n")
  
  return(monthly_stats)
}


# Dit zou nu moeten werken
monthly_2024_parallel <- check_monthly_availability_parallel(
  journal_id, device_id, 2024, workers = 4
)

monthly_2025_parallel <- check_monthly_availability_parallel(
  journal_id, device_id, 2025, workers = 4
)
