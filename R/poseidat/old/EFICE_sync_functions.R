# ============================================================================
# efice data synchronisatie
# ============================================================================

# ============================================================================
# CONFIGURATIE & SETUP
# ============================================================================

#' Setup EFICE data synchronisatie
#'
#' @param base_url Character. API base URL
#' @param api_token Character. API token
#' @param data_dir Character. Directory voor data opslag (default "data/efice")
#' @param log_dir Character. Directory voor logs (default "logs")
#' @export
setup_efice_sync <- function(base_url, 
                       api_token, 
                       data_dir = "data/efice",
                       log_dir = "logs/efice") {
  
  # Sla configuratie op
  options(
    efice_base_url = base_url,
    efice_api_token = api_token,
    efice_data_dir = data_dir,
    efice_log_dir = log_dir
  )
  
  # Maak directories aan
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Maak subdirectories voor verschillende data types
  dir.create(file.path(data_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "processed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "state"), recursive = TRUE, showWarnings = FALSE)
  
  message("✓ EFICE sync configured")
  message(sprintf("  Data directory: %s", data_dir))
  message(sprintf("  Log directory: %s", log_dir))
  
  # Maak initiële state file als die niet bestaat
  state_file <- file.path(data_dir, "state", "sync_state.rds")
  if (!file.exists(state_file)) {
    initial_state <- list(
      created = Sys.time(),
      last_sync = NULL,
      devices = list()
    )
    saveRDS(initial_state, state_file)
    message("  Created initial state file")
  }
  
  invisible(TRUE)
}


# ============================================================================
# STATE MANAGEMENT
# ============================================================================

#' Laad sync state
#'
#' @return List met sync state
get_sync_state <- function() {
  data_dir <- getOption("efice_data_dir", "data/efice")
  state_file <- file.path(data_dir, "state", "sync_state.rds")
  
  if (file.exists(state_file)) {
    return(readRDS(state_file))
  }
  
  # Default state
  return(list(
    created = Sys.time(),
    last_sync = NULL,
    devices = list()
  ))
}

#' Sla sync state op
#'
#' @param state List. Sync state
save_sync_state <- function(state) {
  data_dir <- getOption("efice_data_dir", "data/efice")
  state_file <- file.path(data_dir, "state", "sync_state.rds")
  
  dir.create(dirname(state_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(state, state_file)
  
  invisible(TRUE)
}

#' Haal laatste sync tijd voor een device
#'
#' @param device_id Character. Device ID
#' @return POSIXct or NULL
get_last_sync_time <- function(device_id) {
  state <- get_sync_state()
  
  if (device_id %in% names(state$devices)) {
    return(state$devices[[device_id]]$last_sync_time)
  }
  
  return(NULL)
}

#' Update laatste sync tijd voor een device
#'
#' @param device_id Character. Device ID
#' @param sync_time POSIXct. Sync tijd
#' @param record_count Integer. Aantal opgehaalde records
update_last_sync_time <- function(device_id, sync_time, record_count = 0) {
  state <- get_sync_state()
  
  if (is.null(state$devices)) {
    state$devices <- list()
  }
  
  state$devices[[device_id]] <- list(
    last_sync_time = sync_time,
    last_sync_timestamp = Sys.time(),
    last_record_count = record_count
  )
  
  state$last_sync <- Sys.time()
  
  save_sync_state(state)
  
  invisible(TRUE)
}


# ============================================================================
# LOGGING
# ============================================================================

#' Log een bericht
#'
#' @param message Character. Bericht
#' @param level Character. Level (INFO, WARNING, ERROR)
#' @param device_id Character. Optioneel device ID
log_message <- function(message, level = "INFO", device_id = NULL) {
  
  log_dir <- getOption("efice_log_dir", "logs")
  log_file <- file.path(log_dir, sprintf("efice_sync_%s.log", format(Sys.Date(), "%Y%m")))
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  device_str <- if (!is.null(device_id)) sprintf(" [%s]", device_id) else ""
  log_entry <- sprintf("[%s] %s%s: %s\n", timestamp, level, device_str, message)
  
  # Schrijf naar log file
  cat(log_entry, file = log_file, append = TRUE)
  
  # Print ook naar console
  cat(log_entry)
}


# ============================================================================
# DATA DOWNLOAD & STORAGE
# ============================================================================

#' Download measurements en sla op als Parquet
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param start_time POSIXct. Start tijd
#' @param end_time POSIXct. Eind tijd
#' @param device_name Character. Naam van device (voor bestandsnaam)
#' @return Tibble met measurements
download_and_store_measurements <- function(journal_id, device_id, 
                                            start_time, end_time,
                                            device_name = "unknown") {
  
  data_dir <- getOption("efice_data_dir", "data/efice")
  
  log_message(sprintf("Downloading data from %s to %s", 
                      format(start_time, "%Y-%m-%d"), 
                      format(end_time, "%Y-%m-%d")),
              level = "INFO", device_id = device_id)
  
  # Download data
  measurements <- tryCatch({
    get_measurements_with_progress(journal_id, device_id, start_time, end_time,
                                   show_progress = TRUE)
  }, error = function(e) {
    log_message(sprintf("Download failed: %s", e$message), 
                level = "ERROR", device_id = device_id)
    return(tibble())
  })
  
  if (nrow(measurements) == 0) {
    log_message("No data downloaded", level = "INFO", device_id = device_id)
    return(tibble())
  }
  
  log_message(sprintf("Downloaded %d measurements", nrow(measurements)),
              level = "INFO", device_id = device_id)
  
  # Bepaal bestandsnaam op basis van datum range
  start_date_str <- format(start_time, "%Y%m%d")
  end_date_str <- format(end_time, "%Y%m%d")
  
  # Sanitize device name voor bestandsnaam
  safe_device_name <- gsub("[^a-zA-Z0-9_-]", "_", device_name)
  
  filename <- sprintf("%s_%s_%s_to_%s.parquet",
                      safe_device_name,
                      device_id,
                      start_date_str,
                      end_date_str)
  
  filepath <- file.path(data_dir, "raw", filename)
  
  # Sla op als Parquet
  tryCatch({
    arrow::write_parquet(measurements, filepath)
    log_message(sprintf("Saved to %s", filepath), 
                level = "INFO", device_id = device_id)
  }, error = function(e) {
    log_message(sprintf("Failed to save: %s", e$message), 
                level = "ERROR", device_id = device_id)
  })
  
  return(measurements)
}


#' Incrementele sync voor een device
#'
#' @param journal_id Character. Journal/vessel ID
#' @param device_id Character. Device ID
#' @param device_name Character. Naam van device
#' @param lookback_days Integer. Aantal dagen terug te kijken (default 7)
#' @param force_full Logical. Forceer volledige sync? (default FALSE)
#' @return Tibble met nieuwe measurements
sync_device <- function(journal_id, device_id, device_name = "unknown",
                        lookback_days = 7, force_full = FALSE) {
  
  log_message("Starting sync", level = "INFO", device_id = device_id)
  
  # Bepaal start tijd voor sync
  if (force_full) {
    # Volledige sync vanaf ver in het verleden
    start_time <- as.POSIXct("2024-01-01 00:00:00")
    log_message("Performing FULL sync", level = "INFO", device_id = device_id)
  } else {
    # Incrementele sync
    last_sync <- get_last_sync_time(device_id)
    
    if (is.null(last_sync)) {
      # Eerste keer - haal laatste X dagen op
      start_time <- Sys.time() - (lookback_days * 24 * 60 * 60)
      log_message(sprintf("First sync - downloading last %d days", lookback_days),
                  level = "INFO", device_id = device_id)
    } else {
      # Start vanaf laatste sync minus 1 dag overlap (voor zekerheid)
      start_time <- last_sync - (1 * 24 * 60 * 60)
      log_message(sprintf("Incremental sync from %s", format(last_sync, "%Y-%m-%d")),
                  level = "INFO", device_id = device_id)
    }
  }
  
  end_time <- Sys.time()
  
  # Download en sla op
  measurements <- download_and_store_measurements(
    journal_id, device_id, start_time, end_time, device_name
  )
  
  # Update sync state
  if (nrow(measurements) > 0) {
    max_datetime <- max(measurements$datetime, na.rm = TRUE)
    update_last_sync_time(device_id, max_datetime, nrow(measurements))
    log_message(sprintf("Sync completed - %d new records", nrow(measurements)),
                level = "INFO", device_id = device_id)
  } else {
    log_message("Sync completed - no new data", level = "INFO", device_id = device_id)
  }
  
  return(measurements)
}


#' Sync alle geconfigureerde devices
#'
#' @param device_config Tibble. Device configuratie (journal_id, device_id, device_name)
#' @param lookback_days Integer. Lookback voor eerste sync (default 7)
#' @return List met sync results per device
sync_all_devices <- function(device_config, lookback_days = 7) {
  
  log_message("=== Starting sync for all devices ===", level = "INFO")
  log_message(sprintf("Syncing %d device(s)", nrow(device_config)), level = "INFO")
  
  results <- list()
  
  for (i in seq_len(nrow(device_config))) {
    device <- device_config[i, ]
    
    log_message(sprintf("=== Device %d/%d: %s ===", i, nrow(device_config), device$device_name),
                level = "INFO")
    
    result <- tryCatch({
      sync_device(
        journal_id = device$journal_id,
        device_id = device$device_id,
        device_name = device$device_name,
        lookback_days = lookback_days
      )
    }, error = function(e) {
      log_message(sprintf("Sync failed: %s", e$message), 
                  level = "ERROR", device_id = device$device_id)
      return(tibble())
    })
    
    results[[device$device_id]] <- result
    
    # Wacht tussen devices om API niet te overbelasten
    if (i < nrow(device_config)) {
      Sys.sleep(2)
    }
  }
  
  log_message("=== Sync completed for all devices ===", level = "INFO")
  
  return(results)
}


# ============================================================================
# DATA LOADING & CONSOLIDATIE
# ============================================================================

#' Laad alle Parquet files voor een device
#'
#' @param device_id Character. Device ID
#' @param start_date Date. Optioneel - filter op start datum
#' @param end_date Date. Optioneel - filter op eind datum
#' @return Tibble met alle data
load_device_data <- function(device_id, start_date = NULL, end_date = NULL) {
  
  data_dir <- getOption("efice_data_dir", "data/efice")
  raw_dir <- file.path(data_dir, "raw")
  
  # Vind alle parquet files voor dit device
  pattern <- sprintf(".*_%s_.*\\.parquet$", device_id)
  files <- list.files(raw_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    message(sprintf("No data files found for device %s", device_id))
    return(tibble())
  }
  
  message(sprintf("Loading %d file(s) for device %s", length(files), device_id))
  
  # Laad alle files
  all_data <- map_dfr(files, function(file) {
    arrow::read_parquet(file)
  })
  
  # Filter op datum indien nodig
  if (!is.null(start_date) || !is.null(end_date)) {
    if (!is.null(start_date)) {
      all_data <- all_data |> filter(as.Date(datetime) >= start_date)
    }
    if (!is.null(end_date)) {
      all_data <- all_data |> filter(as.Date(datetime) <= end_date)
    }
  }
  
  # Verwijder duplicaten (kan gebeuren door overlap)
  all_data <- all_data |>
    distinct(entry_id, .keep_all = TRUE) |>
    arrange(datetime)
  
  message(sprintf("Loaded %d measurements", nrow(all_data)))
  
  return(all_data)
}


#' Consolideer oude Parquet files per maand
#'
#' @param device_id Character. Device ID
#' @param device_name Character. Device naam
#' @param keep_daily_for_days Integer. Bewaar dagelijkse files voor X dagen (default 30)
consolidate_device_data <- function(device_id, device_name = "unknown",
                                    keep_daily_for_days = 30) {
  
  data_dir <- getOption("efice_data_dir", "data/efice")
  raw_dir <- file.path(data_dir, "raw")
  
  log_message("Starting data consolidation", level = "INFO", device_id = device_id)
  
  # Laad alle data
  all_data <- load_device_data(device_id)
  
  if (nrow(all_data) == 0) {
    log_message("No data to consolidate", level = "INFO", device_id = device_id)
    return(invisible(NULL))
  }
  
  # Bepaal cut-off datum
  cutoff_date <- Sys.Date() - keep_daily_for_days
  
  # Splits data in recent en oud
  old_data <- all_data |> filter(as.Date(datetime) < cutoff_date)
  
  if (nrow(old_data) == 0) {
    log_message("No old data to consolidate", level = "INFO", device_id = device_id)
    return(invisible(NULL))
  }
  
  # Groepeer oude data per maand
  old_data <- old_data |>
    mutate(
      year = year(datetime),
      month = month(datetime)
    )
  
  # Consolideer per maand
  for (yr in unique(old_data$year)) {
    for (mth in unique(old_data$month[old_data$year == yr])) {
      
      month_data <- old_data |>
        filter(year == yr, month == mth) |>
        select(-year, -month)
      
      if (nrow(month_data) == 0) next
      
      # Sla geconsolideerde file op
      safe_device_name <- gsub("[^a-zA-Z0-9_-]", "_", device_name)
      filename <- sprintf("%s_%s_%04d%02d.parquet",
                          safe_device_name,
                          device_id,
                          yr, mth)
      
      filepath <- file.path(data_dir, "processed", filename)
      arrow::write_parquet(month_data, filepath)
      
      log_message(sprintf("Consolidated %d-%02d: %d records", yr, mth, nrow(month_data)),
                  level = "INFO", device_id = device_id)
    }
  }
  
  # Verwijder oude dagelijkse files
  pattern <- sprintf(".*_%s_.*\\.parquet$", device_id)
  files <- list.files(raw_dir, pattern = pattern, full.names = TRUE)
  
  for (file in files) {
    # Parse datum uit bestandsnaam
    # Format: devicename_deviceid_YYYYMMDD_to_YYYYMMDD.parquet
    basename <- basename(file)
    date_match <- regexec("_(\\d{8})_to_(\\d{8})\\.parquet$", basename)
    
    if (date_match[[1]][1] != -1) {
      start_date_str <- regmatches(basename, date_match)[[1]][2]
      file_date <- as.Date(start_date_str, format = "%Y%m%d")
      
      if (file_date < cutoff_date) {
        file.remove(file)
        log_message(sprintf("Removed old file: %s", basename),
                    level = "INFO", device_id = device_id)
      }
    }
  }
  
  log_message("Consolidation completed", level = "INFO", device_id = device_id)
  
  invisible(NULL)
}

