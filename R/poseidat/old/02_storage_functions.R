# ==============================================================================
# Poseidat Data Storage Functions
# ==============================================================================

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(lubridate)

# Load configuration
if (file.exists("config.json")) {
  config <- read_json("config.json")
} else {
  stop("Configuration file not found. Please run 00_setup.R first.")
}

# Save Poseidat data to Parquet
save_poseidat_data <- function(data, data_type, date_range = NULL, subfolder = NULL) {
  valid_types <- c("trips", "catches", "sensors")
  if (!data_type %in% valid_types) stop(glue("data_type must be one of: {paste(valid_types, collapse = ', ')}"))
  
  dir_path <- if (!is.null(subfolder)) {
    glue("{config$raw_data_path}/{data_type}/{subfolder}")
  } else {
    glue("{config$raw_data_path}/{data_type}")
  }
  dir_create(dir_path)
  
  date_suffix <- if (!is_null(date_range)) glue("{date_range[1]}_{date_range[2]}") else format(Sys.Date(), "%Y%m%d")
  filename <- glue("{dir_path}/{data_type}_{date_suffix}.parquet")
  
  data_with_meta <- data |> mutate(fetch_date = Sys.Date(), fetch_timestamp = Sys.time(), .after = last_col())
  write_parquet(data_with_meta, filename)
  message(glue("✓ Saved {nrow(data)} rows to {filename}"))
  invisible(filename)
}

# Save all data types at once
save_all_poseidat_data <- function(data_list, date_range = NULL, subfolder = NULL) {
  valid_names <- c("trips", "catches", "sensors")
  if (!all(names(data_list) %in% valid_names)) stop("data_list must have names: 'trips', 'catches', 'sensors'")
  
  paths <- map2(data_list, names(data_list), \(data, name) {
    if (nrow(data) > 0) {
      save_poseidat_data(data, name, date_range, subfolder)
    } else {
      message(glue("⚠ Skipping {name} - no data"))
      NULL
    }
  })
  invisible(paths)
}

# Load Poseidat data
load_poseidat_data <- function(data_type, date_from = NULL, date_to = NULL, vessel_ids = NULL, subfolder = NULL) {
  
  data_path <- if (!is.null(subfolder)) {
    glue("{config$raw_data_path}/{data_type}/{subfolder}")
  } else {
    glue("{config$raw_data_path}/{data_type}")
  }
  
  if (!dir.exists(data_path)) {
    warning(glue("No data found at {data_path}"))
    return(tibble())
  }
  
  ds <- open_dataset(data_path)
  
  # Filter by vessel if specified
  if (!is_null(vessel_ids)) {
    ds <- ds |> filter(vessel_id %in% vessel_ids)
  }
  
  # Date filtering - use appropriate column for each data type
  date_column <- switch(data_type,
                        trips = "departure_date",
                        catches = "catch_date",
                        sensors = "date",
                        "date"  # default fallback
  )
  
  # Apply date filters if specified
  if (!is_null(date_from)) {
    ds <- ds |> filter(!!sym(date_column) >= as.Date(date_from))
  }
  if (!is_null(date_to)) {
    ds <- ds |> filter(!!sym(date_column) <= as.Date(date_to))
  }
  
  result <- ds |> collect()
  message(glue("✓ Loaded {nrow(result)} {data_type} records"))
  result
}

# Load all data types
load_all_poseidat_data <- function(date_from = NULL, date_to = NULL, vessel_ids = NULL, subfolder = NULL) {
  list(
    trips = load_poseidat_data("trips", date_from, date_to, vessel_ids, subfolder),
    catches = load_poseidat_data("catches", date_from, date_to, vessel_ids, subfolder),
    sensors = load_poseidat_data("sensors", date_from, date_to, vessel_ids, subfolder)
  )
}

# Incremental update
update_poseidat_data <- function(vessel_ids, data_type = "trips", date_column = "departure_date") {
  existing_data <- tryCatch(load_poseidat_data(data_type, vessel_ids = vessel_ids), error = function(e) tibble())
  
  if (nrow(existing_data) == 0) {
    message("No existing data - fetch manually with date range")
    return(existing_data)
  }
  
  last_date <- existing_data |> pull(!!sym(date_column)) |> max(na.rm = TRUE)
  date_from <- last_date + days(1)
  date_to <- today()
  
  if (date_from > date_to) {
    message("✓ Data is up to date!")
    return(existing_data)
  }
  
  message(glue("Fetching new data from {date_from} to {date_to}..."))
  new_data <- switch(data_type,
    trips = get_trips(vessel_ids, as.character(date_from), as.character(date_to)),
    catches = get_catches(vessel_ids, as.character(date_from), as.character(date_to)),
    sensors = get_sensors(vessel_ids, as.character(date_from), as.character(date_to))
  )
  
  if (nrow(new_data) > 0) {
    save_poseidat_data(new_data, data_type, c(date_from, date_to))
    combined <- bind_rows(existing_data, new_data) |> arrange(!!sym(date_column))
    message(glue("✓ Added {nrow(new_data)} new records"))
    return(combined)
  } else {
    message("✓ No new data available")
    return(existing_data)
  }
}

# Create trip summary (processed data)
create_trip_summary <- function(date_from = NULL, date_to = NULL, save_output = TRUE) {
  message("Creating trip summary...")
  trips <- load_poseidat_data("trips", date_from, date_to)
  catches <- load_poseidat_data("catches", date_from, date_to)
  
  catch_summary <- catches |> group_by(trip_id) |>
    summarise(total_catch_kg = sum(weight_kg, na.rm = TRUE), n_species = n_distinct(species),
              n_hauls = n(), main_species = first(species[which.max(weight_kg)]), .groups = "drop")
  
  trip_analysis <- trips |> left_join(catch_summary, by = "trip_id")
  
  if (save_output) {
    output_path <- glue("{config$processed_data_path}/trip_summary.parquet")
    write_parquet(trip_analysis, output_path)
    message(glue("✓ Saved to {output_path}"))
  }
  trip_analysis
}

# List data files
list_data_files <- function(data_type = NULL) {
  data_types <- if (is_null(data_type)) c("trips", "catches", "sensors") else data_type
  map_dfr(data_types, \(type) {
    path <- glue("{config$raw_data_path}/{type}")
    if (dir.exists(path)) {
      files <- dir_info(path, recurse = TRUE, glob = "*.parquet")
      if (nrow(files) > 0) {
        files |> select(path, size, modification_time) |>
          mutate(data_type = type, filename = basename(path), .before = 1)
      } else tibble()
    } else tibble()
  })
}

# Get storage statistics
get_storage_stats <- function() {
  files <- list_data_files()
  if (nrow(files) == 0) {
    message("No data files found")
    return(tibble())
  }
  files |> group_by(data_type) |>
    summarise(n_files = n(), total_size_mb = sum(size) / 1024^2, latest_file = max(modification_time), .groups = "drop") |>
    arrange(data_type)
}

message("\n✓ Poseidat storage functions loaded")
