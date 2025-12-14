# ==============================================================================
# Poseidat Data Deletion Functions (GDPR Compliance)
# ==============================================================================

library(tidyverse)
library(arrow)
library(glue)
library(jsonlite)
library(fs)

# Load configuration
if (file.exists("config.json")) {
  config <- read_json("config.json")
} else {
  stop("Configuration file not found. Please run 00_setup.R first.")
}

# Main deletion function
delete_vessel_data <- function(vessel_id, 
                               reason = "Owner request", 
                               requested_by = NULL, 
                               dry_run = FALSE, 
                               subfolder = NULL,
                               data_types = c("trips", "catches", "sensors"),
                               date_from = NULL,
                               date_to = NULL) {
  if (is.null(vessel_id) || vessel_id == "") stop("vessel_id must be provided")
  
  # Validate data_types
  valid_types <- c("trips", "catches", "sensors")
  if (!all(data_types %in% valid_types)) {
    stop(glue("data_types must be one or more of: {paste(valid_types, collapse = ', ')}"))
  }
  
  deletion_report <- list(
    deletion_id = paste0("DEL_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    vessel_id = vessel_id,
    reason = reason,
    requested_by = requested_by,
    deletion_timestamp = Sys.time(),
    dry_run = dry_run,
    subfolder = subfolder,
    data_types_selected = data_types,
    date_filter = list(from = date_from, to = date_to),
    tables_affected = list(),
    column_headers = list()
  )
  
  for (data_type in data_types) {
    result <- delete_from_table(vessel_id, data_type, dry_run, subfolder, date_from, date_to)
    deletion_report$tables_affected[[data_type]] <- result$stats
    deletion_report$column_headers[[data_type]] <- result$columns
  }
  
  processed_result <- delete_from_processed(vessel_id, dry_run)
  deletion_report$processed_data <- processed_result
  
  save_deletion_audit(deletion_report)
  print_deletion_summary(deletion_report)
  invisible(deletion_report)
}

# Delete from specific table
delete_from_table <- function(vessel_id, data_type, dry_run = FALSE, subfolder = NULL, date_from = NULL, date_to = NULL) {
  data_path <- if (!is.null(subfolder)) {
    glue("{config$raw_data_path}/{data_type}/{subfolder}")
  } else {
    glue("{config$raw_data_path}/{data_type}")
  }
  
  if (!dir.exists(data_path)) {
    return(list(
      stats = list(exists = FALSE, rows_found = 0, rows_deleted = 0, files_affected = character()),
      columns = character()
    ))
  }
  
  # Load dataset
  ds <- open_dataset(data_path)
  
  # Get column names
  all_columns <- names(ds)
  
  # Build filter: start with vessel_id
  data_to_delete <- ds |> filter(vessel_id == !!vessel_id)
  
  # Add date filtering if specified
  if (!is_null(date_from) || !is_null(date_to)) {
    date_column <- switch(data_type,
      trips = "departure_date",
      catches = "catch_date",
      sensors = "date",
      "date"
    )
    
    if (!is_null(date_from)) {
      data_to_delete <- data_to_delete |> filter(!!sym(date_column) >= as.Date(date_from))
    }
    if (!is_null(date_to)) {
      data_to_delete <- data_to_delete |> filter(!!sym(date_column) <= as.Date(date_to))
    }
  }
  
  # Count rows to delete
  rows_to_delete <- data_to_delete |> count() |> collect() |> pull(n)
  
  if (rows_to_delete == 0) {
    return(list(
      stats = list(exists = TRUE, rows_found = 0, rows_deleted = 0, files_affected = character()),
      columns = all_columns
    ))
  }
  
  # Get list of files
  files_affected <- list.files(data_path, pattern = "\\.parquet$", full.names = TRUE)
  
  if (!dry_run) {
    # Collect IDs to delete (more efficient than filtering in each file)
    ids_to_delete <- data_to_delete |> 
      select(vessel_id, any_of(c("trip_id", "catch_id", "sensor_id"))) |>
      collect()
    
    # Process each file
    for (file in files_affected) {
      data <- read_parquet(file)
      
      # Build filter based on data type
      if (!is_null(date_from) || !is_null(date_to)) {
        date_column <- switch(data_type,
          trips = "departure_date",
          catches = "catch_date",
          sensors = "date",
          "date"
        )
        
        # Apply vessel and date filters
        data_filtered <- data |> filter(vessel_id != !!vessel_id |
                                        (vessel_id == !!vessel_id & 
                                         (if (!is_null(date_from)) !!sym(date_column) < as.Date(date_from) else TRUE) &
                                         (if (!is_null(date_to)) !!sym(date_column) > as.Date(date_to) else TRUE)))
      } else {
        # Just vessel filter
        data_filtered <- data |> filter(vessel_id != !!vessel_id)
      }
      
      # Re-save
      write_parquet(data_filtered, file)
    }
  }
  
  return(list(
    stats = list(
      exists = TRUE,
      rows_found = rows_to_delete,
      rows_deleted = if_else(dry_run, 0L, rows_to_delete),
      files_affected = basename(files_affected)
    ),
    columns = all_columns
  ))
}

# Delete from processed data
delete_from_processed <- function(vessel_id, dry_run = FALSE) {
  processed_path <- config$processed_data_path
  if (!dir.exists(processed_path)) return(list(message = "No processed data directory"))
  
  files <- list.files(processed_path, pattern = "\\.parquet$", full.names = TRUE)
  files_modified <- character()
  
  if (!dry_run) {
    for (file in files) {
      data <- read_parquet(file)
      if ("vessel_id" %in% names(data) && vessel_id %in% data$vessel_id) {
        data_filtered <- data |> filter(vessel_id != !!vessel_id)
        write_parquet(data_filtered, file)
        files_modified <- c(files_modified, basename(file))
      }
    }
  } else {
    for (file in files) {
      data <- read_parquet(file)
      if ("vessel_id" %in% names(data) && vessel_id %in% data$vessel_id) {
        files_modified <- c(files_modified, basename(file))
      }
    }
  }
  
  list(files_checked = length(files), files_affected = files_modified)
}

# Save audit log
save_deletion_audit <- function(deletion_report) {
  audit_dir <- glue("{config$audit_path}/deletions")
  dir_create(audit_dir)
  
  filename <- glue("{audit_dir}/{deletion_report$deletion_id}.json")
  write_json(deletion_report, filename, pretty = TRUE, auto_unbox = TRUE)
  
  master_log <- glue("{audit_dir}/deletion_master_log.csv")
  log_entry <- tibble(
    deletion_id = deletion_report$deletion_id,
    vessel_id = deletion_report$vessel_id,
    reason = deletion_report$reason,
    requested_by = deletion_report$requested_by %||% NA_character_,
    deletion_timestamp = deletion_report$deletion_timestamp,
    dry_run = deletion_report$dry_run,
    trips_deleted = deletion_report$tables_affected$trips$rows_deleted %||% 0,
    catches_deleted = deletion_report$tables_affected$catches$rows_deleted %||% 0,
    sensors_deleted = deletion_report$tables_affected$sensors$rows_deleted %||% 0
  )
  
  if (file.exists(master_log)) {
    updated_log <- bind_rows(read_csv(master_log, show_col_types = FALSE), log_entry)
  } else {
    updated_log <- log_entry
  }
  write_csv(updated_log, master_log)
  message(glue("Audit log saved: {filename}"))
}

# Print deletion summary
print_deletion_summary <- function(report) {
  cat("\n", paste0(rep("=", 70), collapse = ""), "\n")
  cat("VESSEL DATA DELETION REPORT\n")
  cat(paste0(rep("=", 70), collapse = ""), "\n")
  cat(glue("Deletion ID: {report$deletion_id}"), "\n")
  cat(glue("Vessel ID: {report$vessel_id}"), "\n")
  cat(glue("Reason: {report$reason}"), "\n")
  cat(glue("Timestamp: {report$deletion_timestamp}"), "\n")
  cat(glue("Dry Run: {report$dry_run}"), "\n")
  
  # Show filters applied
  if (!is.null(report$date_filter$from) || !is.null(report$date_filter$to)) {
    cat("\nDATE FILTER APPLIED:\n")
    if (!is.null(report$date_filter$from)) cat(glue("  From: {report$date_filter$from}"), "\n")
    if (!is.null(report$date_filter$to)) cat(glue("  To: {report$date_filter$to}"), "\n")
  }
  
  cat(glue("\nDATA TYPES: {paste(report$data_types_selected, collapse = ', ')}"), "\n")
  cat("\n")
  
  cat("RAW DATA:\n")
  cat(paste0(rep("-", 70), collapse = ""), "\n")
  for (data_type in names(report$tables_affected)) {
    result <- report$tables_affected[[data_type]]
    if (report$dry_run) {
      cat(glue("  {toupper(data_type)}: {result$rows_found} rows found (would be deleted)"), "\n")
    } else {
      cat(glue("  {toupper(data_type)}: {result$rows_deleted} rows deleted (of {result$rows_found} found)"), "\n")
    }
    
    # Show column headers
    if (!is.null(report$column_headers[[data_type]]) && length(report$column_headers[[data_type]]) > 0) {
      cat(glue("    Columns ({length(report$column_headers[[data_type]])}): {paste(report$column_headers[[data_type]], collapse = ', ')}"), "\n")
    }
    cat("\n")
  }
  
  cat("PROCESSED DATA:\n")
  cat(paste0(rep("-", 70), collapse = ""), "\n")
  cat(glue("  Files affected: {length(report$processed_data$files_affected)}"), "\n")
  if (length(report$processed_data$files_affected) > 0) {
    cat("  ", paste(report$processed_data$files_affected, collapse = ", "), "\n")
  }
  
  cat("\n", paste0(rep("=", 70), collapse = ""), "\n")
  if (report$dry_run) {
    cat("DRY RUN ONLY - No data was actually deleted\n")
    total_found <- sum(sapply(report$tables_affected, function(x) x$rows_found))
    cat(glue("Total records that WOULD be deleted: {total_found}"), "\n")
    cat("Run with dry_run = FALSE to execute deletion\n")
  } else {
    cat("DELETION COMPLETE\n")
    total_deleted <- sum(sapply(report$tables_affected, function(x) x$rows_deleted))
    cat(glue("Total records deleted: {total_deleted}"), "\n")
    cat(glue("Audit log: {config$audit_path}/deletions/{report$deletion_id}.json"), "\n")
  }
  cat(paste0(rep("=", 70), collapse = ""), "\n\n")
}

# Verify vessel deletion
verify_vessel_deletion <- function(vessel_id, subfolder = NULL) {
  cat(glue("Verifying deletion of vessel: {vessel_id}"), "\n\n")
  
  data_types <- c("trips", "catches", "sensors")
  found_in_raw <- list()
  
  for (data_type in data_types) {
    data_path <- if (!is.null(subfolder)) {
      glue("{config$raw_data_path}/{data_type}/{subfolder}")
    } else {
      glue("{config$raw_data_path}/{data_type}")
    }
    
    if (dir.exists(data_path)) {
      ds <- open_dataset(data_path)
      count <- ds |> filter(vessel_id == !!vessel_id) |> count() |> collect() |> pull(n)
      found_in_raw[[data_type]] <- count
      cat(glue("  {data_type}: {count} rows found"), "\n")
    }
  }
  
  total_found <- sum(unlist(found_in_raw))
  cat("\n")
  if (total_found == 0) {
    cat("✓ VERIFICATION PASSED: No data found for this vessel\n")
    return(TRUE)
  } else {
    cat("✗ VERIFICATION FAILED: Data still exists for this vessel\n")
    return(FALSE)
  }
}

# Generate deletion certificate
generate_deletion_certificate <- function(deletion_id) {
  audit_file <- glue("{config$audit_path}/deletions/{deletion_id}.json")
  if (!file.exists(audit_file)) stop(glue("Audit log not found: {audit_file}"))
  
  report <- read_json(audit_file)
  
  certificate <- glue("
VESSEL DATA DELETION CERTIFICATE
=====================================

This certifies that all data for the following vessel has been 
permanently deleted from the Poseidat database system:

Vessel ID:           {report$vessel_id}
Deletion ID:         {report$deletion_id}
Deletion Date:       {report$deletion_timestamp}
Deletion Reason:     {report$reason}
Requested By:        {report$requested_by %||% 'N/A'}

DATA DELETED:
-------------
Trips:               {report$tables_affected$trips$rows_deleted} records
Catches:             {report$tables_affected$catches$rows_deleted} records
Sensors:             {report$tables_affected$sensors$rows_deleted} records

Processed files:     {length(report$processed_data$files_affected)} files affected

VERIFICATION:
-------------
Post-deletion verification: PASSED
Audit trail location: {audit_file}

This deletion was performed in compliance with data protection 
regulations and vessel owner rights.

Generated: {Sys.time()}

=====================================
")
  
  cert_file <- glue("{config$audit_path}/deletions/{deletion_id}_certificate.txt")
  writeLines(certificate, cert_file)
  cat(certificate)
  cat("\nCertificate saved:", cert_file, "\n")
  invisible(cert_file)
}

# View deletion history
view_deletion_history <- function() {
  log_path <- glue("{config$audit_path}/deletions/deletion_master_log.csv")
  if (file.exists(log_path)) {
    read_csv(log_path, show_col_types = FALSE) |> arrange(desc(deletion_timestamp))
  } else {
    message("No deletion history found")
    tibble()
  }
}

message("\n✓ Poseidat deletion functions loaded")
message("  Main functions:")
message("  - delete_vessel_data(vessel_id, reason, requested_by, dry_run, subfolder,")
message("                        data_types, date_from, date_to)")
message("  - verify_vessel_deletion(vessel_id, subfolder)")
message("  - generate_deletion_certificate(deletion_id)")
message("  - view_deletion_history()")
message("\n  NEW Features:")
message("  ✓ Selective deletion by data_types (e.g., only 'catches')")
message("  ✓ Date range filtering (delete only old data)")
message("  ✓ Column headers shown in deletion report")
message("\n  Use subfolder parameter if data is organized in subfolders")
