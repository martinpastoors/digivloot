# ============================================================================
# UPDATED STORAGE & SYNC FUNCTIONS  (replaces relevant sections in
# Poseidat_functions.R)
#
# Changes from previous version
# ──────────────────────────────────────────────────────────────────────────
# 1. NEW MONTHLY PATH SCHEME
#    raw/<source>/<vessel>/YYYY/MM/<vessel>_<device>_YYYYMM.parquet
#    One file per vessel × device × calendar month.  Content grows
#    additively with every sync; never overwritten, only appended.
#
# 2. read_latest_datetime()  [NEW — Step A]
#    Scans existing monthly parquets and returns the max(datetime) for a
#    given source / vessel / device.  Used by sync_device() as the sync
#    start point, making the state RDS file redundant for that purpose.
#
# 3. .save_monthly_additive()  [NEW — Step B, replaces .save_by_month()]
#    For each calendar month in new data:
#      a. Read existing monthly parquet (if any)
#      b. Bind new records
#      c. Deduplicate on entry_id
#      d. Write back
#
# 4. migrate_to_monthly()  [NEW — Step C]
#    ONE-TIME migration.  Reads all existing date-stamped raw parquets,
#    merges them into the new monthly files, then (optionally) deletes
#    the old files.  Run once from the console before switching to the
#    new daily sync.
#
# 5. sync_device()  [UPDATED — Step D]
#    - start_time now comes from read_latest_datetime() (falls back to
#      lookback_days only when no parquet exists yet)
#    - end_time is now a parameter (default Sys.time()) so you can
#      sync up to any moment you specify
#    - Calls .save_monthly_additive() instead of .save_by_month()
#    - update_last_sync_time() still called for logging / print_sync_state()
#
# 6. sync_all_vessels()  [UPDATED — Step E]
#    - Passes end_time through to sync_device()
#    - lookback_days kept as a parameter (only relevant for first-ever sync)
#
# HOW TO APPLY
# ──────────────────────────────────────────────────────────────────────────
# Replace the following sections in Poseidat_functions.R:
#   "STORAGE PATH HELPERS"         → paste new Step A + B helpers
#   "INCREMENTAL SYNC — one device" → paste new sync_device()
#   "SYNC ALL VESSELS"             → paste new sync_all_vessels()
# Add migrate_to_monthly() anywhere (e.g. after consolidate_data()).
# ============================================================================


# ============================================================================
# STEP A — read_latest_datetime()
# ============================================================================

#' Find the most recent observation datetime stored on disk for one device
#'
#' Scans all monthly parquet files under raw/<source>/<vessel>/
#' that match the device_id and returns the maximum datetime value.
#' Returns NULL when no files exist yet (first-ever sync).
#'
#' @param source      Character. "dbmatic" | "efice" | "pefa"
#' @param vessel_name Character. Vessel name as used in folder structure
#' @param device_id   Character. Device UUID
#' @return POSIXct (UTC) or NULL
#' @export
read_latest_datetime <- function(source, vessel_name, device_id) {

  raw_root <- .raw_dir(source, vessel_name)
  if (!dir.exists(raw_root)) return(NULL)

  safe_dev <- .safe(device_id)

  # Match both old date-stamped files and new monthly files
  files <- list.files(raw_root,
                      pattern   = glue(".*{safe_dev}.*\\.parquet$"),
                      recursive = TRUE,
                      full.names = TRUE)

  if (length(files) == 0) return(NULL)

  # Read only the datetime column from each file for speed
  max_dt <- purrr::map(files, function(fp) {
    tryCatch({
      dt <- arrow::read_parquet(fp, col_select = "datetime")$datetime
      if (length(dt) == 0 || all(is.na(dt))) return(as.POSIXct(NA))
      max(dt, na.rm = TRUE)
    }, error = function(e) as.POSIXct(NA))
  }) |>
    purrr::compact() |>
    purrr::keep(~ !is.na(.x))

  if (length(max_dt) == 0) return(NULL)

  as.POSIXct(max(unlist(lapply(max_dt, as.numeric))),
             origin = "1970-01-01", tz = "UTC")
}


# ============================================================================
# STEP B — updated storage path helpers + .save_monthly_additive()
# ============================================================================

# New monthly filename: <vessel>_<device>_YYYYMM.parquet
# Folder structure unchanged: raw/<source>/<vessel>/YYYY/MM/

.raw_monthly_path <- function(source, vessel_name, device_id, year, month) {
  dir <- file.path(.raw_dir(source, vessel_name),
                   sprintf("%04d", year),
                   sprintf("%02d", month))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, sprintf("%s_%s_%04d%02d.parquet",
                         .safe(vessel_name),
                         .safe(device_id),
                         year, month))
}


#' Save new measurements into additive monthly parquet files
#'
#' For each calendar month present in \code{measurements}:
#'   1. Read the existing monthly parquet (if any)
#'   2. Bind the new records
#'   3. Deduplicate on entry_id
#'   4. Write back
#'
#' @param measurements Tibble of newly downloaded records
#' @param source       Character
#' @param vessel_name  Character
#' @param device_id    Character
#' @return Character vector of file paths written
.save_monthly_additive <- function(measurements, source, vessel_name, device_id) {

  measurements <- measurements |>
    dplyr::mutate(.year  = as.integer(format(datetime, "%Y")),
                  .month = as.integer(format(datetime, "%m")))

  months <- measurements |>
    dplyr::distinct(.year, .month) |>
    dplyr::arrange(.year, .month)

  paths <- character(nrow(months))

  for (i in seq_len(nrow(months))) {
    yr    <- months$.year[i]
    mo    <- months$.month[i]
    chunk <- measurements |>
      dplyr::filter(.year == yr, .month == mo) |>
      dplyr::select(-.year, -.month)

    fp <- .raw_monthly_path(source, vessel_name, device_id, yr, mo)

    # Merge with existing file if present
    if (file.exists(fp)) {
      existing <- tryCatch(
        arrow::read_parquet(fp),
        error = function(e) {
          warning(sprintf("Could not read existing parquet %s: %s — overwriting",
                          basename(fp), e$message))
          tibble::tibble()
        }
      )
      chunk <- dplyr::bind_rows(existing, chunk) |>
        dplyr::distinct(entry_id, .keep_all = TRUE) |>
        dplyr::arrange(datetime)
    }

    arrow::write_parquet(chunk, fp)
    paths[i] <- fp
  }

  paths
}


# ============================================================================
# STEP C — migrate_to_monthly()
# ============================================================================

#' ONE-TIME migration: convert date-stamped raw parquets to monthly files
#'
#' Reads all existing files matching the old naming pattern
#' (<vessel>_<device>_YYYYMMDD_to_YYYYMMDD.parquet), merges them into
#' the new monthly files using .save_monthly_additive(), and optionally
#' removes the old files.
#'
#' Run this ONCE from the console before switching to the new daily sync:
#'
#'   source("R/poseidat/Poseidat_functions.R")
#'   setup_poseidat_sync()
#'   vessel_config <- readRDS(file.path(.onedrive(), "config",
#'                                      "poseidat_vessels.rds"))
#'   migrate_to_monthly(vessel_config, dry_run = TRUE)   # preview first
#'   migrate_to_monthly(vessel_config, dry_run = FALSE)  # then execute
#'
#' @param vessel_config Tibble. From readRDS("poseidat_vessels.rds")
#' @param sources       Character vector or NULL (all sources in config)
#' @param vessel_names  Character vector or NULL (all enabled vessels)
#' @param dry_run       Logical. If TRUE, report what would happen without
#'                      writing or deleting anything (default TRUE for safety)
#' @param delete_old    Logical. If TRUE, remove old date-stamped files after
#'                      successful migration (default TRUE).
#'                      Ignored when dry_run = TRUE.
#' @return Invisible tibble summarising actions taken / planned
#' @export
migrate_to_monthly <- function(vessel_config,
                                sources      = NULL,
                                vessel_names = NULL,
                                dry_run      = TRUE,
                                delete_old   = TRUE) {

  if (dry_run)
    cat("\n*** DRY RUN — no files will be written or deleted ***\n\n")
  else
    cat("\n*** LIVE RUN — files will be written and old files deleted ***\n\n")

  cfg <- vessel_config |> dplyr::filter(enabled)
  if (!is.null(sources))      cfg <- cfg |> dplyr::filter(source %in% sources)
  if (!is.null(vessel_names)) cfg <- cfg |> dplyr::filter(vessel_name %in% vessel_names)

  if (nrow(cfg) == 0) {
    message("No enabled vessels match the requested filters.")
    return(invisible(tibble::tibble()))
  }

  # Old filename pattern: ends with _YYYYMMDD_to_YYYYMMDD.parquet
  old_pattern <- "_\\d{8}_to_\\d{8}\\.parquet$"

  summary_rows <- list()

  for (i in seq_len(nrow(cfg))) {
    vessel <- cfg[i, ]
    raw_root <- .raw_dir(vessel$source, vessel$vessel_name)

    if (!dir.exists(raw_root)) {
      cat(sprintf("  [%s] %s — no raw folder, skipping\n",
                  vessel$source, vessel$vessel_name))
      next
    }

    old_files <- list.files(raw_root,
                             pattern   = old_pattern,
                             recursive = TRUE,
                             full.names = TRUE)

    if (length(old_files) == 0) {
      cat(sprintf("  [%s] %s — no old-style files found\n",
                  vessel$source, vessel$vessel_name))
      next
    }

    cat(sprintf("\n  [%s] %s — %d old file(s) found\n",
                vessel$source, vessel$vessel_name, length(old_files)))

    # Group files by device_id (extracted from filename)
    device_ids <- unique(sub(
      pattern     = sprintf("^%s_(.+)_\\d{8}_to_\\d{8}\\.parquet$",
                             .safe(vessel$vessel_name)),
      replacement = "\\1",
      x           = basename(old_files)
    ))

    for (dev_id in device_ids) {
      safe_dev  <- .safe(dev_id)
      dev_files <- old_files[grepl(safe_dev, old_files, fixed = TRUE)]

      # Tally total records before migration
      n_old <- sum(purrr::map_int(dev_files, function(fp) {
        tryCatch(nrow(arrow::read_parquet(fp)), error = function(e) 0L)
      }))

      cat(sprintf("    Device %-45s  %d file(s)  %s records\n",
                  substr(dev_id, 1, 45),
                  length(dev_files),
                  format(n_old, big.mark = ",")))

      if (!dry_run) {
        # Read and combine all old files for this device
        all_data <- purrr::map_dfr(dev_files, function(fp) {
          tryCatch(arrow::read_parquet(fp),
                   error = function(e) {
                     warning(sprintf("Skipping unreadable file: %s", fp))
                     tibble::tibble()
                   })
        }) |>
          dplyr::distinct(entry_id, .keep_all = TRUE) |>
          dplyr::arrange(datetime)

        if (nrow(all_data) == 0) {
          cat("      → No readable data, skipping\n")
          next
        }

        # Save into new monthly files (additive merge with any existing monthly)
        paths_written <- .save_monthly_additive(
          all_data, vessel$source, vessel$vessel_name, dev_id
        )
        cat(sprintf("      → Written %d monthly file(s)\n", length(paths_written)))

        # Delete old date-stamped files if requested
        if (delete_old) {
          purrr::walk(dev_files, function(fp) {
            if (file.remove(fp))
              cat(sprintf("      → Deleted: %s\n", basename(fp)))
            else
              warning(sprintf("Could not delete: %s", fp))
          })
        }

        summary_rows <- c(summary_rows, list(tibble::tibble(
          source        = vessel$source,
          vessel_name   = vessel$vessel_name,
          device_id     = dev_id,
          old_files     = length(dev_files),
          old_records   = n_old,
          monthly_files = length(paths_written),
          deleted       = delete_old
        )))
      }
    }
  }

  # Clean up empty YYYY/MM subdirectories left behind after deletion
  if (!dry_run && delete_old) {
    for (i in seq_len(nrow(cfg))) {
      vessel   <- cfg[i, ]
      raw_root <- .raw_dir(vessel$source, vessel$vessel_name)
      subdirs  <- list.dirs(raw_root, recursive = TRUE, full.names = TRUE)
      # Remove deepest first (sort descending by path length)
      subdirs  <- subdirs[order(nchar(subdirs), decreasing = TRUE)]
      purrr::walk(subdirs, function(d) {
        if (d != raw_root && length(list.files(d)) == 0)
          unlink(d, recursive = FALSE)
      })
    }
  }

  cat("\n=== Migration complete ===\n")

  summary_tbl <- if (length(summary_rows) > 0)
    dplyr::bind_rows(summary_rows)
  else
    tibble::tibble()

  invisible(summary_tbl)
}


# ============================================================================
# STEP D — updated sync_device()
# ============================================================================

#' Sync one vessel/device combination
#'
#' Start time is determined from the most recent record already on disk
#' (via read_latest_datetime()).  Falls back to \code{lookback_days} only
#' when no parquet exists yet for this device.
#'
#' @param source              Character. "dbmatic" | "efice" | "pefa"
#' @param journal_id          Character. Vessel journal UUID
#' @param device_id           Character. Device UUID
#' @param vessel_name         Character. Used as folder name
#' @param lookback_days       Integer.   Days to fetch on very first sync
#'                            (default 30).  Ignored on subsequent syncs.
#' @param end_time            POSIXct or NULL.  Upper bound of fetch window.
#'                            Defaults to Sys.time().
#' @param force_full          Logical.  Fetch from 2020-01-01 when TRUE.
#'                            Should not be used in the daily runner.
#' @param page_size           Integer.  API page size (default 1000)
#' @param sleep_between_pages Numeric.  Seconds between pages (default 0.1)
#' @return Tibble of newly downloaded records (invisibly)
#' @export
sync_device <- function(source, journal_id, device_id,
                         vessel_name         = "unknown",
                         lookback_days       = 30,
                         end_time            = NULL,
                         force_full          = FALSE,
                         page_size           = 1000,
                         sleep_between_pages = 0.1) {

  if (is.null(end_time)) end_time <- Sys.time()

  log_poseidat("Starting sync", source = source, vessel_id = journal_id)

  # ── Determine start of fetch window ──────────────────────────────────────
  if (force_full) {

    start_time <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
    log_poseidat("FULL sync — from 2020-01-01",
                 source = source, vessel_id = journal_id)

  } else {

    latest_on_disk <- read_latest_datetime(source, vessel_name, device_id)

    if (is.null(latest_on_disk)) {
      # No data on disk yet — first sync
      start_time <- end_time - (lookback_days * 86400)
      log_poseidat(sprintf("First sync — last %d days", lookback_days),
                   source = source, vessel_id = journal_id)
    } else {
      # Resume from last known record (small 1-hour overlap for safety)
      start_time <- latest_on_disk - 3600
      log_poseidat(sprintf("Incremental from %s (latest on disk)",
                           format(latest_on_disk, "%Y-%m-%d %H:%M:%S")),
                   source = source, vessel_id = journal_id)
    }
  }

  log_poseidat(sprintf("Range: %s → %s",
                       format(start_time, "%Y-%m-%d %H:%M:%S"),
                       format(end_time,   "%Y-%m-%d %H:%M:%S")),
               source = source, vessel_id = journal_id)

  # ── Download ──────────────────────────────────────────────────────────────
  measurements <- tryCatch(
    download_measurements(source, journal_id, device_id,
                          start_time, end_time,
                          page_size           = page_size,
                          sleep_between_pages = sleep_between_pages),
    error = function(e) {
      log_poseidat(sprintf("Download failed: %s", e$message),
                   level = "ERROR", source = source, vessel_id = journal_id)
      tibble::tibble()
    }
  )

  if (nrow(measurements) == 0) {
    log_poseidat("No new data returned",
                 source = source, vessel_id = journal_id)
    return(invisible(tibble::tibble()))
  }

  log_poseidat(sprintf("Downloaded %d records", nrow(measurements)),
               source = source, vessel_id = journal_id)

  # ── Save — additive monthly parquets ─────────────────────────────────────
  tryCatch({
    paths <- .save_monthly_additive(measurements, source, vessel_name, device_id)
    log_poseidat(sprintf("Merged into %d monthly file(s):", length(paths)),
                 source = source, vessel_id = journal_id)
    for (p in paths)
      log_poseidat(sprintf("  → %s", p), source = source, vessel_id = journal_id)
  }, error = function(e) {
    log_poseidat(sprintf("Save failed: %s", e$message),
                 level = "ERROR", source = source, vessel_id = journal_id)
  })

  # ── Update state (kept for logging / print_sync_state()) ─────────────────
  max_dt <- max(measurements$datetime, na.rm = TRUE)
  update_last_sync_time(source, journal_id, device_id,
                         max_dt, nrow(measurements))

  log_poseidat(sprintf("Done — latest record: %s",
                       format(max_dt, "%Y-%m-%d %H:%M:%S")),
               source = source, vessel_id = journal_id)

  invisible(measurements)
}


# ============================================================================
# STEP E — updated sync_all_vessels()
# ============================================================================

#' Sync all enabled vessels in vessel_config
#'
#' Iterates over all enabled rows, auto-discovers devices per vessel,
#' and calls sync_device() for each.
#'
#' @param vessel_config       Tibble. From Poseidat_config.R
#' @param sources             Character vector or NULL (all sources)
#' @param vessel_names        Character vector or NULL (all enabled vessels)
#' @param lookback_days       Integer. Days for first-ever sync (default 30)
#' @param end_time            POSIXct or NULL. Upper bound (default Sys.time())
#' @param force_full          Logical. Full re-sync from 2020-01-01
#' @param page_size           Integer
#' @param sleep_between_pages Numeric
#' @return Named list (one entry per source__journal_id__device_id)
#' @export
sync_all_vessels <- function(vessel_config,
                              sources             = NULL,
                              vessel_names        = NULL,
                              lookback_days       = 30,
                              end_time            = NULL,
                              force_full          = FALSE,
                              page_size           = 1000,
                              sleep_between_pages = 0.1) {

  if (is.null(end_time)) end_time <- Sys.time()

  cfg <- vessel_config |> dplyr::filter(enabled)
  if (!is.null(sources))      cfg <- cfg |> dplyr::filter(source %in% sources)
  if (!is.null(vessel_names)) cfg <- cfg |> dplyr::filter(vessel_name %in% vessel_names)

  if (nrow(cfg) == 0) {
    message("No enabled vessels match the requested sources/vessels.")
    return(invisible(list()))
  }

  log_poseidat(sprintf(
    "=== Poseidat sync starting — %d vessel(s), source(s): %s, end: %s ===",
    nrow(cfg),
    paste(unique(cfg$source), collapse = ", "),
    format(end_time, "%Y-%m-%d %H:%M:%S")
  ))

  results <- list()

  for (i in seq_len(nrow(cfg))) {
    vessel <- cfg[i, ]

    log_poseidat(sprintf("--- [%d/%d] %s (%s) ---",
                         i, nrow(cfg), vessel$vessel_name, vessel$source),
                 source = vessel$source)

    # Auto-discover devices
    devices <- tryCatch(
      get_devices(vessel$source, vessel$journal_id),
      error = function(e) {
        log_poseidat(sprintf("Device discovery failed: %s", e$message),
                     level = "ERROR", source = vessel$source,
                     vessel_id = vessel$journal_id)
        tibble::tibble()
      }
    )

    if (nrow(devices) == 0) {
      log_poseidat("No devices found — skipping",
                   level   = "WARNING",
                   source  = vessel$source,
                   vessel_id = vessel$journal_id)
      next
    }

    for (j in seq_len(nrow(devices))) {
      dev <- devices[j, ]
      key <- paste(vessel$source, vessel$journal_id, dev$device_id, sep = "__")

      log_poseidat(sprintf("  Device %d/%d: %s",
                           j, nrow(devices), dev$device_name),
                   source = vessel$source, vessel_id = vessel$journal_id)

      results[[key]] <- tryCatch(
        sync_device(
          source              = vessel$source,
          journal_id          = vessel$journal_id,
          device_id           = dev$device_id,
          vessel_name         = vessel$vessel_name,
          lookback_days       = lookback_days,
          end_time            = end_time,
          force_full          = force_full,
          page_size           = page_size,
          sleep_between_pages = sleep_between_pages
        ),
        error = function(e) {
          log_poseidat(sprintf("Sync error: %s", e$message),
                       level     = "ERROR",
                       source    = vessel$source,
                       vessel_id = vessel$journal_id)
          tibble::tibble()
        }
      )

      Sys.sleep(1)
    }

    if (i < nrow(cfg)) Sys.sleep(3)
  }

  log_poseidat("=== Poseidat sync completed ===")
  invisible(results)
}
