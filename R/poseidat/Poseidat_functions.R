# ============================================================================
# Poseidat_functions.R
#
# All functions for syncing, storing and analysing Poseidat vessel sensor data.
# Source this file in any script that needs Poseidat functionality.
#
# Prerequisites:
#   - .Renviron entries for API tokens and ONEDRIVE_POSEIDAT path
#   - vessel_config saved via Poseidat_config.R
#   - Call setup_poseidat_sync() once per session before using sync functions
#
# Storage scheme (v2 — additive monthly Parquet)
#   raw/<source>/<vessel>/YYYY/MM/<vessel>_<device>_YYYYMM.parquet
#   One file per vessel × device × calendar month.
#   New records are merged into the existing file on every sync; duplicates
#   are removed on entry_id.  No separate consolidation step needed.
#
# Sync start time
#   Determined automatically from read_latest_datetime(), which scans
#   existing Parquet files and returns max(datetime) for the device.
#   Falls back to lookback_days only when no Parquet exists yet (first sync).
#   A small 1-hour overlap is added for safety.
#
# Migration from v1 (date-stamped files)
#   Use migrate_to_monthly() to convert existing
#   <vessel>_<device>_YYYYMMDD_to_YYYYMMDD.parquet files to the new scheme.
#   Run once from the console before switching to the new daily sync:
#     migrate_to_monthly(vessel_config, dry_run = TRUE)   # preview
#     migrate_to_monthly(vessel_config, dry_run = FALSE)  # execute
#
# ============================================================================
#
# FUNCTION INVENTORY
# ============================================================================
#
# --- A. SESSION SETUP -------------------------------------------------------
#   setup_poseidat_sync()
#     Reads API credentials from .Renviron and validates the OneDrive path.
#     Creates the directory skeleton. Call once at the start of each session.
#
#   .onedrive()
#     Returns the OneDrive base path from the ONEDRIVE_POSEIDAT env variable.
#
# --- B. LOGGING & STATE -----------------------------------------------------
#   log_poseidat(msg, level, source, vessel_id)
#     Writes a timestamped log entry to a monthly log file and the console.
#
#   get_sync_state()  /  save_sync_state(state)
#     Read / write the sync state RDS (last sync times per device).
#
#   update_last_sync_time(source, journal_id, device_id, sync_time, record_count)
#     Updates the sync state after a successful device sync.
#
#   print_sync_state()
#     Prints a formatted table of last sync times for all tracked devices.
#
# --- C. API ADAPTERS (internal) ---------------------------------------------
#   .get_adapter(source)
#     Returns a list(base_url, token, get_vessels, get_devices, get_meas)
#     for the given source ("dbmatic" | "efice" | "pefa").
#
#   .dbmatic_get_vessels / .dbmatic_get_devices / .dbmatic_get_measurements
#   .efice_get_vessels   / .efice_get_devices   / .efice_get_measurements
#   .pefa_get_vessels    / .pefa_get_devices     / .pefa_get_measurements
#     Source-specific API implementations. Not called directly — used as
#     function references inside the list returned by .get_adapter().
#
#   .pefa_get_token()
#     Mints a fresh PEFA access token via the Azure AD B2C ROPC flow.
#     Not called directly — used by .pefa_get_bearer_token().
#
#   .pefa_token_cache / .pefa_get_bearer_token(force_refresh, refresh_buffer_sec)
#     Session-level cache for the PEFA access token. Returns the cached
#     token unless it is missing or near expiry, in which case it calls
#     .pefa_get_token() and refreshes the cache. Called by .get_adapter()
#     each time a "pefa" adapter is requested.
#
# --- D. VESSEL & DEVICE HELPERS ---------------------------------------------
#   get_vessels(source)
#     Returns a tibble of vessels accessible via the given source API:
#     journal_id, vessel_name, cfr, flag_state.
#
#   print_vessels(source, vessel_config)
#     Prints a formatted gt table of all accessible vessels, including CFR,
#     flag state, and whether the vessel is present in vessel_config.
#
#   get_devices(source, journal_id, vessel_name)
#     Returns a tibble of devices for a vessel (sampled from recent entries):
#     device_id, device_name, device_type.
#
#   print_devices(source, vessel_config, vessel_name)
#     Prints a formatted gt table of all devices for all vessels (or one
#     vessel), with the friendly label from vessel_config resolved alongside
#     the raw device UUID and value_type.
#
#   get_journal_id(vessel_config, vessel_name)
#     Looks up journal_id for a vessel_name in vessel_config.
#
#   get_device_by_label(vessel_config, vessel_name, label)
#     Looks up a device UUID by its friendly label in vessel_config.
#
# --- E. DOWNLOAD & SYNC -----------------------------------------------------
#   download_measurements(source, journal_id, device_id,
#                         start_time, end_time, page_size, sleep_between_pages)
#     Downloads all measurements for one device/period into a tibble.
#     Does NOT save to disk — use sync_device() to save.
#
#   read_latest_datetime(source, vessel_name, device_id)
#     Scans existing monthly Parquet files and returns max(datetime).
#     Returns NULL when no files exist yet (triggers first-sync fallback).
#
#   sync_device(source, journal_id, device_id, vessel_name,
#               lookback_days, end_time, force_full, page_size,
#               sleep_between_pages)
#     Downloads new records and merges into additive monthly Parquet files.
#     Start time derived from read_latest_datetime(); end_time defaults to
#     Sys.time() but can be set to any POSIXct.
#
#   sync_all_vessels(vessel_config, vessel_names, sources,
#                    lookback_days, end_time, force_full, page_size,
#                    sleep_between_pages)
#     Iterates over all enabled vessels in vessel_config and calls
#     sync_device() for every device found on each vessel.
#
#   migrate_to_monthly(vessel_config, sources, vessel_names, dry_run, delete_old)
#     ONE-TIME migration: merges old date-stamped Parquet files into
#     the new additive monthly scheme. Run with dry_run = TRUE first.
#
# --- F. DATA ACCESS ---------------------------------------------------------
#   poseidat_files(source)
#     Lists all Parquet files on disk with file size and row counts.
#
#   poseidat_value_types(source)
#     Summary table: n_records + first/last obs per vessel/device/value_type.
#     Useful for a quick inventory of what has been synced.
#
#   poseidat_daily(source, vessel_name, value_type, start_date, end_date)
#     Daily record counts per vessel/device/value_type.
#     Useful for spotting gaps and unexpected spikes.
#
#   poseidat_summary(source, value_type, nonzero)
#     Statistical summary (n, min, mean, max) per vessel/value_type.
#
#   poseidat_filter_save(keep_value_types, source)
#     Re-saves Parquet files keeping only the specified value_types.
#     Use to slim storage after deciding which sensor types are needed.
#
# --- G. THINNING & POSITIONS ------------------------------------------------
#   poseidat_thin(source, vessel_name, value_type, start_date, end_date,
#                 interval_min, save)
#     Aggregates raw ~10-second data to N-minute intervals (default 1 min).
#     Type-aware: mean lat/lon for POSITION, mean traction for TRAWL_TENSION,
#     mean value_numeric for all scalar types.
#     Optionally saves to processed/thinned/.
#
#   poseidat_positions(source, vessel_name, data_source,
#                      start_date, end_date, thin_minutes)
#     Loads POSITION records from parquet, ready for mapping or time series.
#     data_source: "thinned" (default) | "raw".
#     thin_minutes: optional display-level thinning (keep one fix per N min).
#
# --- H. FREQUENCY ANALYSIS --------------------------------------------------
#   build_poseidat_frequency(vessel_config, sources, start_date, end_date,
#                            interval_minutes)
#     Builds a weekly frequency table of observation intervals per
#     vessel/device. Returns n_obs and avg_interval_sec per week.
#
#   plot_poseidat_frequency(freq, metric, max_weeks)
#     Heatmap of observation frequency or average interval per vessel/device/week.
#     metric: "n_obs" (default) | "avg_interval".
#
#   table_poseidat_frequency(freq, metric, max_weeks)
#     Formatted gt table of frequency statistics, pivoted weeks as columns.
#
# --- I. AUDIT ---------------------------------------------------------------
#   audit_new_sensors(vessel_config, source, sample_days, sleep_sec)
#     Compares live devices returned by the API against device_labels in
#     vessel_config. Reports unlabelled/unseen devices, their value_types,
#     approximate first-seen date, and record count.
#     Returns a tibble; empty result means nothing new has been activated.
#
# --- J. PLOTS ---------------------------------------------------------------
#   poseidat_map(pos, source, vessel_name, data_source,
#                start_date, end_date, thin_minutes, show_points, show_lines)
#     Interactive leaflet map of vessel tracks, coloured by vessel.
#     Pass a pre-built pos tibble from poseidat_positions(), or let the
#     function load data via data_source. data_source: "thinned" | "raw".
#     show_points / show_lines (both default TRUE) toggle circle markers
#     and connecting lines independently -- at least one must stay TRUE.
#
#   poseidat_availability(source, vessel_name, start_date, end_date)
#     Daily tile plot (calendar heatmap) showing data presence per
#     vessel and value_type. Green = data, orange = zero/NA, grey = absent.
#     NOTE: grid is a full cross-join of every value_type against every
#     vessel fleet-wide, so a vessel can show a grey tile for a sensor it
#     never had. Use poseidat_sensor_availability() below to avoid that.
#
#   poseidat_sensor_availability(source, vessel_name, device_id, start_date,
#                                end_date, by, return_data)
#     Same idea as poseidat_availability(), but each vessel's grid is
#     scoped to only the sensors THAT vessel has ever produced, over that
#     sensor's own observed window. Grey tiles are always genuine gaps,
#     never "not applicable." Uses a single pruned scan of raw/ (see
#     .poseidat_scan_raw_daily()) instead of two full-archive scans, so
#     filtering by device_id or a narrow date range meaningfully reduces
#     how much gets read. by = "value_type" (default) | "device_id" to
#     split rows per device instead of aggregating across them -- when
#     split by device, rows are labelled with the friendly device_label
#     (e.g. "FUEL_1") from vessel_config where available, falling back to
#     a short device UUID suffix otherwise.
#     return_data = TRUE returns the underlying tibble instead of the plot.
#
#   poseidat_sensor_sample(source, vessel_name, device_id, data_source,
#                          start_date, end_date, by, n)
#     Stratified sample of n rows per vessel x sensor (and per device_id
#     when by = "device_id", which also adds the friendly device_label),
#     for spot-checking data content alongside
#     poseidat_sensor_availability(). Defaults to thinned data for speed;
#     data_source = "raw" available when needed.
#
#   poseidat_plot_timeseries(value_type, data_source, source, vessel_name,
#                            device_label, start_date, end_date, nonzero,
#                            by_device, facet_by, scales, ncol,
#                            position_component)
#     Time series of any scalar value_type, one coloured line per vessel.
#     Pass "POSITION" as value_type to plot latitude and/or longitude
#     (controlled by position_component: "latitude" | "longitude" | "both").
#     data_source: "thinned" (default) | "raw" | "overlay".
#     X-axis breaks adapt automatically to the plotted period.
#
#   poseidat_plot_overview(source, vessel_name, value_type,
#                          data_source, start_date, end_date, nonzero, scales,
#                          geom_type, point_size)
#     Facet grid of all scalar value_types (rows) x all vessels (columns).
#     data_source: "thinned" (default) | "raw".
#     X-axis breaks adapt to the plotted period.
#     geom_type: "line" (default) | "point" | "both". point_size controls
#     point size when points are drawn.
#
#   poseidat_plot_trawl(vessel_name, data_source, start_date, end_date,
#                       towing_only, min_traction, show_length, scales, ncol)
#     Time series of TRAWL_TENSION: traction per warp (port/center/starboard)
#     and optionally shooted wire length.
#     data_source: "thinned" (default) | "raw".
#     Facet by vessel; scales and ncol exposed.
#
#   poseidat_plot_operational(vessel_name, data_source, start_date, end_date,
#                             nonzero, scales, ncol)
#     Facet grid of the four key operational variables:
#     fuel consumption, speed (SOG), traction center, depth.
#     Rows = variable, columns = vessel (or facet_wrap when single vessel).
#     data_source: "thinned" (default) | "raw".
#     X-axis breaks adapt to the plotted period.
#
# --- K. TRIPS & FISHING OPERATIONS -------------------------------------------
#   Two-stage segmentation inspired by the iapesca R-package (IFREMER):
#   Stage 1 splits a vessel's POSITION history into trips that start and
#   end in a fishing harbour; stage 2 splits each trip into individual
#   fishing operations (hauls/tows), using speed, towing force and catch-
#   processing events as independent signals that are reconciled afterward.
#
#   poseidat_identify_trips(positions, harbours, harbour_radius_km,
#                           speed_threshold_knots, min_trip_hours,
#                           bridge_gap_mins)
#     STAGE 1. Detects trips from POSITION data (e.g. poseidat_positions())
#     via distance-to-nearest-harbour + speed run detection. harbours
#     defaults to the built-in North Sea/Channel list (.default_fishing_
#     harbours()) if not supplied. Returns list(positions, trips) — a trip
#     table and the input positions tagged with trip_id.
#
#   poseidat_flag_operations_speed(positions, speed_min, speed_max,
#                                  min_duration_min, bridge_gap_min)
#     STAGE 2a. Flags candidate fishing operations from speed_over_ground
#     alone (working-speed window, e.g. ~1-5 kn for towed gear).
#
#   poseidat_flag_operations_traction(trawl_data, trips, traction_col,
#                                     traction_threshold_kg,
#                                     min_duration_min, bridge_gap_min)
#     STAGE 2b. Flags candidate fishing operations from TRAWL_TENSION
#     towing force (traction_center by default). Assigns trip_id to the
#     (separate) trawl device stream via a non-equi join against `trips`.
#
#   poseidat_flag_operations_catch(catch_data, trips, default_haul_minutes)
#     STAGE 2c. Turns kisten/elog_trek catch-weighing records into
#     candidate fishing operations directly (haul-level ground truth, no
#     run detection needed).
#
#   poseidat_reconcile_fishing_operations(..., merge_gap_min)
#     STAGE 2d. Merges overlapping/near candidate windows from any number
#     of the flag_operations_* tibbles above into one set of fishing
#     operations per trip, with a confidence rating ("high" if a catch
#     record anchors it, "medium" if traction confirms it, "low" if
#     speed-only).
#
#   poseidat_identify_fishing_operations(positions, trips, trawl_data,
#                                        catch_data, speed_min, speed_max,
#                                        traction_col, traction_threshold_kg,
#                                        min_duration_min, bridge_gap_min,
#                                        merge_gap_min)
#     Orchestrates all of STAGE 2 in one call. Returns list(operations,
#     positions, trawl_data) with operation_id joined onto positions (and
#     trawl_data, if supplied).
#
#   poseidat_write_trip_ids(vessel_name, detected, harbours,
#                           harbour_radius_km, speed_threshold_knots,
#                           min_trip_hours, bridge_gap_mins, interval_min,
#                           n_backups, dry_run)
#     Persists a trip identification: writes trip_id + a globally-unique
#     trip_uid ("<vessel_code>_####", e.g. "UK158_0007") onto every row of
#     each vessel's thinned Parquet file, and saves the trip table itself
#     to processed/trips/poseidat_trips.parquet as the authoritative
#     record. The write MERGES with what's already on disk -- only vessels
#     present in the trips being written are replaced, everyone else is
#     carried through untouched -- and keeps `n_backups` (default 2)
#     rotating backups of that file. dry_run = TRUE (default) previews
#     without writing. Pass an already-computed poseidat_identify_trips()
#     result as `detected` to persist exactly what you inspected, instead
#     of having this function re-run trip detection internally.
#
#   poseidat_update_trip_ids(vessel_name, lookback_days, harbours,
#                            harbour_radius_km, speed_threshold_knots,
#                            min_trip_hours, bridge_gap_mins, interval_min,
#                            n_backups, dry_run)
#     INCREMENTAL trip update for routine/daily use -- unlike
#     poseidat_write_trip_ids(detected = NULL), which re-derives trips
#     across the ENTIRE time series every call, this only re-runs trip
#     detection over the last `lookback_days` (default 30) per vessel.
#     Trips that already finished before that window are kept exactly as
#     they were (same trip_id/trip_uid, not reallocated); anything in the
#     window is redetected, which naturally extends a trip that was still
#     open last time or adds a new one. A trip flagged as still open is
#     never cut short by lookback_days, and a vessel with no trips yet
#     gets a full first-time run. Delegates the actual write (with merge +
#     backups) to poseidat_write_trip_ids().
#
#   NOTE: poseidat_identify_trips() and poseidat_identify_fishing_
#   operations() already operate on multiple vessels in a single call --
#   every step is grouped by vessel_name internally. Load positions for
#   several (or all) vessels at once via poseidat_positions(vessel_name =
#   NULL) rather than looping.
#
# --- L. FUEL CONSUMPTION & DATA STALENESS ------------------------------------
#   poseidat_fuel_devices(vessel_config, fuel_keywords)
#     Fuel devices per vessel (device_id -> label, e.g. FUEL_1) from
#     vessel_config$device_labels.
#
#   poseidat_fuel_obs(vessel_config, fuel_devices, data_source, sources,
#                     vessel_name, start_date, end_date, fuel_value_type)
#     Loads FUEL_CONSUMPTION observations for all (or selected) vessels in
#     one arrow query. data_source: "thinned" (default) | "raw".
#
#   poseidat_fuel_daily(fuel_obs, flow_unit, max_gap_min, tz,
#                       start_date, end_date)
#     Integrates flow rate to litres per day per vessel x device, with
#     coverage_pct and active_hours. Gaps > max_gap_min are not filled.
#
#   poseidat_fuel_daily_vessel(daily_device, exclude_devices, return_devices)
#     Sums the fuel devices of each vessel per day; return-flow meters can
#     be subtracted.
#
#   poseidat_plot_fuel_daily(daily, by_device, min_coverage)
#     Bar chart of daily fuel consumption, one panel per vessel.
#
#   poseidat_check_fuel_files(vessel_name, data_source, sources,
#                             start_date, fuel_value_type)
#     Diagnostic: which parquet files deliver fuel rows for which days.
#
#   poseidat_check_staleness(vessel_config, sensor_gap_hours,
#                            vessel_silent_days, lookback_days,
#                            exclude_value_types, now, save_csv)
#     Flags SENSOR STALE (type lags behind the vessel's own latest data),
#     VESSEL SILENT and NO DATA. Logs warnings and writes
#     logs/poseidat_staleness_latest.csv. Called daily from
#     Poseidat_sync_daily.R.
#
# ============================================================================

library(httr2)
library(dplyr)
library(purrr)
library(arrow)
library(glue)
library(lubridate)
library(tidyr)


setup_poseidat_sync <- function() {

  onedrive_base <- Sys.getenv("ONEDRIVE_POSEIDAT", unset = "")
  if (!nzchar(onedrive_base)) {
    stop(
      "ONEDRIVE_POSEIDAT is not set.\n",
      "Add it to your .Renviron (usethis::edit_r_environ()):\n",
      "  ONEDRIVE_POSEIDAT = C:/Users/<you>/OneDrive/Poseidat"
    )
  }

  options(poseidat_onedrive = onedrive_base)

  # Create directory skeleton
  for (d in c("config", "logs", "raw", "processed")) {
    dir.create(file.path(onedrive_base, d), recursive = TRUE, showWarnings = FALSE)
  }

  # Initialise state file
  state_file <- file.path(onedrive_base, "config", "sync_state.rds")
  if (!file.exists(state_file)) {
    saveRDS(list(created = Sys.time(), last_sync = NULL, vessels = list()),
            state_file)
    message("  Created initial sync state file")
  }

  # Validate credentials (warn, don't stop — user may only use one source)
  # PEFA uses an Azure AD B2C ROPC flow (see .pefa_get_token()), not a
  # static bearer token, so it needs five variables rather than one.
  source_map <- list(
    DBMATIC_API_URL = "dbmatic", DBMATIC_TOKEN  = "dbmatic",
    EFICE_API_URL   = "efice",   EFICE_TOKEN    = "efice",
    PEFA_API_URL    = "pefa",    PEFA_TOKEN_URL = "pefa",
    PEFA_CLIENT_ID  = "pefa",    PEFA_SCOPE     = "pefa",
    PEFA_USERNAME   = "pefa",    PEFA_PASSWORD  = "pefa"
  )
  for (var in names(source_map)) {
    if (!nzchar(Sys.getenv(var))) {
      message(sprintf("  ⚠ Env var %s not set (needed for %s source)",
                      var, source_map[[var]]))
    }
  }

  message("✓ Poseidat sync configured")
  message(sprintf("  OneDrive root : %s", onedrive_base))
}

.onedrive <- function() {
  od <- getOption("poseidat_onedrive", "")
  if (!nzchar(od)) stop("Poseidat not configured — run setup_poseidat_sync() first.")
  od
}


# ============================================================================
# LOGGING
# ============================================================================

#' Write timestamped log entry to monthly file and console
#'
#' @param msg       Character. Message text
#' @param level     Character. INFO | WARNING | ERROR
#' @param source    Character. "dbmatic" | "efice" | NULL
#' @param vessel_id Character. Optional journal UUID
log_poseidat <- function(msg, level = "INFO", source = NULL, vessel_id = NULL) {
  log_dir  <- file.path(.onedrive(), "logs")
  log_file <- file.path(log_dir,
                        sprintf("poseidat_sync_%s.log", format(Sys.Date(), "%Y%m")))
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  src_str    <- if (!is.null(source))    sprintf("[%s]",     source)    else ""
  vessel_str <- if (!is.null(vessel_id)) sprintf("[%s]", substr(vessel_id, 1, 8)) else ""
  entry <- sprintf("[%s] %s%s%s: %s\n",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   level, src_str, vessel_str, msg)
  cat(entry, file = log_file, append = TRUE)
  cat(entry)
}


# ============================================================================
# STATE MANAGEMENT
# ============================================================================

.state_file <- function() file.path(.onedrive(), "config", "sync_state.rds")

get_sync_state <- function() {
  f <- .state_file()
  if (file.exists(f)) return(readRDS(f))
  list(created = Sys.time(), last_sync = NULL, vessels = list())
}

save_sync_state <- function(state) {
  dir.create(dirname(.state_file()), recursive = TRUE, showWarnings = FALSE)
  saveRDS(state, .state_file())
  invisible(TRUE)
}
update_last_sync_time <- function(source, journal_id, device_id,
                                  sync_time, record_count = 0) {
  key   <- paste(source, journal_id, device_id, sep = "__")
  state <- get_sync_state()
  if (is.null(state$vessels)) state$vessels <- list()

  state$vessels[[key]] <- list(
    last_sync_time      = sync_time,
    last_sync_timestamp = Sys.time(),
    last_record_count   = record_count
  )
  state$last_sync <- Sys.time()
  save_sync_state(state)
  invisible(TRUE)
}


# ============================================================================
# GENERIC HTTP HELPER
# ============================================================================

#' Perform one authenticated GET request and return parsed JSON
#'
#' @param base_url  Character. API root URL (no trailing slash)
#' @param api_token Character. Bearer token
#' @param endpoint  Character. Path fragment appended to base_url
#' @param params    List.      Named query parameters (NULLs removed)
#' @return Parsed JSON (list)
.http_get <- function(base_url, api_token, endpoint, params = list(),
                      timeout_sec = 30, max_tries = 3) {

  params <- compact(params)

  resp <- request(paste0(base_url, "/", endpoint)) |>
    req_auth_bearer_token(api_token) |>
    req_url_query(!!!params, .multi = "comma") |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(timeout_sec) |>
    req_retry(max_tries = max_tries, backoff = ~ 5) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(sprintf(
      "API error %d (%s) on %s: %s",
      resp_status(resp), resp_status_desc(resp),
      endpoint, resp_body_string(resp)
    ))
  }

  resp_body_json(resp)
}



# ============================================================================
# API ADAPTERS
# Each adapter exposes three functions with a common signature:
#   .api_get_vessels(base_url, token)         → tibble(journal_id, vessel_name, ...)
#   .api_get_devices(base_url, token, jid)    → tibble(device_id, device_name, ...)
#   .api_get_measurements(base_url, token,    → tibble (measurements)
#                         jid, did, start, end, page_size, offset)
# ============================================================================

# ---- DBMatic adapter -------------------------------------------------------

.dbmatic_get_vessels <- function(base_url, token) {
  data <- .http_get(base_url, token, "journals")
  if (length(data) == 0) return(tibble(journal_id = character(),
                                       vessel_name = character(),
                                       cfr         = character(),
                                       flag_state  = character()))
  map_dfr(data, function(j) tibble(
    journal_id  = j$journal_id        %||% NA_character_,
    vessel_name = j$vessel$name        %||% NA_character_,
    flag_state  = j$vessel$flag_state  %||% NA_character_,
    cfr         = j$vessel$cfr         %||% NA_character_
  ))
}

.dbmatic_get_devices <- function(base_url, token, journal_id) {
  data <- .http_get(base_url, token,
                    glue("journals/{journal_id}/equipment_inventory"))
  equip_list <- data$equipment
  if (is.null(equip_list) || length(equip_list) == 0) return(tibble())

  map_dfr(equip_list, function(e) {
    if (is.null(e$devices) || length(e$devices) == 0) return(tibble())
    map_dfr(e$devices, function(d) tibble(
      equipment_id   = e$equipment_id  %||% NA_character_,
      equipment_name = e$name          %||% NA_character_,
      device_id      = d$device_id     %||% NA_character_,
      device_name    = d$name          %||% NA_character_,
      device_type    = d$type          %||% NA_character_
    ))
  }) |> filter(!is.na(device_id))
}

.dbmatic_get_measurements <- function(base_url, token,
                                       journal_id, device_id,
                                       start_time, end_time,
                                       page_size = 1000, offset = 0) {

  # Confirmed live (2026-08-03, Anna Hendrika): limit/offset are silently
  # IGNORED -- offset=0 and offset=40000 both returned the identical full
  # 60,480-row week, regardless of page_size. Same non-pagination behavior
  # as PEFA, despite this endpoint's params suggesting otherwise (and despite
  # the original code's untested assumption that it worked). Same fix:
  # short-circuit on offset > 0 so download_measurements()'s pagination loop
  # terminates after one real fetch instead of re-requesting the same full
  # set forever (this is exactly what caused the 45-minute runaway -- 62+
  # identical ~40s calls before being interrupted).
  if (offset > 0) return(tibble())

  data <- .http_get(
    base_url, token,
    glue("journals/{journal_id}/measurements/{device_id}"),
    params = list(
      start  = as.integer(as.numeric(start_time)),
      end    = as.integer(as.numeric(end_time)),
      limit  = page_size,
      offset = offset
    ),
    # Default 30s (fine for small vessel/device-list calls) is too short for
    # large measurement payloads -- confirmed 2026-08-03: a single page for
    # Anna Hendrika was 26MB and got cut off at 30s with 83% received.
    # .http_get() already retries (req_retry, built in above) -- this just
    # gives each attempt enough time to actually finish.
    timeout_sec = 300
  )
  if (length(data) == 0) return(tibble())

  # Confirmed live (2026-08-03, Anna Hendrika): DBMatic's real production
  # vessels use the SAME nested value{type, position/numeric/trawl_tension/
  # fuel_consumption} schema as EFICE/PEFA -- the original flat-scalar
  # rec$value parser above was only ever validated against the simpler
  # WW-9 Test vessel. Vectorized (tidyr::hoist) parse, same approach as
  # .pefa_get_measurements(). Unlike PEFA, this endpoint genuinely supports
  # limit/offset pagination -- no single-shot short-circuit needed here.
  #
  # DBMatic HAS a real `immutable` boolean field (PEFA doesn't); revision
  # uses ".000Z" millisecond format (closer to EFICE than PEFA's "+00:00").
  raw <- tibble(rec = data) |>
    tidyr::hoist(rec,
      entry_id   = "entry_id",
      entry_type = "entry_type",
      journal_id = "journal_id",
      revision   = "revision",
      device_id  = "device_id",
      immutable  = "immutable",
      value_type = list("value", "type"),

      value_numeric = list("value", "numeric", "value"),

      latitude          = list("value", "position", "latitude"),
      longitude         = list("value", "position", "longitude"),
      course_made_good  = list("value", "position", "course_made_good"),
      speed_over_ground = list("value", "position", "speed_over_ground"),
      n_satellites      = list("value", "position", "number_of_satellites"),

      traction_port            = list("value", "trawl_tension", "traction_port"),
      traction_center          = list("value", "trawl_tension", "traction_center"),
      traction_starboard       = list("value", "trawl_tension", "traction_starboard"),
      shooted_length_port      = list("value", "trawl_tension", "shooted_length_port"),
      shooted_length_center    = list("value", "trawl_tension", "shooted_length_center"),
      shooted_length_starboard = list("value", "trawl_tension", "shooted_length_starboard"),

      fuel_current_consumption   = list("value", "fuel_consumption", "current_consumption"),
      fuel_avg_consumption       = list("value", "fuel_consumption", "current_average_consumption"),
      fuel_peak_consumption      = list("value", "fuel_consumption", "current_peak_consumption"),
      fuel_last_hour_consumption = list("value", "fuel_consumption", "last_hour_consumption"),
      fuel_last_day_consumption  = list("value", "fuel_consumption", "last_day_consumption"),

      # SCALE (weighing scale, e.g. MARELEC3853 -- confirmed live 2026-08-04,
      # Anna Hendrika): a discrete catch-weighing EVENT, not a continuous
      # sensor reading -- carries species/haul dimensions alongside the
      # weight, unlike every other value_type seen so far. See warning at
      # the bottom of this function about why poseidat_thin() is NOT
      # appropriate for this value_type as-is.
      scale_category    = list("value", "scale", "category"),
      scale_product      = list("value", "scale", "product"),
      scale_weight        = list("value", "scale", "weight"),
      scale_haul_number = list("value", "scale", "haul_number"),

      .remove = TRUE
    )

  raw |>
    dplyr::mutate(
      entry_id   = as.character(entry_id),
      journal_id = as.character(journal_id),
      device_id  = as.character(device_id),
      entry_type = as.character(entry_type),
      value_type = as.character(value_type),
      immutable  = dplyr::coalesce(as.logical(immutable), FALSE),

      revision   = lubridate::ymd_hms(revision, tz = "UTC", quiet = TRUE),
      datetime   = revision,

      # Force a consistent DOUBLE/INTEGER type even when a column is 100% NA
      # for this device's ENTIRE file (e.g. latitude on a FUEL_MAIN-only
      # device) -- otherwise arrow::write_parquet() infers an ambiguous
      # type (often bool) from an all-NA column, which then fails to merge
      # with POSITION's file (real doubles) under arrow::open_dataset()
      # schema unification. Confirmed live 2026-08-06 on Anna Hendrika.
      latitude                  = as.numeric(latitude),
      longitude                 = as.numeric(longitude),
      course_made_good          = as.numeric(course_made_good),
      speed_over_ground         = as.numeric(speed_over_ground),
      n_satellites              = as.integer(n_satellites),
      traction_port             = as.numeric(traction_port),
      traction_center           = as.numeric(traction_center),
      traction_starboard        = as.numeric(traction_starboard),
      shooted_length_port       = as.numeric(shooted_length_port),
      shooted_length_center     = as.numeric(shooted_length_center),
      shooted_length_starboard  = as.numeric(shooted_length_starboard),
      fuel_avg_consumption      = as.numeric(fuel_avg_consumption),
      fuel_peak_consumption     = as.numeric(fuel_peak_consumption),
      fuel_last_hour_consumption = as.numeric(fuel_last_hour_consumption),
      fuel_last_day_consumption  = as.numeric(fuel_last_day_consumption),
      scale_weight              = as.numeric(scale_weight),

      scale_category    = as.character(scale_category),
      scale_product      = as.character(scale_product),
      scale_haul_number = as.integer(scale_haul_number),

      # NOTE: value_numeric = weight for SCALE is provided for schema
      # consistency ONLY. poseidat_thin()'s per-minute MEAN is semantically
      # wrong for catch weights -- it discards species/haul_number entirely
      # and averages individual weighing events instead of summing them.
      # Analyze SCALE data from raw parquet directly, grouped by
      # haul_number/scale_product with sum(scale_weight), not via thinning.
      value_numeric = dplyr::case_when(
        value_type %in% c("POSITION", "TRAWL_TENSION") ~ NA_real_,
        value_type == "FUEL_CONSUMPTION" ~
          dplyr::coalesce(fuel_current_consumption, as.numeric(value_numeric)),
        value_type == "SCALE" ~ as.numeric(scale_weight),
        TRUE ~ as.numeric(value_numeric)
      )
    ) |>
    dplyr::select(
      entry_id, datetime, revision, journal_id, device_id, entry_type, value_type,
      value_numeric,
      latitude, longitude, course_made_good, speed_over_ground, n_satellites,
      traction_port, traction_center, traction_starboard,
      shooted_length_port, shooted_length_center, shooted_length_starboard,
      fuel_avg_consumption, fuel_peak_consumption,
      fuel_last_hour_consumption, fuel_last_day_consumption,
      scale_category, scale_product, scale_weight, scale_haul_number,
      immutable
    )
}


# ---- EFICE adapter ---------------------------------------------------------
# Confirmed endpoint structure: poseidat-journal.m-catch.com
#
# GET /journals                  -> vessel list
#   response: { items: [{ journal_id, vessel: { name, flag_state,
#                          hull_number } }], limit, skip, total }
#
# GET /journals/entries          -> measurements
#   REQUIRED params: journalId (camelCase), startDate, endDate (ISO datetime)
#   optional params: deviceId, limit, skip, sortBy, sortDirection
#   NOTE: returns 0 records without startDate/endDate — effectively required.
#
# Key differences from DBMatic:
#   - No separate device endpoint; device UUIDs discovered from entry samples
#   - Pagination: limit / skip  (not page_size / offset)
#   - Datetime params: startDate / endDate  (ISO "2025-01-01T00:00:00Z")
#   - journalId / deviceId are camelCase in query params
#   - POSITION records use value$position$* instead of value$numeric$value

.efice_get_vessels <- function(base_url, token) {
  data  <- .http_get(base_url, token, "journals")
  items <- data$items %||% list()

  if (length(items) == 0) return(tibble(journal_id  = character(),
                                        vessel_name = character(),
                                        cfr         = character(),
                                        flag_state  = character()))
  map_dfr(items, function(v) tibble(
    journal_id  = as.character(v$journal_id        %||% NA_character_),
    vessel_name = as.character(v$vessel$name        %||% NA_character_),
    cfr         = as.character(v$vessel$hull_number %||% NA_character_),
    flag_state  = as.character(v$vessel$flag_state  %||% "NLD")
  ))
}

# .efice_get_devices <- function(base_url, token, journal_id) {
#   # EFICE has no dedicated device endpoint — device UUIDs are discovered by
#   # probing each suffix slot individually.
#   #
#   # Strategy:
#   #   - Fetch a small sample to extract the shared UUID prefix for this journal
#   #     (all sensors on a vessel share the same prefix, differing only in the
#   #     last 12-character suffix, e.g. "000200000001", "000200000008").
#   #   - Probe each suffix slot 000200000001 through 000200000016 with a
#   #     limit=1 request — cheap and complete regardless of transmission frequency.
#   #   - Sensors with at least 1 entry ever are considered active (historical or
#   #     current).
#   fmt_dt   <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#   start_dt <- "2020-01-01T00:00:00Z"
#   end_dt   <- fmt_dt(Sys.time())
# 
#   # Step 1: fetch a small sample to obtain the shared prefix for this journal.
#   # Use a recent 90-day window so the API doesn't scan years of history for
#   # just 10 rows -- we only need any device_id to extract the prefix.
#   sample_start <- fmt_dt(Sys.time() - 90 * 86400)
# 
#   sample <- .http_get(base_url, token, "journals/entries",
#                       params = list(
#                         journalId     = journal_id,
#                         startDate     = sample_start,
#                         endDate       = end_dt,
#                         limit         = 10L,
#                         skip          = 0L,
#                         sortBy        = "entry_datetime",
#                         sortDirection = "descending"
#                       ),
#                       timeout_sec = 60)
# 
#   items <- sample$items %||% list()
#   if (length(items) == 0) return(tibble(device_id   = character(),
#                                         device_name = character(),
#                                         device_type = character()))
# 
#   sample_ids <- items |>
#     lapply(\(x) as.character(x$device_id %||% NA_character_)) |>
#     unlist() |>
#     (\(x) x[!is.na(x)])()
#   prefix <- substr(sample_ids[[1]], 1, 24)  # e.g. "61326139-7379-3463-0000-"
# 
#   # Step 2: probe each suffix slot — covers up to 16 sensors per vessel
#   suffixes   <- sprintf("0002%08d", 1:16)
#   candidates <- paste0(prefix, suffixes)
# 
#   found_ids <- character(0)
# 
#   for (cid in candidates) {
#     resp  <- tryCatch(
#       .http_get(base_url, token, "journals/entries",
#                 params = list(
#                   journalId = journal_id,
#                   deviceId  = cid,
#                   startDate = start_dt,
#                   endDate   = end_dt,
#                   limit     = 1L,
#                   skip      = 0L
#                 ),
#                 timeout_sec = 10,   # short timeout — probe only
#                 max_tries   = 1L),  # no retries for probes
#       error = function(e) NULL
#     )
#     total <- resp$total %||% 0L
#     if (total > 0L) {
#       found_ids <- c(found_ids, cid)
#     }
#   }
# 
#   if (length(found_ids) == 0) return(tibble(device_id   = character(),
#                                              device_name = character(),
#                                              device_type = character()))
# 
#   tibble(
#     device_id   = found_ids,
#     device_name = found_ids,   # EFICE provides no human-readable device name
#     device_type = "device-measurement"
#   )
# }

.efice_get_devices <- function(base_url, token, journal_id) {
  # EFICE has no dedicated device endpoint and the probe-based approach
  # (16 sequential API calls) is unreliable due to API timeouts.
  #
  # Device IDs are read directly from vessel_config$device_labels, which
  # is the authoritative source configured in Poseidat_config.R.
  # Use audit_new_sensors() periodically to check for newly added devices.
  
  cfg <- tryCatch(
    readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds")),
    error = function(e) NULL
  )
  
  if (is.null(cfg)) {
    message("    [efice_get_devices] Could not read vessel config — 0 devices")
    return(tibble(device_id = character(),
                  device_name = character(),
                  device_type = character()))
  }
  
  device_labels <- cfg |>
    dplyr::filter(journal_id == !!journal_id) |>
    dplyr::pull(device_labels) |>
    purrr::pluck(1)
  
  if (is.null(device_labels) || length(device_labels) == 0) {
    message(glue("    [efice_get_devices] No device_labels in config for {journal_id}"))
    return(tibble(device_id = character(),
                  device_name = character(),
                  device_type = character()))
  }
  
  tibble(
    device_id   = names(device_labels),
    device_name = unname(device_labels),
    device_type = "device-measurement"
  )
}



.efice_get_measurements <- function(base_url, token,
                                     journal_id, device_id,
                                     start_time, end_time,
                                     page_size = 1000, offset = 0) {
  fmt_dt <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  # NOTE: .http_get() times out when deviceId is passed via !!!params splicing.
  # Use req_url_query() directly -- confirmed to work in manual tests.
  resp <- request(paste0(base_url, "/journals/entries")) |>
    req_auth_bearer_token(token) |>
    req_url_query(
      journalId     = journal_id,
      deviceId      = device_id,
      startDate     = fmt_dt(start_time),
      endDate       = fmt_dt(end_time),
      limit         = page_size,
      skip          = offset,
      sortBy        = "entry_datetime",
      sortDirection = "ascending"
    ) |>
    req_error(is_error = \(r) FALSE) |>
    # 2026-11-26: was a hard-coded 300 s. A request over a long range for a
    # sensor with no data (e.g. Lub Senior FUEL_1-4 since 7 Sept) simply hangs
    # until the timeout, so 300 s cost 5 min per dead device per day. 120 s is
    # still well above what a normal daily page takes (10-40 s). Override with
    # options(poseidat.efice_timeout = ...) if needed.
    req_timeout(getOption("poseidat.efice_timeout", 120)) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(sprintf("EFICE API error %d on journals/entries: %s",
                 resp_status(resp), resp_body_string(resp)))
  }

  data  <- resp_body_json(resp)
  items <- data$items %||% list()
  if (length(items) == 0) return(tibble())

  # ---- Vectorized parse (tidyr::hoist), same approach as the DBMatic/PEFA
  # parsers. Replaces the old map_dfr() that built one tibble per record.
  #
  # Value structures by value$type:
  #   POSITION         -> value$position$*
  #   TRAWL_TENSION    -> value$trawl_tension$*
  #   FUEL_CONSUMPTION -> value$numeric$value, OR the richer
  #                       value$fuel_consumption$* object (as DBMatic/PEFA).
  #                       The old parser only read value$numeric$value, so a
  #                       fuel sensor sending the richer object ended up with
  #                       value_numeric = NA for every row (suspected cause of
  #                       Polar's empty November fuel files, 2026-11-26).
  #   All others       -> value$numeric$value
  raw <- tibble(rec = items) |>
    tidyr::hoist(rec,
      entry_id       = "entry_id",
      entry_datetime = "entry_datetime",
      revision       = "revision",
      journal_id     = "journal_id",
      device_id      = "device_id",
      entry_type     = "entry_type",
      immutable      = "immutable",
      value_type     = list("value", "type"),

      value_numeric  = list("value", "numeric", "value"),

      latitude          = list("value", "position", "latitude"),
      longitude         = list("value", "position", "longitude"),
      course_made_good  = list("value", "position", "course_made_good"),
      speed_over_ground = list("value", "position", "speed_over_ground"),
      n_satellites      = list("value", "position", "number_of_satellites"),

      traction_port            = list("value", "trawl_tension", "traction_port"),
      traction_center          = list("value", "trawl_tension", "traction_center"),
      traction_starboard       = list("value", "trawl_tension", "traction_starboard"),
      shooted_length_port      = list("value", "trawl_tension", "shooted_length_port"),
      shooted_length_center    = list("value", "trawl_tension", "shooted_length_center"),
      shooted_length_starboard = list("value", "trawl_tension", "shooted_length_starboard"),

      fuel_current_consumption   = list("value", "fuel_consumption", "current_consumption"),
      fuel_avg_consumption       = list("value", "fuel_consumption", "current_average_consumption"),
      fuel_peak_consumption      = list("value", "fuel_consumption", "current_peak_consumption"),
      fuel_last_hour_consumption = list("value", "fuel_consumption", "last_hour_consumption"),
      fuel_last_day_consumption  = list("value", "fuel_consumption", "last_day_consumption"),

      .remove = TRUE
    )

  raw |>
    dplyr::mutate(
      entry_id   = as.character(entry_id),
      journal_id = as.character(journal_id),
      device_id  = as.character(device_id),
      entry_type = as.character(entry_type),
      value_type = as.character(value_type),
      immutable  = dplyr::coalesce(as.logical(immutable), FALSE),

      # Same parsing as before (EFICE uses a plain "Z" suffix)
      datetime   = as.POSIXct(as.character(entry_datetime),
                              format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      revision   = as.POSIXct(as.character(revision),
                              format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),

      # Explicit types, also when a column is 100% NA (see DBMatic parser)
      latitude                   = as.numeric(latitude),
      longitude                  = as.numeric(longitude),
      course_made_good           = as.numeric(course_made_good),
      speed_over_ground          = as.numeric(speed_over_ground),
      n_satellites               = as.integer(n_satellites),
      traction_port              = as.numeric(traction_port),
      traction_center            = as.numeric(traction_center),
      traction_starboard         = as.numeric(traction_starboard),
      shooted_length_port        = as.numeric(shooted_length_port),
      shooted_length_center      = as.numeric(shooted_length_center),
      shooted_length_starboard   = as.numeric(shooted_length_starboard),
      fuel_current_consumption   = as.numeric(fuel_current_consumption),
      fuel_avg_consumption       = as.numeric(fuel_avg_consumption),
      fuel_peak_consumption      = as.numeric(fuel_peak_consumption),
      fuel_last_hour_consumption = as.numeric(fuel_last_hour_consumption),
      fuel_last_day_consumption  = as.numeric(fuel_last_day_consumption),

      value_numeric = dplyr::case_when(
        value_type %in% c("POSITION", "TRAWL_TENSION") ~ NA_real_,
        value_type == "FUEL_CONSUMPTION" ~
          dplyr::coalesce(as.numeric(value_numeric), fuel_current_consumption),
        TRUE ~ as.numeric(value_numeric)
      )
    ) |>
    dplyr::select(
      entry_id, datetime, revision, journal_id, device_id, entry_type, value_type,
      value_numeric,
      latitude, longitude, course_made_good, speed_over_ground, n_satellites,
      traction_port, traction_center, traction_starboard,
      shooted_length_port, shooted_length_center, shooted_length_starboard,
      fuel_avg_consumption, fuel_peak_consumption,
      fuel_last_hour_consumption, fuel_last_day_consumption,
      immutable
    )
}


# ---- PEFA token acquisition (Azure AD B2C ROPC) ----------------------------
# PEFA authenticates via an Azure AD B2C Resource Owner Password Credentials
# flow rather than a static bearer token (unlike DBMATIC_TOKEN/EFICE_TOKEN).
# Access tokens expire (~1 hour) so they are minted on demand and cached for
# the session -- see .pefa_get_bearer_token() below.
#
# .Renviron entries required:
#   PEFA_TOKEN_URL = https://pefab2c.b2clogin.com/pefab2c.onmicrosoft.com/b2c_1_ropc_auth/oauth2/v2.0/token
#   PEFA_CLIENT_ID = ...
#   PEFA_SCOPE     = ...
#   PEFA_USERNAME  = ...
#   PEFA_PASSWORD  = ...

.pefa_get_token <- function() {

  token_url <- Sys.getenv("PEFA_TOKEN_URL")
  client_id <- Sys.getenv("PEFA_CLIENT_ID")
  scope     <- Sys.getenv("PEFA_SCOPE")
  username  <- Sys.getenv("PEFA_USERNAME")
  password  <- Sys.getenv("PEFA_PASSWORD")

  required <- c(PEFA_TOKEN_URL = token_url, PEFA_CLIENT_ID = client_id,
               PEFA_SCOPE = scope, PEFA_USERNAME = username,
               PEFA_PASSWORD = password)
  missing_vars <- names(required)[!nzchar(required)]

  if (length(missing_vars) > 0) {
    stop(glue("Missing PEFA credential(s) in .Renviron: ",
             "{paste(missing_vars, collapse = ', ')}"))
  }

  resp <- request(token_url) |>
    req_body_form(
      client_id  = client_id,
      scope      = scope,
      username   = username,
      password   = password,
      grant_type = "password"
    ) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(30) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(glue("PEFA token request failed ({resp_status(resp)}): ",
             "{resp_body_string(resp)}"))
  }

  body <- resp_body_json(resp)

  if (is.null(body$access_token)) {
    stop("PEFA token response succeeded but contained no access_token -- ",
        "check client_id/scope/username/password.")
  }

  list(
    access_token = body$access_token,
    token_type   = body$token_type   %||% "Bearer",
    expires_in   = as.numeric(body$expires_in %||% 3600),
    fetched_at   = Sys.time()
  )
}


# ---- PEFA token cache (session-level) --------------------------------------
# Caches the access token in-session and only refreshes when within
# `refresh_buffer_sec` of expiry, so a sync run touching many devices/vessels
# does not mint a fresh token on every call to .get_adapter().

.pefa_token_cache <- new.env(parent = emptyenv())

.pefa_get_bearer_token <- function(force_refresh = FALSE, refresh_buffer_sec = 300) {

  cached <- .pefa_token_cache$token

  needs_refresh <- force_refresh || is.null(cached) ||
    as.numeric(difftime(Sys.time(), cached$fetched_at, units = "secs")) >
      (cached$expires_in - refresh_buffer_sec)

  if (needs_refresh) {
    cached <- .pefa_get_token()
    assign("token", cached, envir = .pefa_token_cache)

    # Logging is best-effort only. In a future::multisession worker (used by
    # sync_device_parallel()), setup_poseidat_sync() was never run in that
    # process, so .onedrive() has no configured path and log_poseidat() would
    # throw -- which must NOT prevent returning the token we already have.
    tryCatch(
      log_poseidat(
        glue("PEFA token refreshed (expires in {cached$expires_in}s)"),
        source = "pefa"
      ),
      error = function(e) NULL
    )
  }

  cached$access_token
}


# ---- PEFA adapter ----------------------------------------------------------
# PEFA is another distribution channel for Poseidat vessel journal data.
# Confirmed endpoint structure from https://digivloot.pefa.com/swagger/v1/swagger.json
# (OpenAPI 3.0.4, "Pefa Digivloot API" v1). No `servers` block in the spec --
# base URL is the bare origin, with /api/v1/... as part of each path.
#
# GET /api/v1/Journals                                    -> vessel list
#   Response: bare JSON array of Journal { journal_id, vessel: Vessel }
#   Vessel has BOTH a real `cfr` field and a separate `hull_number` --
#   unlike EFICE, which has no true cfr and fakes it from hull_number.
#
# GET /api/v1/Journals/{journal_id}/equipment_inventory    -> device inventory
#   Response: EquipmentInventory { equipment: [Equipment] }
#   Equipment { equipment_id, name, type, devices: [Device] }
#   Device { device_id, name, type, brand, product_no, serial_no, ... }
#   Structurally near-identical to the DBMatic shape -- only the path prefix
#   ("api/v1/") differs, so the parsing logic below mirrors .dbmatic_get_devices.
#
# GET /api/v1/Journals/{journal_id}/measurements/{device_id}?start=&end=
#   start/end are integer (int64) -- presumably Unix epoch seconds, as DBMatic.
#   NOTE: the spec defines NO limit/offset params for this endpoint, unlike
#   DBMatic/EFICE. .pefa_get_measurements below still delegates to the
#   DBMatic-style paginated parser as a placeholder -- this needs revisiting
#   before real measurement syncing, since skip-based paging may not apply
#   here. See TODO at .pefa_get_measurements().

.pefa_get_vessels <- function(base_url, token) {
  data <- .http_get(base_url, token, "api/v1/Journals")
  if (length(data) == 0) return(tibble(journal_id  = character(),
                                       vessel_name = character(),
                                       cfr         = character(),
                                       flag_state  = character()))
  map_dfr(data, function(j) tibble(
    journal_id  = as.character(j$journal_id       %||% NA_character_),
    vessel_name = as.character(j$vessel$name       %||% NA_character_),
    cfr         = as.character(j$vessel$cfr        %||% NA_character_),
    flag_state  = as.character(j$vessel$flag_state %||% NA_character_)
  ))
}

.pefa_get_devices <- function(base_url, token, journal_id) {
  data <- .http_get(base_url, token,
                    glue("api/v1/Journals/{journal_id}/equipment_inventory"))
  equip_list <- data$equipment
  if (is.null(equip_list) || length(equip_list) == 0) return(tibble())

  map_dfr(equip_list, function(e) {
    if (is.null(e$devices) || length(e$devices) == 0) return(tibble())
    map_dfr(e$devices, function(d) tibble(
      equipment_id   = as.character(e$equipment_id  %||% NA_character_),
      equipment_name = as.character(e$name           %||% NA_character_),
      device_id      = as.character(d$device_id      %||% NA_character_),
      device_name    = as.character(d$name           %||% NA_character_),
      device_type    = as.character(d$type           %||% NA_character_),
      # NOTE: name suggests "data guaranteed complete up to this date"
      # (a recency/freshness watermark), which may NOT be the same question
      # as "how far back does data go" -- inspect the actual values before
      # relying on this to plan a backfill start date.
      complete_up_to = if (!is.null(d$complete_up_to))
                        as.POSIXct(d$complete_up_to, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                      else as.POSIXct(NA)
    ))
  }) |> filter(!is.na(device_id))
}

# Confirmed via probe (2026-08-03): GET /api/v1/Journals/{journal_id}/
# measurements/{device_id}?start=&end= returns the ENTIRE result set for the
# requested window in one response -- there is no limit/offset param, and
# none is honoured (60,000 records for one device/7-days came back in a
# single call). download_measurements()'s pagination loop assumes
# `nrow(page) < page_size` signals the last page -- with no true paging,
# that condition is never met and the loop would re-request the same full
# set forever. Fixed here by short-circuiting: offset > 0 means "we already
# have everything", so return an empty tibble without hitting the network,
# which lets download_measurements() terminate normally after exactly 2 calls.
#
# Timestamp: response records carry `revision` (ISO 8601 WITH a +00:00/-05:00
# style offset -- not EFICE's "Z" suffix), no separate entry_datetime field.
# lubridate::ymd_hms() is used instead of as.POSIXct() because base R's %z
# parsing is unreliable with colon-separated offsets across platforms.
#
# Value union -- MeasurementType has far more variants than EFICE's three
# (ACCELERATION, CURRENT, ENERGY_CONSUMPTION, FORCE, HUMIDITY, MAGNETISM,
# ONOFF, POWER, PRESSURE, ROUTE, RPM, SCALE, SPATIAL_AXES, SPEED, SUBSURFACE,
# VOLTAGE, SPEEDLOG, WEATHER, plus POSITION/TRAWL_TENSION/FUEL_CONSUMPTION/
# DEPTH/TEMPERATURE). Only the types confirmed live on Catherina's six
# sensors are given dedicated parsing below (POSITION, TRAWL_TENSION,
# FUEL_CONSUMPTION); everything else falls through to the generic
# value$numeric$value reader, same as EFICE's catch-all. Extend the
# dedicated branches (mirroring the Scale/Speedlog/Weather/Subsurface/
# SpatialAxes schemas in swagger.json) if/when a vessel with those sensor
# types is added.
.pefa_get_measurements <- function(base_url, token,
                                    journal_id, device_id,
                                    start_time, end_time,
                                    page_size = 1000, offset = 0) {

  
  if (!inherits(start_time, "POSIXct")) start_time <- as.POSIXct(start_time, tz = "UTC")
  if (!inherits(end_time,   "POSIXct")) end_time   <- as.POSIXct(end_time,   tz = "UTC")
  
  # No true pagination exists -- the first call retrieves everything for
  # the window, so any subsequent call in download_measurements()'s loop
  # should return empty rather than re-fetching the same full set.
  if (offset > 0) return(tibble())

  resp <- request(base_url) |>
    req_url_path_append("api", "v1", "Journals", journal_id,
                        "measurements", device_id) |>
    req_auth_bearer_token(token) |>
    req_url_query(
      start = as.integer(as.numeric(start_time)),
      end   = as.integer(as.numeric(end_time))
    ) |>
    # Force HTTP/1.1 -- sidesteps HTTP/2 stream-level errors (RST_STREAM/
    # CANCEL) seen intermittently against this host on large single-shot
    # downloads. HTTP/1.1 has no "stream" concept for this error class to
    # occur in. curl's CURL_HTTP_VERSION_1_1 = 2L.
    req_options(http_version = 2) |>
    req_error(is_error = \(r) FALSE) |>
    # req_timeout(300) |>   # large single-shot payloads -- mirrors EFICE's timeout
    req_timeout(getOption("poseidat.pefa_timeout", 300)) |>
    
    # Retry transient connection/stream failures (this endpoint previously had
    # NO retry logic at all, unlike .http_get() -- a single dropped connection
    # failed the whole call outright).
    req_retry(max_tries = 3, backoff = ~ 5,
              is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(glue("PEFA API error {resp_status(resp)} on measurements: ",
             "{resp_body_string(resp)}"))
  }

  data <- resp_body_json(resp)
  if (length(data) == 0) return(tibble())

  # ---- Vectorized parse (tidyr::hoist, no row-wise loop) --------------------
  # The previous version called tibble() once per record inside map_dfr() --
  # for tens of thousands of records that per-row construction dominates
  # runtime. hoist() pulls each (possibly nested) field out across the WHOLE
  # list column in one vectorized pass, so parsing no longer scales with the
  # same per-row overhead.
  raw <- tibble(rec = data) |>
    tidyr::hoist(rec,
      entry_id   = "entry_id",
      entry_type = "entry_type",
      journal_id = "journal_id",
      revision   = "revision",
      device_id  = "device_id",
      value_type = list("value", "type"),

      value_numeric = list("value", "numeric", "value"),

      latitude          = list("value", "position", "latitude"),
      longitude         = list("value", "position", "longitude"),
      course_made_good  = list("value", "position", "course_made_good"),
      speed_over_ground = list("value", "position", "speed_over_ground"),
      n_satellites      = list("value", "position", "number_of_satellites"),

      traction_port            = list("value", "trawl_tension", "traction_port"),
      traction_center          = list("value", "trawl_tension", "traction_center"),
      traction_starboard       = list("value", "trawl_tension", "traction_starboard"),
      shooted_length_port      = list("value", "trawl_tension", "shooted_length_port"),
      shooted_length_center    = list("value", "trawl_tension", "shooted_length_center"),
      shooted_length_starboard = list("value", "trawl_tension", "shooted_length_starboard"),

      fuel_current_consumption   = list("value", "fuel_consumption", "current_consumption"),
      fuel_avg_consumption       = list("value", "fuel_consumption", "current_average_consumption"),
      fuel_peak_consumption      = list("value", "fuel_consumption", "current_peak_consumption"),
      fuel_last_hour_consumption = list("value", "fuel_consumption", "last_hour_consumption"),
      fuel_last_day_consumption  = list("value", "fuel_consumption", "last_day_consumption"),

      .remove = TRUE
    )

  raw |>
    mutate(
      entry_id   = as.character(entry_id),
      journal_id = as.character(journal_id),
      device_id  = as.character(device_id),
      entry_type = as.character(entry_type),
      value_type = as.character(value_type),

      # revision/datetime parsed once, vectorized across the whole column --
      # ymd_hms() handles the "+00:00"-style offset directly.
      revision   = lubridate::ymd_hms(revision, tz = "UTC", quiet = TRUE),
      datetime   = revision,

      # Force a consistent DOUBLE/INTEGER type even when a column is 100% NA
      # for this device's ENTIRE file (e.g. latitude on a FUEL_MAIN-only
      # device) -- otherwise arrow::write_parquet() infers an ambiguous type
      # (often bool) from an all-NA column, which then fails to merge with
      # POSITION's file (real doubles) under arrow::open_dataset() schema
      # unification. Same root cause confirmed live 2026-08-06 on DBMatic;
      # applying the identical fix here since this parser has the same shape.
      latitude                  = as.numeric(latitude),
      longitude                 = as.numeric(longitude),
      course_made_good          = as.numeric(course_made_good),
      speed_over_ground         = as.numeric(speed_over_ground),
      n_satellites              = as.integer(n_satellites),
      traction_port             = as.numeric(traction_port),
      traction_center           = as.numeric(traction_center),
      traction_starboard        = as.numeric(traction_starboard),
      shooted_length_port       = as.numeric(shooted_length_port),
      shooted_length_center     = as.numeric(shooted_length_center),
      shooted_length_starboard  = as.numeric(shooted_length_starboard),
      fuel_avg_consumption      = as.numeric(fuel_avg_consumption),
      fuel_peak_consumption     = as.numeric(fuel_peak_consumption),
      fuel_last_hour_consumption = as.numeric(fuel_last_hour_consumption),
      fuel_last_day_consumption  = as.numeric(fuel_last_day_consumption),

      # FUEL_CONSUMPTION also populates this so poseidat_thin()'s generic
      # "scalar" branch picks it up unchanged; POSITION/TRAWL_TENSION have
      # their own dedicated columns instead.
      #
      # NOTE: confirmed live (2026-08-03) that Catherina's fuel sensor sends
      # FUEL_CONSUMPTION via the plain value.numeric.value wrapper (like
      # DEPTH), NOT the richer value.fuel_consumption object the OpenAPI
      # spec describes -- fuel_current_consumption is NA here in practice.
      # coalesce() prefers the rich object when present (some other vessel's
      # sensor might genuinely send it) and falls back to the plain numeric
      # reading otherwise, so this works either way.
      value_numeric = dplyr::case_when(
        value_type %in% c("POSITION", "TRAWL_TENSION") ~ NA_real_,
        value_type == "FUEL_CONSUMPTION" ~
          dplyr::coalesce(fuel_current_consumption, as.numeric(value_numeric)),
        TRUE ~ as.numeric(value_numeric)
      ),

      immutable = FALSE   # spec has no immutable flag for PEFA records
    ) |>
    select(
      entry_id, datetime, revision, journal_id, device_id, entry_type, value_type,
      value_numeric,
      latitude, longitude, course_made_good, speed_over_ground, n_satellites,
      traction_port, traction_center, traction_starboard,
      shooted_length_port, shooted_length_center, shooted_length_starboard,
      fuel_avg_consumption, fuel_peak_consumption,
      fuel_last_hour_consumption, fuel_last_day_consumption,
      immutable
    )
}


.get_adapter <- function(source) {
  switch(source,
    dbmatic = list(
      base_url      = Sys.getenv("DBMATIC_API_URL"),
      token         = Sys.getenv("DBMATIC_TOKEN"),
      get_vessels   = .dbmatic_get_vessels,
      get_devices   = .dbmatic_get_devices,
      get_meas      = .dbmatic_get_measurements
    ),
    efice = list(
      base_url      = Sys.getenv("EFICE_API_URL"),
      token         = Sys.getenv("EFICE_TOKEN"),
      get_vessels   = .efice_get_vessels,
      get_devices   = .efice_get_devices,
      get_meas      = .efice_get_measurements
    ),
    pefa = list(
      base_url      = Sys.getenv("PEFA_API_URL"),
      token         = .pefa_get_bearer_token(),   # minted + cached, not a static .Renviron value
      get_vessels   = .pefa_get_vessels,
      get_devices   = .pefa_get_devices,
      get_meas      = .pefa_get_measurements
    ),
    stop(sprintf("Unknown source '%s'. Use 'dbmatic', 'efice', or 'pefa'.", source))
  )
}


# ============================================================================
# PUBLIC DISCOVERY FUNCTIONS
# ============================================================================

#' List accessible vessels for a given source
#'
#' @param source Character. "dbmatic" or "efice"
#' @return Tibble with journal_id, vessel_name, cfr, flag_state
#' @export
get_vessels <- function(source) {
  adp <- .get_adapter(source)
  message(sprintf("Fetching vessels from %s ...", source))
  vessels <- adp$get_vessels(adp$base_url, adp$token)
  message(sprintf("✓ Found %d vessel(s)", nrow(vessels)))
  vessels
}

# get_vessels("efice")

#' Print a formatted table of accessible vessels for a given source
#'
#' Calls get_vessels() and displays the result as a gt table with journal_id,
#' vessel name, CFR/hull number, flag state, and — when vessel_config is
#' supplied — whether each vessel is already registered in the config.
#'
#' @param source        Character. "dbmatic" | "efice" | "pefa"
#' @param vessel_config Tibble from readRDS(poseidat_vessels.rds), or NULL
#' @return A gt table (invisibly); also printed to the viewer.
#' @export
print_vessels <- function(source, vessel_config = NULL) {

  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install gt: install.packages('gt')")

  vessels <- get_vessels(source)

  if (nrow(vessels) == 0) {
    message("No vessels found for source: ", source)
    return(invisible(NULL))
  }

  # Resolve short UUID suffix for display
  df <- vessels |>
    mutate(
      journal_short = substr(journal_id, nchar(journal_id) - 7, nchar(journal_id)),
      in_config     = if (!is.null(vessel_config))
        journal_id %in% vessel_config$journal_id
      else
        NA
    )

  tbl <- gt::gt(df) |>
    gt::tab_header(
      title    = sprintf("Vessels accessible via %s", source),
      subtitle = sprintf("%d vessel(s)  |  Generated: %s", nrow(df), Sys.Date())
    ) |>
    gt::cols_hide(columns = journal_id) |>
    gt::cols_label(
      journal_short = "Journal ID (suffix)",
      vessel_name   = "Vessel",
      cfr           = "CFR / Hull",
      flag_state    = "Flag",
      in_config     = "In config"
    ) |>
    gt::cols_align(align = "left",   columns = c(vessel_name, cfr)) |>
    gt::cols_align(align = "center", columns = c(flag_state, in_config)) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(table.font.size = gt::px(12))

  if (!is.null(vessel_config)) {
    tbl <- tbl |>
      gt::tab_style(
        style     = gt::cell_fill(color = "#d4edda"),
        locations = gt::cells_body(rows = in_config == TRUE)
      ) |>
      gt::tab_style(
        style     = gt::cell_fill(color = "#fff3cd"),
        locations = gt::cells_body(rows = in_config == FALSE | is.na(in_config))
      )
  }

  print(tbl)
  invisible(tbl)
}

#' List devices on a vessel
#'
#' @param source     Character. "dbmatic" or "efice"
#' @param journal_id Character. Vessel journal UUID
#' @return Tibble with device_id, device_name, device_type, equipment details
#' @export
get_devices <- function(source, journal_id, vessel_name = NULL,
                        verbose = TRUE) {
  adp   <- .get_adapter(source)
  label <- if (!is.null(vessel_name))
    sprintf("%s / %s / %s", source, vessel_name, journal_id)
  else
    sprintf("%s / %s", source, journal_id)
  message(sprintf("Fetching devices for %s ...", label))
  devices <- adp$get_devices(adp$base_url, adp$token, journal_id)
  if (verbose) message(sprintf("Found %d device(s)", nrow(devices)))
  devices
}

#' Print a formatted table of devices for all (or one) vessel(s)
#'
#' Calls get_devices() for each vessel and displays a gt table with the raw
#' device UUID, the friendly label from vessel_config (if available), and the
#' device type. Rows with no label in config are highlighted in amber.
#'
#' @param source        Character. "dbmatic" | "efice" | "pefa"
#' @param vessel_config Tibble from readRDS(poseidat_vessels.rds)
#' @param vessel_name   Character vector to restrict to specific vessels,
#'                      or NULL for all enabled vessels in vessel_config.
#' @return A gt table (invisibly); also printed to the viewer.
#' @export
print_devices <- function(source, vessel_config, vessel_name = NULL) {

  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install gt: install.packages('gt')")

  cfg <- vessel_config |>
    filter(source == .env$source, enabled)

  if (!is.null(vessel_name))
    cfg <- cfg |> filter(vessel_name %in% .env$vessel_name)

  if (nrow(cfg) == 0) {
    message("No matching vessels found in vessel_config for source: ", source)
    return(invisible(NULL))
  }

  rows <- vector("list", nrow(cfg))

  for (i in seq_len(nrow(cfg))) {
    jid   <- cfg$journal_id[i]
    vname <- cfg$vessel_name[i]

    live  <- tryCatch(
      get_devices(source, jid, vname),
      error = function(e) {
        message(sprintf("  Could not fetch devices for %s: %s", vname, e$message))
        tibble(device_id = character(), device_name = character(),
               device_type = character())
      }
    )

    if (nrow(live) == 0) {
      rows[[i]] <- tibble(vessel_name = vname, device_id = character(),
                          device_short = character(), label = character(),
                          device_type = character())
      next
    }

    # Build label lookup from config for this vessel
    known <- cfg$device_labels[[i]]
    label_vec <- if (length(known) > 0)
      setNames(unlist(known), names(known))
    else
      character(0)

    rows[[i]] <- live |>
      mutate(
        vessel_name  = vname,
        device_short = substr(device_id, nchar(device_id) - 11, nchar(device_id)),
        label        = dplyr::coalesce(label_vec[device_id], NA_character_)
      ) |>
      dplyr::select(vessel_name, device_short, device_id, label, device_type)
  }

  df <- bind_rows(rows)

  if (nrow(df) == 0) {
    message("No devices found")
    return(invisible(NULL))
  }

  tbl <- gt::gt(df, groupname_col = "vessel_name") |>
    gt::tab_header(
      title    = sprintf("Devices accessible via %s", source),
      subtitle = sprintf("%d device(s) across %d vessel(s)  |  Generated: %s",
                         nrow(df), length(unique(df$vessel_name)), Sys.Date())
    ) |>
    gt::cols_hide(columns = device_id) |>
    gt::cols_label(
      device_short = "Device UUID (suffix)",
      label        = "Label (from config)",
      device_type  = "Type"
    ) |>
    gt::cols_align(align = "left",   columns = c(label, device_type)) |>
    gt::cols_align(align = "center", columns = device_short) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#fff3cd"),
      locations = gt::cells_body(rows = is.na(label))
    ) |>
    gt::tab_source_note(
      source_note = "Amber rows: device not yet labelled in vessel_config."
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(12),
      row_group.background.color      = "grey92",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#fafafa"
    )

  print(tbl)
  invisible(tbl)
}

#' Estimate the earliest date for which a device has any measurement data
#'
#' Binary search between a date confirmed to have NO data (empty_date) and
#' a date confirmed to HAVE data (present_date). Converges in
#' log2(range / tolerance_days) calls rather than testing every window
#' sequentially -- e.g. ~4 calls to narrow a 10-week range to +/- 1 day,
#' instead of 10+ manual weekly checks.
#'
#' Each probe downloads a `window_days`-wide slice starting at the midpoint
#' and checks whether it returned any rows -- NOT whether the exact midpoint
#' instant has data, since sparse/intermittent transmission near a real
#' boundary could otherwise cause the search to converge on a false gap.
#'
#' @param source        Character. "dbmatic" | "efice" | "pefa"
#' @param journal_id    Character. Vessel journal UUID
#' @param device_id     Character. Device UUID
#' @param empty_date    POSIXct. A date CONFIRMED to have no data at/after it
#'                      (data starts somewhere after this)
#' @param present_date  POSIXct. A date CONFIRMED to have data at/after it
#' @param window_days   Integer. Width of each probe window (default 3).
#'                      Wider = more resilient to sparse/intermittent data
#'                      near the true boundary, but marginally slower per call.
#' @param tolerance_days Integer. Stop once the bracket is this narrow (default 1)
#' @return POSIXct. Earliest date within `tolerance_days` confirmed to have data
#' @export
find_earliest_data <- function(source, journal_id, device_id,
                               empty_date, present_date,
                               window_days    = 3,
                               tolerance_days = 1) {

  lo <- as.POSIXct(empty_date,   tz = "UTC")   # last known-empty point
  hi <- as.POSIXct(present_date, tz = "UTC")   # last known-present point

  if (lo >= hi) stop("empty_date must be earlier than present_date")

  while (as.numeric(difftime(hi, lo, units = "days")) > tolerance_days) {

    mid        <- lo + as.numeric(difftime(hi, lo, units = "secs")) / 2
    window_end <- min(mid + window_days * 86400, hi)

    n <- nrow(download_measurements(
      source = source, journal_id = journal_id, device_id = device_id,
      start_time = mid, end_time = window_end
    ))

    cat(sprintf("  Probe %s -> %s: %s\n",
               format(mid, "%Y-%m-%d"), format(window_end, "%Y-%m-%d"),
               if (n > 0) sprintf("%d records", n) else "empty"))

    if (n > 0) hi <- mid else lo <- mid
  }

  message(sprintf("Earliest data (within %d day(s)): %s",
                  tolerance_days, format(hi, "%Y-%m-%d")))
  hi
}

#' Look up a vessel's journal_id from vessel_config by vessel name
#'
#' A vessel_name can now legitimately appear under MULTIPLE sources at once
#' (e.g. a vessel currently extracted via dbmatic with a placeholder efice
#' row prepared for a future switch-over -- see Anna Hendrika/Immanuel).
#' Without `source`, an ambiguous match STOPS with a clear error rather than
#' silently returning a vector mixing a real journal_id with a placeholder
#' one from another source's not-yet-active row.
#'
#' @param vessel_config Tibble. From Poseidat_config.R / readRDS()
#' @param vessel_name   Character. Exact vessel name as in config
#' @param source        Character. "dbmatic" | "efice" | "pefa". Required
#'                      if vessel_name matches rows under more than one source
#' @return Character journal_id
#' @export
get_journal_id <- function(vessel_config, vessel_name, source = NULL) {
  row <- vessel_config |> filter(vessel_name == .env$vessel_name)
  if (!is.null(source)) row <- row |> filter(source == .env$source)

  if (nrow(row) == 0)
    stop(sprintf("Vessel not found in config: '%s'%s", vessel_name,
                if (!is.null(source)) sprintf(" (source = '%s')", source) else ""))
  if (nrow(row) > 1)
    stop(sprintf(
      "'%s' matches %d rows under multiple sources (%s) -- pass `source` to disambiguate.",
      vessel_name, nrow(row), paste(row$source, collapse = ", ")
    ))
  row$journal_id
}

#' Look up a device UUID from vessel_config by vessel name and label
#'
#' Searches the device_labels list in vessel_config for the given label
#' and returns the matching UUID. Labels are defined in Poseidat_config.R.
#'
#' Same multi-source ambiguity handling as get_journal_id() -- see its docs.
#'
#' @param vessel_config Tibble. From Poseidat_config.R / readRDS()
#' @param vessel_name   Character. Exact vessel name as in config
#' @param label         Character. Device label e.g. "POSITION", "FUEL_1"
#' @param source        Character. "dbmatic" | "efice" | "pefa". Required
#'                      if vessel_name matches rows under more than one source
#' @return Character device UUID
#' @export
get_device_by_label <- function(vessel_config, vessel_name, label, source = NULL) {
  row <- vessel_config |> filter(vessel_name == .env$vessel_name)
  if (!is.null(source)) row <- row |> filter(source == .env$source)

  if (nrow(row) == 0)
    stop(sprintf("Vessel not found in config: '%s'%s", vessel_name,
                if (!is.null(source)) sprintf(" (source = '%s')", source) else ""))
  if (nrow(row) > 1)
    stop(sprintf(
      "'%s' matches %d rows under multiple sources (%s) -- pass `source` to disambiguate.",
      vessel_name, nrow(row), paste(row$source, collapse = ", ")
    ))
  labels <- row$device_labels[[1]]
  if (length(labels) == 0)
    stop(sprintf("No device labels defined for vessel: '%s'", vessel_name))
  uuid <- names(labels)[unlist(labels) == label]
  if (length(uuid) == 0)
    stop(sprintf("Label '%s' not found for vessel '%s'. Available: %s",
                 label, vessel_name,
                 paste(unlist(labels), collapse = ", ")))
  if (length(uuid) > 1) {
    warning(sprintf("Multiple devices match label '%s' — returning first", label))
    uuid <- uuid[1]
  }
  uuid
}
.safe <- function(x) gsub("[^a-zA-Z0-9_-]", "_", x)

.raw_dir <- function(source, vessel_name)
  file.path(.onedrive(), "raw", source, vessel_name)

.proc_dir <- function(source, vessel_name)
  file.path(.onedrive(), "processed", source, vessel_name)
read_latest_datetime <- function(source, vessel_name, device_id) {

  raw_root <- .raw_dir(source, vessel_name)
  if (!dir.exists(raw_root)) return(NULL)

  safe_dev <- .safe(device_id)
  files    <- list.files(raw_root,
                         pattern    = glue(".*{safe_dev}.*\\.parquet$"),
                         recursive  = TRUE,
                         full.names = TRUE)

  if (length(files) == 0) return(NULL)

  # Read via a temp file copy to avoid holding a Windows memory-map lock on
  # the original file — without this, arrow::read_parquet() keeps the file
  # handle open long enough to block the subsequent write_parquet() call.
  max_dts <- purrr::map(files, function(fp) {
    tryCatch({
      tmp <- tempfile(fileext = ".parquet")
      on.exit(unlink(tmp), add = TRUE)
      file.copy(fp, tmp, overwrite = TRUE)
      dt <- arrow::read_parquet(tmp, col_select = "datetime")$datetime
      if (length(dt) == 0 || all(is.na(dt))) return(as.POSIXct(NA))
      max(dt, na.rm = TRUE)
    }, error = function(e) as.POSIXct(NA))
  })

  max_dts <- purrr::keep(max_dts, ~ !is.na(.x))
  if (length(max_dts) == 0) return(NULL)

  as.POSIXct(max(unlist(lapply(max_dts, as.numeric))),
             origin = "1970-01-01", tz = "UTC")
}


# ============================================================================
# ADDITIVE MONTHLY SAVE  (Step B)
# ============================================================================

#' Save new measurements into additive monthly Parquet files
#'
#' For each calendar month present in \code{measurements}:
#'   1. Read the existing monthly Parquet file (if any)
#'   2. Bind the new records
#'   3. Deduplicate on entry_id
#'   4. Write back
#'
#' @param measurements Tibble of newly downloaded records
#' @param source       Character
#' @param vessel_name  Character
#' @param device_id    Character
#' @return Character vector of file paths written

# Monthly path: raw/<source>/<vessel>/YYYY/MM/<vessel>_<device>_YYYYMM.parquet
.raw_monthly_path <- function(source, vessel_name, device_id, year, month) {
  dir <- file.path(.raw_dir(source, vessel_name),
                   sprintf("%04d", year),
                   sprintf("%02d", month))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, sprintf("%s_%s_%04d%02d.parquet",
                         .safe(vessel_name), .safe(device_id), year, month))
}


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

    # Merge with existing monthly file if present — read via temp copy to
    # avoid holding a Windows memory-map lock on the file we are about to write.
    if (file.exists(fp)) {
      existing <- tryCatch({
        tmp <- tempfile(fileext = ".parquet")
        on.exit(unlink(tmp), add = TRUE)
        file.copy(fp, tmp, overwrite = TRUE)
        arrow::read_parquet(tmp)
      }, error = function(e) {
        warning(sprintf("Could not read existing parquet %s: %s — overwriting",
                        basename(fp), e$message))
        tibble::tibble()
      })
      # New records FIRST, so distinct() keeps the freshly downloaded version
      # of an entry_id over the stored one. Needed to repair records that were
      # stored with a parsing problem (re-download replaces them) and keeps
      # API revisions up to date. (Was bind_rows(existing, chunk) until
      # 2026-11-26, which silently kept the old copy.)
      chunk <- dplyr::bind_rows(chunk, existing) |>
        dplyr::distinct(entry_id, .keep_all = TRUE) |>
        dplyr::arrange(datetime)
    }

    arrow::write_parquet(chunk, fp)
    paths[i] <- fp
  }

  paths
}


# ============================================================================
# PAGINATED DOWNLOAD
# ============================================================================

#' Download all measurements for one device in a time window
#'
#' @param source              Character. "dbmatic", "efice", or "pefa"
#' @param journal_id          Character
#' @param device_id           Character
#' @param start_time          POSIXct (UTC)
#' @param end_time            POSIXct (UTC)
#' @param page_size           Integer. Records per API page (default 1000).
#'   Increase to 5000 to reduce API calls; reduce to 100 for testing.
#' @param sleep_between_pages Numeric. Seconds to wait between pages (default 0.1).
#'   Increase to 0.5-1.0 if the server rate-limits; set to 0 for max speed.
#' @return Tibble, sorted by datetime, duplicates removed
download_measurements <- function(source, journal_id, device_id,
                                   start_time, end_time,
                                   page_size           = 1000,
                                   sleep_between_pages = 0.1) {

  if (is.character(start_time))
    start_time <- as.POSIXct(start_time, tz = "UTC")
  if (is.character(end_time))
    end_time   <- as.POSIXct(end_time,   tz = "UTC")

  adp     <- .get_adapter(source)
  pages   <- list()
  offset  <- 0
  page_no <- 1

  repeat {
    cat(sprintf("\r  [%s] page %d (offset %d) ...", source, page_no, offset))

    page <- tryCatch(
      adp$get_meas(adp$base_url, adp$token,
                   journal_id, device_id,
                   start_time, end_time,
                   page_size, offset),
      error = function(e) {
        cat("\n")
        stop(e)
      }
    )

    if (nrow(page) == 0) break

    # Add provenance columns
    page <- page |>
      mutate(source     = source,
             journal_id = journal_id,
             device_id  = device_id)

    pages   <- c(pages, list(page))
    if (nrow(page) < page_size) break

    offset  <- offset + page_size
    page_no <- page_no + 1
    if (sleep_between_pages > 0) Sys.sleep(sleep_between_pages)
  }

  cat("\n")

  if (length(pages) == 0) return(tibble())

  bind_rows(pages) |>
    distinct(entry_id, .keep_all = TRUE) |>
    arrange(datetime)
}


# ============================================================================
# INCREMENTAL SYNC — one device  (Step D)
# ============================================================================

#' Sync one vessel/device combination
#'
#' Start time is derived from read_latest_datetime() — the most recent record
#' already on disk for this device.  A 1-hour overlap is added for safety.
#' Falls back to lookback_days only when no Parquet exists yet (first sync).
#' New records are merged into additive monthly Parquet files via
#' .save_monthly_additive().
#'
#' @param source              Character. "dbmatic" | "efice" | "pefa"
#' @param journal_id          Character. Vessel journal UUID
#' @param device_id           Character. Device UUID
#' @param vessel_name         Character. Used as folder name
#' @param lookback_days       Integer.   Days to fetch on very first sync
#'                            (default 30). Ignored on subsequent syncs.
#' @param start_time          POSIXct or NULL. When set, overrides
#'                            read_latest_datetime() and force_full — use this
#'                            to backfill a specific historical window.
#' @param end_time            POSIXct or NULL. Upper bound of fetch window.
#'                            Defaults to Sys.time(). Set explicitly to sync
#'                            up to a specific moment.
#' @param force_full          Logical. Fetch from 2020-01-01 when TRUE.
#'                            Should not be used in the daily runner.
#' @param page_size           Integer. API page size (default 1000)
#' @param sleep_between_pages Numeric. Seconds between pages (default 0.1)
#' @param max_gap_days        Numeric or NULL. When the newest record on disk
#'                            is older than this many days, only the last
#'                            `max_gap_days` are requested instead of the whole
#'                            gap. Stops a sensor that went silent weeks ago
#'                            from requesting its entire silent period every
#'                            day (which times out on EFICE). The gap itself is
#'                            not filled -- backfill it with sync_device(...,
#'                            start_time = ) or sync_device_parallel() if the
#'                            sensor comes back. NULL (default) = no cap.
#'                            Ignored when start_time or force_full is given.
#' @return Tibble of newly downloaded records (invisibly)
#' @export
sync_device <- function(source, journal_id, device_id,
                        vessel_name         = "unknown",
                        lookback_days       = 30,
                        start_time          = NULL,
                        end_time            = NULL,
                        force_full          = FALSE,
                        page_size           = 1000,
                        sleep_between_pages = 0.1,
                        max_gap_days        = NULL) {
  
  if (!is.null(start_time) && !inherits(start_time, "POSIXct"))
    start_time <- as.POSIXct(start_time, tz = "UTC")
  if (!is.null(end_time) && !inherits(end_time, "POSIXct"))
    end_time <- as.POSIXct(end_time, tz = "UTC")
  
  if (is.null(end_time)) end_time <- Sys.time()
  
  # ── Determine start of fetch window (unchanged logic, no logging here) ─────
  if (!is.null(start_time)) {
    # explicit override — no message
  } else if (force_full) {
    start_time <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  } else {
    latest_on_disk <- read_latest_datetime(source, vessel_name, device_id)
    start_time <- if (is.null(latest_on_disk))
      end_time - (lookback_days * 86400)
    else
      latest_on_disk - 3600

    # Cap the window for devices that have been silent for a long time
    if (!is.null(latest_on_disk) && !is.null(max_gap_days)) {
      cap_start <- end_time - (max_gap_days * 86400)
      if (start_time < cap_start) {
        log_poseidat(sprintf(
          "No data on disk since %s (%.0f days) -- only requesting the last %g day(s). Backfill with sync_device(start_time = ...) if the sensor is back.",
          format(latest_on_disk, "%Y-%m-%d %H:%M"),
          as.numeric(difftime(end_time, latest_on_disk, units = "days")),
          max_gap_days),
          level = "WARNING", source = source, vessel_id = journal_id)
        start_time <- cap_start
      }
    }
  }
  
  log_poseidat(sprintf("Range: %s → %s",
                       format(start_time, "%Y-%m-%d %H:%M:%S"),
                       format(end_time,   "%Y-%m-%d %H:%M:%S")),
               source = source, vessel_id = journal_id)
  
  # ── Download ────────────────────────────────────────────────────────────────
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
    log_poseidat("No new data returned", source = source, vessel_id = journal_id)
    return(invisible(tibble::tibble()))
  }
  
  log_poseidat(sprintf("Downloaded %d records", nrow(measurements)),
               source = source, vessel_id = journal_id)
  
  # ── Save — merge into additive monthly Parquet files ───────────────────────
  tryCatch({
    paths <- .save_monthly_additive(measurements, source, vessel_name, device_id)
    log_poseidat(paste(basename(paths), collapse = ", "),
                 source = source, vessel_id = journal_id)
  }, error = function(e) {
    log_poseidat(sprintf("Save failed: %s", e$message),
                 level = "ERROR", source = source, vessel_id = journal_id)
  })
  
  # ── Update state (unchanged — still needed for print_sync_state()) ─────────
  max_dt <- max(measurements$datetime, na.rm = TRUE)
  update_last_sync_time(source, journal_id, device_id, max_dt, nrow(measurements))
  
  invisible(measurements)
}

# ============================================================================
# SYNC ALL VESSELS  (Step E)
# ============================================================================

#' Sync all enabled vessels in vessel_config
#'
#' Iterates over all enabled rows, auto-discovers devices per vessel,
#' and calls sync_device() for each.
#'
#' @param vessel_config       Tibble. From Poseidat_config.R
#' @param sources             Character vector or NULL (all sources in config)
#' @param vessel_names        Character vector or NULL (all enabled vessels)
#' @param lookback_days       Integer. Days for first-ever sync (default 30)
#' @param start_time          POSIXct or NULL. When set, overrides
#'                            read_latest_datetime() for all devices — use this
#'                            to backfill a specific historical window.
#'                            Combine with end_time to define the exact range.
#' @param end_time            POSIXct or NULL. Upper bound for all devices.
#'                            Defaults to Sys.time(). Set explicitly to sync
#'                            up to a specific moment.
#' @param force_full          Logical. Full re-sync from 2020-01-01
#' @param page_size           Integer
#' @param sleep_between_pages Numeric
#' @param max_gap_days        Numeric or NULL. Passed to sync_device()
#' @return Named list (one entry per source__journal_id__device_id)
#' @export
sync_all_vessels <- function(vessel_config,
                              sources             = NULL,
                              vessel_names        = NULL,
                              lookback_days       = 30,
                              start_time          = NULL,
                              end_time            = NULL,
                              force_full          = FALSE,
                              page_size           = 1000,
                              sleep_between_pages = 0.1,
                              only_labelled       = FALSE,
                              max_gap_days        = NULL) {

  if (is.null(end_time)) end_time <- Sys.time()

  cfg <- vessel_config |> dplyr::filter(enabled)
  if (!is.null(sources))
    cfg <- cfg |> dplyr::filter(source %in% sources)
  if (!is.null(vessel_names))
    cfg <- cfg |> dplyr::filter(vessel_name %in% vessel_names)
  if (nrow(cfg) == 0) {
    message("No enabled vessels match the requested sources/vessels.")
    return(invisible(list()))
  }

  # log_poseidat(sprintf(
  #   "=== Poseidat sync starting — %d vessel(s), source(s): %s%s, end: %s ===",
  #   nrow(cfg),
  #   paste(unique(cfg$source), collapse = ", "),
  #   if (!is.null(start_time)) sprintf(", start: %s", format(start_time, "%Y-%m-%d %H:%M:%S")) else "",
  #   format(end_time, "%Y-%m-%d %H:%M:%S")
  # ))

  results <- list()

  for (i in seq_len(nrow(cfg))) {
    vessel <- cfg[i, ]

    log_poseidat(sprintf("--- [%d/%d] %s (%s) ---",
                         i, nrow(cfg), vessel$vessel_name, vessel$source),
                 source = vessel$source)

    # Auto-discover devices
    devices <- tryCatch(
      get_devices(vessel$source, vessel$journal_id, vessel$vessel_name),
      error = function(e) {
        log_poseidat(sprintf("Device discovery failed: %s", e$message),
                     level = "ERROR", source = vessel$source,
                     vessel_id = vessel$journal_id)
        tibble::tibble()
      }
    )

    if (only_labelled) {
      lbl <- vessel$device_labels[[1]]
      if (length(lbl) > 0) {
        n_voor  <- nrow(devices)
        devices <- dplyr::filter(devices, device_id %in% names(lbl))
        log_poseidat(sprintf("Device filter: %d of %d device(s) are in device_labels",
                             nrow(devices), n_voor),
                     source = vessel$source, vessel_id = vessel$journal_id)
      }
    }
    
    if (nrow(devices) == 0) {
      log_poseidat("No devices found — skipping", level = "WARNING",
                   source = vessel$source, vessel_id = vessel$journal_id)
      next
    }

    for (j in seq_len(nrow(devices))) {
      dev <- devices[j, ]
      key <- paste(vessel$source, vessel$journal_id, dev$device_id, sep = "__")

      # Resolve friendly label from vessel_config device_labels if available
      dev_label <- tryCatch({
        lbl <- vessel_config |>
          dplyr::filter(journal_id == vessel$journal_id) |>
          dplyr::pull(device_labels) |>
          purrr::pluck(1)
        lbl[dev$device_id] %||% dev$device_id
      }, error = function(e) dev$device_id)

      log_poseidat(sprintf("Device %d/%d: %s [%s]",
                           j, nrow(devices), dev_label, dev$device_id),
                   source = vessel$source, vessel_id = vessel$journal_id)

      results[[key]] <- tryCatch(
        sync_device(
          source              = vessel$source,
          journal_id          = vessel$journal_id,
          device_id           = dev$device_id,
          vessel_name         = vessel$vessel_name,
          lookback_days       = lookback_days,
          start_time          = start_time,
          end_time            = end_time,
          force_full          = force_full,
          page_size           = page_size,
          sleep_between_pages = sleep_between_pages,
          max_gap_days        = max_gap_days
        ),
        error = function(e) {
          log_poseidat(sprintf("Sync error: %s", e$message),
                       level = "ERROR", source = vessel$source,
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
migrate_to_monthly <- function(vessel_config,
                                sources      = NULL,
                                vessel_names = NULL,
                                dry_run      = TRUE,
                                delete_old   = TRUE) {

  if (dry_run)
    cat("\n*** DRY RUN — no files will be written or deleted ***\n\n")
  else
    cat("\n*** LIVE RUN — files will be written; old files deleted if delete_old = TRUE ***\n\n")

  cfg <- vessel_config |> dplyr::filter(enabled)
  if (!is.null(sources))      cfg <- cfg |> dplyr::filter(source %in% sources)
  if (!is.null(vessel_names)) cfg <- cfg |> dplyr::filter(vessel_name %in% vessel_names)

  if (nrow(cfg) == 0) {
    message("No enabled vessels match the requested filters.")
    return(invisible(tibble::tibble()))
  }

  # v1 filename pattern: ends with _YYYYMMDD_to_YYYYMMDD.parquet
  old_pattern <- "_\\d{8}_to_\\d{8}\\.parquet$"
  summary_rows <- list()

  for (i in seq_len(nrow(cfg))) {
    vessel   <- cfg[i, ]
    raw_root <- .raw_dir(vessel$source, vessel$vessel_name)

    if (!dir.exists(raw_root)) {
      cat(sprintf("  [%s] %s — no raw folder, skipping\n",
                  vessel$source, vessel$vessel_name))
      next
    }

    old_files <- list.files(raw_root,
                             pattern    = old_pattern,
                             recursive  = TRUE,
                             full.names = TRUE)

    if (length(old_files) == 0) {
      cat(sprintf("  [%s] %s — no v1 files found\n",
                  vessel$source, vessel$vessel_name))
      next
    }

    cat(sprintf("\n  [%s] %s — %d v1 file(s) found\n",
                vessel$source, vessel$vessel_name, length(old_files)))

    # Group by device_id (extracted from filename)
    device_ids <- unique(sub(
      pattern     = sprintf("^%s_(.+)_\\d{8}_to_\\d{8}\\.parquet$",
                             .safe(vessel$vessel_name)),
      replacement = "\\1",
      x           = basename(old_files)
    ))

    for (dev_id in device_ids) {
      safe_dev  <- .safe(dev_id)
      dev_files <- old_files[grepl(safe_dev, old_files, fixed = TRUE)]

      n_old <- sum(purrr::map_int(dev_files, function(fp) {
        tryCatch(nrow(arrow::read_parquet(fp)), error = function(e) 0L)
      }))

      cat(sprintf("    Device %-45s  %d file(s)  %s records\n",
                  substr(dev_id, 1, 45),
                  length(dev_files),
                  format(n_old, big.mark = ",")))

      if (!dry_run) {
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
          cat("      -> No readable data, skipping\n")
          next
        }

        paths_written <- .save_monthly_additive(
          all_data, vessel$source, vessel$vessel_name, dev_id
        )
        cat(sprintf("      -> Written %d monthly file(s)\n", length(paths_written)))

        if (delete_old) {
          purrr::walk(dev_files, function(fp) {
            if (file.remove(fp))
              cat(sprintf("      -> Deleted: %s\n", basename(fp)))
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

  # Remove empty subdirectories left behind after deletion
  if (!dry_run && delete_old) {
    for (i in seq_len(nrow(cfg))) {
      vessel   <- cfg[i, ]
      raw_root <- .raw_dir(vessel$source, vessel$vessel_name)
      subdirs  <- list.dirs(raw_root, recursive = TRUE, full.names = TRUE)
      subdirs  <- subdirs[order(nchar(subdirs), decreasing = TRUE)]
      purrr::walk(subdirs, function(d) {
        if (d != raw_root && length(list.files(d)) == 0)
          unlink(d, recursive = FALSE)
      })
    }
  }

  cat("\n=== Migration complete ===\n")
  invisible(if (length(summary_rows) > 0) dplyr::bind_rows(summary_rows)
            else tibble::tibble())
}


# ============================================================================
# SYNC STATE OVERVIEW
# ============================================================================

#' Print a summary of the current sync state for all tracked vessels/devices
#'
#' @export
print_sync_state <- function() {
  state <- get_sync_state()
  cat(sprintf("\nPoseidat sync state (last global sync: %s)\n",
              format(state$last_sync)))
  cat(strrep("-", 80), "\n")

  if (length(state$vessels) == 0) {
    cat("  No vessels synced yet.\n\n")
    return(invisible(NULL))
  }

  for (key in sort(names(state$vessels))) {
    v     <- state$vessels[[key]]
    parts <- strsplit(key, "__")[[1]]
    cat(sprintf(
      "  %-10s | %-36s | %-36s\n    last record : %s | records : %d\n\n",
      parts[1], parts[2], parts[3],
      format(v$last_sync_time),
      v$last_record_count
    ))
  }
  invisible(NULL)
}


poseidat_files <- function(source = NULL) {

  base <- .onedrive()

  files <- c(
    list.files(file.path(base, "raw"),       pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE),
    list.files(file.path(base, "processed"), pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE)
  )

  if (length(files) == 0) {
    message("No parquet files found under ", base)
    return(invisible(tibble()))
  }

  result <- map_dfr(files, function(f) {
    rel   <- sub(paste0(base, "/"), "", f, fixed = TRUE)
    parts <- strsplit(rel, "/")[[1]]
    tier  <- parts[1]                          # raw or processed
    src   <- if (length(parts) >= 2) parts[2] else NA_character_
    vsl   <- if (length(parts) >= 3) parts[3] else NA_character_

    rows  <- tryCatch(nrow(read_parquet(f)), error = \(e) NA_integer_)
    size  <- file.size(f)

    tibble(
      tier        = tier,
      source      = src,
      vessel_name = vsl,
      file        = basename(f),
      rows        = rows,
      size_kb     = round(size / 1024, 1),
      path        = f
    )
  })

  if (!is.null(source)) result <- filter(result, source %in% .env$source)

  result |>
    arrange(source, vessel_name, tier, file) |>
    dplyr::select(tier, source, vessel_name, file, rows, size_kb)
}


# ============================================================================
# 2. poseidat_value_types()
#
# Shows which value_types (FUEL_CONSUMPTION, POSITION, etc.) exist
# per vessel and device, with record counts and date range.
# ============================================================================

poseidat_value_types <- function(source = NULL) {

  base  <- .onedrive()
  files <- list.files(file.path(base, "raw"),       pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  files <- c(files,
             list.files(file.path(base, "processed"), pattern = "\\.parquet$",
                        recursive = TRUE, full.names = TRUE))

  if (length(files) == 0) {
    message("No parquet files found")
    return(invisible(tibble()))
  }

  result <- map_dfr(files, function(f) {
    tryCatch({
      df <- read_parquet(f)
      if (!"value_type" %in% names(df)) return(tibble())
      df |>
        group_by(source, journal_id, device_id, value_type) |>
        summarise(
          n_records  = n(),
          first_obs  = min(datetime, na.rm = TRUE),
          last_obs   = max(datetime, na.rm = TRUE),
          .groups    = "drop"
        )
    }, error = \(e) tibble())
  })

  if (nrow(result) == 0) return(result)

  # Overlapping sync windows are handled by re-summing in group_by below
  result <- result |>
    group_by(source, journal_id, device_id, value_type) |>
    summarise(
      n_records = sum(n_records),
      first_obs = min(first_obs),
      last_obs  = max(last_obs),
      .groups   = "drop"
    )

  # Join vessel names from saved config (works for all sources)
  vessels <- tryCatch({
    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    readRDS(cfg_path) |>
      dplyr::select(-device_labels) |>       # drop list-column before any dplyr ops
      dplyr::select(journal_id, vessel_name)
  }, error = function(e) tibble(journal_id = character(), vessel_name = character()))
  result <- left_join(result, vessels, by = "journal_id")

  if (!is.null(source)) result <- filter(result, source %in% .env$source)

  result |>
    arrange(source, vessel_name, device_id, value_type) |>
    dplyr::select(source, vessel_name, journal_id, device_id,
           value_type, n_records, first_obs, last_obs)
}


# ============================================================================
# 3. poseidat_daily()
#
# Daily record counts per vessel / device / value_type.
# Useful for spotting gaps in coverage.
#
# @param source      Filter to one source, or NULL for all
# @param vessel_name Filter to one vessel name, or NULL for all
# @param value_type  Filter to one value_type, or NULL for all
# ============================================================================

poseidat_daily <- function(source      = NULL,
                           vessel_name = NULL,
                           value_type  = NULL) {

  base  <- .onedrive()
  files <- c(
    list.files(file.path(base, "raw"),       pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE),
    list.files(file.path(base, "processed"), pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE)
  )

  if (length(files) == 0) {
    message("No parquet files found")
    return(invisible(tibble()))
  }

  result <- map_dfr(files, function(f) {
    tryCatch({
      df <- read_parquet(f)
      if (!all(c("datetime", "value_type", "source") %in% names(df)))
        return(tibble())

      # Apply filters early to keep memory low
      if (!is.null(source))      df <- filter(df, source      %in% .env$source)
      if (!is.null(value_type))  df <- filter(df, value_type  %in% .env$value_type)

      df |>
        mutate(date = as.Date(datetime)) |>
        group_by(source, journal_id, device_id, value_type, date) |>
        summarise(n_records = n(), .groups = "drop")
    }, error = \(e) tibble())
  })

  if (nrow(result) == 0) return(result)

  # Overlapping sync windows are handled by re-summing in group_by below
  result <- result |>
    group_by(source, journal_id, device_id, value_type, date) |>
    summarise(n_records = sum(n_records), .groups = "drop")

  # Join vessel names from saved config (works for all sources)
  vessels <- tryCatch({
    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    readRDS(cfg_path) |>
      dplyr::select(-device_labels) |>       # drop list-column before any dplyr ops
      dplyr::select(journal_id, vessel_name)
  }, error = function(e) tibble(journal_id = character(), vessel_name = character()))
  result <- left_join(result, vessels, by = "journal_id")

  if (!is.null(vessel_name))
    result <- filter(result, vessel_name %in% .env$vessel_name)

  result |>
    arrange(source, vessel_name, device_id, value_type, date) |>
    dplyr::select(source, vessel_name, device_id, value_type, date, n_records)
}

# ============================================================================
# DAILY RUNNER — vessel-isolated sync + thin
#
# Loops vessels one at a time; thins each vessel immediately after its own
# sync, regardless of whether other vessels succeed, fail, or are still
# running. A slow/stuck device on one vessel can no longer block thinning
# for vessels that already completed.
# ============================================================================

poseidat_daily_sync_all <- function(vessel_config,
                                    sources             = c("efice", "pefa"),
                                    page_size           = 5000,
                                    exclude_value_types = "TEMPERATURE",
                                    lookback_days       = 30,
                                    full_thin           = FALSE,
                                    max_gap_days        = 3) {

  cfg <- vessel_config |> dplyr::filter(enabled, source %in% sources)
  
  if (nrow(cfg) == 0) {
    log_poseidat("No enabled vessels match requested sources — nothing to do",
                 level = "WARNING")
    return(invisible(list(sync_results = list(), timings = tibble())))
  }
  
  all_sync_results <- list()
  timings <- tibble::tibble(vessel_name = character(), step = character(), seconds = numeric())
  
  for (i in seq_len(nrow(cfg))) {
    vname   <- cfg$vessel_name[i]
    vsource <- cfg$source[i]
    
    log_poseidat(sprintf("=== [%d/%d] %s (%s) ===", i, nrow(cfg), vname, vsource))
    
    # ---- Sync this vessel only — a failure here doesn't touch the others --
    t0 <- Sys.time()
    vessel_results <- tryCatch(
      sync_all_vessels(vessel_config, vessel_names = vname, sources = vsource,
                       lookback_days = lookback_days, end_time = NULL,
                       force_full = FALSE, page_size = page_size,
                       sleep_between_pages = 0,
                       only_labelled = (vsource == "pefa"),
                       max_gap_days  = max_gap_days),
      error = function(e) {
        log_poseidat(sprintf("Sync failed for %s: %s", vname, conditionMessage(e)),
                     level = "ERROR")
        list()
      }
    )
    timings <- dplyr::bind_rows(timings, tibble::tibble(
      vessel_name = vname, step = "sync",
      seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
    all_sync_results <- c(all_sync_results, vessel_results)
    
    # ---- Thin THIS vessel now — always runs, even if the sync above failed
    t0 <- Sys.time()
    tryCatch(
      poseidat_thin(source = vsource, vessel_name = vname,
                    exclude_value_types = exclude_value_types,
                    only_labelled = (vsource == "pefa"),
                    full_rebuild  = full_thin,
                    save = TRUE),
      error = function(e) log_poseidat(sprintf("Thinning failed for %s: %s",
                                               vname, conditionMessage(e)),
                                       level = "ERROR")
    )
    timings <- dplyr::bind_rows(timings, tibble::tibble(
      vessel_name = vname, step = "thin",
      seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
  }
  
  log_poseidat("=== Poseidat daily sync completed (all vessels) ===")
  invisible(list(sync_results = all_sync_results, timings = timings))
}


# ============================================================================
# 4. poseidat_summary()
#
# Statistical summary (n, min, mean, max) per vessel / value_type.
# Only includes non-zero values to avoid sensor dropouts skewing averages.
#
# @param source     Filter to one source, or NULL for all
# @param value_type Filter to specific value_types, or NULL for all
# @param nonzero    If TRUE (default), exclude zero values from stats
# ============================================================================

poseidat_summary <- function(source     = NULL,
                             value_type = NULL,
                             nonzero    = TRUE) {

  base  <- .onedrive()
  files <- c(
    list.files(file.path(base, "raw"),       pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE),
    list.files(file.path(base, "processed"), pattern = "\\.parquet$",
               recursive = TRUE, full.names = TRUE)
  )

  if (length(files) == 0) {
    message("No parquet files found")
    return(invisible(tibble()))
  }

  result <- map_dfr(files, function(f) {
    tryCatch({
      df <- read_parquet(f)
      if (!all(c("value_numeric", "value_type", "source") %in% names(df)))
        return(tibble())

      if (!is.null(source))     df <- filter(df, source     %in% .env$source)
      if (!is.null(value_type)) df <- filter(df, value_type %in% .env$value_type)
      if (nonzero)              df <- filter(df, value_numeric != 0)

      if (nrow(df) == 0) return(tibble())

      df |>
        group_by(source, journal_id, device_id, value_type) |>
        summarise(
          n         = n(),
          min_value = min(value_numeric,  na.rm = TRUE),
          mean_value= mean(value_numeric, na.rm = TRUE),
          max_value = max(value_numeric,  na.rm = TRUE),
          first_obs = min(datetime,       na.rm = TRUE),
          last_obs  = max(datetime,       na.rm = TRUE),
          .groups   = "drop"
        )
    }, error = \(e) tibble())
  })

  if (nrow(result) == 0) return(result)

  # Overlapping sync windows handled by re-aggregating across files below
  result <- result |>
    group_by(source, journal_id, device_id, value_type) |>
    summarise(
      n          = sum(n),
      min_value  = min(min_value),
      mean_value = sum(mean_value * n) / sum(n),      # weighted mean across files
      max_value  = max(max_value),
      first_obs  = min(first_obs),
      last_obs   = max(last_obs),
      .groups    = "drop"
    )

  vessels <- tryCatch({
    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    readRDS(cfg_path) |>
      dplyr::select(-device_labels) |>       # drop list-column before any dplyr ops
      dplyr::select(journal_id, vessel_name)
  }, error = function(e) tibble(journal_id = character(), vessel_name = character()))
  result <- left_join(result, vessels, by = "journal_id")

  result |>
    arrange(source, vessel_name, value_type) |>
    dplyr::select(source, vessel_name, device_id, value_type,
           n, min_value, mean_value, max_value, first_obs, last_obs)
}


# ============================================================================
# 5. poseidat_filter_save()
#
# Re-reads all raw parquet files and re-saves them keeping only the
# specified value_types. Use this to slim down storage after you know
# which measurement types you actually need.
#
# @param keep_value_types Character vector of value_types to keep
#                         e.g. c("POSITION", "SPEED", "FUEL_CONSUMPTION")
# @param source           Filter to one source, or NULL for all
# @param dry_run          If TRUE, show what would be done without saving
# ============================================================================

poseidat_filter_save <- function(keep_value_types,
                                 source  = NULL,
                                 dry_run = TRUE) {

  base  <- .onedrive()
  files <- list.files(file.path(base, "raw"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)

  if (!is.null(source)) {
    files <- files[grepl(paste0("/raw/", source, "/"), files, fixed = TRUE)]
  }

  if (length(files) == 0) {
    message("No raw parquet files found")
    return(invisible(NULL))
  }

  cat(sprintf("value_types to keep : %s\n",
              paste(keep_value_types, collapse = ", ")))
  cat(sprintf("Files to process    : %d\n", length(files)))
  if (dry_run) cat("DRY RUN — no files will be modified\n\n")

  for (f in files) {
    df_orig <- tryCatch(read_parquet(f), error = \(e) NULL)
    if (is.null(df_orig)) { cat(sprintf("  SKIP (unreadable): %s\n", basename(f))); next }

    df_filt <- filter(df_orig, value_type %in% keep_value_types)

    cat(sprintf("  %-60s  %d -> %d rows\n",
                basename(f), nrow(df_orig), nrow(df_filt)))

    if (!dry_run && nrow(df_filt) > 0) {
      write_parquet(df_filt, f)
    } else if (!dry_run && nrow(df_filt) == 0) {
      cat(sprintf("    WARNING: all rows removed — file not modified\n"))
    }
  }

  if (dry_run) {
    cat("\nSet dry_run = FALSE to apply the changes.\n")
  } else {
    cat("\nDone. Run poseidat_files() to verify new file sizes.\n")
  }
}


# ============================================================================
# 5b. Quality flagging: default_quality_rules / add_quality_flags()
#
# Adds a quality_flag ("ok" | "out_of_range") + quality_note column to any
# measurements tibble, so suspect/impossible readings can be filtered out of
# analysis and plots (or excluded from poseidat_thin()'s per-bin averages)
# without touching the raw data on disk -- raw parquet stays an untouched,
# faithful copy of what the API returned; flagging is always a derived,
# re-computable layer on top.
#
# default_quality_rules only encodes bounds defensible on physics alone
# (nothing can be negative fuel/depth/tension; lat/lon have hard limits).
# Bounds that depend on YOUR vessel/gear (max plausible fuel burn for this
# engine, expected depth range for these grounds, rated winch tension) are
# left as NA ("no bound") placeholders below -- edit the tibble directly to
# add real max_valid values once you know them; NA in either column simply
# means that direction is unchecked.
# ============================================================================

default_quality_rules <- tibble(
  value_type = c("FUEL_CONSUMPTION", "DEPTH", "TEMPERATURE"),
  min_valid  = c(0,                  0,        -5),
  max_valid  = c(NA_real_,            NA_real_, NA_real_)   # <- fill in real vessel/gear limits
)

#' Flag suspect/impossible readings for filtering in analysis and plots
#'
#' @param df    Tibble in Poseidat schema (raw or thinned) -- must have
#'              value_type, value_numeric, and (for POSITION/TRAWL_TENSION)
#'              their dedicated columns
#' @param rules Tibble with value_type/min_valid/max_valid, default
#'              `default_quality_rules`. NA bound = unchecked in that direction
#' @return      df with quality_flag ("ok" | "out_of_range") and quality_note
#'              (character, reason -- NA when ok) columns added
#' @export
add_quality_flags <- function(df, rules = default_quality_rules) {

  # rules with a vessel_name column (e.g. from suggest_quality_rules()) join
  # on vessel_name + value_type -- different vessels/sensors can have
  # different bounds. Rules without vessel_name (e.g. the built-in
  # default_quality_rules) join on value_type alone and apply to every vessel.
  join_cols <- if ("vessel_name" %in% names(rules)) c("vessel_name", "value_type")
              else "value_type"

  df <- df |>
    dplyr::left_join(rules, by = join_cols)

  df |>
    dplyr::mutate(
      quality_note = dplyr::case_when(
        # POSITION: hard physical limits, defensible regardless of vessel
        value_type == "POSITION" & (latitude < -90 | latitude > 90) ~
          "latitude outside [-90, 90]",
        value_type == "POSITION" & (longitude < -180 | longitude > 180) ~
          "longitude outside [-180, 180]",
        value_type == "POSITION" & speed_over_ground < 0 ~
          "negative speed_over_ground",
        value_type == "POSITION" & (course_made_good < 0 | course_made_good >= 360) ~
          "course_made_good outside [0, 360)",

        # TRAWL_TENSION: traction can't be negative; no vessel-specific upper
        # bound set here -- add one via `rules` if you know the winch's rated
        # capacity (would need extending this branch to read it, since
        # TRAWL_TENSION doesn't use value_numeric/rules the way scalars do)
        value_type == "TRAWL_TENSION" & (
          dplyr::coalesce(traction_port, 0)      < 0 |
          dplyr::coalesce(traction_center, 0)    < 0 |
          dplyr::coalesce(traction_starboard, 0) < 0
        ) ~ "negative traction reading",

        # Generic scalar types matched in `rules` (FUEL_CONSUMPTION, DEPTH, ...)
        !is.na(min_valid) & value_numeric < min_valid ~
          sprintf("value_numeric %.2f below min_valid %.2f", value_numeric, min_valid),
        !is.na(max_valid) & value_numeric > max_valid ~
          sprintf("value_numeric %.2f above max_valid %.2f", value_numeric, max_valid),

        TRUE ~ NA_character_
      ),
      quality_flag = dplyr::if_else(is.na(quality_note), "ok", "out_of_range")
    ) |>
    dplyr::select(-min_valid, -max_valid)
}

#' Suggest per-vessel quality bounds from existing raw data
#'
#' Uses Tukey's IQR fences (Q1 - k*IQR, Q3 + k*IQR), computed SEPARATELY per
#' vessel + value_type -- different vessels/sensors get bounds fitted to
#' their own data rather than one hardcoded set applied to everyone. IQR
#' fences are used rather than mean +/- SD because SD itself gets dragged
#' around by the same outliers you're trying to catch; quantiles are far
#' more robust to that.
#'
#' These are STARTING POINTS to review, not final answers -- if the early
#' part of a vessel's time series has spurious readings (e.g. before a
#' sensor was properly calibrated or installed), the suggested bounds will
#' reflect that and should be widened/tightened by hand. Use
#' print_quality_suggestions() to review them in a readable table before
#' committing to any of them.
#'
#' @param source      Filter to one source, or NULL for all
#' @param vessel_name Filter to one vessel, or NULL for all
#' @param k           IQR multiplier for the fences (default 3, the
#'                    conventional "extreme outlier" threshold -- use a
#'                    smaller k, e.g. 1.5, for tighter/more aggressive bounds)
#' @param min_n       Skip vessel/value_type combinations with fewer than
#'                    this many observations (default 30) -- too few points
#'                    for quantile-based bounds to mean much
#' @return Tibble: vessel_name, value_type, n, obs_min, median, obs_max,
#'         suggested_min, suggested_max
#' @export
suggest_quality_rules <- function(source = NULL, vessel_name = NULL,
                                  k = 3, min_n = 30) {

  base  <- .onedrive()
  files <- list.files(file.path(base, "raw"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)

  if (length(files) == 0) {
    message("No raw parquet files found")
    return(invisible(tibble()))
  }

  raw <- map_dfr(files, function(f) {
    tryCatch({
      df <- read_parquet(f)
      if (!all(c("value_type", "value_numeric", "journal_id") %in% names(df)))
        return(tibble())
      df |>
        dplyr::filter(!is.na(value_numeric)) |>
        dplyr::select(source, journal_id, value_type, value_numeric)
    }, error = \(e) tibble())
  })

  if (nrow(raw) == 0) {
    message("No scalar (value_numeric) data found -- POSITION/TRAWL_TENSION ",
           "have their own dedicated columns and aren't covered here.")
    return(invisible(tibble()))
  }

  # Join vessel names from saved config -- same convention as poseidat_value_types()
  vessels <- tryCatch({
    readRDS(file.path(base, "config", "poseidat_vessels.rds")) |>
      dplyr::select(journal_id, vessel_name)
  }, error = function(e) tibble(journal_id = character(), vessel_name = character()))

  raw <- raw |> dplyr::left_join(vessels, by = "journal_id")

  if (!is.null(source))      raw <- dplyr::filter(raw, source %in% .env$source)
  if (!is.null(vessel_name)) raw <- dplyr::filter(raw, vessel_name %in% .env$vessel_name)

  raw |>
    dplyr::group_by(vessel_name, value_type) |>
    dplyr::filter(dplyr::n() >= min_n) |>
    dplyr::summarise(
      n       = dplyr::n(),
      obs_min = min(value_numeric),
      q1      = stats::quantile(value_numeric, 0.25, na.rm = TRUE),
      median  = stats::median(value_numeric),
      q3      = stats::quantile(value_numeric, 0.75, na.rm = TRUE),
      obs_max = max(value_numeric),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      iqr           = q3 - q1,
      suggested_min = round(q1 - k * iqr, 1),
      suggested_max = round(q3 + k * iqr, 1),
      # Never suggest a negative floor for types that can't physically be
      # negative, regardless of what the quantiles alone would say.
      suggested_min = dplyr::if_else(
        value_type %in% c("FUEL_CONSUMPTION", "DEPTH") & suggested_min < 0,
        0, suggested_min
      )
    ) |>
    dplyr::select(vessel_name, value_type, n, obs_min, median, obs_max,
                 suggested_min, suggested_max) |>
    dplyr::arrange(vessel_name, value_type)
}

#' Print quality-rule suggestions as a readable table (not a raw tibble)
#'
#' @param suggestions Tibble from suggest_quality_rules()
#' @export
print_quality_suggestions <- function(suggestions) {

  if (nrow(suggestions) == 0) {
    cat("No suggestions to show.\n")
    return(invisible(NULL))
  }

  for (v in unique(suggestions$vessel_name)) {

    cat(sprintf("\n=== %s ===\n", v))
    cat(sprintf("  %-18s %6s %13s %13s %13s %18s\n",
               "Sensor", "N", "Obs. min", "Median", "Obs. max", "Suggested range"))
    cat(sprintf("  %s\n", strrep("-", 84)))

    rows <- suggestions |> dplyr::filter(vessel_name == v)

    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      cat(sprintf("  %-18s %6d %13.1f %13.1f %13.1f %10.1f to %-6.1f\n",
                 r$value_type, r$n, r$obs_min, r$median, r$obs_max,
                 r$suggested_min, r$suggested_max))
    }
  }
  cat("\n")
  invisible(suggestions)
}


# ============================================================================
# 6. poseidat_thin()
#
# Summarises raw parquet data to a regular time interval (default 1 minute).
# Reduces storage and speeds up plotting without losing key patterns.
#
# Aggregation rules per value_type:
#   POSITION      -> mean lat/lon, last course/speed, max n_satellites
#   TRAWL_TENSION -> mean traction and shooted length per warp
#   All others    -> mean value_numeric
#
# @param source       Filter to one source, or NULL for all
# @param vessel_name  Filter to specific vessels, or NULL for all
# @param start_date   Date or NULL
# @param end_date     Date or NULL
# @param interval_min Aggregation interval in minutes (default 1)
# @param save         If TRUE, write thinned parquet files to processed/ folder
# ============================================================================

# NA- and Inf-safe integer maximum (all-NA group -> NA_integer_, never -Inf)
.max_int <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_integer_ else as.integer(max(x))
}

poseidat_thin <- function(source              = NULL,
                          vessel_name         = NULL,
                          start_date          = NULL,
                          end_date            = NULL,
                          interval_min        = 1,
                          exclude_value_types = NULL,
                          only_labelled       = FALSE,  # TRUE = skip devices not in device_labels
                          full_rebuild        = FALSE,  # TRUE = all months; FALSE = recent months only
                          recent_months       = 1,      # previous N months + current month
                          save                = FALSE) {
  
  if (save && !full_rebuild && (!is.null(start_date) || !is.null(end_date)))
    stop("With save = TRUE, start_date/end_date are only allowed together with full_rebuild = TRUE")
  
  base     <- .onedrive()
  cfg_path <- file.path(base, "config", "poseidat_vessels.rds")
  cfg_full <- tryCatch(readRDS(cfg_path), error = function(e) NULL)
  
  files <- list.files(file.path(base, "raw"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  if (!is.null(source))
    files <- files[grepl(paste0("/", source, "/"), files)]
  if (!is.null(vessel_name)) {
    pattern <- paste(gsub(" ", "_", vessel_name), collapse = "|")
    files   <- files[grepl(pattern, files)]
  }
  if (length(files) == 0) { message("No raw parquet files found"); return(invisible(tibble())) }
  
  # ---- 1. Optionally drop raw files of devices that are not in device_labels ----
  if (only_labelled && !is.null(cfg_full)) {
    uuid_re <- "([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})_[0-9]{6}"
    dev_of_file <- ifelse(grepl(uuid_re, basename(files)),
                          sub(paste0("^.*", uuid_re, ".*$"), "\\1", basename(files)),
                          NA_character_)
    drop <- rep(FALSE, length(files))
    for (k in seq_len(nrow(cfg_full))) {
      lbl <- cfg_full$device_labels[[k]]
      if (!isTRUE(cfg_full$enabled[k]) || length(lbl) == 0) next
      of_vessel <- grepl(gsub(" ", "_", cfg_full$vessel_name[k]), files, fixed = TRUE)
      drop      <- drop | (of_vessel & !is.na(dev_of_file) & !(dev_of_file %in% names(lbl)))
    }
    if (any(drop))
      message(sprintf("only_labelled: skipping %d raw file(s) of devices not in device_labels",
                      sum(drop)))
    files <- files[!drop]
    if (length(files) == 0) { message("No raw files left after only_labelled filter"); return(invisible(tibble())) }
  }
  
  # ---- 2. Restrict to recent months, unless a full rebuild is requested --------
  window_start <- NULL
  vessels_full <- character()
  
  if (!full_rebuild) {
    window_start <- if (!is.null(start_date)) {
      as.Date(start_date)
    } else {
      seq(lubridate::floor_date(Sys.Date(), "month"),
          by = paste0("-", recent_months, " month"), length.out = 2)[2]
    }
    
    # Vessels without any thinned file yet need a one-time full build
    if (save) {
      scope <- if (!is.null(vessel_name)) vessel_name else if (!is.null(cfg_full)) {
        cfg_full$vessel_name[cfg_full$enabled & (is.null(source) | cfg_full$source %in% source)]
      } else character()
      for (vn in scope) {
        tdir <- file.path(base, "processed", "thinned", .safe(vn))
        if (length(list.files(tdir, pattern = "\\.parquet$")) == 0)
          vessels_full <- c(vessels_full, vn)
      }
      if (length(vessels_full) > 0)
        message("No thinned file yet -> full build for: ", paste(vessels_full, collapse = ", "))
    }
    
    file_month <- lubridate::ymd(
      sub(".*/([0-9]{4})/([0-9]{2})/[^/]+$", "\\1-\\2-01", files), quiet = TRUE)
    keep <- is.na(file_month) | file_month >= as.Date(format(window_start, "%Y-%m-01"))
    if (length(vessels_full) > 0)
      keep <- keep | grepl(paste(gsub(" ", "_", vessels_full), collapse = "|"), files)
    
    message(sprintf("Recent-months mode: %d of %d raw file(s), months from %s (use full_rebuild = TRUE for all months)",
                    sum(keep), length(keep), format(window_start, "%Y-%m")))
    files <- files[keep]
    if (length(files) == 0) { message("No raw files in the recent window"); return(invisible(tibble())) }
  }
  
  # ---- 3. Thin one raw file --------------------------------------------------
  thin_file <- function(f) {
    tryCatch({
      df <- read_parquet(f)
      if (!"datetime" %in% names(df)) return(tibble())
      
      if (!is.null(exclude_value_types))
        df <- df |> dplyr::filter(!value_type %in% exclude_value_types)
      if (nrow(df) == 0) return(tibble())
      
      if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= start_date)
      if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= end_date)
      if (nrow(df) == 0)        return(tibble())
      
      df <- distinct(df, journal_id, device_id, datetime, .keep_all = TRUE)
      
      secs <- interval_min * 60L
      df   <- df |>
        mutate(time_bin = as.POSIXct(floor(as.numeric(datetime) / secs) * secs,
                                     origin = "1970-01-01", tz = "UTC"))
      
      has   <- function(col) col %in% names(df)
      parts <- list()
      
      if (has("latitude") && any(!is.na(df$latitude))) {
        parts[["position"]] <- df |>
          filter(value_type == "POSITION") |>
          group_by(source, journal_id, device_id, value_type, time_bin) |>
          summarise(
            latitude          = mean(latitude,          na.rm = TRUE),
            longitude         = mean(longitude,         na.rm = TRUE),
            course_made_good  = last(course_made_good),
            speed_over_ground = mean(speed_over_ground, na.rm = TRUE),
            n_satellites      = .max_int(n_satellites),
            n_raw             = n(),
            .groups           = "drop"
          )
      }
      
      if (has("traction_port") && any(!is.na(df$traction_port))) {
        parts[["trawl"]] <- df |>
          filter(value_type == "TRAWL_TENSION") |>
          group_by(source, journal_id, device_id, value_type, time_bin) |>
          summarise(
            traction_port            = mean(traction_port,            na.rm = TRUE),
            traction_center          = mean(traction_center,          na.rm = TRUE),
            traction_starboard       = mean(traction_starboard,       na.rm = TRUE),
            shooted_length_port      = mean(shooted_length_port,      na.rm = TRUE),
            shooted_length_center    = mean(shooted_length_center,    na.rm = TRUE),
            shooted_length_starboard = mean(shooted_length_starboard, na.rm = TRUE),
            n_raw                    = n(),
            .groups                  = "drop"
          )
      }
      
      scalar_types <- df |>
        filter(!value_type %in% c("POSITION", "TRAWL_TENSION"), !is.na(value_numeric)) |>
        pull(value_type) |> unique()
      
      if (length(scalar_types) > 0) {
        parts[["scalar"]] <- df |>
          filter(value_type %in% scalar_types) |>
          group_by(source, journal_id, device_id, value_type, time_bin) |>
          summarise(value_numeric = mean(value_numeric, na.rm = TRUE),
                    n_raw         = n(),
                    .groups       = "drop")
      }

      # No usable rows of any type (e.g. a fuel file where every value_numeric
      # is NA): bind_rows(list()) has no columns, so rename() would fail with
      # "Column `time_bin` doesn't exist" (Polar fuel 202611, 2026-11-26).
      if (length(parts) == 0) {
        message("  No usable values in ", basename(f),
                " (all value_numeric / lat-lon / traction NA) -- check the raw file")
        return(tibble())
      }

      bind_rows(parts) |> rename(datetime = time_bin)
      
    }, error = \(e) { message("Error in ", basename(f), ": ", conditionMessage(e)); tibble() })
  }
  
  n_files     <- length(files)
  cat(sprintf("Thinning %d file(s)...\n", n_files))
  result_list <- vector("list", n_files)
  t_start     <- Sys.time()
  
  for (fi in seq_along(files)) {
    result_list[[fi]] <- thin_file(files[fi])
    
    verstreken <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    resterend  <- verstreken / fi * (n_files - fi)
    cat(sprintf("  [%d/%d] %s  (%d rows)  |  elapsed: %ds  |  remaining: ~%ds\n",
                fi, n_files, basename(files[fi]), nrow(result_list[[fi]]),
                round(verstreken), round(resterend)))
  }
  result <- bind_rows(result_list)
  rm(result_list)
  
  if (nrow(result) == 0) { message("No data after thinning"); return(invisible(tibble())) }
  
  # ---- 4. Combine across files -----------------------------------------------
  group_cols <- c("source", "journal_id", "device_id", "value_type", "datetime")
  
  pos_result <- result |>
    filter(value_type == "POSITION") |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(any_of(c("latitude", "longitude", "speed_over_ground")),
             \(x) mean(x, na.rm = TRUE)),
      across(any_of("course_made_good"), last),
      across(any_of("n_satellites"), .max_int),
      n_raw = sum(n_raw, na.rm = TRUE),
      .groups = "drop"
    )
  
  trawl_result <- result |>
    filter(value_type == "TRAWL_TENSION") |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(any_of(c("traction_port", "traction_center", "traction_starboard",
                      "shooted_length_port", "shooted_length_center",
                      "shooted_length_starboard")),
             \(x) mean(x, na.rm = TRUE)),
      n_raw = sum(n_raw, na.rm = TRUE),
      .groups = "drop"
    )
  
  # any_of(): when a vessel only has POSITION / TRAWL_TENSION in the window
  # (e.g. Lub Senior with all fuel sensors silent), value_numeric does not
  # exist in `result` and a bare mean(value_numeric) errors out -- which
  # aborted the whole thinning for that vessel (2026-11-26).
  scalar_result <- result |>
    filter(!value_type %in% c("POSITION", "TRAWL_TENSION")) |>
    group_by(across(all_of(group_cols))) |>
    summarise(across(any_of("value_numeric"), \(x) mean(x, na.rm = TRUE)),
              n_raw         = sum(n_raw, na.rm = TRUE),
              .groups       = "drop")
  
  result <- bind_rows(pos_result, trawl_result, scalar_result)
  
  vessels <- if (!is.null(cfg_full)) {
    cfg_full |> dplyr::select(journal_id, vessel_name, dplyr::any_of("vessel_code"))
  } else tibble(journal_id = character(), vessel_name = character())
  result <- left_join(result, vessels, by = "journal_id")
  
  cat(sprintf("Thinned to %s records at %d-minute intervals\n",
              format(nrow(result), big.mark = ","), interval_min))
  
  # ---- 5. Save: merge into the existing thinned file unless full build --------
  if (save) {
    vessels_in_result <- unique(result$vessel_name)
    vessels_in_result <- vessels_in_result[!is.na(vessels_in_result)]
    
    for (vn in vessels_in_result) {
      vessel_dir <- file.path(base, "processed", "thinned", .safe(vn))
      dir.create(vessel_dir, recursive = TRUE, showWarnings = FALSE)
      
      nw <- filter(result, vessel_name == vn)
      
      if (!full_rebuild && !(vn %in% vessels_full)) {
        win_dt <- as.POSIXct(format(window_start), tz = "UTC")
        
        existing <- sort(list.files(vessel_dir,
                                    pattern = sprintf("_thinned_%dmin_[0-9]{8}\\.parquet$", interval_min),
                                    full.names = TRUE), decreasing = TRUE)
        # Read via a temp copy (Arrow's memory-mapped read locks the file on Windows)
        tmp <- tempfile(fileext = ".parquet")
        file.copy(existing[1], tmp, overwrite = TRUE)
        old <- read_parquet(tmp)
        unlink(tmp)
        
        # Carry over trip ids of rows that are replaced (same key), if present
        old_window <- old |>
          filter(datetime >= win_dt) |>
          select(all_of(group_cols), any_of(c("trip_id", "trip_uid"))) |>
          distinct(across(all_of(group_cols)), .keep_all = TRUE)
        
        old_keep <- filter(old, datetime < win_dt)
        if (only_labelled && !is.null(cfg_full)) {
          lbl <- cfg_full$device_labels[[which(cfg_full$vessel_name == vn)[1]]]
          if (length(lbl) > 0) old_keep <- filter(old_keep, device_id %in% names(lbl))
        }
        
        nw  <- nw |>
          filter(datetime >= win_dt) |>
          left_join(old_window, by = group_cols)
        out <- bind_rows(old_keep, nw)
        message(sprintf("  %s: kept %s older rows, replaced window with %s rows",
                        vn, format(nrow(old_keep), big.mark = ","),
                        format(nrow(nw), big.mark = ",")))
      } else {
        out <- nw
      }
      
      out_file <- file.path(vessel_dir,
                            sprintf("%s_thinned_%dmin_%s.parquet",
                                    .safe(vn), interval_min, format(Sys.Date(), "%Y%m%d")))
      write_parquet(out, out_file)
      message(sprintf("  Saved %s -> %s", vn, basename(out_file)))
      
      existing <- sort(list.files(vessel_dir, pattern = "\\.parquet$", full.names = TRUE),
                       decreasing = TRUE)
      to_delete <- existing[seq_along(existing) > 3]
      if (length(to_delete) > 0) {
        file.remove(to_delete)
        message(sprintf("  Cleaned %d old thinned file(s) for %s", length(to_delete), vn))
      }
    }
  }
  
  invisible(result)
}


# ============================================================================
# Quick usage examples — run these after sourcing this file
# ============================================================================

# # 1. What files exist and how big are they?
# poseidat_files()
#
# # 2. What value_types are stored per vessel/device?
# poseidat_value_types()
#
# # 3. Daily coverage — spot gaps
# poseidat_daily()
# poseidat_daily(vessel_name = "Lub Senior", value_type = "FUEL_CONSUMPTION")
#
# # 4. Statistical summary of non-zero measurements
# poseidat_summary()
# poseidat_summary(value_type = c("FUEL_CONSUMPTION", "TEMPERATURE"))
#
# # 5. See what would be removed if you kept only certain value_types
# poseidat_filter_save(
#   keep_value_types = c("POSITION", "SPEED", "FUEL_CONSUMPTION"),
#   dry_run = TRUE
# )
# # Then apply:
# poseidat_filter_save(
#   keep_value_types = c("POSITION", "SPEED", "FUEL_CONSUMPTION"),
#   dry_run = FALSE
# )


# ============================================================================
# 6. poseidat_positions()
#
# Load POSITION data from parquet files, ready for mapping or time series.
# Returns a tidy tibble with lat/lon and vessel name.
#
# @param source       Filter to one API source, or NULL for all
# @param vessel_name  Character vector of vessel names, or NULL for all
# @param data_source  "thinned" (default) | "raw"
#                     "thinned" reads from processed/thinned/ (faster, 1-min resolution)
#                     "raw"     reads from raw/ (~10-sec resolution, slower)
# @param start_date   Date or NULL
# @param end_date     Date or NULL
# @param thin_minutes Display-level thinning: keep one fix per vessel per N minutes.
#                     Applied on top of data_source. NULL = keep all (default).
#                     Typical values: 5 for overview maps, 1 for detailed tracks.
# ============================================================================


# ============================================================================
# VESSEL-LEVEL RAW/THINNED INVENTORY  (per-device granularity)
# ============================================================================

#' Raw sensor/date inventory for one vessel, grouped by device
#'
#' Lazily scans a vessel's raw Parquet files via arrow (no full read into
#' memory) and summarises record count and date range per device/value_type.
#' Grouping by device_id (not just value_type) matters because several
#' devices on the same vessel can share a value_type (e.g. multiple FUEL_*
#' devices all reporting FUEL_CONSUMPTION) — aggregating by value_type alone
#' would hide per-sensor detail.
#'
#' @param vessel_name Character. Exact vessel name (as used in raw/ folder names)
#' @param source      Character. "dbmatic" | "efice" | "pefa" (default "efice")
#' @return Tibble: device_id, value_type, raw_n, raw_first, raw_last
#' @export
poseidat_raw_inventory <- function(vessel_name, source = "efice") {

  raw_files <- fs::dir_ls(.raw_dir(source, vessel_name),
                          recurse = TRUE, glob = "*.parquet", fail = FALSE)

  if (length(raw_files) == 0) {
    return(tibble(device_id = character(), value_type = character(),
                  raw_n = integer(), raw_first = as.POSIXct(character()),
                  raw_last = as.POSIXct(character())))
  }

  arrow::open_dataset(raw_files) |>
    dplyr::group_by(device_id, value_type) |>
    dplyr::summarise(
      raw_n     = dplyr::n(),
      raw_first = min(datetime, na.rm = TRUE),
      raw_last  = max(datetime, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::collect()
}

#' Thinned sensor/date inventory for one vessel, grouped by device
#'
#' Reads only the single most recent thinned Parquet file for the vessel
#' (poseidat_thin() keeps the 3 most recent per vessel; the latest is the
#' current state) and summarises per device/value_type.
#'
#' @param vessel_name Character. Exact vessel name
#' @return Tibble: device_id, value_type, thinned_n, thinned_first,
#'   thinned_last, thinned_file_date
#' @export
poseidat_thinned_inventory <- function(vessel_name) {

  vessel_dir <- file.path(.onedrive(), "processed", "thinned", .safe(vessel_name))
  files <- if (dir.exists(vessel_dir))
    sort(list.files(vessel_dir, pattern = "\\.parquet$", full.names = TRUE))
  else character(0)

  if (length(files) == 0) {
    return(tibble(device_id = character(), value_type = character(),
                  thinned_n = integer(), thinned_first = as.POSIXct(character()),
                  thinned_last = as.POSIXct(character()),
                  thinned_file_date = as.Date(character())))
  }

  latest_file <- tail(files, 1)
  file_date   <- stringr::str_extract(latest_file, "[0-9]{8}") |> lubridate::ymd()

  arrow::read_parquet(latest_file, col_select = c("device_id", "value_type", "datetime")) |>
    dplyr::group_by(device_id, value_type) |>
    dplyr::summarise(
      thinned_n     = dplyr::n(),
      thinned_first = min(datetime, na.rm = TRUE),
      thinned_last  = max(datetime, na.rm = TRUE),
      .groups       = "drop"
    ) |>
    dplyr::mutate(thinned_file_date = file_date)
}

#' Friendly device-name lookup for one vessel, from vessel_config
#'
#' @param vessel_config Tibble. From Poseidat_config.R / readRDS()
#' @param vessel_name   Character. Exact vessel name as in vessel_config
#' @return Tibble: device_id, device_name
#' @export
poseidat_device_labels <- function(vessel_config, vessel_name) {
  labels <- vessel_config |>
    dplyr::filter(vessel_name == .env$vessel_name) |>
    dplyr::pull(device_labels) |>
    purrr::pluck(1)

  tibble(device_id = names(labels), device_name = unlist(unname(labels)))
}

#' Combined raw-vs-thinned inventory for one vessel, with device labels
#'
#' Joins poseidat_raw_inventory(), poseidat_thinned_inventory(), and
#' poseidat_device_labels() into a single per-device/value_type table, with
#' `lag_days` showing how far the thinned data has fallen behind raw. This is
#' the table poseidat_sync_vessel_stepwise() uses to decide whether thinning
#' needs to run and to report before/after state.
#'
#' @param vessel_name   Character. Exact vessel name
#' @param vessel_config Tibble. From Poseidat_config.R / readRDS()
#' @param source        Character. "dbmatic" | "efice" | "pefa" (default "efice")
#' @return Tibble: vessel_name, device_name, device_id, value_type,
#'   raw_n, raw_first, raw_last, thinned_n, thinned_first, thinned_last,
#'   thinned_file_date, lag_days
#' @export
poseidat_vessel_overview <- function(vessel_name, vessel_config, source = "efice") {

  raw     <- poseidat_raw_inventory(vessel_name, source)
  thinned <- poseidat_thinned_inventory(vessel_name)
  labels  <- poseidat_device_labels(vessel_config, vessel_name)

  raw |>
    dplyr::full_join(thinned, by = c("device_id", "value_type")) |>
    dplyr::left_join(labels, by = "device_id") |>
    dplyr::mutate(
      vessel_name = vessel_name,
      lag_days    = as.numeric(difftime(raw_last, thinned_last, units = "days"))
    ) |>
    dplyr::relocate(vessel_name, device_name, device_id, value_type) |>
    dplyr::arrange(device_name, value_type)
}


# ============================================================================
# INTERACTIVE VESSEL-BY-VESSEL STEPWISE SYNC (diagnostic / manual use)
# ============================================================================

#' Sync and thin a single vessel, step by step, with console output and timing
#'
#' Interactive/diagnostic counterpart to poseidat_daily_sync_all() — runs one
#' vessel through: inventory (before) -> catch-up thinning if raw has moved
#' ahead of the last thinned snapshot -> sync -> thinning -> inventory
#' (after), printing each step's result and timing to the console.
#'
#' Earlier versions of this function had a separate "step 3" that queried the
#' API for the latest available record before syncing, as a cheap pre-check.
#' That step was removed: it depended on `sortDirection = "descending"`,
#' which was confirmed not to work on this API (see poseidat_audit_devices()
#' docs for the full explanation) — there is no cheap way to know what's
#' newest without actually downloading it, so "what's new" is now reported
#' directly from the sync results themselves.
#'
#' @param vessel_name         Character. Exact vessel name as in vessel_config
#' @param vessel_config       Tibble. From Poseidat_config.R / readRDS()
#' @param source              Character. "dbmatic" | "efice" | "pefa" (default "efice")
#' @param lag_threshold_days  Numeric. Re-thin only if raw is ahead of the
#'                            last thinned snapshot by more than this many
#'                            days (default 1)
#' @param page_size           Integer. API page size for the sync (default 5000)
#' @param exclude_value_types Character vector or NULL. Value types dropped
#'                            before thinning (default "TEMPERATURE" — the
#'                            FUEL_* devices' temperature channel is always
#'                            zero/NA and roughly doubles thinning cost for
#'                            no benefit)
#' @return Invisibly, a list(before, new_data, sync_results, after, timings)
#' @export
poseidat_sync_vessel_stepwise <- function(vessel_name,
                                          vessel_config,
                                          source              = "efice",
                                          lag_threshold_days  = 1,
                                          page_size           = 5000,
                                          exclude_value_types = "TEMPERATURE") {

  timings <- tibble(step = character(), seconds = numeric())
  log_step_time <- function(label, t0) {
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("    [%.1fs]\n", secs))
    timings <<- dplyr::bind_rows(timings, tibble(step = label, seconds = secs))
  }

  cat(sprintf("\n========================================================\n"))
  cat(sprintf("  POSEIDAT STEPWISE SYNC — %s\n", vessel_name))
  cat(sprintf("========================================================\n\n"))

  # ---- 1. Raw vs thinned inventory, before --------------------------------
  cat("--- Step 1: raw vs thinned inventory (before) ---\n")
  t0 <- Sys.time()
  overview_before <- poseidat_vessel_overview(vessel_name, vessel_config, source)
  print(overview_before)
  log_step_time("1_inventory_before", t0)

  # ---- 2. Catch up thinning if it's lagging raw ---------------------------
  max_lag <- overview_before |> dplyr::summarise(m = max(lag_days, na.rm = TRUE)) |> dplyr::pull(m)

  t0 <- Sys.time()
  if (is.finite(max_lag) && max_lag > lag_threshold_days) {
    cat(sprintf("\n--- Step 2: thinned lags raw by %.1f day(s) — updating ---\n", max_lag))
    poseidat_thin(source = source, vessel_name = vessel_name,
                 exclude_value_types = exclude_value_types, save = TRUE)
  } else {
    cat("\n--- Step 2: thinned files already up to date with raw — skipping ---\n")
  }
  log_step_time("2_thin_catchup", t0)

  # ---- 3. Retrieve new data & determine what's new from the result --------
  cat("\n--- Step 3: retrieving new data & determining what's new from the result ---\n")
  t0 <- Sys.time()
  sync_results <- sync_all_vessels(vessel_config,
                                   vessel_names        = vessel_name,
                                   sources             = source,
                                   lookback_days       = 30,
                                   end_time            = NULL,
                                   force_full          = FALSE,
                                   page_size           = page_size,
                                   sleep_between_pages = 0)
  log_step_time("3_sync_new_data", t0)

  new_data_summary <- tibble(key = names(sync_results)) |>
    tidyr::separate(key, into = c("src", "journal_id", "device_id"), sep = "__", extra = "merge") |>
    dplyr::mutate(
      n_new      = purrr::map_int(sync_results, nrow),
      new_latest = purrr::map_dbl(sync_results,
                                  ~ if (nrow(.x) == 0) NA_real_ else as.numeric(max(.x$datetime, na.rm = TRUE))) |>
                   as.POSIXct(origin = "1970-01-01", tz = "UTC")
    )
  print(new_data_summary |> dplyr::select(device_id, n_new, new_latest))

  # ---- 4. Regenerate thinned files -----------------------------------------
  cat("\n--- Step 4: updating thinned files after new data ---\n")
  t0 <- Sys.time()
  poseidat_thin(source = source, vessel_name = vessel_name,
               exclude_value_types = exclude_value_types, save = TRUE)
  log_step_time("4_thin_final", t0)

  # ---- 5. Raw vs thinned inventory, after -----------------------------------
  cat("\n--- Step 5: raw vs thinned inventory (after) ---\n")
  t0 <- Sys.time()
  overview_after <- poseidat_vessel_overview(vessel_name, vessel_config, source)
  print(overview_after)
  log_step_time("5_inventory_after", t0)

  cat("\n--- Step timings ---\n")
  print(timings |> dplyr::mutate(pct = round(100 * seconds / sum(seconds), 1)))

  invisible(list(before = overview_before, new_data = new_data_summary,
                sync_results = sync_results, after = overview_after, timings = timings))
}


poseidat_positions <- function(source       = NULL,
                               vessel_name  = NULL,
                               data_source  = "thinned",
                               start_date   = NULL,
                               end_date     = NULL,
                               thin_minutes = NULL) {

  data_source <- match.arg(data_source, c("thinned", "raw"))

  result <- .poseidat_load_data(
    data_source, source, vessel_name,
    start_date = start_date, end_date = end_date, value_types = "POSITION",
    cols = c("latitude", "longitude", "course_made_good",
             "speed_over_ground", "n_satellites"))

  if (nrow(result) == 0) {
    message("No parquet files found")
    return(tibble())
  }

  if (!all(c("latitude", "longitude", "value_type") %in% names(result))) {
    message("POSITION columns not found in loaded data")
    return(tibble())
  }

  result <- result |>
    filter(value_type == "POSITION", !is.na(latitude), !is.na(longitude))

  if (!is.null(start_date)) result <- filter(result, as.Date(datetime) >= start_date)
  if (!is.null(end_date))   result <- filter(result, as.Date(datetime) <= end_date)

  if (nrow(result) == 0) {
    message("No POSITION records found — have you synced POSITION data?")
    return(tibble())
  }

  result <- distinct(result, journal_id, device_id, datetime, .keep_all = TRUE)

  # Display-level thinning: keep one fix per vessel per N-minute bin
  if (!is.null(thin_minutes)) {
    result <- result |>
      mutate(time_bin = floor(as.numeric(datetime) / (thin_minutes * 60))) |>
      group_by(source, journal_id, time_bin) |>
      slice_head(n = 1) |>
      ungroup() |>
      dplyr::select(-time_bin)
  }

  result |>
    arrange(vessel_name, datetime) |>
    dplyr::select(source, vessel_name, dplyr::any_of("vessel_code"), journal_id, datetime,
                  latitude, longitude, course_made_good,
                  speed_over_ground, n_satellites)
}


# ============================================================================
# 7. poseidat_map()
#
# Interactive leaflet map of vessel tracks, coloured by vessel.
# Requires: leaflet
#
# Can be called two ways:
#   poseidat_map()                        — loads data automatically
#   poseidat_map(pos = poseidat_positions(...))  — use pre-built tibble
#
# @param pos          Pre-built tibble from poseidat_positions(), or NULL to
#                     load data automatically using the params below.
# @param source       Filter to one API source, or NULL for all
# @param vessel_name  Character vector of vessel names, or NULL for all
# @param data_source  "thinned" (default) | "raw"
#                     Passed to poseidat_positions() when pos = NULL.
# @param start_date   Date or NULL
# @param end_date     Date or NULL
# @param thin_minutes Display-level thinning (default 5 min). Coarser = faster map.
#                     Set NULL to skip thinning (use when pos is already thinned).
# @param show_points  Draw a circle marker per fix (default TRUE).
# @param show_lines   Draw a connecting line per vessel track (default TRUE).
#                     At least one of show_points/show_lines must stay TRUE.
# ============================================================================

poseidat_map <- function(pos          = NULL,
                         source       = NULL,
                         vessel_name  = NULL,
                         data_source  = "thinned",
                         start_date   = NULL,
                         end_date     = NULL,
                         thin_minutes = 5,
                         show_points  = TRUE,
                         show_lines   = TRUE) {

  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("Install leaflet: install.packages('leaflet')")
  if (!show_points && !show_lines)
    stop("At least one of show_points / show_lines must be TRUE")

  library(leaflet)

  # Load data if not supplied
  if (is.null(pos)) {
    pos <- poseidat_positions(
      source       = source,
      vessel_name  = vessel_name,
      data_source  = data_source,
      start_date   = start_date,
      end_date     = end_date,
      thin_minutes = thin_minutes
    )
  }

  if (nrow(pos) == 0) { message("No position data to map"); return(invisible(NULL)) }

  # Ensure vessel_name is present — join from config if missing/stale
  if (!"vessel_name" %in% names(pos) || all(is.na(pos$vessel_name))) {
    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    if (file.exists(cfg_path)) {
      vmap <- readRDS(cfg_path)[, c("journal_id", "vessel_name")]
      pos  <- pos |>
        dplyr::select(-any_of("vessel_name")) |>
        left_join(vmap, by = "journal_id")
    }
  }

  pos <- dplyr::filter(pos, !is.na(vessel_name))
  if (nrow(pos) == 0) {
    message("No position data with resolved vessel names")
    return(invisible(NULL))
  }

  vessels <- unique(pos$vessel_name)
  colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#A65628", "#F781BF")
  pal     <- setNames(colours[seq_along(vessels)], vessels)

  m <- leaflet(pos) |>
    addProviderTiles("CartoDB.Positron") |>
    addScaleBar(position = "bottomleft")

  for (vsl in vessels) {
    vdf <- filter(pos, vessel_name == vsl) |> arrange(datetime)
    col <- unname(pal[vsl])

    if (show_lines) {
      m <- m |>
        addPolylines(
          data    = vdf,
          lng     = ~longitude, lat = ~latitude,
          color   = col, weight = 1.5, opacity = 0.7,
          group   = vsl
        )
    }

    if (show_points) {
      m <- m |>
        addCircleMarkers(
          data        = vdf,
          lng         = ~longitude, lat = ~latitude,
          radius      = 3, color = col, fillColor = col,
          fillOpacity = 0.8, stroke = FALSE,
          popup       = ~paste0(
            "<b>", vessel_name, "</b><br>",
            format(datetime, "%Y-%m-%d %H:%M"), "<br>",
            "SOG: ", round(speed_over_ground, 1), " kn<br>",
            "COG: ", round(course_made_good, 0), "\u00b0"
          ),
          group = vsl
        )
    }
  }

  m |>
    addLayersControl(
      overlayGroups = vessels,
      options       = layersControlOptions(collapsed = FALSE)
    ) |>
    addLegend(
      position = "bottomright",
      colors   = pal[vessels],
      labels   = vessels,
      title    = "Vessel"
    )
}

# ============================================================================
# 8. poseidat_plot_timeseries()
#
# Time series plot of scalar value_type(s) per vessel.
# Reads from thinned parquet by default (fast); can also show raw data
# or overlay both.
#
# @param value_type          Character vector. e.g. c("FUEL_CONSUMPTION", "DEPTH").
#                            Use "POSITION" to plot latitude and/or longitude over time.
# @param data_source         "thinned" (default) | "raw" | "overlay"
#                            "thinned" : reads from processed/thinned/ (1-min averages)
#                            "raw"     : reads from raw parquet (~10-sec data, slower)
#                            "overlay" : raw points in background, thinned line on top
# @param source              Filter to one API source, or NULL for all
# @param vessel_name         Filter to specific vessels, or NULL for all
# @param device_label        Filter to specific device label(s) from config
#                            e.g. "FUEL_1" or c("FUEL_1", "FUEL_2"). NULL = all devices
# @param start_date          Date or NULL
# @param end_date            Date or NULL
# @param nonzero             If TRUE (default), exclude zero values (not applied to POSITION)
# @param by_device           If TRUE, show each device as a separate coloured line.
#                            If FALSE (default), sum all devices per vessel per timestamp.
# @param facet_by            "value_type" (default) | "vessel" | "device" |
#                            "device_x_vessel" | "both" | "none"
#                            "device_x_vessel" gives a grid with devices as rows
#                            and vessels as columns — useful for comparing the same
#                            sensor across vessels side by side.
# @param scales              ggplot2 facet scales: "free_y" (default) | "fixed" | "free"
# @param ncol                number of columns for facet: default is 1
# @param position_component  When value_type includes "POSITION": which coordinate(s) to plot.
#                            "latitude" (default) | "longitude" | "both"
#                            "both" produces two stacked panels via patchwork.
# ============================================================================

poseidat_plot_timeseries <- function(value_type,
                                     data_source          = "thinned",
                                     source               = NULL,
                                     vessel_name          = NULL,
                                     device_label         = NULL,
                                     start_date           = NULL,
                                     end_date             = NULL,
                                     nonzero              = TRUE,
                                     by_device            = FALSE,
                                     facet_by             = "value_type",
                                     scales               = "free_y",
                                     ncol                 = 1,
                                     position_component   = "latitude") {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")

  data_source        <- match.arg(data_source,        c("thinned", "raw", "overlay"))
  position_component <- match.arg(position_component, c("latitude", "longitude", "both"))

  # ── POSITION shortcut ────────────────────────────────────────────────────
  has_position <- "POSITION" %in% value_type
  scalar_types <- value_type[value_type != "POSITION"]

  if (has_position && length(scalar_types) == 0) {
    return(.plot_position_timeseries(
      data_source        = data_source,
      source             = source,
      vessel_name        = vessel_name,
      start_date         = start_date,
      end_date           = end_date,
      position_component = position_component,
      facet_by           = facet_by,
      scales             = scales,
      ncol               = ncol
    ))
  }

  if (has_position && length(scalar_types) > 0) {
    message("POSITION cannot be combined with scalar value_types in one plot.\n",
            "Plotting scalar type(s) only. Call poseidat_plot_timeseries('POSITION') ",
            "separately for coordinates.")
    value_type <- scalar_types
  }

  base        <- .onedrive()
  thinned_dir <- file.path(base, "processed", "thinned")

  # ── Helper: load thinned data (latest file per vessel) ───────────────────
  load_thinned <- function() {
    df <- .poseidat_load_data("thinned", source, vessel_name,
                              start_date = start_date, end_date = end_date,
                              value_types = value_type, cols = "value_numeric")
    if (nrow(df) == 0 ||
        !all(c("value_type", "value_numeric", "datetime") %in% names(df)))
      return(tibble())
    df
  }

  # ── Helper: load raw data ────────────────────────────────────────────────
  load_raw <- function() {
    df <- .poseidat_load_data("raw", source, vessel_name,
                              start_date = start_date, end_date = end_date,
                              value_types = value_type, cols = "value_numeric")
    if (nrow(df) == 0 ||
        !all(c("value_type", "value_numeric", "datetime") %in% names(df)))
      return(tibble())
    df
  }

  # ── Load data ─────────────────────────────────────────────────────────────
  if (data_source == "raw") {
    df_main <- load_raw()
    df_raw  <- df_main
    df_thin <- NULL
  } else if (data_source == "thinned") {
    df_main <- load_thinned()
    df_raw  <- NULL
    df_thin <- df_main
  } else {   # overlay
    df_raw  <- load_raw()
    df_thin <- load_thinned()
    df_main <- df_thin
  }

  if (nrow(df_main) == 0) {
    message("No data found in ", data_source, " parquet")
    return(invisible(NULL))
  }

  # ── Helper: filter and prepare a data frame ───────────────────────────────
  prep <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(tibble())

    # Join vessel names if missing
    if (!"vessel_name" %in% names(df) || all(is.na(df$vessel_name))) {
      cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
      if (file.exists(cfg_path)) {
        vmap <- readRDS(cfg_path)[, c("journal_id", "vessel_name")]
        df   <- df |>
          dplyr::select(-any_of("vessel_name")) |>
          left_join(vmap, by = "journal_id")
      }
    }

    df <- df |> filter(value_type %in% .env$value_type, !is.na(value_numeric))
    if (nonzero)              df <- filter(df, value_numeric != 0)
    if (!is.null(source))     df <- filter(df, source      %in% .env$source)
    if (!is.null(vessel_name))df <- filter(df, vessel_name %in% .env$vessel_name)
    if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= start_date)
    if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= end_date)

    # Device label filter — join labels from config
    if (!is.null(device_label)) {
      cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
      if (file.exists(cfg_path)) {
        cfg <- readRDS(cfg_path)
        label_map <- cfg |>
          dplyr::select(vessel_name, device_labels) |>
          tidyr::unnest_longer(device_labels,
                               indices_to = "device_id",
                               values_to  = "label") |>
          filter(label %in% .env$device_label) |>
          dplyr::select(vessel_name, device_id, label)
        df <- df |>
          left_join(label_map, by = c("vessel_name", "device_id")) |>
          filter(!is.na(label))
      }
    }

    # Build series label
    if (by_device) {
      # Use friendly label if available, else short UUID suffix
      if (!"label" %in% names(df))
        df <- df |> mutate(label = substr(device_id, nchar(device_id) - 7,
                                          nchar(device_id)))
      df <- df |> mutate(series = paste0(vessel_name, " / ", label))
    } else {
      df <- df |>
        group_by(vessel_name, value_type, datetime) |>
        summarise(value_numeric = sum(value_numeric, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(series = vessel_name)
    }
    df
  }

  df_thin <- if (!is.null(df_thin)) prep(df_thin) else NULL
  df_raw  <- if (!is.null(df_raw))  prep(df_raw)  else NULL
  df_plot <- if (!is.null(df_thin) && nrow(df_thin) > 0) df_thin else df_raw

  if (is.null(df_plot) || nrow(df_plot) == 0) {
    message("No records after filtering for value_type: ",
            paste(value_type, collapse = ", "))
    return(invisible(NULL))
  }

  # ── Facet formula ─────────────────────────────────────────────────────────
  n_types   <- length(unique(df_plot$value_type))
  n_vessels <- length(unique(df_plot$vessel_name))
  n_devices <- if ("series" %in% names(df_plot)) length(unique(df_plot$series)) else 1

  # Ensure facet_label exists for device_x_vessel grid layout
  # (label only, no vessel prefix — vessel goes to columns via facet_grid)
  if (facet_by %in% c("device", "device_x_vessel")) {
    device_label_col <- if ("label" %in% names(df_plot)) "label" else "series"
    df_plot <- df_plot |>
      mutate(facet_label = .data[[device_label_col]])
    if (!is.null(df_raw) && nrow(df_raw) > 0)
      df_raw <- df_raw |>
      mutate(facet_label = .data[[device_label_col]])
  }
  
  # Ensure a facet_label column exists for device faceting
  # if (facet_by == "device") {
  #   df_plot <- df_plot |>
  #     mutate(facet_label = if ("label" %in% names(df_plot))
  #              paste0(vessel_name, " / ", label)
  #            else
  #              series)
  #   if (!is.null(df_raw) && nrow(df_raw) > 0)
  #     df_raw <- df_raw |>
  #       mutate(facet_label = if ("label" %in% names(df_raw))
  #                paste0(vessel_name, " / ", label)
  #              else
  #                series)
  # }

  facet_layer <- switch(
    facet_by,
    value_type     = if (n_types   > 1) ggplot2::facet_wrap(~ value_type,
                                                            scales = scales, ncol = ncol)
    else               NULL,
    vessel         = if (n_vessels > 1) ggplot2::facet_wrap(~ vessel_name,
                                                            scales = scales, ncol = ncol)
    else               NULL,
    device         = if (n_devices > 1) ggplot2::facet_wrap(~ facet_label,
                                                            scales = scales, ncol = ncol)
    else               NULL,
    device_x_vessel = ggplot2::facet_grid(
      facet_label ~ vessel_name,
      scales = "free_y"   # each device row gets its own y scale
    ),
    both           = ggplot2::facet_grid(value_type ~ vessel_name, scales = scales),
    none           = NULL,
    ggplot2::facet_wrap(~ value_type, scales = scales, ncol = ncol)
  )
  
  
  # ── Subtitle ──────────────────────────────────────────────────────────────
  lo  <- format(min(df_plot$datetime, na.rm = TRUE), "%Y-%m-%d")
  hi  <- format(max(df_plot$datetime, na.rm = TRUE), "%Y-%m-%d")
  src_label <- switch(data_source,
                      thinned = "1-min thinned",
                      raw     = "raw (~10 sec)",
                      overlay = "raw + thinned overlay")
  subtitle <- sprintf("%s \u2013 %s  |  %s", lo, hi, src_label)

  # ── Build plot ─────────────────────────────────────────────────────────────
  colour_var <- "series"

  p <- ggplot2::ggplot()

  if (data_source == "overlay" && !is.null(df_raw) && nrow(df_raw) > 0) {
    p <- p +
      ggplot2::geom_point(
        data    = df_raw,
        mapping = ggplot2::aes(x = datetime, y = value_numeric,
                               colour = .data[[colour_var]]),
        alpha = 0.5, size = 1.0, shape = 16
      )
  }

  p <- p +
    ggplot2::geom_line(
      data    = df_plot,
      mapping = ggplot2::aes(x = datetime, y = value_numeric,
                             colour = .data[[colour_var]]),
      alpha = 0.8, linewidth = 0.5
    )

  p <- p +
    facet_layer +
    ggplot2::scale_colour_brewer(palette = "Set1", name = NULL) +
    ggplot2::scale_x_datetime(
      date_labels = .poseidat_date_label(df_plot$datetime),
      date_breaks = .poseidat_date_breaks(df_plot$datetime)
    ) +
    ggplot2::labs(
      title    = paste(paste(value_type, collapse = " / "), "over time"),
      subtitle = subtitle,
      x        = NULL,
      y        = "Value",
      caption  = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "bottom"
    )

  p
}

# ============================================================================
# poseidat_plot_overview()
#
# Add this function to Poseidat_functions.R, e.g. directly after
# poseidat_plot_timeseries() (~line 2700).
#
# PURPOSE
# -------
# Produces a facet_grid with ALL available time series as rows and vessels
# as columns. POSITION is handled by pivoting latitude/longitude into two
# separate rows named "Latitude" and "Longitude", giving them the same
# value_numeric treatment as scalar types.
#
# USAGE
# -----
# poseidat_plot_overview(
#   vessel_name = c("Lub Senior", "Polar", "Willem Jacob"),
#   start_date  = as.Date("2026-04-13")
# )
#
# poseidat_plot_overview(
#   vessel_name  = c("Lub Senior", "Polar"),
#   start_date   = as.Date("2026-05-01"),
#   value_types  = c("FUEL_CONSUMPTION", "DEPTH", "Latitude", "Longitude"),
#   by_device    = TRUE    # separate line per device within each panel
# )
#
# PARAMETERS
# ----------
# vessel_name   Character vector of vessel names to include. NULL = all.
# start_date    Date. Left edge of time window. NULL = all available.
# end_date      Date. Right edge. NULL = today.
# value_types   Character vector of series to show. NULL = all found in data.
#               Use the names as they appear in the value_type column, plus
#               "Latitude" and "Longitude" for position components.
# by_device     If TRUE, draw a separate coloured line per device within
#               each facet panel. If FALSE (default), sum across devices.
# data_source   "thinned" (default) | "raw"
# scales        Passed to facet_grid rows. "free_y" (default) gives each
#               row its own y scale — strongly recommended given the very
#               different units across series.
# nonzero       If TRUE (default), drop zero values before plotting.
# geom_type     "line" (default) draws a line per series, as before.
#               "point" draws points instead (size controlled by
#               point_size). "both" draws lines with points on top.
# point_size    Numeric point size used when geom_type is "point" or
#               "both" (default 1). Has no effect for geom_type = "line".
# ============================================================================

poseidat_plot_overview <- function(vessel_name  = NULL,
                                   value_types  = NULL,
                                   by_device    = FALSE,
                                   data_source  = "thinned",
                                   start_date   = NULL,
                                   end_date     = NULL,
                                   nonzero      = TRUE,
                                   scales       = "free_y",
                                   geom_type    = "line",
                                   point_size   = 1) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")
  
  data_source <- match.arg(data_source, c("thinned", "raw"))
  geom_type   <- match.arg(geom_type, c("line", "point", "both"))
  base        <- .onedrive()
  
  # ── 1. Load data ─────────────────────────────────────────────────────────────
  
  # vessel and period are filtered while loading (slim reading); all columns
  # are kept because this function pivots POSITION / TRAWL_TENSION columns.
  # Tip: always pass start_date for the full fleet.
  df_all <- .poseidat_load_data(data_source, source = NULL, vessel_name = vessel_name,
                                start_date = start_date, end_date = end_date)
  
  
  if (nrow(df_all) == 0) {
    message("No data found for data_source = '", data_source, "'")
    return(invisible(NULL))
  }
  
  # Apply vessel filter explicitly — .poseidat_load_data only filters by
  # filename for raw; thinned loads all vessel dirs regardless
  if (!is.null(vessel_name))
    df_all <- df_all |> dplyr::filter(vessel_name %in% .env$vessel_name)
  
  if (nrow(df_all) == 0) {
    message("No data after vessel filter for: ", paste(vessel_name, collapse = ", "))
    return(invisible(NULL))
  }
  
  # ── 2. Load device label map from config ──────────────────────────────────────
  
  cfg_path  <- file.path(base, "config", "poseidat_vessels.rds")
  label_map <- NULL
  if (file.exists(cfg_path)) {
    cfg <- readRDS(cfg_path)
    if ("device_labels" %in% names(cfg)) {
      label_map <- cfg |>
        dplyr::select(vessel_name, device_labels) |>
        tidyr::unnest_longer(device_labels,
                             indices_to = "device_id",
                             values_to  = "label") |>
        dplyr::select(vessel_name, device_id, label)
    }
  }
  
  # ── 3. Pivot non-scalar columns into value_numeric rows ──────────────────────
  #
  # These value_types store their useful data in named columns rather than
  # value_numeric:
  #
  #   POSITION     -> latitude, longitude, speed_over_ground
  #   TRAWL_TENSION -> traction_center
  #
  # We pivot each into a plain numeric row with a descriptive value_type name
  # so they sit naturally in the same facet_grid as scalar types.
  
  df_scalar <- df_all |>
    dplyr::filter(!value_type %in% c("POSITION", "TRAWL_TENSION"),
                  !is.na(value_numeric))
  
  df_pos    <- df_all |> dplyr::filter(value_type == "POSITION")
  df_trawl  <- df_all |> dplyr::filter(value_type == "TRAWL_TENSION")
  
  .pivot_col <- function(df, col, new_type) {
    if (nrow(df) == 0 || !col %in% names(df)) return(tibble::tibble())
    shared_cols <- intersect(
      c("datetime", "vessel_name", "device_id", "journal_id", "source"),
      names(df)
    )
    df |>
      dplyr::filter(!is.na(.data[[col]])) |>
      dplyr::select(dplyr::all_of(shared_cols), value_numeric = dplyr::all_of(col)) |>
      dplyr::mutate(value_type = new_type)
  }
  
  pos_rows <- dplyr::bind_rows(
    .pivot_col(df_pos,   "latitude",          "Latitude"),
    .pivot_col(df_pos,   "longitude",         "Longitude"),
    .pivot_col(df_pos,   "speed_over_ground", "Speed (SOG)"),
    .pivot_col(df_pos,   "course_made_good",  "Course (COG)")
    # n_satellites omitted — count not a sensor measurement
  )
  
  trawl_rows <- dplyr::bind_rows(
    .pivot_col(df_trawl, "traction_port",            "Traction port"),
    .pivot_col(df_trawl, "traction_center",          "Traction center"),
    .pivot_col(df_trawl, "traction_starboard",       "Traction starboard"),
    .pivot_col(df_trawl, "shooted_length_port",      "Shooted length port"),
    .pivot_col(df_trawl, "shooted_length_center",    "Shooted length center"),
    .pivot_col(df_trawl, "shooted_length_starboard", "Shooted length starboard")
  )
  
  df_combined <- dplyr::bind_rows(df_scalar, pos_rows, trawl_rows)
  
  
  # ── 4. Filters ────────────────────────────────────────────────────────────────
  
  if (!is.null(start_date))
    df_combined <- df_combined |> dplyr::filter(as.Date(datetime) >= start_date)
  if (!is.null(end_date))
    df_combined <- df_combined |> dplyr::filter(as.Date(datetime) <= end_date)
  if (nonzero)
    df_combined <- df_combined |> dplyr::filter(value_numeric != 0)
  
  available_types <- sort(unique(df_combined$value_type))
  
  if (!is.null(value_types)) {
    missing_req <- setdiff(value_types, available_types)
    if (length(missing_req) > 0)
      message("value_types not found in data (skipping): ",
              paste(missing_req, collapse = ", "))
    df_combined <- df_combined |>
      dplyr::filter(value_type %in% value_types)
  }
  
  if (nrow(df_combined) == 0) {
    message("No data remaining after all filters")
    return(invisible(NULL))
  }
  
  # ── 5. Attach device labels ───────────────────────────────────────────────────
  
  if (!is.null(label_map))
    df_combined <- df_combined |>
    dplyr::left_join(label_map, by = c("vessel_name", "device_id"))
  
  if (!"label" %in% names(df_combined))
    df_combined <- df_combined |> dplyr::mutate(label = NA_character_)
  
  df_combined <- df_combined |>
    dplyr::mutate(
      label = dplyr::coalesce(
        label,
        substr(device_id, nchar(device_id) - 7, nchar(device_id))
      )
    )
  
  # ── 6. Build the facet row variable ──────────────────────────────────────────
  #
  # by_device = FALSE : one row per value_type, summed across devices
  # by_device = TRUE  : one row per value_type x device_label combination
  #                     Latitude / Longitude are not split by device
  
  if (by_device) {
    
    no_device_split <- c(
      "Latitude", "Longitude", "Speed (SOG)", "Course (COG)",
      "Traction port", "Traction center", "Traction starboard",
      "Shooted length port", "Shooted length center", "Shooted length starboard"
    )
    
    df_combined <- df_combined |>
      dplyr::mutate(
        row_label = dplyr::if_else(
          value_type %in% no_device_split,
          value_type,
          paste0(value_type, "  \u2014  ", label)
        )
      ) |>
      dplyr::group_by(vessel_name, row_label, datetime) |>
      dplyr::summarise(value_numeric = mean(value_numeric, na.rm = TRUE),
                       .groups = "drop")
  } else {
    df_combined <- df_combined |>
      dplyr::group_by(vessel_name, value_type, datetime) |>
      dplyr::summarise(value_numeric = sum(value_numeric, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(row_label = value_type)
  }
  
  # ── 7. Row ordering ───────────────────────────────────────────────────────────
  # Fixed order for known types; any unknown types appended alphabetically after.
  
  known_order <- c(
    "DEPTH",
    "FUEL_CONSUMPTION",
    "Speed (SOG)",
    "Course (COG)",
    "Traction port",
    "Traction center",
    "Traction starboard",
    "Shooted length port",
    "Shooted length center",
    "Shooted length starboard",
    "Latitude",
    "Longitude"
  )  
  
  # Rows present in data, in known order first, then any extras alphabetically
  present_rows  <- unique(as.character(df_combined$row_label))
  known_present <- known_order[known_order %in% present_rows]
  extra_present <- sort(setdiff(present_rows, known_order))
  
  # When by_device = TRUE, row_label is e.g. "FUEL_CONSUMPTION — FUEL_1"
  # so match on prefix to preserve device sub-rows in the right position
  if (by_device) {
    ordered_rows <- row_order <- present_rows[order(match(
      stringr::str_extract(present_rows, paste(known_order, collapse = "|")),
      known_order
    ))]
    # Fill NAs (unrecognised prefixes) to the end
    row_order <- c(
      ordered_rows[!is.na(match(
        stringr::str_extract(ordered_rows, paste(known_order, collapse = "|")),
        known_order
      ))],
      ordered_rows[is.na(match(
        stringr::str_extract(ordered_rows, paste(known_order, collapse = "|")),
        known_order
      ))]
    )
  } else {
    row_order <- c(known_present, extra_present)
  }
  
  df_combined <- df_combined |>
    dplyr::mutate(row_label = factor(row_label, levels = row_order))
  
  
  # ── 8. Colours: hand-picked palette, one distinct colour per vessel ───────────
  
  vessel_colours <- c(
    "#E63946",   # vivid red
    "#2196F3",   # strong blue
    "#FF9800",   # orange
    "#4CAF50",   # green
    "#9C27B0",   # purple
    "#00BCD4",   # cyan
    "#F06292",   # pink
    "#795548"    # brown
  )
  
  vessels_present <- sort(unique(df_combined$vessel_name))
  colour_scale    <- setNames(
    vessel_colours[seq_along(vessels_present)],
    vessels_present
  )
  
  # ── 9. Build plot ─────────────────────────────────────────────────────────────
  
  n_rows    <- length(row_order)
  n_vessels <- length(vessels_present)
  lo        <- format(min(df_combined$datetime, na.rm = TRUE), "%Y-%m-%d")
  hi        <- format(max(df_combined$datetime, na.rm = TRUE), "%Y-%m-%d")
  src_label <- if (data_source == "thinned") "1-min thinned" else "raw (~10 sec)"
  subtitle  <- sprintf("%s \u2013 %s  |  %s", lo, hi, src_label)
  
  x_breaks <- .poseidat_date_breaks(df_combined$datetime)
  x_labels <- .poseidat_date_label(df_combined$datetime)
  
  p <- ggplot2::ggplot(
    df_combined,
    ggplot2::aes(x      = datetime,
                 y      = value_numeric,
                 colour = vessel_name)
  )
  
  if (geom_type %in% c("line", "both"))
    p <- p + ggplot2::geom_line(alpha = 0.85, linewidth = 0.45)
  if (geom_type %in% c("point", "both"))
    p <- p + ggplot2::geom_point(alpha = 0.7, size = point_size)
  
  p <- p +
    ggplot2::facet_grid(
      row_label ~ vessel_name,
      scales = scales
    ) +
    ggplot2::scale_colour_manual(values = colour_scale, guide = "none") +
    ggplot2::scale_x_datetime(date_labels = x_labels, date_breaks = x_breaks) +
    ggplot2::labs(
      title    = "All sensor time series",
      subtitle = subtitle,
      x        = NULL,
      y        = NULL,
      caption  = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text.y     = ggplot2::element_text(face  = "bold", angle = 0,
                                               hjust = 0,     size  = 7),
      strip.text.x     = ggplot2::element_text(face = "bold", size = 9),
      strip.background = ggplot2::element_rect(fill = "grey92"),
      axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1, size = 7),
      axis.text.y      = ggplot2::element_text(size = 6),
      panel.spacing.y  = ggplot2::unit(0.15, "cm"),
      panel.spacing.x  = ggplot2::unit(0.4,  "cm"),
      legend.position  = "none",
      plot.title       = ggplot2::element_text(face = "bold")
    )
  
  message(sprintf(
    "Overview: %d row(s) \u00d7 %d vessel(s) — suggested ggsave height: %.0f in",
    n_rows, n_vessels, n_rows * 1.2 + 1.5
  ))
  
  p
}


# ============================================================================
# INTERNAL HELPERS shared by all poseidat plot functions
# ============================================================================

# Adaptive x-axis break interval based on the time span in the data.
#   ≤ 3 days  → 1 day  (with time labels)
#   ≤ 21 days → 2 days (day-month labels)
#   > 21 days → 1 week (day-month labels)
.poseidat_date_breaks <- function(datetime_vec) {
  span_days <- as.numeric(
    difftime(max(datetime_vec, na.rm = TRUE),
             min(datetime_vec, na.rm = TRUE),
             units = "days")
  )
  dplyr::case_when(
    span_days <= 3  ~ "1 day",
    span_days <= 21 ~ "2 days",
    TRUE            ~ "1 week"
  )
}

.poseidat_date_label <- function(datetime_vec) {
  span_days <- as.numeric(
    difftime(max(datetime_vec, na.rm = TRUE),
             min(datetime_vec, na.rm = TRUE),
             units = "days")
  )
  dplyr::case_when(
    span_days <= 3  ~ "%d %b %H:%M",
    TRUE            ~ "%d %b"
  )
}

# Shared data loader: reads thinned or raw parquet and optionally joins vessel names.
# Returns a tibble ready for filtering.
.poseidat_load_data <- function(data_source, source = NULL, vessel_name = NULL,
                                start_date  = NULL, end_date = NULL,
                                value_types = NULL, device_ids = NULL,
                                cols        = NULL) {
  # SLIM READING: when start_date / end_date / value_types / device_ids / cols
  # are supplied, those filters are applied INSIDE Arrow before collect(), so
  # only the matching rows and columns ever enter R memory. Without any of them
  # the behaviour is the original full read (needed by writers such as
  # poseidat_write_trip_ids(), which rewrite the complete thinned file).
  # `cols` is extended with the identifying columns the loader itself needs.
  base        <- .onedrive()
  thinned_dir <- file.path(base, "processed", "thinned")

  slim <- !is.null(start_date) || !is.null(end_date) || !is.null(value_types) ||
          !is.null(device_ids) || !is.null(cols)

  t_start <- if (!is.null(start_date)) as.POSIXct(start_date, tz = "UTC") else NULL
  t_end   <- if (!is.null(end_date))   as.POSIXct(as.Date(end_date) + 1, tz = "UTC") else NULL
  keep_cols <- if (!is.null(cols))
    union(cols, c("source", "journal_id", "vessel_name", "vessel_code",
                  "value_type", "datetime", "device_id"))
  else NULL

  # Read via a temp copy, not the original path directly -- Arrow's
  # memory-mapped read on Windows leaves a lock on the source file that
  # blocks writing back to that same path until released. Matters here
  # because callers like poseidat_write_trip_ids() read a vessel's thinned
  # file via this function and then write back to a same-named file
  # (today's date) later in the same call -- same workaround already used
  # in migrate_to_monthly().
  .read_parquet_safe <- function(f) {
    tryCatch({
      tmp <- tempfile(fileext = ".parquet")
      file.copy(f, tmp, overwrite = TRUE)
      out <- arrow::read_parquet(tmp)
      unlink(tmp)
      out
    }, error = \(e) tibble())
  }

  # Same, but with row/column filters pushed down into Arrow
  .read_parquet_slim <- function(f) {
    tryCatch({
      tmp <- tempfile(fileext = ".parquet")
      file.copy(f, tmp, overwrite = TRUE)
      ds   <- arrow::open_dataset(tmp)
      have <- ds$schema$names
      q    <- ds
      if (!is.null(keep_cols))
        q <- dplyr::select(q, dplyr::all_of(intersect(keep_cols, have)))
      if (!is.null(value_types) && "value_type" %in% have)
        q <- dplyr::filter(q, value_type %in% value_types)
      if (!is.null(device_ids) && "device_id" %in% have)
        q <- dplyr::filter(q, device_id %in% device_ids)
      if (!is.null(t_start)) q <- dplyr::filter(q, datetime >= t_start)
      if (!is.null(t_end))   q <- dplyr::filter(q, datetime <  t_end)
      out <- dplyr::collect(q)
      rm(ds, q)
      invisible(gc())      # release the memory map before removing the temp copy
      unlink(tmp)
      out
    }, error = \(e) tibble())
  }

  read_one <- if (slim) .read_parquet_slim else .read_parquet_safe

  load_thinned <- function() {
    vessel_dirs <- list.dirs(thinned_dir, full.names = TRUE, recursive = FALSE)
    if (length(vessel_dirs) == 0) return(tibble())

    # ── Filter vessel directories by name when vessel_name is supplied ──────
    if (!is.null(vessel_name)) {
      dir_names   <- basename(vessel_dirs)
      keep        <- dir_names %in% gsub(" ", "_", vessel_name)
      vessel_dirs <- vessel_dirs[keep]
    }
    if (length(vessel_dirs) == 0) return(tibble())

    pieces <- vector("list", length(vessel_dirs))
    for (vi in seq_along(vessel_dirs)) {
      f <- tail(sort(list.files(vessel_dirs[vi], pattern = "\\.parquet$", full.names = TRUE)), 1)
      if (length(f) == 0) next
      pieces[[vi]] <- read_one(f)
    }
    dplyr::bind_rows(pieces)
  }

  load_raw <- function() {
    raw_files <- list.files(file.path(base, "raw"), pattern = "\\.parquet$",
                            recursive = TRUE, full.names = TRUE)
    if (!is.null(source)) {
      raw_files <- raw_files[grepl(paste0("/", source, "/"), raw_files)]
    }
    if (!is.null(vessel_name)) {
      pat       <- paste(gsub(" ", "_", vessel_name), collapse = "|")
      raw_files <- raw_files[grepl(pat, raw_files)]
    }
    # Prune by the YYYY/MM path segments before opening any file
    if (length(raw_files) > 0 && (!is.null(t_start) || !is.null(t_end))) {
      ym   <- lubridate::ymd(sub(".*/([0-9]{4})/([0-9]{2})/[^/]+$", "\\1-\\2-01", raw_files),
                             quiet = TRUE)
      keep <- !is.na(ym)
      if (!is.null(t_start)) keep <- keep & ym >= lubridate::floor_date(as.Date(t_start), "month")
      if (!is.null(t_end))   keep <- keep & ym <= as.Date(t_end)
      raw_files <- raw_files[keep | is.na(ym)]
    }
    if (length(raw_files) == 0) return(tibble())
    pieces <- vector("list", length(raw_files))
    for (fi in seq_along(raw_files)) pieces[[fi]] <- read_one(raw_files[fi])
    dplyr::bind_rows(pieces)
  }

  df <- switch(data_source,
               raw     = load_raw(),
               thinned = load_thinned(),
               overlay = {
                 thin <- load_thinned()
                 if (nrow(thin) > 0) thin else load_raw()
               })

  if (nrow(df) == 0) return(df)

  # Join vessel_name / vessel_code from config, independently -- vessel_name
  # may already be baked into older thinned files while vessel_code is new,
  # so each is joined only if actually missing/all-NA, not as a single check.
  cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
  if (file.exists(cfg_path)) {
    cfg <- readRDS(cfg_path)

    need_vessel_name <- !"vessel_name" %in% names(df) || all(is.na(df$vessel_name))
    need_vessel_code <- "vessel_code" %in% names(cfg) &&
      (!"vessel_code" %in% names(df) || all(is.na(df$vessel_code)))

    if (need_vessel_name || need_vessel_code) {
      join_cols <- intersect(c("journal_id", "vessel_name", "vessel_code"), names(cfg))
      vmap      <- cfg[, join_cols]
      drop_cols <- setdiff(intersect(names(vmap), names(df)), "journal_id")
      df <- df |>
        dplyr::select(-any_of(drop_cols)) |>
        left_join(vmap, by = "journal_id")
    }
  }
  
  # ── Post-load vessel filter (safety net, covers thinned + any join edge cases) ──
  if (!is.null(vessel_name)) {
    df <- df |> dplyr::filter(vessel_name %in% .env$vessel_name)
  }
  
  df
}

# ── Internal: plot latitude and/or longitude as time series ──────────────────
.plot_position_timeseries <- function(data_source,
                                      source,
                                      vessel_name,
                                      start_date,
                                      end_date,
                                      position_component,
                                      facet_by,
                                      scales,
                                      ncol) {

  df <- .poseidat_load_data(data_source, source, vessel_name,
                            start_date = start_date, end_date = end_date,
                            value_types = "POSITION",
                            cols = c("latitude", "longitude"))

  if (nrow(df) == 0) { message("No POSITION data found"); return(invisible(NULL)) }

  df <- df |>
    filter(value_type == "POSITION", !is.na(latitude), !is.na(longitude))

  if (!is.null(source))      df <- filter(df, source      %in% .env$source)
  if (!is.null(vessel_name)) df <- filter(df, vessel_name %in% .env$vessel_name)
  if (!is.null(start_date))  df <- filter(df, as.Date(datetime) >= start_date)
  if (!is.null(end_date))    df <- filter(df, as.Date(datetime) <= end_date)

  if (nrow(df) == 0) {
    message("No POSITION records after filtering")
    return(invisible(NULL))
  }

  x_breaks <- .poseidat_date_breaks(df$datetime)
  x_labels <- .poseidat_date_label(df$datetime)

  lo       <- format(min(df$datetime, na.rm = TRUE), "%Y-%m-%d")
  hi       <- format(max(df$datetime, na.rm = TRUE), "%Y-%m-%d")
  subtitle <- sprintf("%s \u2013 %s  |  POSITION", lo, hi)

  n_vessels   <- length(unique(df$vessel_name))
  facet_layer <- if (n_vessels > 1 && facet_by != "none")
    ggplot2::facet_wrap(~ vessel_name, scales = scales, ncol = ncol)
  else
    NULL

  make_coord_panel <- function(coord_col, y_label) {
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = datetime, y = .data[[coord_col]], colour = vessel_name)
    ) +
      ggplot2::geom_line(alpha = 0.8, linewidth = 0.5) +
      facet_layer +
      ggplot2::scale_colour_brewer(palette = "Set1", name = NULL) +
      ggplot2::scale_x_datetime(date_labels = x_labels, date_breaks = x_breaks) +
      ggplot2::labs(
        title    = paste(y_label, "over time"),
        subtitle = subtitle,
        x        = NULL,
        y        = y_label,
        caption  = sprintf("Generated: %s", Sys.Date())
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.text      = ggplot2::element_text(face = "bold"),
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position = "bottom"
      )
  }

  if (position_component == "latitude")  return(make_coord_panel("latitude",  "Latitude (\u00b0N)"))
  if (position_component == "longitude") return(make_coord_panel("longitude", "Longitude (\u00b0E)"))

  # "both" — stack with patchwork
  p_lat <- make_coord_panel("latitude",  "Latitude (\u00b0N)")  |>
    (\(p) p + ggplot2::labs(subtitle = subtitle, caption = NULL, x = NULL))()
  p_lon <- make_coord_panel("longitude", "Longitude (\u00b0E)") |>
    (\(p) p + ggplot2::labs(title = NULL, subtitle = NULL,
                             caption = sprintf("Generated: %s", Sys.Date())))()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_lat, p_lon, ncol = 1) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    message("Install patchwork for stacked lat/lon: install.packages('patchwork')")
    print(p_lat)
    print(p_lon)
  }
}


poseidat_availability <- function(source      = NULL,
                                  vessel_name = NULL,
                                  start_date  = NULL,
                                  end_date    = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")

  # Load most recent thinned parquet if available, else fall back to raw
  # Load most recent thinned parquet per vessel and combine
  thinned_dir <- file.path(.onedrive(), "processed", "thinned")
  vessel_dirs <- list.dirs(thinned_dir, full.names = TRUE, recursive = FALSE)

  # Only the columns needed for the daily presence counts, and rows before
  # start_date dropped per vessel file, so memory stays small.
  avail_cols <- c("source", "journal_id", "vessel_name", "value_type", "datetime",
                  "latitude", "longitude", "traction_center", "value_numeric")

  if (length(vessel_dirs) > 0) {
    vessel_list <- vector("list", length(vessel_dirs))
    for (vi in seq_along(vessel_dirs)) {
      files <- sort(list.files(vessel_dirs[vi], pattern = "\\.parquet$", full.names = TRUE))
      if (length(files) == 0) next
      piece <- tryCatch(
        arrow::read_parquet(tail(files, 1), col_select = dplyr::any_of(avail_cols)),
        error = \(e) tibble())
      if (!is.null(start_date) && nrow(piece) > 0)
        piece <- dplyr::filter(piece, as.Date(datetime) >= start_date)
      vessel_list[[vi]] <- piece
    }
    df <- dplyr::bind_rows(vessel_list)
    rm(vessel_list)
  } else {
    message("No thinned files found — reading raw parquet (may be slow)")
    raw_files <- list.files(file.path(.onedrive(), "raw"),
                            pattern = "\\.parquet$",
                            recursive = TRUE, full.names = TRUE)
    if (length(raw_files) == 0) {
      message("No parquet files found")
      return(invisible(NULL))
    }
    df <- map_dfr(raw_files, function(f) tryCatch(read_parquet(f),
                                                   error = \(e) tibble()))
  }

  if (nrow(df) == 0) { message("No data found"); return(invisible(NULL)) }

  # Join vessel names if not present
  if (!"vessel_name" %in% names(df) || all(is.na(df$vessel_name))) {
    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    if (file.exists(cfg_path)) {
      vmap <- readRDS(cfg_path)[, c("journal_id", "vessel_name")]
      df   <- df |>
        dplyr::select(-any_of("vessel_name")) |>
        left_join(vmap, by = "journal_id")
    }
  }

  # Apply filters
  if (!is.null(source))      df <- filter(df, source      %in% .env$source)
  if (!is.null(vessel_name)) df <- filter(df, vessel_name %in% .env$vessel_name)
  if (!is.null(start_date))  df <- filter(df, as.Date(datetime) >= start_date)
  if (!is.null(end_date))    df <- filter(df, as.Date(datetime) <= end_date)

  if (nrow(df) == 0) { message("No data after filtering"); return(invisible(NULL)) }

  # Daily presence per vessel / value_type
  #
  # Four categories, not two -- "zero" (a real, valid reading of exactly 0)
  # and "NA" (a record exists but the value field itself is missing) mean
  # different things diagnostically. E.g. FUEL_CONSUMPTION all-zero on a
  # given day means the sensor is fine and the engine was idle (mundane);
  # FUEL_CONSUMPTION all-NA would mean records arrived but were malformed/
  # empty (a different kind of problem). Confirmed useful in practice
  # (2026-08-12, Willem Jacob fuel investigation).
  #
  # POSITION additionally treats an exact (0,0) fix as "zero" rather than
  # "data" -- a GPS reporting (0,0) ("Null Island") is a well-known
  # lost-fix artifact, not a real position, distinct from lat/lon being NA
  # outright (no fix data recorded at all).
  daily <- df |>
    mutate(date = as.Date(datetime)) |>
    group_by(vessel_name, value_type, date) |>
    summarise(
      n_total = n(),
      n_data  = case_when(
        any(value_type == "POSITION") ~
          sum(!is.na(latitude) & !(latitude == 0 & longitude == 0), na.rm = TRUE),
        any(value_type == "TRAWL_TENSION") ~
          sum(!is.na(traction_center) & traction_center > 0, na.rm = TRUE),
        TRUE ~ sum(!is.na(value_numeric) & value_numeric != 0, na.rm = TRUE)
      ),
      n_zero = case_when(
        any(value_type == "POSITION") ~
          sum(!is.na(latitude) & latitude == 0 & longitude == 0, na.rm = TRUE),
        any(value_type == "TRAWL_TENSION") ~
          sum(!is.na(traction_center) & traction_center == 0, na.rm = TRUE),
        TRUE ~ sum(!is.na(value_numeric) & value_numeric == 0, na.rm = TRUE)
      ),
      n_na = case_when(
        any(value_type == "POSITION")      ~ sum(is.na(latitude)),
        any(value_type == "TRAWL_TENSION") ~ sum(is.na(traction_center)),
        TRUE                                ~ sum(is.na(value_numeric))
      ),
      .groups = "drop"
    ) |>
    mutate(
      status = case_when(
        n_data  > 0 ~ "data",
        n_zero  > 0 ~ "zero",
        n_na    > 0 ~ "NA",
        n_total > 0 ~ "zero",   # records exist but none matched data/zero/na
                                # explicitly (shouldn't normally happen) --
                                # fall back to "zero" rather than miscount
                                # as a gap
        TRUE        ~ "no data"
      )
    )

  # SCALE data is deliberately excluded from poseidat_thin() (averaging catch
  # weights per minute is meaningless -- see the note in
  # .dbmatic_get_measurements()), so it never appears in the thinned files
  # read above and would otherwise be completely invisible here. Pull its
  # daily presence directly from raw instead, using known SCALE device_ids
  # from vessel_config -- far cheaper than scanning every raw file in the
  # fleet just to filter down to value_type == "SCALE".
  scale_daily <- tryCatch({

    cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
    if (!file.exists(cfg_path)) stop("no config")
    vessels_cfg <- readRDS(cfg_path)

    scale_lookup <- tibble()
    for (i in seq_len(nrow(vessels_cfg))) {
      labels <- vessels_cfg$device_labels[[i]]
      if (length(labels) == 0) next
      hits <- names(labels)[unlist(labels) == "SCALE"]
      if (length(hits) > 0) {
        scale_lookup <- dplyr::bind_rows(scale_lookup, tibble(
          source      = vessels_cfg$source[i],
          vessel_name = vessels_cfg$vessel_name[i],
          device_id   = hits[1]
        ))
      }
    }
    if (nrow(scale_lookup) == 0) stop("no SCALE devices configured")

    scale_raw <- tibble()
    for (i in seq_len(nrow(scale_lookup))) {
      vessel_dir <- file.path(.onedrive(), "raw",
                              scale_lookup$source[i], scale_lookup$vessel_name[i])
      files <- list.files(vessel_dir,
                          pattern = paste0(scale_lookup$device_id[i], ".*\\.parquet$"),
                          recursive = TRUE, full.names = TRUE)
      for (f in files) {
        piece <- tryCatch(read_parquet(f), error = \(e) tibble())
        if (nrow(piece) > 0)
          scale_raw <- dplyr::bind_rows(scale_raw,
                                        dplyr::mutate(piece, vessel_name = scale_lookup$vessel_name[i]))
      }
    }
    if (nrow(scale_raw) == 0) stop("no SCALE data on disk yet")

    if (!is.null(source))      scale_raw <- dplyr::filter(scale_raw, source %in% .env$source)
    if (!is.null(vessel_name)) scale_raw <- dplyr::filter(scale_raw, vessel_name %in% .env$vessel_name)
    if (!is.null(start_date))  scale_raw <- dplyr::filter(scale_raw, as.Date(datetime) >= start_date)
    if (!is.null(end_date))    scale_raw <- dplyr::filter(scale_raw, as.Date(datetime) <= end_date)

    scale_raw |>
      dplyr::mutate(date = as.Date(datetime), value_type = "SCALE") |>
      dplyr::group_by(vessel_name, value_type, date) |>
      dplyr::summarise(
        n_total = dplyr::n(),
        n_data  = sum(!is.na(scale_weight) & scale_weight != 0, na.rm = TRUE),
        n_zero  = sum(!is.na(scale_weight) & scale_weight == 0, na.rm = TRUE),
        n_na    = sum(is.na(scale_weight)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        status = dplyr::case_when(
          n_data  > 0 ~ "data",
          n_zero  > 0 ~ "zero",
          n_na    > 0 ~ "NA",
          n_total > 0 ~ "zero",
          TRUE        ~ "no data"
        )
      )
  }, error = function(e) tibble())

  daily <- dplyr::bind_rows(daily, scale_daily)

  # Fill in missing date/vessel/value_type combinations so gaps show as grey
  all_dates  <- seq(min(daily$date), max(daily$date), by = "day")
  all_combos <- tidyr::expand_grid(
    vessel_name = unique(daily$vessel_name),
    value_type  = unique(daily$value_type),
    date        = all_dates
  )

  daily_full <- left_join(all_combos, daily,
                          by = c("vessel_name", "value_type", "date")) |>
    mutate(
      status = tidyr::replace_na(status, "no data"),
      status = factor(status, levels = c("data", "zero", "NA", "no data"))
    )

  # Plot
  ggplot2::ggplot(daily_full,
                  ggplot2::aes(x = date, y = value_type, fill = status)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(~ vessel_name, ncol = 1) +
    ggplot2::scale_fill_manual(
      values = c(
        "data"    = "#4DAF4A",
        "zero"    = "#FF7F00",
        "NA"      = "#984EA3",
        "no data" = "grey90"
      ),
      drop = FALSE   # keep all 4 categories in the legend even if one is absent
                    # from THIS particular plot's data, for consistent reading
                    # across different calls/filters
    ) +
    ggplot2::scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
    ggplot2::labs(
      title   = "Daily data availability per vessel and value type",
      x       = NULL, y = NULL, fill = NULL,
      caption = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      panel.grid      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "bottom"
    )
}


# ============================================================================
# ============================================================================
# SENSOR AVAILABILITY — scoped to sensors each vessel actually has
#
# poseidat_availability() above cross-joins every value_type seen ANYWHERE
# in the fleet against every vessel, so a vessel that never had a given
# sensor still gets a grey "no data" tile for it. The functions below scope
# the grid per vessel instead, using a single pruned scan of raw/ (see
# .poseidat_scan_raw_daily() below) to find each vessel's own sensors,
# devices, and observed window, so a grey tile always means a genuine gap
# rather than "not applicable to this vessel."
#
# Performance note: poseidat_value_types() and poseidat_daily() each scan
# EVERY parquet file under raw/ and processed/ for the whole fleet, with
# start_date/end_date applied only after the full read. Calling both (as
# an earlier version of poseidat_sensor_availability() did) meant two full
# archive scans per call. .poseidat_scan_raw_daily() below does a single
# pass instead, and prunes files BEFORE opening them using information
# already encoded in the storage scheme itself:
#   raw/<source>/<vessel>/YYYY/MM/<vessel>_<device>_YYYYMM.parquet
# -- source/vessel are pruned via directory, device via filename, and
# month via the YYYY/MM path segments, so a device- or date-scoped call
# only ever opens the files it actually needs, and only reads the columns
# needed for a presence count (not the value columns).
#
# Written in dplyr style: no apply()/sapply()/lapply(), and no purrr::map()
# -- the row-wise date-sequence expansion uses rowwise() + a list-column
# instead, which is plain dplyr/tidyr.
# ============================================================================

#' Single-pass scan of raw parquet for daily record counts, pruned early
#'
#' Internal helper for poseidat_sensor_availability(). Lists only the
#' source/vessel directories asked for, drops files for devices not asked
#' for by filename (before opening them), drops files for months outside
#' start_date/end_date by path (before opening them), then reads only the
#' columns needed to count records per day.
#'
#' @return A tibble: source, vessel_name, device_id, value_type, date,
#'   n_records. Empty tibble if nothing matches.
#' @keywords internal
.poseidat_scan_raw_daily <- function(source      = NULL,
                                      vessel_name = NULL,
                                      device_id   = NULL,
                                      start_date  = NULL,
                                      end_date    = NULL) {

  base    <- .onedrive()
  raw_dir <- file.path(base, "raw")

  # ---- Prune to relevant source directories first -- skips whole
  # subtrees for sources that weren't asked for. (Vessel pruning happens
  # below via full-path matching, not via directory name, since the
  # vessel's directory itself is not reliably underscored even though the
  # filename inside it always is -- e.g. ".../raw/efice/Willem Jacob/2026/
  # 08/Willem_Jacob_<device>_202608.parquet" has a space in the directory
  # but an underscore in the filename. Matching the full path rather than
  # the directory name is robust to that inconsistency, same approach
  # already used by .poseidat_load_data()'s load_raw().)
  search_dirs <- if (!is.null(source)) {
    cand <- file.path(raw_dir, source)
    cand[dir.exists(cand)]
  } else {
    list.dirs(raw_dir, recursive = FALSE)
  }
  if (length(search_dirs) == 0) return(tibble())

  files <- character()
  for (d in search_dirs)
    files <- c(files, list.files(d, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE))
  if (length(files) == 0) return(tibble())

  # ---- Vessel filter -- match against the full path, not just a
  # constructed directory name (see note above).
  if (!is.null(vessel_name)) {
    pat   <- paste(gsub(" ", "_", vessel_name), collapse = "|")
    files <- files[grepl(pat, files)]
  }
  if (length(files) == 0) return(tibble())

  # ---- Device filter -- the device UUID is embedded in the filename, so
  # this skips opening files for devices that weren't asked for.
  if (!is.null(device_id)) {
    pat   <- paste(device_id, collapse = "|")
    files <- files[grepl(pat, basename(files))]
  }
  if (length(files) == 0) return(tibble())

  # ---- Month filter -- files are one-per-calendar-month with YYYY/MM in
  # the path, so this drops files entirely outside the requested range
  # without opening them.
  if (!is.null(start_date) || !is.null(end_date)) {
    ym      <- sub(".*/([0-9]{4})/([0-9]{2})/[^/]+$", "\\1-\\2-01", files)
    ym_date <- suppressWarnings(as.Date(ym))
    keep    <- !is.na(ym_date)
    if (!is.null(start_date))
      keep <- keep & (ym_date >= as.Date(format(as.Date(start_date), "%Y-%m-01")))
    if (!is.null(end_date))
      keep <- keep & (ym_date <= as.Date(format(as.Date(end_date), "%Y-%m-01")))
    files <- files[keep]
  }
  if (length(files) == 0) return(tibble())

  need_cols <- c("source", "journal_id", "device_id", "value_type", "datetime")

  daily <- tibble()
  for (f in files) {

    # vessel_name is taken from the path itself, not the file's own
    # columns, so this is robust to older files where vessel_name may be
    # missing or stale inside the parquet (see .poseidat_load_data()).
    rel   <- sub(paste0(raw_dir, "/"), "", f, fixed = TRUE)
    parts <- strsplit(rel, "/")[[1]]
    vsl   <- gsub("_", " ", parts[2])

    piece <- tryCatch(
      arrow::read_parquet(f, col_select = dplyr::any_of(need_cols)),
      error = \(e) tibble()
    )
    if (nrow(piece) == 0 || !"datetime" %in% names(piece)) next

    piece <- piece |>
      mutate(vessel_name = vsl, date = as.Date(datetime)) |>
      group_by(source, vessel_name, device_id, value_type, date) |>
      summarise(n_records = n(), .groups = "drop")

    daily <- bind_rows(daily, piece)
  }

  if (nrow(daily) == 0) return(daily)

  # Row-level date filter, for the rare case a file's own dates spill
  # outside a partial-month start/end_date request
  if (!is.null(start_date)) daily <- filter(daily, date >= as.Date(start_date))
  if (!is.null(end_date))   daily <- filter(daily, date <= as.Date(end_date))

  daily |>
    group_by(source, vessel_name, device_id, value_type, date) |>
    summarise(n_records = sum(n_records), .groups = "drop")
}


#' Calendar heatmap of sensor coverage, scoped to sensors each vessel has
#'
#' For every vessel, shows one row per sensor (value_type, or per device if
#' by = "device_id") that vessel has ever produced, and one tile per day,
#' coloured by whether that sensor produced any records that day. Sensors
#' the vessel was never fitted with don't appear at all -- so grey tiles
#' mean a genuine gap (sensor known, no data that day), not "not
#' applicable." Reads only the raw files actually in scope (see
#' .poseidat_scan_raw_daily()), so filtering by device_id or a narrow date
#' range meaningfully reduces how much gets read.
#'
#' @param source      Character. "dbmatic" | "efice" | "pefa", or NULL for all.
#' @param vessel_name Character vector to restrict to, or NULL for all vessels.
#' @param device_id   Character vector of device UUIDs to restrict to, or
#'                     NULL for all devices. Narrowing this skips reading
#'                     other devices' files entirely.
#' @param start_date  Optional Date/character. Clips both the plotted range
#'                     and each sensor's own first/last-observed window, and
#'                     skips reading files for months outside it.
#' @param end_date    Optional Date/character, same treatment.
#' @param by          "value_type" (default) aggregates across devices per
#'                     sensor, as before. "device_id" shows one row per
#'                     device instead, labelled "<value_type> (...<device
#'                     suffix>)" -- use this when a vessel has more than one
#'                     device reporting the same sensor and you need to see
#'                     them separately.
#' @param return_data If TRUE, return the underlying tibble instead of the
#'                     plot. Default FALSE.
#' @return A ggplot object, or a tibble if return_data = TRUE.
#' @export
#' Friendly device labels (e.g. "FUEL_1") keyed by device UUID
#'
#' Internal helper shared by poseidat_sensor_availability() and
#' poseidat_sensor_sample(). Reads device_labels from the saved
#' vessel_config and flattens it into a device_id -> device_label lookup
#' for joining onto device-level output. Uses a plain for loop, same
#' pattern already used in print_devices() -- there's nothing here where
#' purrr::map would meaningfully outperform it (looping over vessels in
#' the config, typically a handful).
#'
#' @return A tibble: device_id, device_label. Empty (0-row) if no config
#'   or no labels found.
#' @keywords internal
.poseidat_device_labels <- function() {

  cfg_path <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
  cfg      <- tryCatch(readRDS(cfg_path), error = function(e) NULL)

  if (is.null(cfg) || !"device_labels" %in% names(cfg))
    return(tibble(device_id = character(), device_label = character()))

  lookup <- tibble(device_id = character(), device_label = character())

  for (i in seq_len(nrow(cfg))) {
    labels <- cfg$device_labels[[i]]
    if (length(labels) == 0) next
    lookup <- bind_rows(lookup, tibble(
      device_id    = names(labels),
      device_label = unname(unlist(labels))
    ))
  }

  distinct(lookup, device_id, .keep_all = TRUE)
}


poseidat_sensor_availability <- function(source      = NULL,
                                          vessel_name = NULL,
                                          device_id   = NULL,
                                          start_date  = NULL,
                                          end_date    = NULL,
                                          by          = "value_type",
                                          return_data = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Install tidyr: install.packages('tidyr')")

  by <- match.arg(by, c("value_type", "device_id"))

  daily <- .poseidat_scan_raw_daily(source, vessel_name, device_id, start_date, end_date)

  if (nrow(daily) == 0) {
    message("No raw data found for the requested scope")
    return(invisible(NULL))
  }

  group_cols <- c("source", "vessel_name", "value_type",
                  if (by == "device_id") "device_id")

  # ---- 1. Each vessel/sensor(/device)'s own observed window -----------------
  vessel_scope <- daily |>
    group_by(across(all_of(group_cols))) |>
    summarise(first_obs = min(date), last_obs = max(date), .groups = "drop")

  if (!is.null(start_date))
    vessel_scope <- mutate(vessel_scope, first_obs = pmax(first_obs, as.Date(start_date)))
  if (!is.null(end_date))
    vessel_scope <- mutate(vessel_scope, last_obs = pmin(last_obs, as.Date(end_date)))

  vessel_scope <- filter(vessel_scope, first_obs <= last_obs)

  if (nrow(vessel_scope) == 0) {
    message("No vessel/sensor combination falls within the requested date range")
    return(invisible(NULL))
  }

  # ---- 2. Expand each pair over its own date window --------------------------
  # rowwise() + a list-column of seq() -- one seq() per vessel/sensor(/device)
  # pair, not per data row, so plain dplyr/tidyr stays fine here rather than
  # reaching for purrr::map().
  grid <- vessel_scope |>
    rowwise() |>
    mutate(date = list(seq(first_obs, last_obs, by = "day"))) |>
    ungroup() |>
    tidyr::unnest(date) |>
    dplyr::select(all_of(group_cols), date)

  # ---- 3. Daily counts at the requested granularity, and classify -----------
  daily_agg <- daily |>
    group_by(across(all_of(c(group_cols, "date")))) |>
    summarise(n_records = sum(n_records), .groups = "drop")

  calendar <- grid |>
    left_join(daily_agg, by = c(group_cols, "date")) |>
    mutate(
      status = case_when(
        !is.na(n_records) & n_records > 0 ~ "data",
        TRUE                              ~ "no data"
      ),
      status = factor(status, levels = c("data", "no data"))
    )

  # ---- 4. Friendly device labels (e.g. "FUEL_1") when broken out by device --
  calendar <- if (by == "device_id") {
    calendar |>
      left_join(.poseidat_device_labels(), by = "device_id") |>
      mutate(
        sensor_label = sprintf(
          "%s (%s)", value_type,
          dplyr::coalesce(
            device_label,
            sprintf("...%s", substr(device_id, pmax(1, nchar(device_id) - 5), nchar(device_id)))
          )
        )
      )
  } else {
    calendar |> mutate(sensor_label = value_type)
  }

  calendar <- arrange(calendar, vessel_name, sensor_label, date)

  if (isTRUE(return_data)) return(calendar)

  ggplot2::ggplot(calendar, ggplot2::aes(x = date, y = sensor_label, fill = status)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(~ vessel_name, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = c("data" = "#4DAF4A", "no data" = "grey85"),
      drop   = FALSE
    ) +
    ggplot2::scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
    ggplot2::labs(
      title   = if (by == "device_id")
        "Sensor coverage per vessel and device"
      else
        "Sensor coverage per vessel (only sensors that vessel has)",
      x       = NULL, y = NULL, fill = NULL,
      caption = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      panel.grid      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "bottom"
    )
}


#' Sample raw/thinned rows per vessel and sensor (optionally per device)
#'
#' Draws a stratified random sample of `n` rows per group, after any
#' date-range filter. Useful as a quick "does this look right" check
#' alongside poseidat_sensor_availability(). Defaults to thinned data,
#' since raw pulls the full ~10-second-interval archive.
#'
#' @param source      Character. "dbmatic" | "efice" | "pefa", or NULL for all.
#' @param vessel_name Character vector to restrict to, or NULL for all vessels.
#' @param device_id   Character vector of device UUIDs to restrict to, or
#'                     NULL for all devices on the matched vessels.
#' @param data_source "thinned" (default, minute-level, fast) | "raw"
#'                     (full-resolution) | "overlay" -- passed to the same
#'                     internal loader used by poseidat_positions().
#' @param start_date, end_date  Optional Date bounds on datetime.
#' @param by          "value_type" (default) samples per vessel x sensor,
#'                     pooling across devices. "device_id" samples per
#'                     vessel x sensor x device instead (and adds the
#'                     friendly device_label, e.g. "FUEL_1", alongside the
#'                     raw device_id) -- use this whenever a vessel has
#'                     more than one device on the same sensor and you
#'                     need to see each one separately, not just when
#'                     device_id is also used to filter.
#' @param n           Rows to sample per group (default 5).
#' @return A tibble, up to `n` rows per group, sorted by vessel, sensor,
#'   device (if applicable), then time.
#' @export
poseidat_sensor_sample <- function(source      = NULL,
                                    vessel_name = NULL,
                                    device_id   = NULL,
                                    data_source = "thinned",
                                    start_date  = NULL,
                                    end_date    = NULL,
                                    by          = "value_type",
                                    n           = 5) {

  data_source <- match.arg(data_source, c("thinned", "raw", "overlay"))
  by          <- match.arg(by, c("value_type", "device_id"))

  df <- .poseidat_load_data(data_source, source, vessel_name,
                            start_date = start_date, end_date = end_date,
                            device_ids = device_id)

  if (nrow(df) == 0) {
    message("No data found")
    return(tibble())
  }
  if (!"value_type" %in% names(df)) {
    message("value_type column not found in loaded data")
    return(tibble())
  }

  if (by == "device_id" && !"device_id" %in% names(df)) {
    message("device_id column not found in loaded data -- falling back to by = 'value_type'")
    by <- "value_type"
  }

  if (!is.null(device_id)) {
    if (!"device_id" %in% names(df)) {
      message("device_id column not found in loaded data")
      return(tibble())
    }
    df <- filter(df, device_id %in% .env$device_id)
  }

  if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= start_date)
  if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= end_date)

  if (nrow(df) == 0) {
    message("No data after filtering")
    return(tibble())
  }

  group_cols <- c("vessel_name", "value_type", if (by == "device_id") "device_id")

  result <- df |>
    group_by(across(all_of(group_cols))) |>
    slice_sample(n = n) |>
    ungroup()

  # Friendly device labels (e.g. "FUEL_1"), placed right after device_id
  if (by == "device_id") {
    result <- result |>
      left_join(.poseidat_device_labels(), by = "device_id") |>
      dplyr::relocate(device_label, .after = device_id)
  }

  result |>
    arrange(across(any_of(c("vessel_name", "value_type", "device_id", "datetime"))))
}

# SAVE AVAILABILITY PLOT — for use in the daily runner
# ============================================================================

#' Generate the daily availability plot and save it as a dated PNG
#'
#' Wraps poseidat_availability() and writes the result to
#' outputs/figures/poseidat_availability_YYYYMMDD.png. Keeps only the most
#' recent `keep_n` files so the folder doesn't grow unbounded — same pattern
#' poseidat_thin() already uses for thinned Parquet files (keep 3 most
#' recent per vessel).
#'
#' @param keep_n Integer. Number of most recent plot files to retain (default 14)
#' @param width  Numeric. Plot width in inches (default 14)
#' @param height Numeric. Plot height in inches (default 10)
#' @param dpi    Numeric. Resolution (default 150)
#' @return Character path to the saved file (invisibly), or NULL if plot
#'   generation or saving failed
#' @export
poseidat_save_availability_plot <- function(keep_n = 14, width = 14, height = 10, dpi = 150,
                                           start_date = as.Date("2026-03-01")) {

  figures_dir <- file.path(.onedrive(), "outputs", "figures")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  gc()   # free memory left over from the sync/thinning steps of the daily run
  p <- tryCatch(
    poseidat_availability(start_date = start_date),
    error = function(e) {
      log_poseidat(sprintf("Availability plot generation failed: %s", conditionMessage(e)),
                  level = "ERROR")
      NULL
    }
  )

  if (is.null(p)) return(invisible(NULL))

  plot_file <- file.path(figures_dir,
                         sprintf("poseidat_availability_%s.png", format(Sys.Date(), "%Y%m%d")))

  saved_ok <- tryCatch({
    ggplot2::ggsave(plot_file, p, width = width, height = height, dpi = dpi, bg = "white")
    log_poseidat(sprintf("Saved availability plot -> %s", basename(plot_file)))
    TRUE
  }, error = function(e) {
    log_poseidat(sprintf("Saving availability plot failed: %s", conditionMessage(e)),
                level = "ERROR")
    FALSE
  })

  if (!saved_ok) return(invisible(NULL))

  # ---- Cleanup: keep only the keep_n most recent plot files -----------------
  existing <- sort(list.files(figures_dir,
                              pattern    = "^poseidat_availability_[0-9]{8}\\.png$",
                              full.names = TRUE),
                   decreasing = TRUE)
  to_delete <- existing[seq_along(existing) > keep_n]
  if (length(to_delete) > 0) {
    file.remove(to_delete)
    log_poseidat(sprintf("Cleaned %d old availability plot(s)", length(to_delete)))
  }

  invisible(plot_file)
}


# ============================================================================
# 10. poseidat_plot_trawl()
#
# Time series plot of TRAWL_TENSION data (traction per warp + shooted length).
#
# @param vessel_name  Character vector of vessel names, or NULL for all
# @param data_source  "thinned" (default) | "raw"
# @param start_date   Date or NULL
# @param end_date     Date or NULL
# @param towing_only  If TRUE (default), filter to traction_center > min_traction
# @param min_traction Minimum traction_center (kg) to consider as towing (default 500)
# @param show_length  If TRUE, add a second panel with shooted wire length
# @param scales       ggplot2 facet scales: "free_y" (default) | "fixed" | "free"
# @param ncol         Number of facet columns (default 1 = vertical stack)
# ============================================================================

poseidat_plot_trawl <- function(vessel_name  = NULL,
                                data_source  = "thinned",
                                start_date   = NULL,
                                end_date     = NULL,
                                towing_only  = TRUE,
                                min_traction = 500,
                                show_length  = FALSE,
                                scales       = "free_y",
                                ncol         = 1) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")

  data_source <- match.arg(data_source, c("thinned", "raw"))

  df <- .poseidat_load_data(
    data_source, vessel_name = vessel_name,
    start_date = start_date, end_date = end_date, value_types = "TRAWL_TENSION",
    cols = c("traction_port", "traction_center", "traction_starboard",
             "shooted_length_port", "shooted_length_center", "shooted_length_starboard"))

  if (nrow(df) == 0) { message("No data found"); return(invisible(NULL)) }

  df <- filter(df, value_type == "TRAWL_TENSION")

  if (!is.null(vessel_name)) df <- filter(df, vessel_name %in% .env$vessel_name)
  if (!is.null(start_date))  df <- filter(df, as.Date(datetime) >= start_date)
  if (!is.null(end_date))    df <- filter(df, as.Date(datetime) <= end_date)

  if (nrow(df) == 0) {
    message("No TRAWL_TENSION data found for the given filters")
    return(invisible(NULL))
  }

  req_cols <- c("traction_port", "traction_center", "traction_starboard")
  if (!all(req_cols %in% names(df))) {
    message("TRAWL_TENSION columns not found — re-sync with updated adapter")
    return(invisible(NULL))
  }

  if (towing_only) df <- filter(df, traction_center > min_traction)

  if (nrow(df) == 0) {
    message(sprintf("No records with traction_center > %d kg", min_traction))
    return(invisible(NULL))
  }

  x_breaks    <- .poseidat_date_breaks(df$datetime)
  x_labels    <- .poseidat_date_label(df$datetime)
  n_vessels   <- length(unique(df$vessel_name))
  facet_layer <- if (n_vessels > 1)
    ggplot2::facet_wrap(~ vessel_name, scales = scales, ncol = ncol)
  else
    NULL

  traction <- df |>
    tidyr::pivot_longer(
      cols      = c(traction_port, traction_center, traction_starboard),
      names_to  = "warp",
      values_to = "traction_kg"
    ) |>
    mutate(warp = dplyr::recode(warp,
                                traction_port      = "Port",
                                traction_center    = "Center",
                                traction_starboard = "Starboard"))

  p_traction <- ggplot2::ggplot(
    traction,
    ggplot2::aes(x = datetime, y = traction_kg, colour = warp)
  ) +
    ggplot2::geom_line(alpha = 0.7, linewidth = 0.4) +
    facet_layer +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "Warp") +
    ggplot2::scale_x_datetime(date_labels = x_labels, date_breaks = x_breaks) +
    ggplot2::labs(
      title   = if (towing_only)
        sprintf("Trawl tension — towing only (traction center > %d kg)", min_traction)
      else
        "Trawl tension",
      subtitle = sprintf("Source: %s parquet", data_source),
      x       = NULL,
      y       = "Traction (kg)",
      caption = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "bottom"
    )

  if (!show_length) return(p_traction)

  length_cols <- c("shooted_length_port", "shooted_length_center",
                   "shooted_length_starboard")
  if (!all(length_cols %in% names(df))) {
    message("Shooted length columns not found — showing traction only")
    return(p_traction)
  }

  wire <- df |>
    tidyr::pivot_longer(
      cols      = all_of(length_cols),
      names_to  = "warp",
      values_to = "length_m"
    ) |>
    mutate(warp = dplyr::recode(warp,
                                shooted_length_port      = "Port",
                                shooted_length_center    = "Center",
                                shooted_length_starboard = "Starboard"))

  p_length <- ggplot2::ggplot(
    wire,
    ggplot2::aes(x = datetime, y = length_m, colour = warp)
  ) +
    ggplot2::geom_line(alpha = 0.7, linewidth = 0.4) +
    facet_layer +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "Warp") +
    ggplot2::scale_x_datetime(date_labels = x_labels, date_breaks = x_breaks) +
    ggplot2::labs(x = NULL, y = "Shooted length (m)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "none"
    )

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_traction, p_length, ncol = 1, heights = c(2, 1))
  } else {
    message("Install patchwork for combined plot: install.packages('patchwork')")
    print(p_traction)
    print(p_length)
  }
}


# # ============================================================================
# # 11. poseidat_plot_overview()
# #
# # Facet grid plot showing all scalar value_types x all vessels in one view.
# # Rows = value_type, Columns = vessel_name.
# #
# # @param source      Filter to one source, or NULL for all
# # @param vessel_name Filter to specific vessels, or NULL for all
# # @param value_type  Filter to specific value_types, or NULL for all scalars
# # @param data_source "thinned" (default) | "raw"
# # @param start_date  Date or NULL
# # @param end_date    Date or NULL
# # @param nonzero     If TRUE (default), exclude zero/NA values
# # @param scales      ggplot2 facet scales: "free_y" (default), "fixed", "free"
# # ============================================================================
# 
# poseidat_plot_overview <- function(source      = NULL,
#                                    vessel_name = NULL,
#                                    value_type  = NULL,
#                                    data_source = "thinned",
#                                    start_date  = NULL,
#                                    end_date    = NULL,
#                                    nonzero     = TRUE,
#                                    scales      = "free_y") {
# 
#   if (!requireNamespace("ggplot2", quietly = TRUE))
#     stop("Install ggplot2: install.packages('ggplot2')")
# 
#   data_source <- match.arg(data_source, c("thinned", "raw"))
# 
#   df <- .poseidat_load_data(data_source, source, vessel_name)
# 
#   if (nrow(df) == 0) { message("No data found"); return(invisible(NULL)) }
# 
#   # Keep only scalar value_types
#   df <- df |>
#     filter(!value_type %in% c("POSITION", "TRAWL_TENSION"),
#            !is.na(value_numeric))
# 
#   if (nonzero) df <- filter(df, value_numeric != 0)
# 
#   if (!is.null(source))      df <- filter(df, source      %in% .env$source)
#   if (!is.null(vessel_name)) df <- filter(df, vessel_name %in% .env$vessel_name)
#   if (!is.null(value_type))  df <- filter(df, value_type  %in% .env$value_type)
#   if (!is.null(start_date))  df <- filter(df, as.Date(datetime) >= start_date)
#   if (!is.null(end_date))    df <- filter(df, as.Date(datetime) <= end_date)
# 
#   if (nrow(df) == 0) {
#     message("No scalar data found after filtering")
#     return(invisible(NULL))
#   }
# 
#   n_types   <- length(unique(df$value_type))
#   n_vessels <- length(unique(df$vessel_name))
#   cat(sprintf("Plotting %d value_type(s) x %d vessel(s)\n", n_types, n_vessels))
# 
#   ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = value_numeric,
#                                     colour = vessel_name)) +
#     ggplot2::geom_line(alpha = 0.6, linewidth = 0.3) +
#     ggplot2::facet_grid(value_type ~ vessel_name, scales = scales) +
#     ggplot2::scale_colour_brewer(palette = "Set1", guide = "none") +
#     ggplot2::scale_x_datetime(
#       date_labels = .poseidat_date_label(df$datetime),
#       date_breaks = .poseidat_date_breaks(df$datetime)
#     ) +
#     ggplot2::labs(
#       title   = "Sensor data overview — all value types x all vessels",
#       subtitle = sprintf("Source: %s parquet", data_source),
#       x       = NULL,
#       y       = NULL,
#       caption = sprintf("Generated: %s", Sys.Date())
#     ) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(
#       strip.text.x     = ggplot2::element_text(face = "bold", size = 8),
#       strip.text.y     = ggplot2::element_text(face = "bold", size = 7,
#                                                 angle = 0, hjust = 0),
#       strip.background = ggplot2::element_rect(fill = "grey92"),
#       axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1, size = 7),
#       axis.text.y      = ggplot2::element_text(size = 7),
#       panel.spacing    = ggplot2::unit(0.3, "lines"),
#       plot.title       = ggplot2::element_text(face = "bold")
#     )
# }


# ============================================================================
# 12. poseidat_plot_operational()
#
# Time series of the key operational variables, faceted as variable (rows) x
# vessel (columns):
#   fuel      FUEL_CONSUMPTION  (value_numeric)
#   speed     SPEED over ground (speed_over_ground from POSITION)
#   traction  TRACTION centre   (traction_center from TRAWL_TENSION)
#   depth     DEPTH             (value_numeric)
#
# @param vessel_name   Filter to specific vessels, or NULL for all
# @param data_source   "thinned" (default) | "raw"
# @param start_date    Date or NULL
# @param end_date      Date or NULL
# @param variables     Which variables to show: any of "fuel", "speed",
#                      "traction", "depth" (default: all four)
# @param device_labels Show only sensors with these labels from the vessel
#                      config, e.g. c("FUEL_1", "FUEL_3"), or NULL for all
# @param by_device     TRUE = one coloured line per sensor (label) with a legend,
#                      FALSE = one colour per vessel. NULL (default) = TRUE when
#                      a single variable is selected, else FALSE
# @param nonzero       If TRUE (default), exclude zero values per variable
# @param scales        facet scales: "free_y" (default), "fixed", "free"
# @param ncol          Number of facet columns for the single-vessel layout
#
# Examples:
#   poseidat_plot_operational(variables = "fuel")                        # all fuel sensors
#   poseidat_plot_operational(variables = "fuel", vessel_name = "Aravis",
#                             start_date = Sys.Date() - 14)
#   poseidat_plot_operational(variables = c("fuel", "traction"),
#                             device_labels = c("FUEL_1", "Hekgalgmeter"))
# ============================================================================

poseidat_plot_operational <- function(vessel_name   = NULL,
                                      data_source   = "thinned",
                                      start_date    = NULL,
                                      end_date      = NULL,
                                      variables     = c("fuel", "speed", "traction", "depth"),
                                      device_labels = NULL,
                                      by_device     = NULL,
                                      nonzero       = TRUE,
                                      scales        = "free_y",
                                      ncol          = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")

  data_source <- match.arg(data_source, c("thinned", "raw"))
  variables   <- match.arg(variables, c("fuel", "speed", "traction", "depth"),
                           several.ok = TRUE)
  if (is.null(by_device)) by_device <- length(variables) == 1

  # Variable definitions
  spec <- tibble(
    key   = c("fuel", "speed", "traction", "depth"),
    vtype = c("FUEL_CONSUMPTION", "POSITION", "TRAWL_TENSION", "DEPTH"),
    col   = c("value_numeric", "speed_over_ground", "traction_center", "value_numeric"),
    label = c("Fuel consumption", "Speed (SOG)", "Traction center", "Depth")
  ) |>
    filter(key %in% variables)

  # Load only the rows (selected vessels, value types, dates) and columns that
  # are needed -- the filters are pushed down into Arrow, so the full fleet
  # history (tens of millions of rows) never enters R memory.
  df <- .poseidat_load_data(
    data_source, vessel_name = vessel_name,
    start_date = start_date, end_date = end_date,
    value_types = unique(spec$vtype),
    cols = c("value_numeric", "speed_over_ground", "traction_center"))

  if (nrow(df) == 0) { message("No data found for the selection"); return(invisible(NULL)) }

  if (!"device_id" %in% names(df)) df$device_id <- NA_character_

  df <- df |>
    left_join(.poseidat_device_labels(), by = "device_id") |>
    mutate(device_label = dplyr::coalesce(device_label, substr(device_id, 1, 8),
                                          value_type))

  if (!is.null(device_labels)) {
    df <- filter(df, device_label %in% device_labels)
    if (nrow(df) == 0) {
      message("No data for sensor label(s): ", paste(device_labels, collapse = ", "))
      return(invisible(NULL))
    }
  }

  extract <- function(data, vtype, col, label) {
    if (!col %in% names(data)) return(tibble())
    d <- data |>
      filter(value_type == vtype, !is.na(.data[[col]])) |>
      dplyr::select(vessel_name, device_label, datetime, value = all_of(col)) |>
      mutate(variable = label)
    if (nonzero) d <- filter(d, value != 0)
    d
  }

  pieces <- vector("list", nrow(spec))
  for (k in seq_len(nrow(spec))) {
    pieces[[k]] <- extract(df, spec$vtype[k], spec$col[k], spec$label[k])
  }
  rm(df)
  long <- bind_rows(pieces)
  rm(pieces)

  if (nrow(long) == 0) {
    message("No data found for the selected variable(s)")
    return(invisible(NULL))
  }

  long <- long |>
    mutate(variable = factor(variable, levels = spec$label),
           line_id  = paste(vessel_name, device_label))

  n_vars    <- length(unique(long$variable))
  n_vessels <- length(unique(long$vessel_name))
  n_sensors <- length(unique(long$line_id))
  cat(sprintf("Plotting %d variable(s) x %d vessel(s), %d sensor line(s)\n",
              n_vars, n_vessels, n_sensors))

  facet_layer <- if (n_vessels == 1)
    ggplot2::facet_wrap(~ variable, scales = scales, ncol = ncol %||% 1)
  else
    ggplot2::facet_grid(variable ~ vessel_name, scales = scales)

  colour_var <- if (by_device) "device_label" else "vessel_name"

  p <- ggplot2::ggplot(long, ggplot2::aes(x = datetime, y = value,
                                          colour = .data[[colour_var]],
                                          group  = line_id)) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.3) +
    facet_layer +
    ggplot2::scale_x_datetime(
      date_labels = .poseidat_date_label(long$datetime),
      date_breaks = .poseidat_date_breaks(long$datetime)
    ) +
    ggplot2::labs(
      title    = paste("Operational overview —", paste(tolower(spec$label), collapse = ", ")),
      subtitle = sprintf("Source: %s parquet", data_source),
      x        = NULL,
      y        = NULL,
      colour   = NULL,
      caption  = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text.x     = ggplot2::element_text(face = "bold", size = 9),
      strip.text.y     = ggplot2::element_text(face = "bold", size = 8,
                                               angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "grey92"),
      axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1, size = 7),
      axis.text.y      = ggplot2::element_text(size = 7),
      panel.spacing    = ggplot2::unit(0.4, "lines"),
      plot.title       = ggplot2::element_text(face = "bold"),
      legend.position  = if (by_device) "bottom" else "none"
    )

  if (!by_device) p <- p + ggplot2::scale_colour_brewer(palette = "Set1", guide = "none")

  p
}


# ============================================================================
# Quick usage examples
# ============================================================================

# # After re-syncing to get POSITION data with the updated adapter:
#
# # Map of all vessel tracks
# poseidat_map()
#
# # Map thinned to one position every 10 minutes
# poseidat_map(thin_minutes = 10)
#
# # Map for specific vessels and date range
# pos <- poseidat_positions(
#   vessel_name  = c("Lub Senior", "Polar"),
#   start_date   = as.Date("2026-02-20"),
#   end_date     = as.Date("2026-03-03"),
#   thin_minutes = 5
# )
# poseidat_map(pos)
#
# # Fuel consumption time series for all vessels
# poseidat_plot_timeseries("FUEL_CONSUMPTION")
#
# # Towing force for specific vessels
# poseidat_plot_timeseries(
#   value_type  = "TOWING_FORCE",
#   vessel_name = c("Lub Senior", "Willem Jacob"),
#   nonzero     = TRUE
# )
#
# # Multiple value types in one plot
# poseidat_plot_timeseries(c("FUEL_CONSUMPTION", "TOWING_FORCE"))

# # All vessels
# poseidat_plot_operational()
# 
# # Specific vessels and date range
# poseidat_plot_operational(
#   vessel_name = c("Willem Jacob"),
#   start_date  = as.Date("2026-02-23"),
#   end_date    = as.Date("2026-03-05")
# )
# # Poseidat data availability
# poseidat_availability()
#
# Read raw file
# files <- c(
#   list.files(file.path(.onedrive(), "raw"),       
#              pattern = "\\.parquet$",
#              recursive = TRUE, 
#              full.names = TRUE)
# )
# df <- read_parquet(sort(files) |> head(1))

# Read thinned file
# thinned_files <- list.files(
#   file.path(.onedrive(), "processed", "thinned"),
#   pattern = "\\.parquet$", full.names = TRUE
# )
# df <- read_parquet(sort(thinned_files) |> tail(1))

# df %>% 
#   group_by(vessel_name) %>% 
#   summarise(mindate = min(datetime, na.rm=TRUE),
#             maxdate = max(datetime, na.rm=TRUE))




# ============================================================================
# SECTION I — AUDIT
# ============================================================================

#' Audit vessels for newly activated sensors not yet in vessel_config
#'
#' Compares the live device list returned by the API against the device UUIDs
#' stored in vessel_config$device_labels. For every device that is present on
#' the API but absent from the config, the function probes a recent sample of
#' entries to identify the value_type, an approximate activation date, and the
#' total record count within the look-back window.
#'
#' Returns a tibble with one row per new device. An empty result (or all-NA
#' device_id rows) means no new sensors have been activated.
#'
#' After identifying a new device, add it to vessel_config:
#'   vessel_config$device_labels[[row]][[uuid]] <- "MY_LABEL"
#'   saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))
#'
#' @param vessel_config Tibble from readRDS(poseidat_vessels.rds)
#' @param source        Character. "dbmatic" | "efice" | "pefa"
#' @param sample_days   Integer. How many days back to probe for value_types
#'                      and record counts (default 30). Increase if a sensor
#'                      was installed more than a month ago.
#' @param sleep_sec     Numeric. Pause between API calls in seconds (default 0.3).
#' @return Tibble: vessel_name, device_id, value_types, first_seen, n_records,
#'         labelled (always FALSE — these are the unlabelled devices).
#' @export
# SUPERSEDED by poseidat_review_devices() (SECTION K). Keep until the review has run
# cleanly for ~2 weeks, then remove. For EFICE the device list comes from the config, so it can never
# find a new device; the probe endpoint only works for EFICE.
audit_new_sensors <- function(vessel_config,
                               source      = "efice",
                               sample_days = 30,
                               sleep_sec   = 0.3) {

  adp       <- .get_adapter(source)
  cutoff_dt <- format(Sys.time() - lubridate::days(sample_days),
                      "%Y-%m-%dT%H:%M:%SZ")
  now_dt    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  active_vessels <- vessel_config |>
    filter(source == .env$source, enabled)

  if (nrow(active_vessels) == 0) {
    message("No enabled vessels found for source: ", source)
    return(tibble(vessel_name = character(), device_id = character(),
                  value_types = character(), first_seen = as.POSIXct(NA),
                  n_records   = integer(),  labelled    = logical()))
  }

  cat(sprintf("Auditing %d vessel(s) on %s ...\n",
              nrow(active_vessels), source))

  results <- vector("list", nrow(active_vessels))

  for (i in seq_len(nrow(active_vessels))) {

    row        <- active_vessels[i, ]
    jid        <- row$journal_id
    vname      <- row$vessel_name
    dev_labels <- row$device_labels[[1]]          # named list/vector: uuid -> label
    known_ids  <- names(dev_labels)               # character vector of known UUIDs

    cat(sprintf("  %s: checking live devices ...\n", vname))

    live_devices <- tryCatch(
      get_devices(source, jid, vname, verbose = TRUE),
      error = function(e) {
        message(sprintf("  Could not fetch devices for %s: %s", vname, e$message))
        tibble(device_id = character())
      }
    )

    # Print a compact per-device table showing label and known/new status
    if (nrow(live_devices) > 0) {
      cat(sprintf("    %-14s  %-22s  %s\n", "UUID (suffix)", "Label", "Status"))
      cat(sprintf("    %s\n", strrep("-", 50)))
      for (k in seq_len(nrow(live_devices))) {
        did_k    <- live_devices$device_id[k]
        lbl_k    <- dev_labels[[did_k]]
        lbl_k    <- if (is.null(lbl_k) || is.na(lbl_k)) "(unlabelled)" else lbl_k
        status_k <- if (did_k %in% known_ids) "known" else "NEW ⚠"
        cat(sprintf("    %-14s  %-22s  %s\n",
                    substr(did_k, nchar(did_k) - 11, nchar(did_k)),
                    lbl_k,
                    status_k))
      }
      cat("\n")
    }

    new_devices <- live_devices |>
      filter(!device_id %in% known_ids)

    if (nrow(new_devices) == 0) {
      cat(sprintf("    -> no new devices\n"))
      results[[i]] <- tibble(
        vessel_name = vname,
        device_id   = character(),
        value_types = character(),
        first_seen  = as.POSIXct(NA),
        n_records   = integer(),
        labelled    = logical()
      )
      next
    }

    cat(sprintf("    → %d new device(s) found — probing ...\n", nrow(new_devices)))

    probed <- vector("list", nrow(new_devices))

    for (j in seq_len(nrow(new_devices))) {

      did  <- new_devices$device_id[j]
      data <- tryCatch({
        resp <- httr2::request(paste0(adp$base_url, "/journals/entries")) |>
          httr2::req_auth_bearer_token(adp$token) |>
          httr2::req_url_query(
            journalId = jid,
            deviceId  = did,
            startDate = cutoff_dt,
            endDate   = now_dt,
            limit     = 100L,
            skip      = 0L
          ) |>
          httr2::req_error(is_error = \(r) FALSE) |>
          httr2::req_perform()
        httr2::resp_body_json(resp)
      }, error = function(e) list(items = list(), total = 0L))

      items <- data$items %||% list()

      vtypes <- if (length(items) == 0)
        NA_character_
      else
        paste(unique(sapply(items, \(x) x$value$type %||% NA_character_)),
              collapse = ", ")

      # Earliest timestamp in the sample — proxy for activation date
      first_seen <- if (length(items) == 0)
        as.POSIXct(NA)
      else {
        ts <- sapply(items, \(x) x$entry_datetime %||% NA_character_)
        ts <- ts[!is.na(ts)]
        if (length(ts) == 0) as.POSIXct(NA)
        else min(as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }

      probed[[j]] <- tibble(
        vessel_name = vname,
        device_id   = did,
        value_types = vtypes,
        first_seen  = first_seen,
        n_records   = as.integer(data$total %||% 0L),
        labelled    = FALSE
      )

      cat(sprintf("      ...%s  types: %-20s  n: %s\n",
                  substr(did, nchar(did) - 7, nchar(did)),
                  vtypes,
                  format(as.integer(data$total %||% 0L), big.mark = ",")))

      Sys.sleep(sleep_sec)
    }

    results[[i]] <- bind_rows(probed)
  }

  out <- bind_rows(results) |> filter(!is.na(device_id))

  if (nrow(out) == 0)
    cat("✓ No new sensors detected.\n")
  else
    cat(sprintf("⚠ %d new sensor(s) detected — add to vessel_config.\n", nrow(out)))

  out
}

#' Audit how current each device's data is, per vessel — disk-side only
#'
#' Reports the most recent record on disk for every device on a vessel, and
#' flags devices that have never synced or have fallen stale relative to
#' "now". This intentionally does NOT query the API for a "true latest"
#' comparison — an earlier version of this function did, using
#' `sortDirection = "descending"` on the EFICE `/journals/entries` endpoint,
#' but that parameter was confirmed (via a live A/B test) to be silently
#' ignored by the API: ascending and descending requests returned identical
#' results. Since `limit = 1` under a silently-ascending sort just returns
#' the OLDEST record in the query window — not the newest — that approach
#' produced a misleading "latest available" value that tracked the sliding
#' window boundary rather than anything about the device. There is no cheap
#' way to ask this API for "the latest record" without downloading and
#' scanning the actual range (i.e. running the real sync). This function
#' sticks to what's reliably knowable without that cost: what's on disk,
#' and how stale it is relative to wall-clock time.
#'
#' @param vessel_name  Character. Exact vessel name as in vessel_config
#' @param vessel_config Tibble. From Poseidat_config.R / readRDS()
#' @param source       Character. "dbmatic" | "efice" | "pefa" (default "efice")
#' @param stale_days   Numeric. Devices whose disk_latest is more than this
#'                     many days behind "now" are flagged `is_stale = TRUE`
#'                     (default 2)
#' @return Tibble: vessel_name, device_name, device_id, disk_latest,
#'   never_synced, is_stale, staleness_days — one row per device
#' @export
# SUPERSEDED by poseidat_review_devices() (SECTION K). Keep until the review has run
# cleanly for ~2 weeks, then remove. Disk-side staleness per device is covered by the SILENT flag.
poseidat_audit_devices <- function(vessel_name, vessel_config,
                                   source     = "efice",
                                   stale_days = 2) {

  journal_id <- get_journal_id(vessel_config, vessel_name, source = source)
  devices    <- get_devices(source, journal_id, vessel_name) |>
    dplyr::mutate(device_name = unlist(device_name))

  devices |>
    dplyr::rowwise() |>
    dplyr::mutate(
      disk_latest = {
        d <- read_latest_datetime(source, vessel_name, device_id)
        if (is.null(d)) as.POSIXct(NA) else d
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      vessel_name     = vessel_name,
      never_synced    = is.na(disk_latest),
      staleness_days  = as.numeric(difftime(Sys.time(), disk_latest, units = "days")),
      is_stale        = !never_synced & staleness_days > stale_days
    ) |>
    dplyr::relocate(vessel_name, device_name, device_id, disk_latest,
                    never_synced, is_stale, staleness_days)
}

# poseidat_audit_devices("Willem Jacob", vessel_config)

build_poseidat_frequency <- function(
    base_dir           = Sys.getenv("ONEDRIVE_POSEIDAT"),
    sources            = c("efice", "pefa"),
    start_date         = NULL,
    end_date           = Sys.Date(),
    vessel_config      = NULL,
    device_label_chars = 8
) {

  if (!nzchar(base_dir)) stop("Set ONEDRIVE_POSEIDAT in .Renviron or pass base_dir.")

  # Collect Parquet files from raw/ and processed/ trees
  parquet_files <- character(0)

  for (tree in c("raw", "processed")) {
    tree_root <- file.path(base_dir, tree)
    if (!dir.exists(tree_root)) next

    # Restrict to requested sources (sub-folders directly under raw/ or processed/)
    candidates <- if (!is.null(sources)) {
      file.path(tree_root, sources)
    } else {
      list.dirs(tree_root, recursive = FALSE, full.names = TRUE)
    }

    for (src_dir in candidates[dir.exists(candidates)]) {
      parquet_files <- c(
        parquet_files,
        list.files(src_dir, pattern = "\\.parquet$",
                   recursive = TRUE, full.names = TRUE)
      )
    }
  }

  parquet_files <- unique(parquet_files)

  if (length(parquet_files) == 0) {
    message("No Parquet files found under ", base_dir)
    return(tibble())
  }

  message(sprintf("Reading %d Parquet file(s)...", length(parquet_files)))

  # Parse source and vessel from path structure:
  #   <base_dir>/(raw|processed)/<source>/<vessel>/.../<file>.parquet
  path_meta <- tibble(filepath = parquet_files) |>
    mutate(
      rel   = sub(paste0("^", normalizePath(base_dir, winslash = "/"), "/?"), "",
                  normalizePath(filepath, winslash = "/")),
      parts = strsplit(rel, "/")
    ) |>
    mutate(
      tree        = sapply(parts, `[[`, 1),
      source      = sapply(parts, function(p) if (length(p) >= 2) p[[2]] else NA_character_),
      vessel_name = sapply(parts, function(p) if (length(p) >= 3) p[[3]] else NA_character_)
    ) |>
    select(filepath, source, vessel_name)
  
  # t <- read_parquet(path_meta$filepath[1])
  
  # Read only the datetime column (+ entry_id for dedup) from each file
  safe_read <- function(fp) {
    tryCatch({
      cols_available <- arrow::open_dataset(fp)$schema$names
      cols_to_read <- intersect(c("datetime", "entry_id", "device_id"), cols_available)
      arrow::read_parquet(fp, col_select = all_of(cols_to_read))
    }, error = function(e) {
      message("  Skipping ", basename(fp), ": ", conditionMessage(e))
      NULL
    })
  }

  all_data <- purrr::map2_dfr(path_meta$filepath, seq_len(nrow(path_meta)), function(fp, i) {
    df <- safe_read(fp)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df |> mutate(
      filepath    = fp,
      source      = path_meta$source[i],
      vessel_name = path_meta$vessel_name[i]
    )
  })

  if (nrow(all_data) == 0) {
    message("Parquet files found but no readable rows.")
    return(tibble())
  }

  # Deduplicate on entry_id if present
  if ("entry_id" %in% names(all_data)) {
    all_data <- all_data |> distinct(entry_id, .keep_all = TRUE)
  }

  # Ensure datetime is POSIXct
  if (!"datetime" %in% names(all_data)) {
    stop("No 'datetime' column found in any Parquet file.")
  }
  all_data <- all_data |>
    mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

  # Optional date filter
  if (!is.null(start_date)) {
    all_data <- all_data |> filter(as.Date(datetime) >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    all_data <- all_data |> filter(as.Date(datetime) <= as.Date(end_date))
  }

  # device_id: use column if present, else "unknown"
  if (!"device_id" %in% names(all_data)) {
    all_data <- all_data |> mutate(device_id = "unknown")
  }

  # Build a flat device_id → device_name lookup from vessel_config$device_labels.
  # device_labels is expected to be a list-column of named character vectors:
  #   names  = device_id (full UUID)
  #   values = human-readable label (e.g. "GPS", "Brandstofmeter", "Weegschaal")
  device_lookup <- if (!is.null(vessel_config) &&
                       "device_labels" %in% names(vessel_config)) {
    purrr::map2_dfr(
      vessel_config$vessel_name,
      vessel_config$device_labels,
      function(vname, labels) {
        if (is.null(labels) || length(labels) == 0) return(NULL)
        tibble(
          vessel_name = vname,
          device_id   = names(labels),
          device_name = unname(labels)
        )
      }
    )
  } else {
    tibble(vessel_name = character(), device_id = character(),
           device_name = character())
  }

  if (nrow(device_lookup) > 0) {
    message(sprintf("  Device labels loaded: %d entries from vessel_config",
                    nrow(device_lookup)))
  } else {
    message("  No device_labels found in vessel_config — UUIDs will be used as labels")
  }

  # Short device label for readability
  all_data <- all_data |>
    mutate(
      device_short = substr(device_id, 1, device_label_chars),
      week_start   = floor_date(as.Date(datetime), unit = "week", week_start = 1),
      year_week    = paste0(isoyear(datetime), "-W",
                            formatC(isoweek(datetime), width = 2, flag = "0"))
    )

  # Aggregate — n_obs and mean interval between consecutive observations
  freq <- all_data |>
    arrange(source, vessel_name, device_id, datetime) |>
    group_by(source, vessel_name, device_id, device_short, week_start, year_week) |>
    summarise(
      n_obs            = n(),
      avg_interval_sec = if (n() < 2) NA_real_ else
        mean(diff(as.numeric(datetime)), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(source, vessel_name, device_short, week_start)

  # Join device names from vessel_config lookup; fall back to short UUID
  freq <- freq |>
    left_join(device_lookup, by = c("vessel_name", "device_id")) |>
    mutate(device_name = ifelse(
      !is.na(device_name) & nzchar(device_name),
      device_name,
      device_short
    ))

  message(sprintf("✓ Frequency table built: %d rows (%d vessel×device×week combos)",
                  nrow(freq),
                  n_distinct(freq |> select(vessel_name, device_id))))
  message(sprintf("  Devices with labels: %d of %d",
                  sum(freq$device_name != freq$device_short),
                  n_distinct(freq |> select(vessel_name, device_id))))
  freq
}


# ----------------------------------------------------------------------------
# 2. HEATMAP PLOT
#    One panel per vessel; rows = devices; columns = weeks; fill = chosen metric
# ----------------------------------------------------------------------------

#' Plot observation frequency as a heatmap: devices × weeks, faceted by vessel
#'
#' @param freq       Tibble returned by build_poseidat_frequency().
#' @param metric     Character. Which metric to display:
#'                   \code{"n_obs"} (default) — number of observations per week;
#'                   \code{"avg_interval"} — mean time between observations,
#'                   shown in the most readable unit (seconds, minutes, or hours).
#' @param title      Plot title. NULL = auto-generated from metric.
#' @param log_scale  Logical. Use log10 colour scale?  Default TRUE (recommended
#'                   for n_obs when frequencies span orders of magnitude).
#'                   Ignored for avg_interval (always linear).
#' @param max_weeks  Integer. Show only this many most-recent weeks.
#'                   NULL = show all weeks.
#'
#' @return A ggplot2 object.
plot_poseidat_frequency <- function(
    freq,
    metric     = c("n_obs", "avg_interval"),
    title      = NULL,
    log_scale  = TRUE,
    max_weeks  = 26
) {
  metric <- match.arg(metric)
  
  if (nrow(freq) == 0) {
    message("Empty frequency table — nothing to plot.")
    return(invisible(NULL))
  }
  
  # Restrict to most recent N weeks if requested
  if (!is.null(max_weeks)) {
    recent_weeks <- freq |> distinct(week_start) |>
      arrange(desc(week_start)) |> slice_head(n = max_weeks) |>
      pull(week_start)
    freq <- freq |> filter(week_start %in% recent_weeks)
  }
  
  # Choose the display column and derive a readable unit for avg_interval
  if (metric == "n_obs") {
    freq <- freq |> mutate(.value = n_obs)
    legend_title <- if (log_scale) "Obs (log10)" else "Observations"
    plot_title   <- title %||% "Poseidat observation count by vessel, device, and week"
    fill_scale <- if (log_scale) {
      scale_fill_viridis_c(
        name     = legend_title,
        option   = "C",
        na.value = "grey90",
        trans    = "log10",
        labels   = scales::label_number(big.mark = ",")
      )
    } else {
      scale_fill_viridis_c(
        name     = legend_title,
        option   = "C",
        na.value = "grey90",
        labels   = scales::label_number(big.mark = ",")
      )
    }
  } else {
    # Pick the most readable time unit based on median interval
    med_sec <- median(freq$avg_interval_sec, na.rm = TRUE)
    if (is.na(med_sec) || med_sec < 120) {
      unit_label <- "sec";   divisor <- 1
    } else if (med_sec < 7200) {
      unit_label <- "min";   divisor <- 60
    } else {
      unit_label <- "hours"; divisor <- 3600
    }
    freq <- freq |> mutate(.value = avg_interval_sec / divisor)
    plot_title <- title %||%
      "Poseidat mean interval between observations by vessel, device, and week"
    fill_scale <- scale_fill_viridis_c(
      name      = paste0("Avg interval\n(", unit_label, ")"),
      option    = "D",
      direction = -1,
      na.value  = "grey90",
      labels    = scales::label_number(accuracy = 0.1, big.mark = ",")
    )
  }
  
  # Complete grid so missing combos appear as NA (grey)
  freq_complete <- freq |>
    complete(
      nesting(source, vessel_name, device_id, device_short, device_name),
      week_start,
      fill = list(.value = NA)
    ) |>
    mutate(year_week = paste0(isoyear(week_start), "-W",
                              formatC(isoweek(week_start), width = 2, flag = "0")))
  
  # Device label: source + device_name (falls back to short UUID if no label)
  freq_complete <- freq_complete |>
    mutate(device_label = paste0(source, "\n", device_name))
  
  # Week axis: show every 2nd or 4th label to avoid crowding
  week_levels <- sort(unique(freq_complete$week_start))
  n_weeks     <- length(week_levels)
  label_step  <- if (n_weeks > 20) 4 else if (n_weeks > 10) 2 else 1
  week_labels <- ifelse(
    seq_along(week_levels) %% label_step == 1,
    format(week_levels, "%d %b"),
    ""
  )
  
  ggplot(freq_complete,
         aes(x = week_start, y = device_label, fill = .value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    facet_wrap(~ vessel_name, ncol = 1, scales = "free_y") +
    fill_scale +
    scale_x_date(
      breaks = week_levels,
      labels = week_labels,
      expand = expansion(add = 3)
    ) +
    labs(
      title   = plot_title,
      x       = NULL,
      y       = "Source / Device name",
      caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y        = element_text(size = 7),
      strip.text         = element_text(face = "bold", size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "right",
      plot.title         = element_text(face = "bold")
    )
}



# ----------------------------------------------------------------------------
# 3. GT TABLE
#    Pivot wider: rows = vessel × device; columns = weeks; cells = formatted value
# ----------------------------------------------------------------------------

#' Display observation frequency as a gt table: vessels × devices as rows,
#' weeks as columns (most recent N weeks).
#'
#' @param freq       Tibble returned by build_poseidat_frequency().
#' @param metric     Character. Which metric to display:
#'                   \code{"n_obs"} (default) — number of observations;
#'                   \code{"avg_interval"} — mean time between observations,
#'                   formatted in the most readable unit (sec / min / hours).
#' @param max_weeks  Integer. Number of most recent weeks to display.
#' @param title      Table title string. NULL = auto-generated from metric.
#'
#' @return A gt table object (print to display or save with gtsave()).
table_poseidat_frequency <- function(
    freq,
    metric    = c("n_obs", "avg_interval"),
    max_weeks = 12,
    title     = NULL
) {
  metric <- match.arg(metric)

  if (nrow(freq) == 0) {
    message("Empty frequency table — nothing to display.")
    return(invisible(NULL))
  }

  # Determine display title and format function based on metric
  if (metric == "n_obs") {
    tbl_title  <- title %||% "Poseidat observations per vessel, device, and week"
    fmt_value  <- function(x) ifelse(is.na(x), "—",
                                     formatC(x, format = "d", big.mark = ","))
    col_footer <- "Observations"
  } else {
    med_sec <- median(freq$avg_interval_sec, na.rm = TRUE)
    if (is.na(med_sec) || med_sec < 120) {
      unit_label <- "sec";   divisor <- 1;    accuracy <- 1
    } else if (med_sec < 7200) {
      unit_label <- "min";   divisor <- 60;   accuracy <- 0.1
    } else {
      unit_label <- "hours"; divisor <- 3600; accuracy <- 0.01
    }
    tbl_title  <- title %||%
      paste0("Poseidat mean interval between observations per vessel, device, and week (",
             unit_label, ")")
    fmt_value  <- function(x) ifelse(is.na(x), "—",
                                     formatC(round(x / divisor, accuracy),
                                             format = "f",
                                             digits = nchar(sub(".*\\.", "", accuracy))))
    col_footer <- paste0("Avg interval (", unit_label, ")")
  }

  # Select most recent weeks
  recent_weeks <- freq |>
    distinct(week_start, year_week) |>
    arrange(desc(week_start)) |>
    slice_head(n = max_weeks) |>
    arrange(week_start)

  freq_filtered <- freq |>
    filter(week_start %in% recent_weeks$week_start)

  # Pivot wider: one column per week
  freq_wide <- freq_filtered |>
    mutate(
      .display = fmt_value(if (metric == "n_obs") n_obs else avg_interval_sec)
    ) |>
    pivot_wider(
      id_cols     = c(source, vessel_name, device_id, device_short, device_name),
      names_from  = year_week,
      values_from = .display,
      values_fill = "—"
    ) |>
    arrange(source, vessel_name, device_name)

  week_cols <- recent_weeks$year_week

  gt(freq_wide, rowname_col = "device_name", groupname_col = "vessel_name") |>
    tab_header(
      title    = tbl_title,
      subtitle = paste0(
        format(min(recent_weeks$week_start), "%d %b %Y"), " – ",
        format(max(recent_weeks$week_start), "%d %b %Y")
      )
    ) |>
    cols_hide(columns = c(device_id, device_short)) |>
    cols_label(
      source      = "Source",
      device_name = "Device"
    ) |>
    cols_align(align = "right",  columns = all_of(week_cols)) |>
    cols_align(align = "left",   columns = c(source, device_name)) |>
    tab_style(
      style     = cell_fill(color = "#f0f0f0"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.size      = px(12),
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#fafafa"
    ) |>
    tab_source_note(
      source_note = paste0(col_footer, "  |  Generated: ",
                           format(Sys.time(), "%Y-%m-%d %H:%M"))
    )
}


# ============================================================================
# EXAMPLE USAGE (run interactively, not sourced automatically)
# ============================================================================
if (FALSE) {

  # Load vessel config (contains device_labels list-column)
  config_dir    <- file.path(Sys.getenv("ONEDRIVE_POSEIDAT"), "config")
  vessel_config <- readRDS(file.path(config_dir, "poseidat_vessels.rds"))

  # vessel_config$device_labels should be a list-column of named character vectors,
  # one entry per vessel row.  Example of how it is constructed in Poseidat_config.R:
  #
  #   vessel_config <- vessel_config |>
  #     mutate(device_labels = list(
  #       # Lub Senior
  #       c("3864776a-...0001" = "GPS",
  #         "3864776a-...0002" = "Brandstofmeter",
  #         "3864776a-...0007" = "Trekkrachtmeting"),
  #       # Polar
  #       c("abcd1234-...0001" = "GPS",
  #         "abcd1234-...0003" = "Weegschaal")
  #     ))

  # 1. Build the frequency summary (reads all Parquet files)
  #    Returns n_obs AND avg_interval_sec for every vessel × device × week
  freq <- build_poseidat_frequency(
    sources       = c("efice", "pefa"),
    start_date    = as.Date("2025-10-01"),
    end_date      = Sys.Date(),
    vessel_config = vessel_config
  )

  # 2a. Heatmap — observation count (log scale), last 16 weeks
  p1 <- plot_poseidat_frequency(freq, metric = "n_obs", max_weeks = 16)
  print(p1)

  # 2b. Heatmap — average interval between observations, last 16 weeks
  #     Colour scale auto-selects unit (sec / min / hours) based on median interval
  p2 <- plot_poseidat_frequency(freq, metric = "avg_interval", max_weeks = 16)
  print(p2)

  # 3a. GT table — observation count, last 12 weeks
  tbl1 <- table_poseidat_frequency(freq, metric = "n_obs",        max_weeks = 12)
  print(tbl1)

  # 3b. GT table — average interval, last 12 weeks
  tbl2 <- table_poseidat_frequency(freq, metric = "avg_interval", max_weeks = 12)
  print(tbl2)

  # Save plots / tables
  # ggsave(file.path(Sys.getenv("ONEDRIVE_POSEIDAT"), "reports",
  #                  paste0("poseidat_nobs_",     Sys.Date(), ".png")), p1,
  #        width = 14, height = 8, dpi = 150)
  # ggsave(file.path(Sys.getenv("ONEDRIVE_POSEIDAT"), "reports",
  #                  paste0("poseidat_interval_", Sys.Date(), ".png")), p2,
  #        width = 14, height = 8, dpi = 150)
  # gt::gtsave(tbl1, file.path(Sys.getenv("ONEDRIVE_POSEIDAT"),
  #                            "reports", "poseidat_nobs.html"))
  gt::gtsave(tbl2, file.path(Sys.getenv("ONEDRIVE_POSEIDAT"),
                             "outputs", "reports", "poseidat_interval.html"))

  # Quick look at a specific vessel
  freq |>
    filter(vessel_name == "Lub Senior") |>
    select(device_name, week_start, n_obs, avg_interval_sec) |>
    mutate(avg_interval_min = round(avg_interval_sec / 60, 1)) |>
    print(n = 40)
}


# ============================================================================
# CONFIG SENSOR AUDIT
# ============================================================================

#' Audit configured device_labels against sensors actually seen on the API
#'
#' For each enabled vessel in vessel_config, fetches a recent sample of
#' journal entries directly from the source API (bypassing the unreliable
#' .xxx_get_devices() discovery functions) and compares the device_id /
#' value_type combinations seen against vessel_config$device_labels.
#'
#' Reports:
#'   - "missing_in_config": device_ids seen in recent data but not present
#'                           in vessel_config$device_labels (i.e. need to be
#'                           added, as happened with FUEL_2/FUEL_3 on
#'                           Willem Jacob)
#'   - "missing_in_data"  : device_ids present in vessel_config$device_labels
#'                          but not seen in the recent sample (sensor may
#'                          have stopped transmitting, or the sample window
#'                          is too short)
#'   - "ok"               : device_ids present in both
#'
#' NOTE: Currently implemented for source = "efice" only. For "pefa", extend
#' the inner fetch block using the equivalent endpoint once available.
#'
#' @param vessel_config Tibble from readRDS(poseidat_vessels.rds)
#' @param source        "efice" (default). "pefa" not yet implemented.
#' @param sample_days   How many days back to sample (default 30)
#' @param sample_limit  Max entries to fetch per vessel (default 500)
#' @return Tibble: vessel_name, journal_id, device_id, value_type, label,
#'         status (one of "ok", "missing_in_config", "missing_in_data")
#' @export
# SUPERSEDED by poseidat_review_devices() (SECTION K). Keep until the review has run
# cleanly for ~2 weeks, then remove. EFICE only; its entry sample is the OLDEST part of the window
# (sort direction is ignored by the API), so recent changes are missed.
audit_sensor_config <- function(vessel_config,
                                source       = "efice",
                                sample_days  = 30,
                                sample_limit = 500L) {

  if (!identical(source, "efice")) {
    stop("audit_sensor_config() currently only supports source = 'efice'.")
  }

  adp <- .get_adapter(source)

  cfg <- vessel_config |>
    filter(.data$source == .env$source, enabled)

  if (nrow(cfg) == 0) {
    message(sprintf("No enabled vessels for source '%s'.", source))
    return(tibble(
      vessel_name = character(), journal_id = character(),
      device_id   = character(), value_type = character(),
      label       = character(), status = character()
    ))
  }

  sample_start <- format(Sys.time() - sample_days * 86400, "%Y-%m-%dT%H:%M:%SZ")
  sample_end   <- format(Sys.time(),                        "%Y-%m-%dT%H:%M:%SZ")

  results <- vector("list", nrow(cfg))

  for (i in seq_len(nrow(cfg))) {

    vessel_name <- cfg$vessel_name[i]
    journal_id  <- cfg$journal_id[i]
    labels      <- cfg$device_labels[[i]]   # named list: device_id -> label

    message(sprintf("[%d/%d] %s (%s) ...", i, nrow(cfg), vessel_name, journal_id))

    # ── Fetch a recent sample of entries directly (bypasses .efice_get_devices) ──
    seen <- tryCatch({
      resp <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(
          journalId     = journal_id,
          startDate     = sample_start,
          endDate       = sample_end,
          limit         = sample_limit,
          sortBy        = "entry_datetime",
          sortDirection = "descending"
        ) |>
        req_error(is_error = \(r) FALSE) |>
        req_timeout(60) |>
        req_perform()

      if (resp_status(resp) >= 400) {
        message(sprintf("  API error %d — skipping vessel", resp_status(resp)))
        tibble(device_id = character(), value_type = character())
      } else {
        data <- resp_body_json(resp)

        tibble(item = data$items %||% list()) |>
          rowwise() |>
          mutate(
            device_id  = as.character(item$device_id  %||% NA_character_),
            value_type = as.character(item$value$type %||% NA_character_)
          ) |>
          ungroup() |>
          select(device_id, value_type) |>
          filter(!is.na(device_id)) |>
          distinct()
      }
    }, error = function(e) {
      message(sprintf("  Error fetching sample: %s — skipping vessel", e$message))
      tibble(device_id = character(), value_type = character())
    })

    # ── Devices/labels currently in config for this vessel ──────────────────
    config_devices <- tibble(
      device_id = names(labels) %||% character(),
      label     = unlist(labels, use.names = FALSE) %||% character()
    )

    # ── Combine: full join on device_id, classify status ─────────────────────
    combined <- full_join(seen, config_devices, by = "device_id") |>
      mutate(
        vessel_name = vessel_name,
        journal_id  = journal_id,
        status = case_when(
          !is.na(value_type) &  is.na(label) ~ "missing_in_config",
           is.na(value_type) & !is.na(label) ~ "missing_in_data",
          TRUE                                ~ "ok"
        )
      ) |>
      select(vessel_name, journal_id, device_id, value_type, label, status)

    results[[i]] <- combined

    Sys.sleep(0.3)
  }

  out <- bind_rows(results) |>
    arrange(vessel_name, status, device_id)

  n_missing_cfg  <- sum(out$status == "missing_in_config")
  n_missing_data <- sum(out$status == "missing_in_data")

  message(sprintf(
    "\nDone. %d device(s) seen in data but missing from config; %d device(s) in config not seen in last %d day(s).",
    n_missing_cfg, n_missing_data, sample_days
  ))

  out
}

#' Print a formatted overview of audit_sensor_config() results
#'
#' @param audit Tibble returned by audit_sensor_config()
#' @return A gt table (invisibly); also printed to the viewer.
#' @export
# SUPERSEDED by poseidat_review_devices() (SECTION K). Keep until the review has run
# cleanly for ~2 weeks, then remove. Display helper for audit_sensor_config().
print_sensor_audit <- function(audit) {

  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install gt: install.packages('gt')")

  if (nrow(audit) == 0) {
    message("Nothing to display — audit result is empty.")
    return(invisible(NULL))
  }

  df <- audit |>
    mutate(
      device_suffix = substr(device_id, nchar(device_id) - 11, nchar(device_id)),
      label         = ifelse(is.na(label), "(not in config)", label),
      value_type    = ifelse(is.na(value_type), "(no recent data)", value_type)
    ) |>
    select(vessel_name, device_suffix, label, value_type, status)

  tbl <- gt::gt(df, groupname_col = "vessel_name") |>
    gt::tab_header(
      title    = "Sensor configuration audit",
      subtitle = sprintf("Generated: %s", Sys.Date())
    ) |>
    gt::cols_label(
      device_suffix = "UUID (suffix)",
      label         = "Config label",
      value_type    = "Value type (recent data)",
      status        = "Status"
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#fff3cd"),
      locations = gt::cells_body(rows = status == "missing_in_config")
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#f8d7da"),
      locations = gt::cells_body(rows = status == "missing_in_data")
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#d4edda"),
      locations = gt::cells_body(rows = status == "ok")
    ) |>
    gt::tab_options(table.font.size = gt::px(12))

  print(tbl)
  invisible(tbl)
}

# Example usage:
# audit <- audit_sensor_config(vessel_config, source = "efice", sample_days = 30)
# print_sensor_audit(audit)


# ============================================================================
# PARALLEL BACKFILL — one device, date-chunked  (backfill only)
# ============================================================================

#' Backfill one device using parallel date-chunked downloads
#'
#' For sources where each deviceId-filtered API call has a large fixed
#' overhead regardless of page size (e.g. EFICE, ~50-70s/call due to a
#' full table scan), pagination alone is very slow for large historical
#' backfills. This function instead splits [start_time, end_time] into
#' chunks (default: monthly) and downloads each chunk's pages in parallel
#' using furrr.
#'
#' This is a BACKFILL-ONLY alternative to sync_device(). It is not intended
#' for the daily incremental runner: small incremental ranges don't benefit
#' from parallelism and add unnecessary worker startup overhead. Use
#' sync_device() (or sync_all_vessels()) for daily syncs, and
#' sync_device_parallel() when catching up a newly added or long-dormant
#' device over a wide historical range.
#'
#' Within each chunk, pagination is sequential (skip-based, as required by
#' the API) but chunks run concurrently across workers. Results are
#' combined, deduplicated by entry_id, and merged into the same additive
#' monthly Parquet files used by sync_device(), so downstream code
#' (thinning, plotting) needs no changes.
#'
#' @param source              Character. "dbmatic" | "efice" | "pefa"
#' @param journal_id          Character. Vessel journal UUID
#' @param device_id           Character. Device UUID
#' @param vessel_name         Character. Used as folder name for Parquet output
#' @param start_time          POSIXct (UTC). Start of backfill window
#' @param end_time            POSIXct or NULL. Defaults to Sys.time()
#' @param chunk_by             Character. Passed to seq.POSIXct's `by`,
#'                             e.g. "1 month" (default), "1 week", "2 weeks"
#'                             Smaller chunks = more parallelism but more
#'                             total API calls (each with fixed overhead) --
#'                             tune based on data density.
#' @param page_size           Integer. Records per API page within a chunk
#'                             (default 10000 -- large, since per-call
#'                             overhead dominates over payload size for
#'                             this API)
#' @param n_workers           Integer. Number of parallel workers (default 4).
#'                            Start conservatively -- some APIs rate-limit
#'                            concurrent connections from the same token.
#' @return Tibble of newly downloaded records (invisibly)
#' @export
sync_device_parallel <- function(source, journal_id, device_id,
                                 vessel_name = "unknown",
                                 start_time,
                                 end_time    = NULL,
                                 chunk_by    = "1 month",
                                 page_size   = 10000,
                                 n_workers   = 4) {

  if (!requireNamespace("furrr", quietly = TRUE))
    stop("Install furrr: install.packages('furrr')")

  if (is.null(end_time)) end_time <- Sys.time()
  if (is.character(start_time)) start_time <- as.POSIXct(start_time, tz = "UTC")
  if (is.character(end_time))   end_time   <- as.POSIXct(end_time,   tz = "UTC")

  log_poseidat(sprintf("Parallel backfill — range: %s -> %s (chunks: %s, workers: %d)",
                       format(start_time, "%Y-%m-%d %H:%M:%S"),
                       format(end_time,   "%Y-%m-%d %H:%M:%S"),
                       chunk_by, n_workers),
               source = source, vessel_id = journal_id)

  # ── Build chunk boundaries ──────────────────────────────────────────────
  chunk_starts <- seq(start_time, end_time, by = chunk_by)
  if (length(chunk_starts) == 0 || tail(chunk_starts, 1) < end_time) {
    chunk_starts <- c(chunk_starts, end_time)
  }

  date_chunks <- tibble(chunk_start = chunk_starts) |>
    mutate(chunk_end = lead(chunk_start, default = end_time)) |>
    filter(chunk_start < end_time, chunk_start < chunk_end)

  n_chunks <- nrow(date_chunks)
  log_poseidat(sprintf("Split into %d chunk(s)", n_chunks),
               source = source, vessel_id = journal_id)

  # ── Per-chunk download (sequential pagination within the chunk) ────────
  # Returns list(data=, error=). NOT bare NULL/tibble -- message() calls
  # inside future workers don't reliably reach the caller's console (confirmed
  # 2026-08-05: a chunk failed 3x with zero visible error text, while the
  # identical call succeeded instantly run directly in the main session).
  # Capturing the error message as DATA and returning it explicitly is the
  # only reliable way to see why a chunk failed when it happens inside a
  # worker process.
  fetch_one_chunk <- function(chunk_start, chunk_end,
                              source, journal_id, device_id, page_size) {
    tryCatch(
      list(
        data = download_measurements(
          source              = source,
          journal_id          = journal_id,
          device_id           = device_id,
          start_time          = chunk_start,
          end_time            = chunk_end,
          page_size           = page_size,
          sleep_between_pages = 0   # workers are already isolated; avoid extra delay
        ),
        error = NULL
      ),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
  }

  # A long interactive session can accumulate large objects in the global
  # workspace; since fetch_one_chunk's closure reaches back to it (see note
  # below), future's global-detection may need to ship more than the 500MB
  # default allows. Raise it temporarily for this call only, restored on exit
  # -- self-contained, doesn't rely on the caller having set this themselves.
  old_max_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = 2000 * 1024^2)   # 2GB
  on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)

  old_plan <- future::plan()
  future::plan(future::multisession, workers = n_workers)
  on.exit(future::plan(old_plan), add = TRUE)

  # ── Chunk-level retry ────────────────────────────────────────────────────
  # A per-request retry already lives inside .pefa_get_measurements()/
  # .http_get(), but a transient issue that outlasts THAT retry window
  # previously dropped the whole chunk silently -- this has now happened
  # twice on real backfills. Retry failed chunks (attempt again, workers
  # only re-requested for what's still missing) up to max_chunk_retries
  # total attempts before finally giving up and reporting exactly what's
  # still missing (WITH the real error text), rather than leaving a silent gap.
  max_chunk_retries <- 3
  pending           <- seq_len(n_chunks)
  chunk_results     <- vector("list", n_chunks)
  chunk_errors      <- vector("list", n_chunks)   # last-seen error text per chunk

  for (attempt in seq_len(max_chunk_retries)) {
    if (length(pending) == 0) break

    if (attempt > 1) {
      log_poseidat(sprintf("Retrying %d failed chunk(s) (attempt %d/%d)",
                          length(pending), attempt, max_chunk_retries),
                 source = source, vessel_id = journal_id)
      Sys.sleep(5)   # brief pause before re-hitting the same endpoint
    }

    # fetch_one_chunk is a closure defined inside this function -- since this
    # file is source()'d into an interactive session (not loaded as a
    # package), its environment chain reaches the global workspace. Rely on
    # AUTOMATIC global detection (globals = TRUE, the default) rather than
    # an explicit whitelist -- fetch_one_chunk calls download_measurements(),
    # which calls .get_adapter()/.*_get_measurements()/log_poseidat() and
    # more, so a hand-maintained list is fragile and silently goes stale the
    # next time any of those functions changes (this exact thing happened
    # once already -- an explicit list here broke the backfill outright with
    # "could not find function download_measurements"). The actual size
    # problem (826MB from accumulated session objects) is handled below by
    # temporarily raising future.globals.maxSize instead.
    attempt_results <- furrr::future_map(
      pending,
      \(i) fetch_one_chunk(date_chunks$chunk_start[i], date_chunks$chunk_end[i],
                          source, journal_id, device_id, page_size),
      .options = furrr::furrr_options(seed = TRUE)
    )

    for (j in seq_along(pending)) {
      chunk_results[[pending[j]]] <- attempt_results[[j]]$data
      if (!is.null(attempt_results[[j]]$error))
        chunk_errors[[pending[j]]] <- attempt_results[[j]]$error
    }

    # lengths() on chunk_results (unwrapped .data, not the list wrapper)
    # distinguishes NULL (failed, length 0) from any tibble (length = ncol,
    # > 0 even with zero rows) -- vectorized, no explicit per-element loop
    # needed to find which chunks still need retrying.
    pending <- pending[lengths(chunk_results[pending]) == 0]
  }

  if (length(pending) > 0) {
    error_text <- character(length(pending))
    for (k in seq_along(pending)) {
      err <- chunk_errors[[pending[k]]]
      error_text[k] <- if (is.null(err)) "no error captured" else err
    }

    failure_detail <- paste(
      sprintf("%s->%s [%s]",
             format(date_chunks$chunk_start[pending], "%Y-%m-%d"),
             format(date_chunks$chunk_end[pending],   "%Y-%m-%d"),
             error_text),
      collapse = "; "
    )
    log_poseidat(sprintf(
      "%d chunk(s) FAILED after %d attempts and were skipped -- data is INCOMPLETE for: %s",
      length(pending), max_chunk_retries, failure_detail
    ), level = "ERROR", source = source, vessel_id = journal_id)
  }

  measurements <- bind_rows(chunk_results)   # NULL entries are simply dropped

  if (nrow(measurements) == 0) {
    log_poseidat("No data returned across all chunks",
                 source = source, vessel_id = journal_id)
    return(invisible(tibble()))
  }

  measurements <- measurements |>
    distinct(entry_id, .keep_all = TRUE) |>
    arrange(datetime)

  log_poseidat(sprintf("Downloaded %d record(s) across %d chunk(s)",
                       nrow(measurements), n_chunks),
               source = source, vessel_id = journal_id)

  # ── Save — merge into additive monthly Parquet files (same as sync_device) ──
  tryCatch({
    paths <- .save_monthly_additive(measurements, source, vessel_name, device_id)
    log_poseidat(sprintf("Merged into %d monthly file(s):", length(paths)),
                 source = source, vessel_id = journal_id)
    for (p in paths)
      log_poseidat(sprintf("  -> %s", p), source = source, vessel_id = journal_id)
  }, error = function(e) {
    log_poseidat(sprintf("Save failed: %s", e$message),
                 level = "ERROR", source = source, vessel_id = journal_id)
  })

  # ── Update sync state ────────────────────────────────────────────────────
  max_dt <- max(measurements$datetime, na.rm = TRUE)
  update_last_sync_time(source, journal_id, device_id,
                        max_dt, nrow(measurements))

  log_poseidat(sprintf("Done -- latest record: %s",
                       format(max_dt, "%Y-%m-%d %H:%M:%S")),
               source = source, vessel_id = journal_id)

  invisible(measurements)
}

# Example usage (backfill a newly added device over several months):
# sync_device_parallel(
#   source      = "efice",
#   journal_id  = "f4fc3064-fd63-4c9a-9635-0b292d534827",
#   device_id   = "61326139-7379-3463-0000-000200000007",
#   vessel_name = "Willem Jacob",
#   start_time  = as.POSIXct("2026-02-15 00:00:00", tz = "UTC"),
#   chunk_by    = "1 month",
#   page_size   = 10000,
#   n_workers   = 4
# )


# ============================================================================
# ONE-TIME MIGRATION: fix ambiguous all-NA column types in existing raw parquet
# ============================================================================

#' Retype existing raw parquet files, no API calls
#'
#' Files written before the explicit type-casting fix in the *_get_measurements()
#' parsers may have latitude/longitude/traction_*/fuel_*/scale_weight/etc.
#' written with an ambiguous inferred type (often bool) whenever that column
#' was 100% NA for a given device's entire file -- confirmed 2026-08-06 on
#' Anna Hendrika, where POSITION's file has latitude as double but every other
#' device's file has it as bool, breaking arrow::open_dataset(unify_schemas=TRUE)
#' across a whole vessel folder. The underlying DATA was never wrong, only the
#' on-disk column type -- so this just re-reads, re-casts, and re-writes each
#' file in place. No API calls needed.
#'
#' Makes a .bak copy of each file actually changed, as a safety net -- delete
#' those once you're satisfied the migration worked correctly.
#'
#' @param dry_run Logical. Default TRUE -- reports what WOULD change without
#'                writing anything. Run once with dry_run = TRUE first, review
#'                the count, then dry_run = FALSE to actually apply.
#' @return Invisibly, a list(changed=, skipped=, errors=)
#' @export
migrate_fix_column_types <- function(dry_run = TRUE) {

  numeric_cols <- c(
    "latitude", "longitude", "course_made_good", "speed_over_ground",
    "traction_port", "traction_center", "traction_starboard",
    "shooted_length_port", "shooted_length_center", "shooted_length_starboard",
    "fuel_avg_consumption", "fuel_peak_consumption",
    "fuel_last_hour_consumption", "fuel_last_day_consumption",
    "scale_weight"
  )
  integer_cols <- c("n_satellites", "scale_haul_number")

  files <- list.files(file.path(.onedrive(), "raw"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)

  cat(sprintf("%s: scanning %d raw parquet file(s)\n",
             if (dry_run) "DRY RUN" else "MIGRATING", length(files)))

  n_changed <- 0
  n_skipped <- 0
  n_errors  <- 0

  for (f in files) {

    outcome <- tryCatch({
      # Read via a temp copy, not the original path directly -- Arrow's
      # memory-mapped read on Windows leaves a lock on the source file that
      # blocks writing back to that same path until released. Same workaround
      # already used in .save_monthly_additive() for the identical reason.
      tmp <- tempfile(fileext = ".parquet")
      file.copy(f, tmp, overwrite = TRUE)
      df <- arrow::read_parquet(tmp)
      unlink(tmp)

      present_numeric <- intersect(numeric_cols, names(df))
      present_integer <- intersect(integer_cols, names(df))
      cols_to_check   <- c(present_numeric, present_integer)

      if (length(cols_to_check) == 0) {
        "skipped"   # this file predates these columns entirely (e.g. old EFICE)
      } else {

        before_types <- character(length(cols_to_check))
        for (i in seq_along(cols_to_check))
          before_types[i] <- class(df[[cols_to_check[i]]])[1]

        for (col in present_numeric) df[[col]] <- as.numeric(df[[col]])
        for (col in present_integer) df[[col]] <- as.integer(df[[col]])

        after_types <- character(length(cols_to_check))
        for (i in seq_along(cols_to_check))
          after_types[i] <- class(df[[cols_to_check[i]]])[1]

        if (identical(before_types, after_types)) {
          "skipped"   # already the correct type -- nothing to change
        } else {
          if (!dry_run) {
            backup <- paste0(f, ".bak")
            if (!file.exists(backup)) file.copy(f, backup)
            arrow::write_parquet(df, f)
          }
          "changed"
        }
      }
    }, error = function(e) {
      message(sprintf("  ERROR on %s: %s", basename(f), conditionMessage(e)))
      "error"
    })

    if (outcome == "changed") n_changed <- n_changed + 1
    if (outcome == "skipped") n_skipped <- n_skipped + 1
    if (outcome == "error")   n_errors  <- n_errors + 1
  }

  cat(sprintf("\n%s: %d file(s) %s, %d already OK, %d error(s) -- out of %d total\n",
             if (dry_run) "DRY RUN COMPLETE" else "MIGRATION COMPLETE",
             n_changed, if (dry_run) "WOULD BE changed" else "changed",
             n_skipped, n_errors, length(files)))

  invisible(list(changed = n_changed, skipped = n_skipped, errors = n_errors))
}


# ============================================================================
# ONE-TIME CLEAN-UP: merge OneDrive conflict copies of raw monthly files
# ============================================================================

#' Merge OneDrive conflict copies ("..._202610-LAPTOP-I1T0G274.parquet") back
#' into their main monthly file
#'
#' OneDrive creates such a copy when two computers write the same file. Both
#' copies are then picked up by poseidat_thin() (double work, n_raw counted
#' twice) and the "main" file can be the less complete one. For every conflict
#' copy this binds it with its main file, removes duplicate entry_ids, and
#' writes the result to the main file name. The conflict copy is then RENAMED
#' to "<name>.parquet.merged" (not deleted), so it is no longer read by any
#' function here; delete those files yourself once you are happy.
#'
#' @param dry_run Logical. TRUE (default) only reports what would happen.
#' @return Invisibly, a tibble with one row per conflict copy
#' @export
poseidat_merge_conflict_copies <- function(dry_run = TRUE) {

  files    <- list.files(file.path(.onedrive(), "raw"), pattern = "\\.parquet$",
                         recursive = TRUE, full.names = TRUE)
  conflict <- files[grepl("_[0-9]{6}-[^/]+\\.parquet$", files)]

  if (length(conflict) == 0) {
    cat("No conflict copies found.\n")
    return(invisible(tibble()))
  }

  cat(sprintf("%s: %d conflict cop%s found\n",
              if (dry_run) "DRY RUN" else "MERGING",
              length(conflict), if (length(conflict) == 1) "y" else "ies"))

  .read_copy <- function(fp) {
    tmp <- tempfile(fileext = ".parquet")
    file.copy(fp, tmp, overwrite = TRUE)
    out <- arrow::read_parquet(tmp)
    unlink(tmp)
    out
  }

  report <- vector("list", length(conflict))

  for (k in seq_along(conflict)) {
    cf   <- conflict[k]
    main <- sub("(_[0-9]{6})-[^/]+\\.parquet$", "\\1.parquet", cf)

    report[[k]] <- tryCatch({
      df_cf   <- .read_copy(cf)
      df_main <- if (file.exists(main)) .read_copy(main) else tibble()

      merged <- dplyr::bind_rows(df_main, df_cf) |>
        dplyr::distinct(entry_id, .keep_all = TRUE) |>
        dplyr::arrange(datetime)

      cat(sprintf("  %-75s main %6d + conflict %6d -> %6d rows\n",
                  basename(main), nrow(df_main), nrow(df_cf), nrow(merged)))

      if (!dry_run) {
        arrow::write_parquet(merged, main)
        file.rename(cf, paste0(cf, ".merged"))
      }

      tibble(main_file = basename(main), conflict_file = basename(cf),
             n_main = nrow(df_main), n_conflict = nrow(df_cf),
             n_merged = nrow(merged), status = if (dry_run) "dry_run" else "merged")
    }, error = function(e) {
      message(sprintf("  ERROR on %s: %s", basename(cf), conditionMessage(e)))
      tibble(main_file = basename(main), conflict_file = basename(cf),
             status = paste("error:", conditionMessage(e)))
    })
  }

  out <- dplyr::bind_rows(report)
  if (dry_run) cat("\nNothing written. Run with dry_run = FALSE to merge.\n")
  else cat("\nDone. Conflict copies renamed to *.parquet.merged -- delete them when satisfied,\n",
           "then rebuild thinned data once: poseidat_thin(..., full_rebuild = TRUE, save = TRUE)\n")
  invisible(out)
}


# ============================================================================
# 13. TRIPS & FISHING OPERATIONS
#
# Two-stage segmentation of vessel sensor data, inspired by the methodology
# of the iapesca R-package (Rodriguez; IAPESCA & DELMOGES programs, IFREMER;
# https://gitlab.ifremer.fr/iapesca/r-packages_iapesca):
#
#   STAGE 1 -- TRIPS
#     A trip starts and ends in a fishing harbour. Detected from POSITION
#     data (poseidat_positions()) using distance-to-nearest-harbour + speed,
#     turned into contiguous AT_SEA/HARBOUR runs with the same lag()/
#     cumsum() "run detection" idiom used throughout this file.
#     -> poseidat_identify_trips()
#
#   STAGE 2 -- FISHING OPERATIONS (hauls / tows)
#     Within each trip, individual fishing operations are detected from up
#     to three independent signals, kept separate and reconciled afterward:
#       - speed_over_ground in the gear-specific "working speed" window
#       - towing force (TRAWL_TENSION traction_center/port/starboard)
#       - catch-processing events (kisten / elog_trek weighings) -- the
#         most authoritative signal when available, since these are
#         haul-level ground truth rather than an inferred window
#     iapesca trains an XGBoost model on speed/turning-angle/distance-to-
#     port covariates for this step; the functions below use transparent,
#     tunable thresholds + run detection instead, so every classification
#     stays auditable.
#     -> poseidat_flag_operations_speed()
#     -> poseidat_flag_operations_traction()
#     -> poseidat_flag_operations_catch()
#     -> poseidat_reconcile_fishing_operations()
#     -> poseidat_identify_fishing_operations()   (runs all of the above)
#
# No apply-family functions are used below, and purrr::map is not used
# either -- every step vectorises naturally over rows/groups with dplyr.
#
# Requires the optional package geosphere for the harbour-distance
# calculation in poseidat_identify_trips() (guarded with requireNamespace(),
# same pattern as leaflet in poseidat_map()).
# ============================================================================

# ---- internal: default harbour list ----------------------------------------

#' Fallback list of North Sea / Channel fishing harbours (name, lat, lon)
#'
#' Used by poseidat_identify_trips() when no `harbours` table is supplied.
#' Coordinates are approximate (inner harbour / fish quay, WGS84, accuracy
#' order of a few hundred metres) -- for production use, supply your own
#' curated `harbours` tibble (same columns: name, lat, lon, optionally
#' harbour_code) rather than relying on this default.
.default_fishing_harbours <- function() {
  tibble(
    name = c("Harlingen", "Den Oever", "Den Helder", "IJmuiden", "Scheveningen",
             "Stellendam", "Colijnsplaat", "Vlissingen", "Breskens",
             "Lauwersoog", "Eemshaven", "Urk",
             "Oostende", "Zeebrugge", "Nieuwpoort",
             "Boulogne-sur-Mer", "Calais", "Dunkerque",
             "Lowestoft", "Grimsby", "Hull", "Peterhead", "Fraserburgh",
             "Esbjerg", "Thyboron", "Hanstholm",
             "Cuxhaven", "Buesum"),
    lat  = c(53.1745, 52.9357, 52.9597, 52.4614, 52.1058,
             51.8161, 51.5966, 51.4426, 51.4008,
             53.4076, 53.4392, 52.6633,
             51.2320, 51.3300, 51.1420,
             50.7290, 50.9680, 51.0450,
             52.4730, 53.5820, 53.7440, 57.4990, 57.6940,
             55.4670, 56.7040, 57.1170,
             53.8670, 54.1170),
    lon  = c(5.4200, 5.0350, 4.7580, 4.5990, 4.2680,
             4.0330, 3.8390, 3.5730, 3.5580,
             6.2080, 6.8330, 5.6060,
             2.9200, 3.2010, 2.7460,
             1.5940, 1.8560, 2.3670,
             1.7500, -0.0770, -0.3360, -1.7930, -2.0000,
             8.4390, 8.2170, 8.6000,
             8.7000, 8.8570)
  )
}


# ---- internal: run detection ------------------------------------------------

#' Detect TRUE/FALSE runs in a boolean flag over time, grouped by vessel
#'
#' Internal helper for poseidat_identify_trips(). Orders each vessel's rows
#' by datetime and turns a boolean flag column into contiguous runs using
#' lag() + cumsum(). Short FALSE runs sandwiched between two TRUE runs and
#' shorter than `bridge_gap_secs` are folded into a single TRUE run first,
#' so one noisy GPS fix near the coast doesn't split a trip into two.
#'
#' @param df data.frame with vessel_name, datetime (POSIXct), and flag_col
#' @param flag_col name (string) of the logical column to run-detect on
#' @param bridge_gap_secs FALSE runs shorter than this, between two TRUE
#'   runs, are absorbed into one TRUE run. 0 disables bridging.
#' @return list(
#'   rows = df joined to its final run_id / run_flag / run_start / run_end,
#'   runs = one row per run: vessel_name, run_id, run_flag, run_start,
#'          run_end, n_obs, run_duration_secs
#' )
.detect_runs_by_vessel <- function(df, flag_col, bridge_gap_secs = 0) {

  d <- df |>
    arrange(vessel_name, datetime) |>
    group_by(vessel_name) |>
    mutate(
      .flag    = .data[[flag_col]],
      .changed = .flag != lag(.flag, default = dplyr::first(.flag)),
      .run_id  = cumsum(.changed)
    ) |>
    ungroup()

  runs <- d |>
    group_by(vessel_name, .run_id) |>
    summarise(
      run_flag  = dplyr::first(.flag),
      run_start = min(datetime, na.rm = TRUE),
      run_end   = max(datetime, na.rm = TRUE),
      n_obs     = n(),
      .groups   = "drop"
    ) |>
    mutate(run_duration_secs = as.numeric(difftime(run_end, run_start, units = "secs"))) |>
    dplyr::select(-.run_id)

  if (bridge_gap_secs > 0) {
    runs <- runs |>
      group_by(vessel_name) |>
      arrange(run_start, .by_group = TRUE) |>
      mutate(
        .bridged  = !run_flag &
          run_duration_secs <= bridge_gap_secs &
          lag(run_flag, default = FALSE) &
          lead(run_flag, default = FALSE),
        run_flag  = run_flag | .bridged,
        .changed2 = run_flag != lag(run_flag, default = dplyr::first(run_flag)),
        .run_id2  = cumsum(.changed2)
      ) |>
      ungroup() |>
      group_by(vessel_name, .run_id2) |>
      summarise(
        run_flag  = dplyr::first(run_flag),
        run_start = min(run_start, na.rm = TRUE),
        run_end   = max(run_end, na.rm = TRUE),
        n_obs     = sum(n_obs),
        .groups   = "drop"
      ) |>
      mutate(run_duration_secs = as.numeric(difftime(run_end, run_start, units = "secs"))) |>
      dplyr::select(-.run_id2)
  }

  runs <- runs |>
    group_by(vessel_name) |>
    arrange(run_start, .by_group = TRUE) |>
    mutate(run_id = row_number()) |>
    ungroup() |>
    dplyr::relocate(run_id, .after = vessel_name)

  # Join the final run back onto every row of df via a non-equi (between)
  # join on datetime -- avoids re-deriving row-level run membership by hand
  # after bridging changed the run boundaries.
  rows <- df |>
    dplyr::select(-dplyr::any_of(c(".flag", ".changed", ".run_id"))) |>
    inner_join(
      runs |> dplyr::select(vessel_name, run_id, run_flag, run_start, run_end),
      by = dplyr::join_by(vessel_name, dplyr::between(datetime, run_start, run_end))
    )

  list(rows = rows, runs = runs)
}


#' Detect TRUE/FALSE runs in a boolean flag over time, grouped by vessel + trip
#'
#' Same idiom as .detect_runs_by_vessel(), grouped by vessel_name AND
#' trip_id instead of vessel_name alone. Used by
#' poseidat_flag_operations_speed() and poseidat_flag_operations_traction()
#' so a run never crosses a trip boundary.
#'
#' @inheritParams .detect_runs_by_vessel
#' @param df data.frame with vessel_name, trip_id, datetime, and flag_col
#' @return same shape as .detect_runs_by_vessel(), with trip_id added
.detect_runs_by_vessel_trip <- function(df, flag_col, bridge_gap_secs = 0) {

  d <- df |>
    arrange(vessel_name, trip_id, datetime) |>
    group_by(vessel_name, trip_id) |>
    mutate(
      .flag    = .data[[flag_col]],
      .changed = .flag != lag(.flag, default = dplyr::first(.flag)),
      .run_id  = cumsum(.changed)
    ) |>
    ungroup()

  runs <- d |>
    group_by(vessel_name, trip_id, .run_id) |>
    summarise(
      run_flag  = dplyr::first(.flag),
      run_start = min(datetime, na.rm = TRUE),
      run_end   = max(datetime, na.rm = TRUE),
      n_obs     = n(),
      .groups   = "drop"
    ) |>
    mutate(run_duration_secs = as.numeric(difftime(run_end, run_start, units = "secs"))) |>
    dplyr::select(-.run_id)

  if (bridge_gap_secs > 0) {
    runs <- runs |>
      group_by(vessel_name, trip_id) |>
      arrange(run_start, .by_group = TRUE) |>
      mutate(
        .bridged  = !run_flag &
          run_duration_secs <= bridge_gap_secs &
          lag(run_flag, default = FALSE) &
          lead(run_flag, default = FALSE),
        run_flag  = run_flag | .bridged,
        .changed2 = run_flag != lag(run_flag, default = dplyr::first(run_flag)),
        .run_id2  = cumsum(.changed2)
      ) |>
      ungroup() |>
      group_by(vessel_name, trip_id, .run_id2) |>
      summarise(
        run_flag  = dplyr::first(run_flag),
        run_start = min(run_start, na.rm = TRUE),
        run_end   = max(run_end, na.rm = TRUE),
        n_obs     = sum(n_obs),
        .groups   = "drop"
      ) |>
      mutate(run_duration_secs = as.numeric(difftime(run_end, run_start, units = "secs"))) |>
      dplyr::select(-.run_id2)
  }

  runs <- runs |>
    group_by(vessel_name, trip_id) |>
    arrange(run_start, .by_group = TRUE) |>
    mutate(run_id = row_number()) |>
    ungroup() |>
    dplyr::relocate(run_id, .after = trip_id)

  rows <- df |>
    dplyr::select(-dplyr::any_of(c(".flag", ".changed", ".run_id"))) |>
    inner_join(
      runs |> dplyr::select(vessel_name, trip_id, run_id, run_flag, run_start, run_end),
      by = dplyr::join_by(vessel_name, trip_id, dplyr::between(datetime, run_start, run_end))
    )

  list(rows = rows, runs = runs)
}


# ---- STAGE 1: trips ----------------------------------------------------------

#' Split vessel position data into fishing trips
#'
#' A trip is a continuous period at sea that starts and ends in a fishing
#' harbour (iapesca convention). Detected as a distance-to-nearest-harbour +
#' speed run over poseidat_positions()-style data.
#'
#' @param positions   Tibble from poseidat_positions(): vessel_name,
#'                    datetime, latitude, longitude, speed_over_ground
#' @param harbours    Tibble of fishing harbours: name, lat, lon. Defaults
#'                    to .default_fishing_harbours() -- supply your own for
#'                    production use (see that function's documentation).
#' @param harbour_radius_km    Positions within this distance of a harbour
#'                    count as "in harbour". Default 3 km; use ~5 km for
#'                    large ports (IJmuiden, Boulogne, Esbjerg).
#' @param speed_threshold_knots  Additionally require speed at/below this to
#'                    count as "in harbour", so a vessel steaming past the
#'                    coast isn't misclassified as being in port. Default 1.5 kn.
#' @param min_trip_hours    Discard AT_SEA runs shorter than this --
#'                    typically harbour manoeuvres or GPS noise, not real
#'                    trips. Default 1 h.
#' @param bridge_gap_mins   Short HARBOUR pings inside an AT_SEA run (e.g.
#'                    one noisy fix near the coast) shorter than this are
#'                    absorbed into the surrounding trip. Default 10 min.
#'
#' @return list(
#'   positions = input positions + trip_id (NA outside any detected trip),
#'   trips     = one row per trip: vessel_name, trip_id, departure_harbour,
#'               departure_time, arrival_harbour, arrival_time,
#'               duration_hours, n_positions
#' )
poseidat_identify_trips <- function(positions,
                                    harbours              = NULL,
                                    harbour_radius_km     = 3,
                                    speed_threshold_knots = 1.5,
                                    min_trip_hours        = 1,
                                    bridge_gap_mins       = 10) {

  if (!requireNamespace("geosphere", quietly = TRUE))
    stop("Install geosphere: install.packages('geosphere')")

  stopifnot(all(c("vessel_name", "datetime", "latitude", "longitude") %in% names(positions)))
  if (is.null(harbours)) harbours <- .default_fishing_harbours()
  stopifnot(all(c("name", "lat", "lon") %in% names(harbours)))

  dist_m      <- geosphere::distm(
    as.matrix(positions[, c("longitude", "latitude")]),
    as.matrix(harbours[,  c("lon", "lat")])
  )
  nearest_col <- max.col(-dist_m, ties.method = "first")           # vectorised arg-min per row
  nearest_km  <- do.call(pmin, as.data.frame(dist_m)) / 1000       # vectorised row-min, no apply

  positions <- positions |>
    mutate(
      .nearest_harbour = harbours$name[nearest_col],
      .dist_harbour_km = nearest_km,
      .in_harbour      = .dist_harbour_km <= harbour_radius_km &
        (is.na(speed_over_ground) | speed_over_ground <= speed_threshold_knots),
      .at_sea          = !.in_harbour
    )

  detected <- .detect_runs_by_vessel(
    df              = positions,
    flag_col        = ".at_sea",
    bridge_gap_secs = bridge_gap_mins * 60
  )
  runs <- detected$runs

  # Harbour name for each HARBOUR run = modal .nearest_harbour among its rows
  harbour_names <- detected$rows |>
    filter(!run_flag) |>
    count(vessel_name, run_id, .nearest_harbour, sort = TRUE) |>
    group_by(vessel_name, run_id) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    dplyr::select(vessel_name, run_id, harbour_name = .nearest_harbour)

  harbour_runs <- runs |>
    filter(!run_flag) |>
    left_join(harbour_names, by = c("vessel_name", "run_id"))

  dep <- harbour_runs |>
    transmute(vessel_name, dep_run_id = run_id + 1,
             departure_harbour = harbour_name, departure_time = run_end)
  arr <- harbour_runs |>
    transmute(vessel_name, arr_run_id = run_id - 1,
             arrival_harbour = harbour_name, arrival_time = run_start)

  trips <- runs |>
    filter(run_flag) |>
    left_join(dep, by = dplyr::join_by(vessel_name, run_id == dep_run_id)) |>
    left_join(arr, by = dplyr::join_by(vessel_name, run_id == arr_run_id)) |>
    mutate(
      departure_time = coalesce(departure_time, run_start),
      arrival_time    = coalesce(arrival_time, run_end),
      duration_hours  = as.numeric(difftime(arrival_time, departure_time, units = "hours"))
    ) |>
    filter(duration_hours >= min_trip_hours) |>
    group_by(vessel_name) |>
    arrange(departure_time, .by_group = TRUE) |>
    mutate(trip_id = row_number()) |>
    ungroup() |>
    dplyr::select(vessel_name, trip_id, departure_harbour, departure_time,
                  arrival_harbour, arrival_time, duration_hours, n_obs) |>
    rename(n_positions = n_obs)

  # Assign trip_id back onto positions via a non-equi (between) join on
  # departure_time/arrival_time -- includes the harbour bookend fixes, per
  # the "starts and ends in a fishing harbour" definition.
  positions_out <- positions |>
    dplyr::select(-starts_with(".")) |>
    left_join(
      trips |> dplyr::select(vessel_name, trip_id, departure_time, arrival_time),
      by = dplyr::join_by(vessel_name, dplyr::between(datetime, departure_time, arrival_time))
    ) |>
    dplyr::select(-departure_time, -arrival_time)

  list(positions = positions_out, trips = trips)
}


# ---- STAGE 2: fishing operations -- per-signal flaggers -----------------------

#' Flag candidate fishing operations from speed alone
#'
#' @param positions   Tibble with vessel_name, trip_id, datetime,
#'                    speed_over_ground -- typically the $positions element
#'                    returned by poseidat_identify_trips()
#' @param speed_min, speed_max  Working-speed window in knots (e.g. ~1-5 kn
#'                    for towed gear -- tune to your gear type)
#' @param min_duration_min   Discard candidate operations shorter than this
#' @param bridge_gap_min     Bridge short out-of-window blips shorter than this
#'
#' @return tibble: vessel_name, trip_id, op_start, op_end, source = "speed"
poseidat_flag_operations_speed <- function(positions,
                                           speed_min        = 1,
                                           speed_max        = 5,
                                           min_duration_min = 5,
                                           bridge_gap_min   = 3) {

  stopifnot(all(c("vessel_name", "trip_id", "datetime", "speed_over_ground") %in% names(positions)))

  d <- positions |>
    filter(!is.na(trip_id)) |>
    mutate(.working_speed = dplyr::between(speed_over_ground, speed_min, speed_max))

  detected <- .detect_runs_by_vessel_trip(
    df              = d,
    flag_col        = ".working_speed",
    bridge_gap_secs = bridge_gap_min * 60
  )

  detected$runs |>
    filter(run_flag, run_duration_secs >= min_duration_min * 60) |>
    transmute(vessel_name, trip_id, op_start = run_start, op_end = run_end,
             source = "speed")
}


#' Flag candidate fishing operations from towing force (traction)
#'
#' @param trawl_data  TRAWL_TENSION tibble, e.g.
#'                    .poseidat_load_data(data_source) |> filter(value_type == "TRAWL_TENSION")
#'                    -- needs vessel_name, datetime, and traction_col
#' @param trips       Trip table from poseidat_identify_trips()$trips --
#'                    used to assign trip_id, since trawl_data is a separate
#'                    device stream from positions
#' @param traction_col   Name (string) of the traction column to use, e.g.
#'                    "traction_center" (default), "traction_port", or
#'                    "traction_starboard"
#' @param traction_threshold_kg  Above this, gear is assumed to be towing.
#'                    Default 1500 kg -- tune against a known haul.
#' @param min_duration_min, bridge_gap_min   As in poseidat_flag_operations_speed()
#'
#' @return tibble: vessel_name, trip_id, op_start, op_end, source = "traction"
poseidat_flag_operations_traction <- function(trawl_data,
                                              trips,
                                              traction_col          = "traction_center",
                                              traction_threshold_kg = 1500,
                                              min_duration_min      = 5,
                                              bridge_gap_min        = 3) {

  stopifnot(all(c("vessel_name", "datetime") %in% names(trawl_data)))
  stopifnot(traction_col %in% names(trawl_data))

  # Drop any trip_id already present in trawl_data (e.g. baked in by a
  # previous poseidat_write_trip_ids() run) -- otherwise the join below
  # collides into trip_id.x/trip_id.y instead of a plain trip_id column.
  d <- trawl_data |>
    dplyr::select(-dplyr::any_of("trip_id")) |>
    left_join(
      trips |> dplyr::select(vessel_name, trip_id, departure_time, arrival_time),
      by = dplyr::join_by(vessel_name, dplyr::between(datetime, departure_time, arrival_time))
    ) |>
    filter(!is.na(trip_id)) |>
    mutate(.towing = .data[[traction_col]] >= traction_threshold_kg)

  detected <- .detect_runs_by_vessel_trip(
    df              = d,
    flag_col        = ".towing",
    bridge_gap_secs = bridge_gap_min * 60
  )

  detected$runs |>
    filter(run_flag, run_duration_secs >= min_duration_min * 60) |>
    transmute(vessel_name, trip_id, op_start = run_start, op_end = run_end,
             source = "traction")
}


#' Turn catch-processing (weighing) records into candidate fishing operations
#'
#' Catch data (kisten / elog_trek) is haul-level ground truth rather than an
#' inferred window, so each haul becomes one candidate operation directly --
#' no run detection needed. shoot_time is used as op_start when available;
#' otherwise op_start is backed off from weighing_time by
#' default_haul_minutes, as a rough placeholder that reconciliation can
#' still widen using the other sources.
#'
#' @param catch_data  Tibble with vessel_name, haul_id, weighing_time, and
#'                    optionally trip_id and shoot_time (from your kisten /
#'                    elog_trek processing -- not loaded from Poseidat
#'                    parquet directly)
#' @param trips       Trip table -- used to assign trip_id if catch_data
#'                    doesn't already have one
#' @param default_haul_minutes  Fallback haul duration when shoot_time is
#'                    missing. Default 90 min.
#'
#' @return tibble: vessel_name, trip_id, op_start, op_end, source = "catch", haul_id
poseidat_flag_operations_catch <- function(catch_data,
                                           trips,
                                           default_haul_minutes = 90) {

  stopifnot(all(c("vessel_name", "haul_id", "weighing_time") %in% names(catch_data)))

  d <- catch_data
  if (!"trip_id" %in% names(d)) {
    d <- d |>
      left_join(
        trips |> dplyr::select(vessel_name, trip_id, departure_time, arrival_time),
        by = dplyr::join_by(vessel_name, dplyr::between(weighing_time, departure_time, arrival_time))
      ) |>
      dplyr::select(-departure_time, -arrival_time)
  }

  has_shoot_time <- "shoot_time" %in% names(d)

  d |>
    filter(!is.na(trip_id)) |>
    transmute(
      vessel_name, trip_id, haul_id,
      op_start = if (has_shoot_time) {
        coalesce(shoot_time, weighing_time - default_haul_minutes * 60)
      } else {
        weighing_time - default_haul_minutes * 60
      },
      op_end   = weighing_time,
      source   = "catch"
    )
}


# ---- STAGE 2: reconciliation --------------------------------------------------

#' Reconcile candidate fishing operations from multiple sources into one set
#'
#' Overlapping (or near-overlapping, within merge_gap_min) candidate windows
#' from different sources are merged into a single fishing operation using a
#' cumulative-max-end sweep -- a standard interval-union pattern, expressed
#' here with dplyr (cummax()/lag()) rather than a loop.
#'
#' @param ...  Any number of candidate tibbles from the flag_operations_*
#'             functions above (each with vessel_name, trip_id, op_start,
#'             op_end, source)
#' @param merge_gap_min  Windows separated by less than this are merged into
#'             one operation. Default 15 min.
#'
#' @return tibble: vessel_name, trip_id, operation_id (sequential within
#'   trip), op_start, op_end, duration_min, sources (comma-separated),
#'   n_sources, confidence ("high" if catch present, "medium" if traction
#'   present, "low" if speed-only), haul_id (if any source supplied one)
poseidat_reconcile_fishing_operations <- function(..., merge_gap_min = 15) {

  candidates <- bind_rows(...)
  if (nrow(candidates) == 0) {
    return(candidates |>
             mutate(operation_id = integer(0), duration_min = numeric(0),
                    sources = character(0), n_sources = integer(0),
                    confidence = character(0)))
  }

  gap_secs <- merge_gap_min * 60

  merged <- candidates |>
    group_by(vessel_name, trip_id) |>
    arrange(op_start, .by_group = TRUE) |>
    mutate(
      .running_end = cummax(as.numeric(op_end)),
      .new_group   = as.numeric(op_start) > (lag(.running_end, default = dplyr::first(.running_end)) + gap_secs),
      operation_id = cumsum(.new_group)
    ) |>
    ungroup()

  merged |>
    group_by(vessel_name, trip_id, operation_id) |>
    summarise(
      op_start  = min(op_start),
      op_end    = max(op_end),
      sources   = paste(sort(unique(source)), collapse = ","),
      n_sources = n_distinct(source),
      haul_id   = if ("haul_id" %in% names(merged)) dplyr::first(stats::na.omit(haul_id)) else NA_character_,
      .groups   = "drop"
    ) |>
    mutate(
      duration_min = as.numeric(difftime(op_end, op_start, units = "mins")),
      confidence   = case_when(
        grepl("catch",    sources) ~ "high",
        grepl("traction", sources) ~ "medium",
        TRUE                       ~ "low"
      )
    ) |>
    group_by(vessel_name, trip_id) |>
    arrange(op_start, .by_group = TRUE) |>
    mutate(operation_id = row_number()) |>
    ungroup() |>
    dplyr::select(vessel_name, trip_id, operation_id, op_start, op_end,
                  duration_min, sources, n_sources, confidence, haul_id)
}


#' End-to-end fishing-operation detection for a set of trips
#'
#' Orchestrates poseidat_flag_operations_speed() / _traction() / _catch()
#' and poseidat_reconcile_fishing_operations(), then assigns operation_id
#' back onto positions (and trawl_data, if supplied) via non-equi joins.
#'
#' @param positions   Positions tibble already carrying trip_id, e.g. the
#'                    $positions element returned by poseidat_identify_trips()
#' @param trips       Trip table, e.g. the $trips element from
#'                    poseidat_identify_trips()
#' @param trawl_data  Optional TRAWL_TENSION tibble (vessel_name, datetime,
#'                    traction_*). Omit to skip the traction signal.
#' @param catch_data  Optional catch/weighing tibble (vessel_name, haul_id,
#'                    weighing_time[, shoot_time]). Omit to skip the catch signal.
#' @param speed_min, speed_max, traction_col, traction_threshold_kg,
#'   min_duration_min, bridge_gap_min, merge_gap_min   See the individual
#'   flag/reconcile functions above for definitions.
#'
#' @return list(
#'   operations = one row per fishing operation (see
#'                poseidat_reconcile_fishing_operations()),
#'   positions  = input positions + operation_id,
#'   trawl_data = input trawl_data + operation_id (NULL if trawl_data not supplied)
#' )
poseidat_identify_fishing_operations <- function(positions,
                                                 trips,
                                                 trawl_data            = NULL,
                                                 catch_data            = NULL,
                                                 speed_min             = 1,
                                                 speed_max             = 5,
                                                 traction_col          = "traction_center",
                                                 traction_threshold_kg = 1500,
                                                 min_duration_min      = 5,
                                                 bridge_gap_min        = 3,
                                                 merge_gap_min         = 15) {

  candidates <- list(
    poseidat_flag_operations_speed(
      positions, speed_min = speed_min, speed_max = speed_max,
      min_duration_min = min_duration_min, bridge_gap_min = bridge_gap_min
    )
  )

  if (!is.null(trawl_data)) {
    candidates <- c(candidates, list(
      poseidat_flag_operations_traction(
        trawl_data, trips, traction_col = traction_col,
        traction_threshold_kg = traction_threshold_kg,
        min_duration_min = min_duration_min, bridge_gap_min = bridge_gap_min
      )
    ))
  }

  if (!is.null(catch_data)) {
    candidates <- c(candidates, list(
      poseidat_flag_operations_catch(catch_data, trips)
    ))
  }

  operations <- do.call(poseidat_reconcile_fishing_operations,
                        c(candidates, list(merge_gap_min = merge_gap_min)))

  positions_out <- positions |>
    left_join(
      operations |> dplyr::select(vessel_name, trip_id, operation_id, op_start, op_end),
      by = dplyr::join_by(vessel_name, trip_id, dplyr::between(datetime, op_start, op_end))
    ) |>
    dplyr::select(-op_start, -op_end)

  trawl_out <- NULL
  if (!is.null(trawl_data)) {
    trawl_out <- trawl_data |>
      dplyr::select(-dplyr::any_of("trip_id")) |>
      left_join(
        trips |> dplyr::select(vessel_name, trip_id, departure_time, arrival_time),
        by = dplyr::join_by(vessel_name, dplyr::between(datetime, departure_time, arrival_time))
      ) |>
      left_join(
        operations |> dplyr::select(vessel_name, trip_id, operation_id, op_start, op_end),
        by = dplyr::join_by(vessel_name, trip_id, dplyr::between(datetime, op_start, op_end))
      ) |>
      dplyr::select(-departure_time, -arrival_time, -op_start, -op_end)
  }

  list(operations = operations, positions = positions_out, trawl_data = trawl_out)
}


# ---- persisting trips ----------------------------------------------------------

#' Tag thinned Parquet files with trip_id / trip_uid and re-save
#'
#' Runs poseidat_identify_trips() on the current thinned POSITION data and
#' writes two columns onto EVERY row of each vessel's thinned data (not
#' just POSITION -- TRAWL_TENSION, fuel, depth etc. all happened within the
#' same trip time window):
#'   trip_id  -- sequential within vessel, as returned by
#'               poseidat_identify_trips() (NOT unique across vessels)
#'   trip_uid -- globally unique across the whole fleet:
#'               "<vessel_code>_<4-digit trip_id>", e.g. "UK158_0007".
#'               Uses vessel_code from vessel_config when available;
#'               falls back to a sanitised vessel_name for any vessel
#'               without a code assigned yet.
#'
#' Also writes the trip table itself to
#' processed/trips/poseidat_trips.parquet as the authoritative record --
#' treat trip_id/trip_uid on the thinned files as a convenience join for
#' time-series queries, and this trips table as the source of truth. The
#' write MERGES with whatever is already on disk: only vessels present in
#' the trips being written are replaced, so calling this for one vessel (or
#' a subset) never touches other vessels' rows in poseidat_trips.parquet.
#' Before writing, up to `n_backups` previous versions of that file are
#' kept as poseidat_trips_bak1.parquet, _bak2.parquet, etc. (bak1 = most
#' recent previous version).
#'
#' IMPORTANT CAVEATS:
#' - Trip boundaries are a DERIVED label, not raw data. If you change
#'   harbours/thresholds, or resync new data that shifts where a trip
#'   starts/ends, you need to re-run this to keep it current.
#' - This does NOT run automatically as part of poseidat_thin(save = TRUE).
#'   If you want trip_id to always be present after a sync, call this
#'   right after the thinning step in your sync script.
#' - `interval_min` must match the interval your existing thinned files
#'   actually use (check via poseidat_files() or the filenames themselves,
#'   e.g. "..._thinned_1min_...") -- otherwise this writes a
#'   differently-named file that .poseidat_load_data("thinned") won't
#'   necessarily pick up as "latest".
#'
#' @param vessel_name  Character vector of vessels to tag, or NULL for all.
#'                      Ignored if `detected` is supplied (see below).
#' @param detected  Optional -- the list already returned by
#'                   poseidat_identify_trips(), if you've already run and
#'                   inspected it (e.g. tuned harbour_radius_km/
#'                   speed_threshold_knots and checked the trips table
#'                   looks right). Passing it here skips recomputing trip
#'                   detection and guarantees you persist exactly what you
#'                   inspected, rather than silently re-deriving trips with
#'                   a possibly different set of thresholds. When supplied,
#'                   `vessel_name`/`harbours`/`harbour_radius_km`/
#'                   `speed_threshold_knots`/`min_trip_hours`/
#'                   `bridge_gap_mins` below are all ignored -- everything
#'                   about the trips themselves comes from `detected`.
#'                   Leave NULL to have this function load thinned
#'                   POSITION data and run poseidat_identify_trips()
#'                   itself, using the threshold arguments below.
#' @param harbours, harbour_radius_km, speed_threshold_knots, min_trip_hours,
#'   bridge_gap_mins   Passed through to poseidat_identify_trips() -- only
#'   used when `detected` is NULL.
#' @param interval_min  Must match the thinning interval already used for
#'                      these files (poseidat_thin()'s default is 1).
#' @param n_backups  How many previous versions of poseidat_trips.parquet
#'                   to keep as rotating backups (default 2). 0 disables.
#' @param dry_run  If TRUE (default), returns the tagged data and a
#'                 coverage summary WITHOUT writing anything to disk --
#'                 inspect before committing.
#'
#' @return list(tagged = tibble of all thinned rows + trip_id/trip_uid,
#'              trips  = the trip table, with trip_uid added,
#'              coverage = per-vessel n_total/n_tagged/pct_tagged)
poseidat_write_trip_ids <- function(vessel_name            = NULL,
                                    detected                = NULL,
                                    harbours               = NULL,
                                    harbour_radius_km      = 3,
                                    speed_threshold_knots  = 1.5,
                                    min_trip_hours         = 1,
                                    bridge_gap_mins        = 10,
                                    interval_min           = 1,
                                    n_backups              = 2,
                                    dry_run                = TRUE) {

  # When `detected` is supplied, load only the vessels it actually covers --
  # vessel_name (the argument) is ignored in that case, per the docs above.
  vn_load <- if (!is.null(detected)) unique(detected$trips$vessel_name) else vessel_name

  base <- .onedrive()
  df   <- .poseidat_load_data("thinned", vessel_name = vn_load)
  if (nrow(df) == 0) { message("No thinned data found"); return(invisible(NULL)) }
  df <- filter(df, !is.na(vessel_name))

  # Drop any trip_id/trip_uid already baked into these thinned files from a
  # previous run -- otherwise the join below produces trip_id.x/trip_id.y
  # instead of erroring, silently breaking the coverage summary downstream.
  # This is what makes the function safe to re-run on already-tagged files.
  df <- df |> dplyr::select(-dplyr::any_of(c("trip_id", "trip_uid")))

  if (is.null(detected)) {
    pos <- df |>
      filter(value_type == "POSITION", !is.na(latitude), !is.na(longitude))
    if (nrow(pos) == 0) { message("No POSITION rows in thinned data"); return(invisible(NULL)) }

    detected <- poseidat_identify_trips(
      positions              = pos,
      harbours               = harbours,
      harbour_radius_km      = harbour_radius_km,
      speed_threshold_knots  = speed_threshold_knots,
      min_trip_hours         = min_trip_hours,
      bridge_gap_mins        = bridge_gap_mins
    )
  } else {
    stopifnot(is.list(detected), "trips" %in% names(detected),
             all(c("vessel_name", "trip_id", "departure_time", "arrival_time") %in% names(detected$trips)))
  }

  # Prefer vessel_code (e.g. "UK158") for trip_uid when it's present in
  # vessel_config; fall back to a sanitised vessel_name for any vessel that
  # doesn't have a code assigned yet, so this never errors on a missing code.
  code_map <- df |>
    dplyr::select(vessel_name, dplyr::any_of("vessel_code")) |>
    dplyr::distinct()
  if (!"vessel_code" %in% names(code_map)) code_map$vessel_code <- NA_character_
  code_map <- code_map |>
    mutate(trip_prefix = coalesce(vessel_code, .safe(vessel_name)))

  trips <- detected$trips |>
    dplyr::select(-dplyr::any_of("trip_uid")) |>
    left_join(code_map |> dplyr::select(vessel_name, trip_prefix), by = "vessel_name") |>
    mutate(trip_uid = sprintf("%s_%04d", trip_prefix, trip_id)) |>
    dplyr::select(-trip_prefix) |>
    dplyr::relocate(trip_uid, .after = trip_id)

  # Tag every row (all value_types) that falls inside one of its vessel's
  # trip windows -- same between-join pattern poseidat_identify_trips()
  # uses internally for POSITION rows, applied here to the full thinned df.
  tagged <- df |>
    left_join(
      trips |> dplyr::select(vessel_name, trip_id, trip_uid, departure_time, arrival_time),
      by = dplyr::join_by(vessel_name, dplyr::between(datetime, departure_time, arrival_time))
    ) |>
    dplyr::select(-departure_time, -arrival_time)

  coverage <- tagged |>
    group_by(vessel_name) |>
    summarise(
      n_total    = n(),
      n_tagged   = sum(!is.na(trip_id)),
      pct_tagged = round(100 * n_tagged / n_total, 1),
      .groups    = "drop"
    )

  if (dry_run) {
    message("dry_run = TRUE -- nothing written. Inspect $coverage, then re-run with dry_run = FALSE.")
    return(invisible(list(tagged = tagged, trips = trips, coverage = coverage)))
  }

  # ---- write the authoritative trip table -------------------------------------
  # Only vessels present in `trips` are replaced -- any other vessel already
  # in poseidat_trips.parquet (e.g. because this call only processed a subset)
  # is carried through untouched, so this is always safe to call per-vessel.
  trips_dir  <- file.path(base, "processed", "trips")
  dir.create(trips_dir, recursive = TRUE, showWarnings = FALSE)
  trips_file <- file.path(trips_dir, "poseidat_trips.parquet")

  # Read via a temp copy, not trips_file directly -- Arrow's memory-mapped
  # read on Windows leaves a lock on the source file that blocks writing
  # back to that same path until released. Same workaround already used in
  # migrate_to_monthly() for the identical reason.
  existing_trips <- if (file.exists(trips_file)) {
    tmp <- tempfile(fileext = ".parquet")
    file.copy(trips_file, tmp, overwrite = TRUE)
    et <- arrow::read_parquet(tmp)
    unlink(tmp)
    et
  } else {
    trips[0, ]
  }
  merged_trips   <- bind_rows(
    existing_trips |> filter(!vessel_name %in% unique(trips$vessel_name)),
    trips
  ) |>
    arrange(vessel_name, trip_id)

  # Rotate backups: bak1 -> bak2 -> ... -> bak{n_backups} (oldest dropped),
  # then the file about to be overwritten -> bak1.
  if (n_backups > 0 && file.exists(trips_file)) {
    bak_path <- function(i) file.path(trips_dir, sprintf("poseidat_trips_bak%d.parquet", i))
    if (n_backups >= 2) {
      for (i in seq(n_backups, 2)) {
        if (file.exists(bak_path(i - 1))) file.copy(bak_path(i - 1), bak_path(i), overwrite = TRUE)
      }
    }
    file.copy(trips_file, bak_path(1), overwrite = TRUE)
  }

  write_parquet(merged_trips, trips_file)
  message(sprintf(
    "  Saved trips table -> processed/trips/poseidat_trips.parquet (%d trips total, %d updated for %d vessel(s), %d backup(s) kept)",
    nrow(merged_trips), nrow(trips), dplyr::n_distinct(trips$vessel_name), n_backups
  ))

  # ---- overwrite each vessel's thinned file, same naming/pruning poseidat_thin() uses ----
  for (vn in unique(tagged$vessel_name)) {
    vessel_dir <- file.path(base, "processed", "thinned", .safe(vn))
    dir.create(vessel_dir, recursive = TRUE, showWarnings = FALSE)

    out_file <- file.path(vessel_dir,
                          sprintf("%s_thinned_%dmin_%s.parquet",
                                  .safe(vn), interval_min, format(Sys.Date(), "%Y%m%d")))

    tagged |>
      filter(vessel_name == vn) |>
      write_parquet(out_file)
    message(sprintf("  Saved %s -> %s", vn, basename(out_file)))

    existing  <- sort(list.files(vessel_dir, pattern = "\\.parquet$", full.names = TRUE),
                      decreasing = TRUE)
    to_delete <- existing[seq_along(existing) > 3]
    if (length(to_delete) > 0) {
      file.remove(to_delete)
      message(sprintf("  Cleaned %d old thinned file(s) for %s", length(to_delete), vn))
    }
  }

  invisible(list(tagged = tagged, trips = merged_trips, coverage = coverage))
}


#' Incrementally update the trip table for recent data only
#'
#' Unlike calling poseidat_write_trip_ids() with `detected = NULL` (which
#' re-derives trips for the ENTIRE thinned time series every time), this
#' only re-runs trip detection over a recent window and leaves
#' already-identified trips untouched:
#'   - trips that had already finished before the reprocessing window
#'     started are kept exactly as they were (same trip_id, trip_uid,
#'     everything) -- NOT reallocated
#'   - anything from the window onward is redetected, which naturally
#'     extends a trip that was still open (no arrival yet) last time, or
#'     adds a brand-new trip using the next trip_id in sequence
#'   - a vessel with no trips recorded at all yet gets a full first-time run
#'
#' The reprocessing window, per vessel, is
#'   min(Sys.time() - lookback_days, departure_time of that vessel's
#'       currently-open trip, if any)
#' so a normal daily run only touches the last `lookback_days`, but an
#' unusually long trip that's still ongoing is never cut off mid-trip.
#'
#' @param vessel_name    Character vector to restrict to, or NULL (default)
#'                        for every vessel with thinned data. Vessels NOT
#'                        included are left completely untouched in the
#'                        saved trip table (see poseidat_write_trip_ids()).
#' @param lookback_days   Default reprocessing window in days (30). Must
#'                        comfortably exceed your longest realistic trip --
#'                        the observed range so far tops out around 7 days,
#'                        so 30 has wide margin.
#' @param harbours, harbour_radius_km, speed_threshold_knots, min_trip_hours,
#'   bridge_gap_mins   Passed to poseidat_identify_trips() for the window.
#' @param interval_min   Must match the thinning interval already in use.
#' @param n_backups      Rotating backups of poseidat_trips.parquet to keep
#'                       (default 2). Passed to poseidat_write_trip_ids().
#' @param dry_run  If TRUE (default), previews without writing anything.
#'
#' @return Same shape as poseidat_write_trip_ids(): list(tagged, trips,
#'   coverage) -- `trips` covers only the vessel(s) processed this call
#'   (kept-old + redetected), not the whole fleet.
poseidat_update_trip_ids <- function(vessel_name            = NULL,
                                     lookback_days          = 30,
                                     harbours               = NULL,
                                     harbour_radius_km      = 3,
                                     speed_threshold_knots  = 1.5,
                                     min_trip_hours         = 1,
                                     bridge_gap_mins        = 10,
                                     interval_min           = 1,
                                     n_backups              = 2,
                                     dry_run                = TRUE) {

  base       <- .onedrive()
  trips_file <- file.path(base, "processed", "trips", "poseidat_trips.parquet")

  old_trips <- if (file.exists(trips_file)) {
    # Read via a temp copy -- this same trips_file gets written back to
    # (via poseidat_write_trip_ids() below) later in this call, and a
    # direct read_parquet() would leave a Windows memory-map lock on it.
    tmp <- tempfile(fileext = ".parquet")
    file.copy(trips_file, tmp, overwrite = TRUE)
    ot <- arrow::read_parquet(tmp)
    unlink(tmp)
    ot
  } else {
    tibble(vessel_name = character(), trip_id = integer(),
          departure_harbour = character(), departure_time = as.POSIXct(character(), tz = "UTC"),
          arrival_harbour = character(), arrival_time = as.POSIXct(character(), tz = "UTC"),
          duration_hours = numeric(), n_positions = integer())
  }

  pos <- poseidat_positions(vessel_name = vessel_name, data_source = "thinned")
  if (nrow(pos) == 0) { message("No thinned POSITION data found"); return(invisible(NULL)) }

  vessels_to_process <- unique(pos$vessel_name)
  known_vessels       <- unique(old_trips$vessel_name)

  # ---- reprocessing window per vessel ----------------------------------------
  default_cutoff <- Sys.time() - lubridate::days(lookback_days)

  # A trip that hadn't reached a harbour yet when previously computed has
  # arrival_harbour == NA (arrival_time itself is never NA -- it's always
  # filled with the last known position timestamp, see
  # poseidat_identify_trips()) -- this is the correct "still open" signal.
  open_trip_start <- old_trips |>
    filter(vessel_name %in% vessels_to_process, is.na(arrival_harbour)) |>
    group_by(vessel_name) |>
    summarise(open_start = min(departure_time), .groups = "drop")

  windows <- tibble(vessel_name = vessels_to_process) |>
    left_join(open_trip_start, by = "vessel_name") |>
    mutate(
      window_start = dplyr::case_when(
        !vessel_name %in% known_vessels ~ as.POSIXct("1970-01-01", tz = "UTC"),  # brand-new vessel: full history
        TRUE ~ as.POSIXct(
          pmin(as.numeric(default_cutoff), as.numeric(coalesce(open_start, default_cutoff))),
          origin = "1970-01-01", tz = "UTC"
        )
      )
    )

  # ---- split old trips for the vessels being processed: kept vs. reprocessed --
  kept_old_trips <- old_trips |>
    filter(vessel_name %in% vessels_to_process) |>
    left_join(windows |> dplyr::select(vessel_name, window_start), by = "vessel_name") |>
    filter(departure_time < window_start) |>
    dplyr::select(-window_start)

  # ---- redetect only the window -----------------------------------------------
  pos_window <- pos |>
    left_join(windows |> dplyr::select(vessel_name, window_start), by = "vessel_name") |>
    filter(datetime >= window_start) |>
    dplyr::select(-window_start)

  if (nrow(pos_window) == 0) {
    message("Nothing within the reprocessing window for the selected vessel(s)")
    new_trips <- old_trips[0, ]
  } else {
    redetected <- poseidat_identify_trips(
      positions              = pos_window,
      harbours               = harbours,
      harbour_radius_km      = harbour_radius_km,
      speed_threshold_knots  = speed_threshold_knots,
      min_trip_hours         = min_trip_hours,
      bridge_gap_mins        = bridge_gap_mins
    )

    # Continue each vessel's trip_id sequence from its highest KEPT trip_id
    # (0 if none), rather than restarting at 1 within the window -- this is
    # what avoids reallocating/renumbering the trips already kept above.
    max_kept_id <- kept_old_trips |>
      group_by(vessel_name) |>
      summarise(max_id = max(trip_id), .groups = "drop")

    new_trips <- redetected$trips |>
      left_join(max_kept_id, by = "vessel_name") |>
      mutate(max_id = coalesce(max_id, 0L)) |>
      group_by(vessel_name) |>
      arrange(departure_time, .by_group = TRUE) |>
      mutate(trip_id = max_id + row_number()) |>
      ungroup() |>
      dplyr::select(-max_id)
  }

  combined_trips <- bind_rows(kept_old_trips, new_trips) |>
    arrange(vessel_name, trip_id)

  cat(sprintf(
    "  %d vessel(s): kept %d existing trip(s) unchanged, (re)detected %d trip(s) in the window\n",
    length(vessels_to_process), nrow(kept_old_trips), nrow(new_trips)
  ))

  poseidat_write_trip_ids(
    detected      = list(trips = combined_trips),
    interval_min  = interval_min,
    n_backups     = n_backups,
    dry_run       = dry_run
  )
}



# ============================================================================
# 14. FUEL CONSUMPTION PER DAY  &  DATA STALENESS CHECK
#
# Written in dplyr style: no apply-family functions and no purrr::map.
# Fleet-wide reads go through a single arrow::open_dataset() over the
# relevant parquet files (see .poseidat_open_dataset()), with filtering done
# lazily inside arrow before collect(), so only fuel rows (or only the
# aggregated last-seen table) are pulled into memory.
#
# Fuel integration:
#   each observation counts for the time until the next observation of the
#   same device, capped at `max_gap_min`. Longer gaps (sensor offline,
#   harbour, outage) contribute nothing and show up as low coverage_pct.
#   litres = flow (L/h) * minutes / 60
#
# Staleness:
#   SENSOR STALE  -- a value_type lags behind the vessel's OWN latest data
#                    by more than `sensor_gap_hours` (e.g. fuel stops while
#                    positions continue: Lub Senior, 7 Sept 2026)
#   VESSEL SILENT -- no data at all from a vessel for > `vessel_silent_days`
#   NO DATA       -- enabled vessel in config without thinned data in the
#                    last `lookback_days`
# ============================================================================

# ---- internal: open thinned or raw parquet as one arrow dataset ------------

#' Open thinned or raw parquet files as a single arrow Dataset
#'
#' Lists the parquet files first (so `.parquet.bak` backups are skipped) and
#' passes them to open_dataset() with schema unification. For raw data,
#' monthly files outside start_date/end_date are dropped by their YYYY/MM
#' path before opening -- same pruning idea as .poseidat_scan_raw_daily().
#'
#' @param data_source "thinned" | "raw"
#' @param sources     Raw source folder(s) -- only used for "raw"
#' @param start_date, end_date  Optional Date bounds for raw month pruning
#' @return arrow Dataset
#' @keywords internal
.poseidat_open_dataset <- function(data_source = "thinned",
                                   sources     = c("efice", "pefa", "dbmatic"),
                                   start_date  = NULL,
                                   end_date    = NULL) {

  dirs <- if (data_source == "thinned") {
    file.path(.onedrive(), "processed", "thinned")
  } else {
    file.path(.onedrive(), "raw", sources)
  }
  dirs <- dirs[dir.exists(dirs)]
  if (length(dirs) == 0) stop("No ", data_source, " parquet directory found")

  files <- list.files(dirs, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

  if (data_source == "raw" && (!is.null(start_date) || !is.null(end_date))) {
    ym_date <- suppressWarnings(as.Date(
      sub(".*/([0-9]{4})/([0-9]{2})/[^/]+$", "\\1-\\2-01", files)
    ))
    keep <- !is.na(ym_date)
    if (!is.null(start_date))
      keep <- keep & ym_date >= as.Date(format(as.Date(start_date), "%Y-%m-01"))
    if (!is.null(end_date))
      keep <- keep & ym_date <= as.Date(format(as.Date(end_date), "%Y-%m-01"))
    files <- files[keep]
  }

  if (length(files) == 0) stop("No ", data_source, " parquet files in the requested range")
  arrow::open_dataset(files, unify_schemas = TRUE)
}


# ---- fuel devices -----------------------------------------------------------

#' Fuel devices per vessel, from vessel_config$device_labels
#'
#' Used to attach friendly labels (FUEL_1 ... FUEL_4) to fuel data. Fuel rows
#' themselves are selected on value_type (see poseidat_fuel_obs()), so a fuel
#' device missing from the config is still included -- just labelled with
#' its device_id.
#'
#' @param vessel_config  Tibble from readRDS(poseidat_vessels.rds)
#' @param fuel_keywords  Label substrings that identify fuel devices
#' @return tibble: vessel_name, device_id, device_name
#' @export
poseidat_fuel_devices <- function(vessel_config,
                                  fuel_keywords = c("fuel", "brandstof",
                                                    "flow", "verbruik")) {
  vessel_config |>
    dplyr::select(vessel_name, device_labels) |>
    tidyr::unnest_longer(device_labels,
                         indices_to = "device_id",
                         values_to  = "device_name") |>
    dplyr::mutate(device_name = as.character(device_name)) |>
    dplyr::filter(grepl(paste(fuel_keywords, collapse = "|"),
                        device_name, ignore.case = TRUE)) |>
    dplyr::distinct(vessel_name, device_id, device_name)
}


# ---- fuel observations --------------------------------------------------------

#' Load fuel flow observations for all (or selected) vessels
#'
#' Selects rows on value_type == fuel_value_type when that column exists,
#' otherwise on the device_ids in `fuel_devices`. The flow value is taken
#' from value_numeric (falling back to value_mean / value for files written
#' by older thinning code -- with schema unification the missing column is
#' NA, so all candidates are coalesced). Rows without a flow value are
#' dropped rather than counted as zero.
#'
#' @param vessel_config   Tibble from readRDS(poseidat_vessels.rds)
#' @param fuel_devices    Output of poseidat_fuel_devices(), or NULL to
#'                        build it from vessel_config
#' @param data_source     "thinned" (default, 1-min, fast) | "raw" (high-res)
#' @param sources         Raw source folder(s), only used for "raw"
#' @param vessel_name     Character vector to restrict to, or NULL for all
#' @param start_date, end_date  Date range (inclusive)
#' @param fuel_value_type value_type of fuel rows (default "FUEL_CONSUMPTION")
#' @return tibble: vessel_name, device_id, device_name, datetime, flow
#' @export
poseidat_fuel_obs <- function(vessel_config,
                              fuel_devices    = NULL,
                              data_source     = c("thinned", "raw"),
                              sources         = c("efice", "pefa", "dbmatic"),
                              vessel_name     = NULL,
                              start_date      = Sys.Date() - 30,
                              end_date        = Sys.Date(),
                              fuel_value_type = "FUEL_CONSUMPTION") {

  data_source <- match.arg(data_source)
  if (is.null(fuel_devices)) fuel_devices <- poseidat_fuel_devices(vessel_config)

  # Pad by one day so that local-time day boundaries are complete
  t_start <- as.POSIXct(as.Date(start_date) - 1, tz = "UTC")
  t_end   <- as.POSIXct(as.Date(end_date)   + 2, tz = "UTC")

  ds <- .poseidat_open_dataset(data_source, sources,
                               start_date = as.Date(start_date) - 1,
                               end_date   = as.Date(end_date) + 1)

  value_cols <- intersect(c("value_numeric", "value_mean", "value"), names(ds))
  if (length(value_cols) == 0) {
    stop("No flow column found. Columns in the ", data_source, " files: ",
         paste(names(ds), collapse = ", "))
  }

  use_type <- "value_type" %in% names(ds)
  q <- if (use_type) {
    dplyr::filter(ds, value_type == fuel_value_type)
  } else {
    dplyr::filter(ds, device_id %in% fuel_devices$device_id)
  }

  obs <- q |>
    dplyr::filter(datetime >= t_start, datetime < t_end) |>
    dplyr::select(dplyr::any_of(c("vessel_name", "journal_id", "device_id",
                                  "datetime", value_cols))) |>
    dplyr::collect() |>
    dplyr::mutate(flow = dplyr::coalesce(!!!rlang::syms(value_cols))) |>
    dplyr::filter(!is.na(flow)) |>
    dplyr::distinct(device_id, datetime, .keep_all = TRUE)

  if (nrow(obs) == 0) {
    message("No fuel observations found in the requested range")
    return(tibble(vessel_name = character(), device_id = character(),
                  device_name = character(),
                  datetime = as.POSIXct(character(), tz = "UTC"),
                  flow = numeric()))
  }

  # Vessel name: thinned files carry it; raw files don't -- fill it via
  # journal_id from the config, then via the device lookup as last resort
  if (!"vessel_name" %in% names(obs)) obs <- dplyr::mutate(obs, vessel_name = NA_character_)

  if ("journal_id" %in% names(obs)) {
    obs <- obs |>
      dplyr::left_join(vessel_config |>
                         dplyr::distinct(journal_id, vessel_name_cfg = vessel_name),
                       by = "journal_id") |>
      dplyr::mutate(vessel_name = dplyr::coalesce(vessel_name, vessel_name_cfg))
  }

  obs <- obs |>
    dplyr::left_join(fuel_devices |>
                       dplyr::distinct(device_id, vessel_name_dev = vessel_name,
                                       device_name),
                     by = "device_id") |>
    dplyr::mutate(vessel_name = dplyr::coalesce(vessel_name, vessel_name_dev),
                  device_name = dplyr::coalesce(device_name, device_id)) |>
    dplyr::filter(!is.na(vessel_name))

  if (!is.null(vessel_name)) obs <- dplyr::filter(obs, vessel_name %in% .env$vessel_name)

  obs |>
    dplyr::select(vessel_name, device_id, device_name, datetime, flow) |>
    dplyr::arrange(vessel_name, device_id, datetime)
}


# ---- daily consumption per device -----------------------------------------------

#' Integrate fuel flow to litres per day, per vessel x fuel device
#'
#' @param fuel_obs     Output of poseidat_fuel_obs()
#' @param flow_unit    "L_per_h" (default), "L_per_min" or "kg_per_h"
#' @param max_gap_min  Max minutes a single observation may represent.
#'                     1 = strict sum of 1-min bins; 5 (default) bridges
#'                     short dropouts.
#' @param tz           Time zone that defines "a day" (default
#'                     "Europe/Amsterdam"; "UTC" matches the parquet timestamps)
#' @param start_date, end_date  Trim the output to this range
#' @return tibble per vessel x device x date: fuel_L, minutes_cov,
#'   coverage_pct, active_hours, mean_flow_L_h, max_flow_L_h, n_obs
#' @export
poseidat_fuel_daily <- function(fuel_obs,
                                flow_unit   = c("L_per_h", "L_per_min", "kg_per_h"),
                                max_gap_min = 5,
                                tz          = "Europe/Amsterdam",
                                start_date  = NULL,
                                end_date    = NULL) {

  flow_unit      <- match.arg(flow_unit)
  diesel_density <- 0.84   # kg/L, only used for kg_per_h

  out <- fuel_obs |>
    dplyr::group_by(vessel_name, device_id, device_name) |>
    dplyr::arrange(datetime, .by_group = TRUE) |>
    dplyr::mutate(
      flow_L_per_h = dplyr::case_when(
        flow_unit == "L_per_min" ~ flow * 60,
        flow_unit == "kg_per_h"  ~ flow / diesel_density,
        TRUE                     ~ flow
      ),
      step_min = as.numeric(difftime(dplyr::lead(datetime), datetime, units = "mins")),
      step_min = pmin(dplyr::coalesce(step_min, 1), max_gap_min),
      litres   = flow_L_per_h * step_min / 60,
      date     = as.Date(lubridate::with_tz(datetime, tz))
    ) |>
    dplyr::group_by(vessel_name, device_id, device_name, date) |>
    dplyr::summarise(
      fuel_L        = sum(litres),
      minutes_cov   = sum(step_min),
      coverage_pct  = pmin(100, minutes_cov / 1440 * 100),
      active_hours  = sum(step_min[flow_L_per_h > 1]) / 60,
      mean_flow_L_h = mean(flow_L_per_h[flow_L_per_h > 1]),
      max_flow_L_h  = max(flow_L_per_h),
      n_obs         = dplyr::n(),
      .groups       = "drop"
    ) |>
    dplyr::mutate(dplyr::across(c(fuel_L, coverage_pct, active_hours, mean_flow_L_h),
                                \(x) round(x, 1)))

  if (!is.null(start_date)) out <- dplyr::filter(out, date >= as.Date(start_date))
  if (!is.null(end_date))   out <- dplyr::filter(out, date <= as.Date(end_date))
  out
}


# ---- daily consumption per vessel -------------------------------------------------

#' Sum daily fuel consumption over the fuel devices of each vessel
#'
#' @param daily_device     Output of poseidat_fuel_daily()
#' @param exclude_devices  device_ids to leave out of the vessel total
#' @param return_devices   device_ids of return-flow meters, subtracted
#'                         instead of added (supply - return = net use)
#' @return tibble per vessel x date: fuel_L, n_devices, min_coverage_pct,
#'   active_hours
#' @export
poseidat_fuel_daily_vessel <- function(daily_device,
                                       exclude_devices = character(0),
                                       return_devices  = character(0)) {
  daily_device |>
    dplyr::filter(!device_id %in% exclude_devices) |>
    dplyr::mutate(sign = dplyr::if_else(device_id %in% return_devices, -1, 1)) |>
    dplyr::group_by(vessel_name, date) |>
    dplyr::summarise(
      fuel_L           = round(sum(sign * fuel_L), 1),
      n_devices        = dplyr::n_distinct(device_id),
      min_coverage_pct = min(coverage_pct),
      active_hours     = max(active_hours),
      .groups          = "drop"
    )
}


# ---- plot ---------------------------------------------------------------------------

#' Bar chart of daily fuel consumption, one panel per vessel
#'
#' @param daily         Output of poseidat_fuel_daily() (by_device = TRUE
#'                      stacks the devices) or poseidat_fuel_daily_vessel()
#' @param by_device     Fill bars by device (requires device-level input)
#' @param min_coverage  Drop days with less coverage (%) than this
#' @export
poseidat_plot_fuel_daily <- function(daily, by_device = FALSE, min_coverage = 0) {

  cov_col <- intersect(c("coverage_pct", "min_coverage_pct"), names(daily))[1]
  df <- if (is.na(cov_col)) daily else dplyr::filter(daily, .data[[cov_col]] >= min_coverage)
  if (by_device && !"device_name" %in% names(df)) by_device <- FALSE
  df <- dplyr::mutate(df, fill_var = if (by_device) device_name else vessel_name)

  ggplot2::ggplot(df, ggplot2::aes(x = date, y = fuel_L, fill = fill_var)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(~ vessel_name, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_date(date_labels = "%d %b") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title    = "Daily fuel consumption per vessel",
      subtitle = sprintf("Flow rate integrated over time  |  gaps not filled  |  coverage >= %s%%",
                         min_coverage),
      x = NULL, y = "Fuel (L/day)", fill = NULL,
      caption  = sprintf("Generated: %s", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = if (by_device) "bottom" else "none",
      strip.text      = ggplot2::element_text(face = "bold"),
      plot.title      = ggplot2::element_text(face = "bold")
    )
}


# ---- diagnostic: which files deliver fuel rows -------------------------------------

#' Which parquet files deliver fuel rows for which days?
#'
#' One row per file x device x day, with the number of rows and how many of
#' them have a value in each candidate flow column. Use it to spot stale
#' thinned files, schema differences, or devices that stopped reporting.
#'
#' @param vessel_name     Vessel to check (matched on the file name), or NULL
#' @param data_source     "thinned" (default) | "raw"
#' @param sources         Raw source folder(s), only used for "raw"
#' @param start_date      Only look at data from this date onward
#' @param fuel_value_type value_type of fuel rows
#' @export
poseidat_check_fuel_files <- function(vessel_name     = NULL,
                                      data_source     = "thinned",
                                      sources         = c("efice", "pefa", "dbmatic"),
                                      start_date      = Sys.Date() - 60,
                                      fuel_value_type = "FUEL_CONSUMPTION") {

  ds         <- .poseidat_open_dataset(data_source, sources, start_date = start_date)
  value_cols <- intersect(c("value_numeric", "value_mean", "value"), names(ds))

  out <- ds |>
    dplyr::filter(value_type == fuel_value_type,
                  datetime >= as.POSIXct(as.Date(start_date), tz = "UTC")) |>
    dplyr::mutate(file = arrow::add_filename()) |>
    dplyr::select(file, device_id, datetime, dplyr::any_of(value_cols)) |>
    dplyr::collect() |>
    dplyr::mutate(file = basename(file), date = as.Date(datetime))

  if (!is.null(vessel_name)) {
    out <- dplyr::filter(out, grepl(paste(.safe(vessel_name), collapse = "|"), file))
  }

  out |>
    dplyr::group_by(file, device_id, date) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      dplyr::across(dplyr::any_of(value_cols), \(x) sum(!is.na(x)), .names = "n_{.col}"),
      .groups = "drop"
    ) |>
    dplyr::arrange(device_id, date, file)
}


# ---- staleness check ------------------------------------------------------------------

#' Flag vessels and sensor types that stopped delivering data
#'
#' Reads the last observation per vessel x value_type from the thinned
#' parquet in one aggregate arrow query. A sensor type is compared with the
#' vessel's own most recent observation, not with today, so a vessel lying
#' in port with the digihub off does not raise sensor warnings.
#'
#' Warnings are printed and written to the monthly log via log_poseidat();
#' the full status table is saved to logs/poseidat_staleness_latest.csv.
#' A sensor type that stopped longer than `lookback_days` ago drops out of
#' the check, so keep lookback_days generous.
#'
#' @param vessel_config       Tibble from readRDS(poseidat_vessels.rds)
#' @param sensor_gap_hours    SENSOR STALE threshold (default 48 h)
#' @param vessel_silent_days  VESSEL SILENT threshold (default 7 days)
#' @param lookback_days       How far back to look for data (default 120)
#' @param exclude_value_types value_types to ignore (default "TEMPERATURE")
#' @param now                 Reference time (default Sys.time())
#' @param save_csv            Write logs/poseidat_staleness_latest.csv
#' @return Invisibly, tibble: vessel_name, value_type, last_obs,
#'   vessel_last_obs, lag_vs_vessel_h, vessel_silent_d, status
#' @export
poseidat_check_staleness <- function(vessel_config,
                                     sensor_gap_hours    = 48,
                                     vessel_silent_days  = 7,
                                     lookback_days       = 120,
                                     exclude_value_types = "TEMPERATURE",
                                     now                 = Sys.time(),
                                     save_csv            = TRUE) {

  cutoff <- now - as.difftime(lookback_days, units = "days")

  # Read each thinned file separately and only the 3 needed columns, so a
  # schema difference in an unrelated column (e.g. n_satellites int vs double)
  # cannot break the check.
  thinned_files <- list.files(file.path(.onedrive(), "processed", "thinned"),
                              pattern = "\\.parquet$", recursive = TRUE,
                              full.names = TRUE)
  if (length(thinned_files) == 0) stop("No thinned parquet files found")

  per_file <- vector("list", length(thinned_files))
  for (fi in seq_along(thinned_files)) {
    q <- arrow::open_dataset(thinned_files[fi]) |>
      dplyr::select(vessel_name, value_type, datetime) |>
      dplyr::filter(datetime >= cutoff, !is.na(vessel_name))
    if (length(exclude_value_types) > 0)
      q <- dplyr::filter(q, !value_type %in% exclude_value_types)
    per_file[[fi]] <- q |>
      dplyr::group_by(vessel_name, value_type) |>
      dplyr::summarise(last_obs = max(datetime)) |>
      dplyr::collect() |>
      dplyr::ungroup()
  }

  last_seen <- dplyr::bind_rows(per_file) |>
    dplyr::group_by(vessel_name, value_type) |>
    dplyr::summarise(last_obs = max(last_obs), .groups = "drop")

  status <- last_seen |>
    dplyr::group_by(vessel_name) |>
    dplyr::mutate(vessel_last_obs = max(last_obs)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      lag_vs_vessel_h = as.numeric(difftime(vessel_last_obs, last_obs, units = "hours")),
      vessel_silent_d = as.numeric(difftime(now, vessel_last_obs, units = "days")),
      status = dplyr::case_when(
        vessel_silent_d > vessel_silent_days ~ "VESSEL SILENT",
        lag_vs_vessel_h > sensor_gap_hours   ~ "SENSOR STALE",
        TRUE                                 ~ "ok"
      ),
      dplyr::across(c(lag_vs_vessel_h, vessel_silent_d), \(x) round(x, 1))
    )

  # Enabled vessels in the config without any thinned data in the window
  cfg_vessels <- vessel_config
  if ("enabled" %in% names(cfg_vessels)) cfg_vessels <- dplyr::filter(cfg_vessels, enabled)

  no_data <- cfg_vessels |>
    dplyr::distinct(vessel_name) |>
    dplyr::anti_join(last_seen, by = "vessel_name") |>
    dplyr::mutate(status = "NO DATA")

  status <- dplyr::bind_rows(status, no_data) |>
    dplyr::arrange(status == "ok", vessel_name, value_type)

  # ---- console / log output ----------------------------------------------------
  vessel_lines <- status |>
    dplyr::filter(status == "VESSEL SILENT") |>
    dplyr::distinct(vessel_name, vessel_last_obs, vessel_silent_d) |>
    dplyr::mutate(txt = sprintf("VESSEL SILENT  %-20s no data since %s (%.1f days)",
                                vessel_name,
                                format(vessel_last_obs, "%Y-%m-%d %H:%M"),
                                vessel_silent_d))

  sensor_lines <- status |>
    dplyr::filter(status == "SENSOR STALE") |>
    dplyr::mutate(txt = sprintf("SENSOR STALE   %-20s %-20s last %s (%.1f days behind vessel)",
                                vessel_name, value_type,
                                format(last_obs, "%Y-%m-%d %H:%M"),
                                lag_vs_vessel_h / 24))

  nodata_lines <- no_data |>
    dplyr::mutate(txt = sprintf("NO DATA        %-20s nothing in the last %d days",
                                vessel_name, lookback_days))

  all_lines <- c(vessel_lines$txt, sensor_lines$txt, nodata_lines$txt)

  if (length(all_lines) == 0) {
    log_poseidat("Staleness check: all vessels and sensor types up to date")
  } else {
    log_poseidat(sprintf("Staleness check: %d warning(s)", length(all_lines)),
                 level = "WARNING")
    for (txt in all_lines) log_poseidat(txt, level = "WARNING")
  }

  if (save_csv) {
    log_dir <- file.path(.onedrive(), "logs")
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    .write_csv_excel(status, file.path(log_dir, "poseidat_staleness_latest.csv"))
  }

  invisible(status)
}


# ============================================================================
# SECTION K — DEVICE REVIEW
#
# Daily review of sensors: what the API offers  vs  what is in the config
# vs  what is on disk (raw parquet). Flags every change, proposes fixes,
# NEVER edits the config by itself.
#
# Three views of the same sensors
#   API    : pefa / dbmatic -> full device inventory incl. brand, product_no,
#                              serial_no (get_device_details())
#            efice          -> no inventory endpoint; a few short windows of
#                              recent entries are sampled instead
#   CONFIG : vessel_config$device_labels
#   DISK   : raw parquet (device id from the file name; first/last record read
#            cheaply through Arrow)
# A registry (config/poseidat_device_registry.rds) remembers every device ever
# seen, with names, types and hardware identity, so a renumbered or swapped
# sensor can be recognised afterwards.
#
# Flags
#   UNLABELLED_NEW        HIGH  device never seen before, not in config
#   GONE_FROM_API         HIGH  configured device no longer in the API list AND no data
#   SILENT                HIGH  configured device silent for > silent_days while
#                               the vessel still delivers (<= recent_days)
#   REPLACEMENT_CANDIDATE HIGH  gone/silent device + unlabelled device of the
#                               same kind starting around the same time
#   HARDWARE_CHANGED      HIGH  same device_id, other serial / product / brand
#   UNLABELLED_ACTIVE     INFO  active device without a label (repeats daily)
#   CONFIG_ID_IS_EQUIPMENT HIGH configured id is an equipment id, not a device id
#                               (config mistake: the label is never synced)
#   NOT_IN_INVENTORY      INFO  configured device delivers data but is missing from
#                               the API device list (the list is not always complete)
#   NAME_CHANGED          INFO  same device_id, different name in the API
#   CONFIGURED_NO_DATA    INFO  labelled device with nothing on disk
#   API_UNAVAILABLE       INFO  inventory could not be fetched for a vessel
# Devices you decided to ignore (poseidat_ignore_devices()) are not flagged.
# A silent/gone device is NOT flagged when another device with the same label is
# delivering (i.e. after apply_device_renumbering()).
#
# Typical use
#   rev <- poseidat_review_devices(vessel_config)     # also in the daily runner
#   rev$flags ; rev$proposals
#   apply_device_renumbering(vessel_config, <chosen proposals>, save = TRUE)
#
# dplyr style, no apply family, no purrr.
# ============================================================================

# ---- full device detail (DBMatic / PEFA) -------------------------------------

#' Full device/sensor detail for one vessel (DBMatic or PEFA only)
#'
#' .dbmatic_get_devices() / .pefa_get_devices() keep only a few fields for
#' day-to-day syncing. The equipment_inventory response holds more per device
#' (brand, product_no, serial_no, ...). This re-fetches the same endpoint and
#' keeps EVERY field present, via tidyr::unnest_wider()/unnest_longer().
#' EFICE has no equipment_inventory endpoint and is not supported.
#'
#' @param source     "dbmatic" or "pefa"
#' @param journal_id Character. Vessel journal UUID
#' @return Tibble, one row per device (journal_id, equipment_*, device_*, then
#'   every other field the API returned; columns vary by source/vessel)
get_device_details <- function(source, journal_id) {

  source <- match.arg(source, c("dbmatic", "pefa"))

  adp <- .get_adapter(source)

  endpoint <- switch(source,
    dbmatic = glue("journals/{journal_id}/equipment_inventory"),
    pefa    = glue("api/v1/Journals/{journal_id}/equipment_inventory")
  )

  data       <- .http_get(adp$base_url, adp$token, endpoint)
  equip_list <- data$equipment

  if (is.null(equip_list) || length(equip_list) == 0) {
    return(tibble(journal_id = journal_id))
  }

  tibble(equipment = equip_list) |>
    tidyr::unnest_wider(equipment) |>
    dplyr::rename(equipment_name = name, equipment_type = type) |>
    tidyr::unnest_longer(devices, values_to = "device", keep_empty = TRUE) |>
    tidyr::unnest_wider(device) |>
    dplyr::rename(device_name = name, device_type = type) |>
    dplyr::filter(!is.na(device_id)) |>
    dplyr::mutate(journal_id = journal_id, .before = 1) |>
    dplyr::relocate(journal_id, equipment_id, equipment_name, equipment_type,
                     device_id, device_name, device_type)
}

#' Full device/sensor detail for every vessel of one source in vessel_config
#'
#' @param source        "dbmatic" or "pefa"
#' @param vessel_config  Your vessel_config tibble (needs journal_id, vessel_name)
#' @return Tibble as get_device_details(), with vessel_name added
get_all_device_details <- function(source, vessel_config) {

  vc <- vessel_config |>
    dplyr::filter(source == !!source)

  out <- vector("list", nrow(vc))

  for (i in seq_len(nrow(vc))) {
    out[[i]] <- tryCatch(
      get_device_details(source, vc$journal_id[i]) |>
        dplyr::mutate(vessel_name = vc$vessel_name[i], .after = journal_id),
      error = function(e) {
        message(glue("  [get_all_device_details] {vc$vessel_name[i]}: {conditionMessage(e)}"))
        NULL
      }
    )
  }

  dplyr::bind_rows(out)
}

# ---- helpers -----------------------------------------------------------------

.uuid_rx <- "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"

# CSV that opens correctly in English Excel: comma separator, decimal point,
# UTF-8 with BOM (readr), blanks for NA
.write_csv_excel <- function(df, path) {
  if (requireNamespace("readr", quietly = TRUE))
    readr::write_excel_csv(df, path, na = "")
  else
    utils::write.csv(df, path, row.names = FALSE, na = "")
  invisible(path)
}

# Any API field -> character; list fields collapsed; blanks become NA
.dev_chr <- function(x) {
  if (is.list(x)) {
    out <- character(length(x))
    for (k in seq_along(x))
      out[k] <- if (length(x[[k]]) == 0) NA_character_
                else paste(as.character(unlist(x[[k]])), collapse = ", ")
    x <- out
  }
  x <- as.character(x)
  x[!is.na(x) & trimws(x) == ""] <- NA_character_
  x
}

# Serial numbers that can serve as evidence: DBMatic fills "1" for many devices,
# so short, repeated or placeholder values are treated as missing
.valid_serial <- function(x) {
  x  <- trimws(x)
  ok <- !is.na(x) & nchar(x) >= 4 & !grepl("^(.)\\1*$", x) &
        !tolower(x) %in% c("n/a", "na", "none", "unknown", "null", "-")
  ifelse(ok, x, NA_character_)
}

# Collapse value types: ignore TEMPERATURE unless it is the only one
.main_value_type <- function(v) {
  v  <- unique(v[!is.na(v)])
  v2 <- setdiff(v, "TEMPERATURE")
  paste(sort(if (length(v2) > 0) v2 else v), collapse = ", ")
}

# ---- config view ---------------------------------------------------------------
.review_cfg_devices <- function(vessel_config) {
  cfg <- vessel_config |> filter(enabled)
  out <- vector("list", nrow(cfg))
  for (i in seq_len(nrow(cfg))) {
    lab <- cfg$device_labels[[i]]
    out[[i]] <- tibble(
      source      = cfg$source[i],
      vessel_name = cfg$vessel_name[i],
      device_id   = names(lab) %||% character(),
      label       = as.character(unlist(lab, use.names = FALSE)) %||% character())
  }
  bind_rows(out)
}

# ---- EFICE: devices seen in a few short windows of recent entries ------------
# (EFICE has no inventory endpoint; its entries endpoint ignores sort direction
#  and returns the OLDEST entries of a window, so only short windows are used.)
.review_efice_probe <- function(jid, offsets_h, window_min = 60, limit = 2000L) {
  adp <- .get_adapter("efice")
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out <- list()
  for (off in offsets_h) {
    t0   <- Sys.time() - off * 3600
    t1   <- t0 + window_min * 60
    resp <- tryCatch(
      httr2::request(paste0(adp$base_url, "/journals/entries")) |>
        httr2::req_auth_bearer_token(adp$token) |>
        httr2::req_url_query(journalId = jid, startDate = fmt(t0),
                             endDate = fmt(t1), limit = limit) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_timeout(120) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_perform(),
      error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) >= 400) next
    items <- httr2::resp_body_json(resp)$items %||% list()
    if (length(items) == 0) next
    dev <- character(length(items)); vt <- character(length(items))
    for (k in seq_along(items)) {
      dev[k] <- as.character(items[[k]]$device_id  %||% NA_character_)
      vt[k]  <- as.character(items[[k]]$value$type %||% NA_character_)
    }
    out[[length(out) + 1]] <- tibble(device_id = dev, value_type = vt)
  }
  bind_rows(out) |>
    filter(!is.na(device_id)) |>
    group_by(device_id) |>
    summarise(api_value_type = .main_value_type(value_type), .groups = "drop")
}

# ---- API view ------------------------------------------------------------------
.review_api_inventory <- function(vessel_config, probe_efice, efice_offsets_h) {
  cfg    <- vessel_config |> filter(enabled)
  devs   <- list()
  status <- list()
  id_cols <- c("equipment_id", "device_name", "equipment_name", "device_type",
               "brand", "product_no", "serial_no", "complete_up_to")

  for (i in seq_len(nrow(cfg))) {
    src <- cfg$source[i]; jid <- cfg$journal_id[i]; vn <- cfg$vessel_name[i]

    if (src %in% c("pefa", "dbmatic")) {
      dev <- NULL
      for (attempt in 1:2) {            # one retry: the API is occasionally unreachable
        dev <- tryCatch(get_device_details(src, jid),
                        error = function(e) {
                          message("  inventory failed for ", vn, " (attempt ", attempt, "): ",
                                  conditionMessage(e)); NULL })
        if (!is.null(dev)) break
        if (attempt == 1) Sys.sleep(5)
      }
      if (is.null(dev) || nrow(dev) == 0 || !"device_id" %in% names(dev)) {
        status[[length(status) + 1]] <- tibble(source = src, vessel_name = vn,
                                               api_complete = FALSE,
                                               note = "inventory unavailable or empty")
        next
      }
      for (cn in id_cols) if (!cn %in% names(dev)) dev[[cn]] <- NA
      cu <- dev$complete_up_to
      if (!inherits(cu, "POSIXct"))
        cu <- suppressWarnings(lubridate::ymd_hms(.dev_chr(cu), quiet = TRUE))
      devs[[length(devs) + 1]] <- tibble(
        source = src, vessel_name = vn,
        device_id      = .dev_chr(dev$device_id),
        equipment_id   = .dev_chr(dev$equipment_id),
        device_name    = .dev_chr(dev$device_name),
        equipment_name = .dev_chr(dev$equipment_name),
        device_type    = .dev_chr(dev$device_type),
        brand          = .dev_chr(dev$brand),
        product_no     = .dev_chr(dev$product_no),
        serial_no      = .dev_chr(dev$serial_no),
        complete_up_to = cu,
        api_value_type = NA_character_) |>
        distinct(device_id, .keep_all = TRUE)
      status[[length(status) + 1]] <- tibble(source = src, vessel_name = vn,
                                             api_complete = TRUE, note = "inventory")

    } else if (src == "efice" && probe_efice) {
      seen <- .review_efice_probe(jid, efice_offsets_h)
      status[[length(status) + 1]] <- tibble(source = src, vessel_name = vn,
                                             api_complete = FALSE,
                                             note = "sample of recent entries")
      if (nrow(seen) > 0)
        devs[[length(devs) + 1]] <- seen |>
          transmute(source = src, vessel_name = vn, device_id,
                    equipment_id = NA_character_,
                    device_name = NA_character_, equipment_name = NA_character_,
                    device_type = NA_character_, brand = NA_character_,
                    product_no = NA_character_, serial_no = NA_character_,
                    complete_up_to = as.POSIXct(NA, tz = "UTC"), api_value_type)
    }
  }
  list(devices = bind_rows(devs), status = bind_rows(status))
}

# ---- disk view: from raw file names + cheap Arrow summaries ---------------------
.review_file_summary <- function(files) {
  pieces <- vector("list", length(files))
  for (k in seq_along(files)) {
    pieces[[k]] <- tryCatch(
      arrow::open_dataset(files[k]) |>
        group_by(value_type) |>
        summarise(first = min(datetime), last = max(datetime)) |>
        collect(),
      error = function(e) tibble())
  }
  bind_rows(pieces)
}

.review_disk_inventory <- function(vessel_config) {
  cfg <- vessel_config |> filter(enabled) |> distinct(source, vessel_name)
  out <- list()
  for (i in seq_len(nrow(cfg))) {
    root <- .raw_dir(cfg$source[i], cfg$vessel_name[i])
    if (!dir.exists(root)) next
    files <- list.files(root, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) next
    tab <- tibble(path      = files,
                  device_id = stringr::str_extract(basename(files), .uuid_rx),
                  ym        = stringr::str_extract(basename(files), "(?<=_)[0-9]{6}(?=[-.])")) |>
      filter(!is.na(device_id), !is.na(ym))

    for (d in unique(tab$device_id)) {
      t       <- filter(tab, device_id == d)
      f_last  <- t$path[t$ym == max(t$ym)]
      f_first <- t$path[t$ym == min(t$ym)]
      s_last  <- .review_file_summary(f_last)
      if (nrow(s_last) == 0) next
      s_first <- if (identical(f_first, f_last)) s_last else .review_file_summary(f_first)
      if (nrow(s_first) == 0) s_first <- s_last
      out[[length(out) + 1]] <- tibble(
        source      = cfg$source[i],
        vessel_name = cfg$vessel_name[i],
        device_id   = d,
        value_type  = .main_value_type(s_last$value_type),
        first_data  = min(s_first$first),
        last_data   = max(s_last$last))
    }
  }
  bind_rows(out)
}

# ---- ignore list and config update ------------------------------------------------

#' Add devices to the ignore list (config/poseidat_device_ignore.csv)
#' Ignored devices are never flagged by poseidat_review_devices().
poseidat_ignore_devices <- function(vessel_name, device_ids, reason = "") {
  path <- file.path(.onedrive(), "config", "poseidat_device_ignore.csv")
  new  <- tibble(vessel_name = vessel_name, device_id = device_ids,
                 reason = reason, date = as.character(Sys.Date()))
  old  <- if (file.exists(path))
    tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
  else tibble()
  out <- bind_rows(old, new) |> distinct(vessel_name, device_id, .keep_all = TRUE)
  utils::write.csv(out, path, row.names = FALSE)
  cat(sprintf("%d device(s) added to the ignore list (%d in total)\n", nrow(new), nrow(out)))
  invisible(out)
}

#' Add a new device_id to a vessel's device_labels under the SAME label as the
#' old one (the old id stays, so history keeps its label).
#' pairs: tibble with vessel_name, new_device_id, label
#' Nothing is written unless save = TRUE.
apply_device_renumbering <- function(vessel_config, pairs, save = FALSE) {
  for (k in seq_len(nrow(pairs))) {
    i <- which(vessel_config$vessel_name == pairs$vessel_name[k] & vessel_config$enabled)
    if (length(i) != 1) stop("Vessel not unique in config: ", pairs$vessel_name[k])
    lab <- vessel_config$device_labels[[i]]
    lab[[pairs$new_device_id[k]]] <- pairs$label[k]
    vessel_config$device_labels[[i]] <- lab
    cat(sprintf("%s: %s -> %s\n", pairs$vessel_name[k], pairs$new_device_id[k], pairs$label[k]))
  }
  if (save) {
    saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))
    cat("Config saved.\n")
  }
  invisible(vessel_config)
}

#' EFICE diagnostic: which devices deliver data right AFTER a device went silent?
#' The EFICE entries endpoint ignores sort direction (oldest first), so a broad
#' recent sample misses changes; small windows just after `after` are probed.
#' @param vessel_name  EFICE vessel
#' @param after        POSIXct, when the device went silent (its last record)
poseidat_probe_after_silence <- function(vessel_config, vessel_name, after,
                                         offsets_h = c(0, 6, 24, 72, 168),
                                         width_h   = 2,
                                         limit     = 1000L) {

  adp <- .get_adapter("efice")
  vn  <- vessel_name
  row <- vessel_config |> filter(enabled, source == "efice", vessel_name == vn)
  if (nrow(row) != 1) stop("Expected exactly one enabled EFICE vessel named ", vn)
  jid <- row$journal_id

  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out <- list()

  for (off in offsets_h) {
    t0 <- after + off * 3600
    t1 <- t0 + width_h * 3600
    cat(sprintf("%s: window %s (+%dh)\n", vn, fmt(t0), off))

    resp <- tryCatch(
      httr2::request(paste0(adp$base_url, "/journals/entries")) |>
        httr2::req_auth_bearer_token(adp$token) |>
        httr2::req_url_query(journalId = jid, startDate = fmt(t0),
                             endDate = fmt(t1), limit = limit) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_timeout(120) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_perform(),
      error = function(e) { message("  failed: ", conditionMessage(e)); NULL })

    if (is.null(resp) || httr2::resp_status(resp) >= 400) next
    items <- httr2::resp_body_json(resp)$items %||% list()
    if (length(items) == 0) next

    dev <- character(length(items)); vt <- character(length(items))
    for (k in seq_along(items)) {
      dev[k] <- as.character(items[[k]]$device_id  %||% NA_character_)
      vt[k]  <- as.character(items[[k]]$value$type %||% NA_character_)
    }
    out[[length(out) + 1]] <- tibble(offset_h = off, device_id = dev, value_type = vt)
  }

  seen <- bind_rows(out)
  if (nrow(seen) == 0) { cat("Nothing returned.\n"); return(invisible(tibble())) }

  lab   <- row$device_labels[[1]]
  known <- tibble(device_id = names(lab) %||% character(),
                  label     = as.character(unlist(lab, use.names = FALSE)) %||% character())

  res <- seen |>
    count(device_id, value_type, name = "n_entries") |>
    left_join(known, by = "device_id") |>
    mutate(in_config = !is.na(label)) |>
    arrange(in_config, value_type)

  print(res, n = Inf)
  invisible(res)
}

# ---- the review --------------------------------------------------------------------

#' Review sensors: API vs config vs disk. See the header of this section.
#'
#' @param silent_days    configured device silent longer than this (relative to
#'                       the vessel's own latest data) -> SILENT
#' @param recent_days    SILENT is only reported up to this many days (older
#'                       failures are known and chronic)
#' @param handover_days  max gap between an old device's last and a new device's
#'                       first record for a REPLACEMENT_CANDIDATE
#' @param probe_efice    sample recent EFICE entries for unknown devices
#' @param save           update the registry and write logs/poseidat_device_review_latest.csv
#' @return invisible list(flags, proposals, inventory, api_status)
poseidat_review_devices <- function(vessel_config,
                                    silent_days     = 7,
                                    recent_days     = 60,
                                    handover_days   = 14,
                                    probe_efice     = TRUE,
                                    efice_offsets_h = c(1, 7, 13, 19),
                                    save            = TRUE,
                                    now             = Sys.time()) {

  reg_path    <- file.path(.onedrive(), "config", "poseidat_device_registry.rds")
  ignore_path <- file.path(.onedrive(), "config", "poseidat_device_ignore.csv")
  today       <- as.Date(now)

  registry <- if (file.exists(reg_path)) readRDS(reg_path) else tibble()
  baseline <- nrow(registry) == 0
  ignore   <- if (file.exists(ignore_path))
    tibble::as_tibble(utils::read.csv(ignore_path, stringsAsFactors = FALSE))
  else tibble(vessel_name = character(), device_id = character())

  cfg_dev <- .review_cfg_devices(vessel_config)
  api     <- .review_api_inventory(vessel_config, probe_efice, efice_offsets_h)
  disk    <- .review_disk_inventory(vessel_config)

  if (nrow(api$devices) == 0)
    api$devices <- tibble(source = character(), vessel_name = character(),
                          device_id = character(), equipment_id = character(),
                          device_name = character(),
                          equipment_name = character(), device_type = character(),
                          brand = character(), product_no = character(), serial_no = character(),
                          complete_up_to = as.POSIXct(character()), api_value_type = character())
  if (nrow(disk) == 0)
    disk <- tibble(source = character(), vessel_name = character(), device_id = character(),
                   value_type = character(), first_data = as.POSIXct(character()),
                   last_data = as.POSIXct(character()))

  if (!baseline)
    for (cn in c("brand", "product_no", "serial_no"))
      if (!cn %in% names(registry)) registry[[cn]] <- NA_character_

  reg <- if (baseline)
    tibble(source = character(), vessel_name = character(), device_id = character(),
           reg_name = character(), reg_equipment = character(), reg_type = character(),
           reg_value_type = character(), reg_brand = character(), reg_product = character(),
           reg_serial = character(), first_seen = as.Date(character()),
           last_seen_api = as.Date(character()))
  else
    registry |>
      transmute(source, vessel_name, device_id, reg_name = device_name,
                reg_equipment = equipment_name, reg_type = device_type,
                reg_value_type = value_type, reg_brand = brand, reg_product = product_no,
                reg_serial = serial_no, first_seen, last_seen_api)

  by3 <- c("source", "vessel_name", "device_id")
  keys <- bind_rows(select(cfg_dev, all_of(by3)),
                    select(api$devices, all_of(by3)),
                    select(disk, all_of(by3))) |> distinct()

  cur <- keys |>
    left_join(cfg_dev, by = by3) |>
    left_join(api$devices |>
                transmute(source, vessel_name, device_id, in_api = TRUE,
                          api_name = device_name, api_equipment = equipment_name,
                          api_type = device_type, api_brand = brand, api_product = product_no,
                          api_serial = serial_no, api_last = complete_up_to, api_value_type),
              by = by3) |>
    left_join(api$devices |> filter(!is.na(equipment_id)) |>
                distinct(source, vessel_name, equipment_id) |>
                transmute(source, vessel_name, device_id = equipment_id, in_api_equip = TRUE),
              by = by3) |>
    left_join(disk, by = by3) |>
    left_join(reg, by = by3) |>
    left_join(api$status |> select(source, vessel_name, api_complete),
              by = c("source", "vessel_name")) |>
    mutate(
      in_config      = !is.na(label),
      # an id that matches an EQUIPMENT id of the inventory (not a device id) is a
      # config mistake: such a label is never synced (see CONFIG_ID_IS_EQUIPMENT)
      in_api_dev     = coalesce(in_api, FALSE),
      in_api         = in_api_dev | coalesce(in_api_equip, FALSE),
      api_complete   = coalesce(api_complete, FALSE),
      device_name    = coalesce(api_name, reg_name),
      equipment_name = coalesce(api_equipment, reg_equipment),
      device_type    = coalesce(api_type, reg_type),
      brand          = coalesce(api_brand, reg_brand),
      product_no     = coalesce(api_product, reg_product),
      serial_no      = coalesce(api_serial, reg_serial),
      value_type     = coalesce(value_type, api_value_type, reg_value_type)
    ) |>
    group_by(source, vessel_name) |>
    mutate(vessel_last = if (all(is.na(last_data))) as.POSIXct(NA, tz = "UTC")
                         else max(last_data, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      lag_d         = as.numeric(difftime(vessel_last, last_data, units = "days")),
      is_new        = !baseline & is.na(first_seen),
      delivering    = !is.na(lag_d) & lag_d <= silent_days,
      active        = !in_config & (in_api | delivering)
    ) |>
    # A label that another (renumbered / replacement) device already serves is
    # not a problem any more: the old id is superseded, not missing.
    group_by(source, vessel_name, label) |>
    mutate(label_alive = !is.na(label) & any(in_config & delivering)) |>
    ungroup() |>
    mutate(
      # not in the inventory AND no data: gone. Still delivering data although
      # absent from the inventory list: only informational (the list is not
      # always complete).
      gone_from_api    = in_config & api_complete & !in_api & !delivering & !label_alive,
      not_in_inventory = in_config & api_complete & !in_api & delivering,
      silent           = in_config & !is.na(lag_d) & lag_d > silent_days &
                         lag_d <= recent_days & !label_alive
    )

  # ---- flags ---------------------------------------------------------------------
  f_new <- cur |> filter(active, is_new) |>
    mutate(flag = "UNLABELLED_NEW", severity = "HIGH",
           detail = "new device, not in config")
  f_unl <- cur |> filter(active, !is_new) |>
    mutate(flag = "UNLABELLED_ACTIVE", severity = "INFO",
           detail = "active device without label")
  f_gone <- cur |> filter(gone_from_api) |>
    mutate(flag = "GONE_FROM_API", severity = "HIGH",
           detail = "configured device no longer in the API device list")
  f_notinv <- cur |> filter(not_in_inventory) |>
    mutate(flag = "NOT_IN_INVENTORY", severity = "INFO",
           detail = "delivers data but is missing from the API device list")
  f_sil <- cur |> filter(silent, !gone_from_api) |>
    mutate(flag = "SILENT", severity = "HIGH",
           detail = sprintf("no data for %.0f days while the vessel delivers", lag_d))
  f_eq <- cur |> filter(in_config, !in_api_dev, coalesce(in_api_equip, FALSE)) |>
    mutate(flag = "CONFIG_ID_IS_EQUIPMENT", severity = "HIGH",
           detail = "configured id is an equipment id, not a device id: this label is never synced")
  f_nodata <- cur |> filter(in_config, is.na(last_data), !gone_from_api,
                            !(!in_api_dev & coalesce(in_api_equip, FALSE))) |>
    mutate(flag = "CONFIGURED_NO_DATA", severity = "INFO",
           detail = "labelled device without data on disk")
  f_name <- cur |>
    filter(!is.na(api_name), !is.na(reg_name),
           (api_name != reg_name) |
             (!is.na(api_equipment) & !is.na(reg_equipment) & api_equipment != reg_equipment)) |>
    mutate(flag = "NAME_CHANGED", severity = "INFO",
           detail = sprintf("was '%s'", reg_name))
  f_hw <- cur |>
    filter(in_api,
           (!is.na(.valid_serial(reg_serial)) & !is.na(.valid_serial(api_serial)) &
              reg_serial != api_serial) |
           (!is.na(reg_product) & !is.na(api_product) & reg_product != api_product) |
           (!is.na(reg_brand)   & !is.na(api_brand)   & reg_brand   != api_brand)) |>
    mutate(flag = "HARDWARE_CHANGED", severity = "HIGH",
           detail = sprintf("same device_id, other hardware (serial %s -> %s, product %s -> %s)",
                            coalesce(reg_serial, "?"), coalesce(api_serial, "?"),
                            coalesce(reg_product, "?"), coalesce(api_product, "?")))
  f_api <- api$status |> filter(!api_complete, note != "sample of recent entries") |>
    mutate(device_id = NA_character_, flag = "API_UNAVAILABLE", severity = "INFO",
           detail = note)

  # ---- replacement candidates --------------------------------------------------------
  old <- cur |> filter(in_config, gone_from_api | silent) |>
    transmute(source, vessel_name, old_device_id = device_id, old_label = label,
              old_value_type = value_type, old_equipment = equipment_name,
              old_type = device_type, old_serial = .valid_serial(serial_no), old_product = product_no,
              old_last = last_data)
  new <- cur |> filter(active) |>
    transmute(source, vessel_name, new_device_id = device_id, new_name = device_name,
              new_value_type = value_type, new_equipment = equipment_name,
              new_type = device_type, new_serial = .valid_serial(serial_no), new_product = product_no,
              new_first = first_data)

  proposals <- old |>
    inner_join(new, by = c("source", "vessel_name"), relationship = "many-to-many") |>
    mutate(
      gap_days     = as.numeric(difftime(new_first, old_last, units = "days")),
      same_serial  = !is.na(old_serial) & !is.na(new_serial) & old_serial == new_serial,
      same_product = !is.na(old_product) & !is.na(new_product) & old_product == new_product,
      same_value   = !is.na(old_value_type) & !is.na(new_value_type) & old_value_type == new_value_type,
      same_equip   = !is.na(old_type) & !is.na(new_type) & old_type == new_type &
                     !is.na(old_equipment) & !is.na(new_equipment) & old_equipment == new_equipment,
      gap_ok       = is.na(gap_days) | abs(gap_days) <= handover_days
    ) |>
    filter(same_serial | same_value | same_equip, gap_ok) |>
    mutate(score = 3 * same_serial + 2 * same_value + same_equip + same_product -
                   0.01 * abs(coalesce(gap_days, 0))) |>
    arrange(vessel_name, old_label, desc(score)) |>
    select(vessel_name, source, old_label, old_device_id, new_device_id, new_name,
           value_type = new_value_type, old_last, new_first, gap_days,
           same_serial, same_product, same_value, same_equip, score)

  f_rep <- proposals |>
    transmute(source, vessel_name, device_id = old_device_id, label = old_label,
              value_type, flag = "REPLACEMENT_CANDIDATE", severity = "HIGH",
              detail = sprintf("possibly replaced by %s (%s), gap %s days%s",
                               coalesce(new_name, "?"), substr(new_device_id, nchar(new_device_id) - 11, nchar(new_device_id)),
                               ifelse(is.na(gap_days), "?", sprintf("%.1f", gap_days)),
                               ifelse(same_serial, ", SAME SERIAL", "")))

  keep_cols <- c("source", "vessel_name", "device_id", "label", "device_name",
                 "equipment_name", "device_type", "brand", "product_no", "serial_no",
                 "value_type", "last_data", "lag_d", "flag", "severity", "detail")
  flags <- bind_rows(f_new, f_unl, f_gone, f_notinv, f_sil, f_eq, f_nodata, f_name, f_hw, f_api, f_rep) |>
    select(any_of(keep_cols)) |>
    anti_join(ignore |> select(any_of(c("vessel_name", "device_id"))),
              by = c("vessel_name", "device_id")) |>
    arrange(match(severity, c("HIGH", "INFO")), vessel_name, flag)

  # ---- registry update (after the flags are computed) ---------------------------------
  new_reg <- cur |>
    filter(in_api | !is.na(last_data) | in_config) |>
    transmute(source, vessel_name, device_id, device_name, equipment_name, device_type,
              brand, product_no, serial_no, value_type,
              first_seen = coalesce(first_seen, today),
              last_seen_api = if_else(in_api, today, last_seen_api))
  if (!baseline) {
    old_only <- registry |> anti_join(new_reg, by = by3)
    new_reg  <- bind_rows(new_reg, old_only)
  }

  if (save) {
    saveRDS(new_reg, reg_path)
    log_dir <- file.path(.onedrive(), "logs")
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    .write_csv_excel(flags, file.path(log_dir, "poseidat_device_review_latest.csv"))
  }

  # ---- report ---------------------------------------------------------------------------
  n_high <- sum(flags$severity == "HIGH")
  n_info <- sum(flags$severity == "INFO")
  if (baseline) cat("Baseline run: registry created, nothing is reported as NEW yet.\n")
  cat(sprintf("\nDevice review: %d HIGH flag(s), %d INFO flag(s)\n", n_high, n_info))

  high <- flags |> filter(severity == "HIGH")
  if (nrow(high) > 0) {
    print(high |> select(vessel_name, flag, label, device_name, value_type, detail),
          n = Inf, width = Inf)
    for (k in seq_len(nrow(high)))
      log_poseidat(sprintf("DEVICE REVIEW %s  %s  %s  %s", high$flag[k], high$vessel_name[k],
                           coalesce(high$label[k], high$device_name[k], high$device_id[k]),
                           high$detail[k]), level = "WARNING")
  } else {
    log_poseidat("Device review: no HIGH flags")
  }
  if (n_info > 0)
    cat(sprintf("  %d INFO flag(s): see logs/poseidat_device_review_latest.csv\n", n_info))

  invisible(list(flags = flags, proposals = proposals, inventory = cur, api_status = api$status))
}


# ---- example usage ----------------------------------------------------------------------
if (FALSE) {

  vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

  # Daily fuel per device, from 1-min thinned data
  fuel_obs <- poseidat_fuel_obs(vessel_config,
                                data_source = "thinned",
                                start_date  = as.Date("2026-08-01"),
                                end_date    = Sys.Date() - 1)

  daily_device <- poseidat_fuel_daily(fuel_obs, flow_unit = "L_per_h",
                                      max_gap_min = 5, tz = "Europe/Amsterdam")
  daily_vessel <- poseidat_fuel_daily_vessel(daily_device)
  # daily_vessel <- poseidat_fuel_daily_vessel(daily_device,
  #                                            return_devices = "<device_id>")

  poseidat_plot_fuel_daily(daily_device, by_device = TRUE, min_coverage = 50)
  poseidat_plot_fuel_daily(daily_vessel)

  # Wide table: one row per day, one column per vessel
  daily_vessel |>
    dplyr::select(date, vessel_name, fuel_L) |>
    tidyr::pivot_wider(names_from = vessel_name, values_from = fuel_L) |>
    dplyr::arrange(date)

  # Which thinned files deliver fuel data for which days?
  poseidat_check_fuel_files("Lub Senior", start_date = as.Date("2026-09-01")) |>
    dplyr::group_by(file) |>
    dplyr::summarise(first = min(date), last = max(date), rows = sum(n_rows))

  # Staleness check (also runs daily from Poseidat_sync_daily.R)
  st <- poseidat_check_staleness(vessel_config)
}
