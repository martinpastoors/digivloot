# ============================================================================
# Poseidat_sync_daily.R
# Daily runner — scheduled via Windows Task Scheduler or run manually.
# Sources Poseidat_functions.R which contains all sync and overview functions.
#
# Vessel-isolated: each vessel is synced and thinned in turn, so a slow or
# failing vessel/device cannot block thinning for vessels already completed.
#
# .Renviron entries needed (usethis::edit_r_environ()):
#   DBMATIC_API_URL   = https://api.dbmatic.eu/api/v1
#   DBMATIC_TOKEN     = your-dbmatic-bearer-token
#   EFICE_API_URL     = https://...
#   EFICE_TOKEN       = your-efice-bearer-token
#   PEFA_API_URL      = https://...
#   PEFA_TOKEN        = your-pefa-bearer-token
#   ONEDRIVE_POSEIDAT = C:/Users/<you>/OneDrive/Poseidat
# ============================================================================

library(dplyr)
library(purrr)
library(arrow)
library(httr2)
library(glue)
library(lubridate)
library(ggplot2)

sync_start_time <- Sys.time()

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

# ----------------------------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------------------------
setup_poseidat_sync()

# ----------------------------------------------------------------------------
# 2. Load vessel config
# ----------------------------------------------------------------------------
config_dir    <- file.path(.onedrive(), "config")
vessel_config <- readRDS(file.path(config_dir, "poseidat_vessels.rds"))

# ----------------------------------------------------------------------------
# 3. Sync + thin — vessel by vessel, isolated
# ----------------------------------------------------------------------------

full_thin <- FALSE   # TRUE = rebuild thinned data from ALL raw months (slow)

run <- poseidat_daily_sync_all(
  vessel_config       = vessel_config,
  sources             = c("efice", "pefa", "dbmatic"),
  # sources             = c("pefa"),
  page_size           = 5000,
  exclude_value_types = "TEMPERATURE",
  lookback_days       = 30,
  full_thin           = full_thin,
  max_gap_days        = 3      # silent sensors: only ask the last 3 days, not the whole gap
)

results <- run$sync_results
timings <- run$timings

# ----------------------------------------------------------------------------
# 3a. Device review — sensors on the API vs the config vs the raw data on disk
#     HIGH flags (new / vanished / silent / replaced / hardware-changed sensor)
#     go to the log as WARNING; all flags to logs/poseidat_device_review_latest.csv.
#     Never changes the config. See poseidat_review_devices() in
#     Poseidat_functions.R (SECTION K).
# ----------------------------------------------------------------------------
cat("\n=== Device review ===\n")

device_review <- tryCatch(
  poseidat_review_devices(vessel_config),
  error = function(e) {
    log_poseidat(sprintf("Device review failed: %s", conditionMessage(e)),
                 level = "ERROR")
    NULL
  }
)

# ----------------------------------------------------------------------------
# 3b. Update trip identification — incremental, not the whole time series
#     Only reprocesses the last `lookback_days` per vessel (plus the full
#     span of any trip still open from a previous run); everything before
#     that stays exactly as previously identified. Writes trip_id/trip_uid
#     onto every vessel's thinned Parquet file and updates
#     processed/trips/poseidat_trips.parquet, keeping 2 rotating backups
#     of that file. See poseidat_update_trip_ids() in Poseidat_functions.R.
# ----------------------------------------------------------------------------
# cat("\n=== Updating trip identification ===\n")

trip_update <- tryCatch(
  poseidat_update_trip_ids(
    vessel_name   = NULL,     # all vessels with thinned data
    lookback_days = 30,       # matches the raw-sync lookback above
    interval_min  = 1,        # must match poseidat_daily_sync_all()'s thinning interval
    n_backups     = 2,
    dry_run       = FALSE
  ),
  error = function(e) {
    log_poseidat(sprintf("Trip identification update failed: %s", conditionMessage(e)),
                level = "ERROR")
    NULL
  }
)

# ----------------------------------------------------------------------------
# 3c. Staleness check — which vessels / sensor types stopped delivering data?
#     SENSOR STALE  : type lags > 48 h behind the vessel's own latest data
#     VESSEL SILENT : no data at all from the vessel for > 7 days
#     NO DATA       : vessel in config without thinned data (last 120 days)
#     Full table: logs/poseidat_staleness_latest.csv
# ----------------------------------------------------------------------------
cat("\n=== Staleness check ===\n")

staleness <- tryCatch(
  poseidat_check_staleness(
    vessel_config       = vessel_config,
    sensor_gap_hours    = 48,
    vessel_silent_days  = 7,
    lookback_days       = 120,
    exclude_value_types = "TEMPERATURE"
  ),
  error = function(e) {
    log_poseidat(sprintf("Staleness check failed: %s", conditionMessage(e)),
                 level = "ERROR")
    NULL
  }
)


# ----------------------------------------------------------------------------
# 4. Generate & save the availability plot
#    Saved to outputs/figures/poseidat_availability_YYYYMMDD.png
#    (keeps the 14 most recent files — see poseidat_save_availability_plot())
# ----------------------------------------------------------------------------
# cat("\n=== Generating availability plot ===\n")

plot_path <- poseidat_save_availability_plot()

if (!is.null(plot_path)) {
  cat(sprintf("  Saved -> %s\n", plot_path))
} else {
  cat("  Availability plot was not saved — see log for the error\n")
}

# ----------------------------------------------------------------------------
# 5. Print state summary
# ----------------------------------------------------------------------------
print_sync_state()

cat(sprintf("\n=== Poseidat sync completed: %s ===\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ----------------------------------------------------------------------------
# 6. Console summary
# ----------------------------------------------------------------------------
sync_end_time <- Sys.time()
elapsed       <- as.numeric(difftime(sync_end_time, sync_start_time, units = "mins"))

cat("\n========================================================\n")
cat("           POSEIDAT SYNC SUMMARY\n")
cat("========================================================\n")

result_keys <- names(results)[purrr::map_lgl(results, ~ nrow(.x) > 0)]

if (length(result_keys) == 0) {

  cat("  Downloaded : 0 records (nothing new since last sync)\n")

} else {

  run_data <- tibble::tibble(key = result_keys) |>
    tidyr::separate(key, into = c("source", "journal_id", "device_id"),
                    sep = "__", extra = "merge") |>
    dplyr::mutate(
      n    = purrr::map_int(results[result_keys], nrow),
      last = purrr::map_dbl(results[result_keys],
                           ~ max(.x$datetime, na.rm = TRUE)) |>
             as.POSIXct(origin = "1970-01-01", tz = "UTC")
    ) |>
    dplyr::left_join(vessel_config |> dplyr::select(journal_id, vessel_name),
                     by = "journal_id")

  downloaded <- run_data |>
    dplyr::summarise(vessels = dplyr::n_distinct(journal_id),
                     devices = dplyr::n(), records = sum(n))

  cat(sprintf("  Downloaded : %s records across %d device(s) on %d vessel(s)\n",
              format(downloaded$records, big.mark = ","),
              downloaded$devices, downloaded$vessels))

  cat("\n  Most recent observation per vessel (this run):\n")
  vessel_summary <- run_data |>
    dplyr::group_by(vessel_name) |>
    dplyr::summarise(last_obs = max(last, na.rm = TRUE), records = sum(n),
                     .groups = "drop") |>
    dplyr::arrange(vessel_name)

  for (i in seq_len(nrow(vessel_summary))) {
    cat(sprintf("    %-20s  %s  (%s records)\n",
                vessel_summary$vessel_name[i],
                format(vessel_summary$last_obs[i], "%Y-%m-%d %H:%M:%S"),
                format(vessel_summary$records[i], big.mark = ",")))
  }
}

cat("\n  Sync/thin time per vessel:\n")
timings |>
  tidyr::pivot_wider(names_from = step, values_from = seconds) |>
  dplyr::mutate(total = sync + thin) |>
  print(n = Inf)

# Staleness warnings (details in section 3c output / staleness CSV)
if (!is.null(staleness)) {
  n_warn <- sum(staleness$status != "ok")
  cat(sprintf("\n  Staleness     : %s\n",
              if (n_warn == 0) "all vessels and sensor types up to date"
              else sprintf("%d WARNING(S) — see staleness check above", n_warn)))
}

# Device review flags (details in section 3a output / device review CSV)
if (!is.null(device_review)) {
  n_high <- sum(device_review$flags$severity == "HIGH")
  cat(sprintf("\n  Device review : %s\n",
              if (n_high == 0) "no sensor changes to act on"
              else sprintf("%d HIGH FLAG(S) — see device review above", n_high)))
}

# total <- arrow::open_dataset(file.path(.onedrive(), "raw")) |>
#   dplyr::count() |> dplyr::collect()

# cat(sprintf("\n  Total on disk : %s records\n", format(total$n, big.mark = ",")))
cat(sprintf("  Started       : %s\n", format(sync_start_time, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Completed     : %s\n", format(sync_end_time,   "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Elapsed       : %.1f minutes\n", elapsed))
cat("========================================================\n\n")



# Daily data availability calendar heatmap (all vessels x value_types)
poseidat_availability(start_date = dmy("1/10/2026"))


# ── Operational overview: fuel, speed, traction, depth ──────────────────────
# All vessels, thinned data (default)
# poseidat_plot_operational()
poseidat_plot_operational(
  variables = "fuel", 
  vessel="Anna Hendrika", 
  start_date = Sys.time() - lubridate::days(14))

