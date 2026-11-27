# ============================================================================
# poseidat_efice_explore.R
#
# Interactive exploration of what the efice API returns for vessels,
# devices and measurements. Run section by section.
# ============================================================================

library(dplyr)
library(httr2)
library(glue)
library(lubridate)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

config_dir    <- file.path(.onedrive(), "config")
vessel_config <- readRDS(file.path(config_dir, "poseidat_vessels.rds"))

efice_url   <- Sys.getenv("EFICE_API_URL")
efice_token <- Sys.getenv("EFICE_TOKEN")

# Pick one vessel to test with — change vessel_name to try others
test_vessel     <- vessel_config |> dplyr::filter(vessel_name == "Lub Senior")
test_journal_id <- test_vessel$journal_id
cat(glue("Testing with : {test_vessel$vessel_name}  ({test_journal_id})\n\n"))

# ============================================================================
# SECTION 1 — What vessels does the API know about?
# ============================================================================
cat("── SECTION 1: Vessel list ──────────────────────────────────────────────\n")

vessels_raw <- .http_get(efice_url, efice_token, "journals")
cat(glue("Top-level keys : {paste(names(vessels_raw), collapse = ', ')}\n"))
cat(glue("Total vessels  : {vessels_raw$total %||% length(vessels_raw$items)}\n\n"))

vessels <- get_vessels("efice")
print(vessels)

# ============================================================================
# SECTION 2 — What does a recent sample of entries look like?
#             (This is the only way to discover device IDs in efice)
# ============================================================================
cat("\n── SECTION 2: Recent entries sample ───────────────────────────────────\n")

fmt_dt <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

sample <- .http_get(efice_url, efice_token, "journals/entries",
                    params = list(
                      journalId     = test_journal_id,
                      startDate     = fmt_dt(Sys.time() - 7 * 86400),
                      endDate       = fmt_dt(Sys.time()),
                      limit         = 5L,
                      skip          = 0L,
                      sortBy        = "entry_datetime",
                      sortDirection = "descending"
                    ),
                    timeout_sec = 60)

cat(glue("Total entries in last 7 days : {sample$total %||% 'unknown'}\n"))
cat(glue("Items returned               : {length(sample$items %||% list())}\n\n"))

if (length(sample$items) > 0) {
  first <- sample$items[[1]]
  cat("Keys in one entry:\n")
  cat(glue("  {paste(names(first), collapse = ', ')}\n\n"))

  # Show device_id, datetime and value structure for first 5 items
  entry_overview <- sample$items |>
    lapply(\(x) tibble::tibble(
      device_id      = as.character(x$device_id      %||% NA),
      entry_datetime = as.character(x$entry_datetime %||% NA),
      value_keys     = paste(names(x$value %||% list()), collapse = ", ")
    )) |>
    dplyr::bind_rows()

  print(entry_overview)
}

# ============================================================================
# SECTION 3 — What device IDs are known for this vessel?
#             Uses vessel_config (reliable) rather than probing
# ============================================================================
cat("\n── SECTION 3: Known device IDs from vessel_config ─────────────────────\n")

device_labels <- test_vessel$device_labels[[1]]

if (is.null(device_labels) || length(device_labels) == 0) {
  cat("No device_labels found in vessel_config for this vessel.\n")
  cat("Run audit_new_sensors() or inspect entries manually (Section 2).\n")
} else {
  known_devices <- tibble::tibble(
    device_id = names(device_labels),
    label     = unname(device_labels)
  )
  cat(glue("{nrow(known_devices)} device(s) configured:\n\n"))
  print(known_devices)
}

# ============================================================================
# SECTION 4 — For one known device: how many records exist and what is
#             the date range?
# ============================================================================
cat("\n── SECTION 4: Record count and date range for one device ───────────────\n")

if (exists("known_devices") && nrow(known_devices) > 0) {

  test_device_id    <- known_devices$device_id[[1]]
  test_device_label <- known_devices$label[[1]]
  cat(glue("Device : {test_device_label}  ({test_device_id})\n\n"))

  # Total count across all time
  count_all <- .http_get(efice_url, efice_token, "journals/entries",
                         params = list(
                           journalId = test_journal_id,
                           deviceId  = test_device_id,
                           startDate = "2020-01-01T00:00:00Z",
                           endDate   = fmt_dt(Sys.time()),
                           limit     = 1L,
                           skip      = 0L
                         ),
                         timeout_sec = 60)
  cat(glue("Total records (all time) : {count_all$total %||% 'unknown'}\n"))

  # Most recent record
  latest <- .http_get(efice_url, efice_token, "journals/entries",
                      params = list(
                        journalId     = test_journal_id,
                        deviceId      = test_device_id,
                        startDate     = "2020-01-01T00:00:00Z",
                        endDate       = fmt_dt(Sys.time()),
                        limit         = 1L,
                        skip          = 0L,
                        sortBy        = "entry_datetime",
                        sortDirection = "descending"
                      ),
                      timeout_sec = 60)

  if (length(latest$items) > 0) {
    cat(glue("Most recent record       : {latest$items[[1]]$entry_datetime}\n"))
  }

  # Oldest record
  oldest <- .http_get(efice_url, efice_token, "journals/entries",
                      params = list(
                        journalId     = test_journal_id,
                        deviceId      = test_device_id,
                        startDate     = "2020-01-01T00:00:00Z",
                        endDate       = fmt_dt(Sys.time()),
                        limit         = 1L,
                        skip          = 0L,
                        sortBy        = "entry_datetime",
                        sortDirection = "ascending"
                      ),
                      timeout_sec = 60)

  if (length(oldest$items) > 0) {
    cat(glue("Oldest record            : {oldest$items[[1]]$entry_datetime}\n\n"))
  }
}

# ============================================================================
# SECTION 5 — Download a small sample of actual measurements for inspection
# ============================================================================
cat("\n── SECTION 5: Small measurement download (last 24 hours) ───────────────\n")

if (exists("test_device_id")) {

  t_start <- Sys.time()

  meas <- download_measurements(
    source     = "efice",
    journal_id = test_journal_id,
    device_id  = test_device_id,
    start_time = Sys.time() - 86400,
    end_time   = Sys.time(),
    page_size  = 100
  )

  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 1)
  cat(glue("Downloaded {nrow(meas)} record(s) in {elapsed}s\n\n"))

  if (nrow(meas) > 0) {
    cat("Column types:\n")
    dplyr::glimpse(meas)

    cat("\nFirst 5 rows:\n")
    meas |>
      dplyr::arrange(datetime) |>
      dplyr::slice_head(n = 5) |>
      print()

    cat("\nValue types present:\n")
    meas |>
      dplyr::count(value_type, sort = TRUE) |>
      print()
  }
}

# ============================================================================
# SECTION 6 — Quick record count per device from parquet (no API needed)
# ============================================================================
cat("\n── SECTION 6: Records on disk per device ───────────────────────────────\n")

tryCatch({
  arrow::open_dataset(file.path(.onedrive(), "raw", "efice")) |>
    dplyr::filter(vessel_name == test_vessel$vessel_name) |>
    dplyr::group_by(device_id) |>
    dplyr::summarise(
      records     = dplyr::n(),
      first_obs   = min(datetime, na.rm = TRUE),
      last_obs    = max(datetime, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      first_obs = as.POSIXct(first_obs, tz = "UTC"),
      last_obs  = as.POSIXct(last_obs,  tz = "UTC"),
      days_since_last = round(as.numeric(difftime(Sys.time(), last_obs,
                                                  units = "days")), 1)
    ) |>
    dplyr::left_join(
      tibble::tibble(device_id = names(device_labels),
                     label     = unname(device_labels)),
      by = "device_id"
    ) |>
    dplyr::arrange(device_id) |>
    print()
}, error = function(e) {
  cat(glue("Could not read parquet: {conditionMessage(e)}\n"))
})
