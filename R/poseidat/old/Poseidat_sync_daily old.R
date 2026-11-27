# ============================================================================
# Poseidat_sync_daily.R
# Daily runner — scheduled via Windows Task Scheduler or run manually.
# Sources Poseidat_functions.R which contains all sync and overview functions.
#
# .Renviron entries needed (usethis::edit_r_environ()):
#   DBMATIC_API_URL   = https://api.dbmatic.eu/api/v1
#   DBMATIC_TOKEN     = your-dbmatic-bearer-token
#   EFICE_API_URL     = https://...            <- fill once known
#   EFICE_TOKEN       = your-efice-bearer-token
#   PEFA_API_URL      = https://...            <- fill once known
#   PEFA_TOKEN        = your-pefa-bearer-token
#   ONEDRIVE_POSEIDAT = C:/Users/<you>/OneDrive/Poseidat
# ============================================================================

library(dplyr)
library(purrr)
library(arrow)
library(httr2)
library(glue)
library(lubridate)

# Record start time for elapsed time reporting
sync_start_time <- Sys.time()

# Adjust this path to wherever Poseidat_functions.R lives in your project
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
# 3. Sync
#
# Sync EFICE and PEFA (both production Poseidat sensor data) in the daily run.
# To also sync DBMatic test data, add "dbmatic" to the sources vector.
# To sync only one source, e.g. sources = "pefa".
# ----------------------------------------------------------------------------
results <- sync_all_vessels(
  vessel_config       = vessel_config,
  vessel_names        = NULL,
  sources             = c("efice", "pefa"),  # change to NULL for all three sources
  lookback_days       = 1,                   # daily run only needs a small window
  force_full          = FALSE,               # never TRUE in daily runner
  page_size           = 5000,               # faster downloads
  sleep_between_pages = 0                   # no throttling unless server complains
)

# ----------------------------------------------------------------------------
# 4. Weekly consolidation — every Monday
# ----------------------------------------------------------------------------
# if (wday(Sys.Date(), label = FALSE) == 2) {
#
#   cat("\n=== Weekly consolidation ===\n")
#
#   active_vessels <- vessel_config |>
#     filter(enabled, source %in% c("efice", "pefa"))
#
#   for (i in seq_len(nrow(active_vessels))) {
#     vessel <- active_vessels[i, ]
#
#     raw_root <- file.path(.onedrive(),
#                           "raw", vessel$source, vessel$vessel_name)
#     if (!dir.exists(raw_root)) next
#
#     raw_files <- list.files(raw_root, pattern = "\\.parquet$",
#                             recursive = TRUE, full.names = FALSE)
#
#     if (length(raw_files) == 0) next
#
#     safe_vessel <- gsub("[^a-zA-Z0-9_-]", "_", vessel$vessel_name)
#     device_ids  <- unique(sub(
#       pattern     = paste0("^", safe_vessel, "_([^_]+(?:_[^_]+)*)_\\d{8}_to.*$"),
#       replacement = "\\1",
#       x           = basename(raw_files)
#     ))
#     device_ids <- device_ids[nchar(device_ids) > 0]
#
#     for (dev_id in device_ids) {
#       consolidate_data(
#         source              = vessel$source,
#         vessel_name         = vessel$vessel_name,
#         device_id           = dev_id,
#         keep_daily_for_days = 30
#       )
#     }
#   }
# }

# ----------------------------------------------------------------------------
# 5. Print state summary
# ----------------------------------------------------------------------------
print_sync_state()

cat(sprintf("\n=== Poseidat sync completed: %s ===\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ----------------------------------------------------------------------------
# 6. Update poseidat_thin
# ----------------------------------------------------------------------------
cat("\n=== Updating poseidat_thin ===\n")

local({
  # Poseidat_functions.R already sourced above — poseidat_thin() available
  poseidat_thin(save = TRUE)
})

cat(sprintf("=== poseidat_thin updated: %s ===\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ----------------------------------------------------------------------------
# 7. Console summary
# ----------------------------------------------------------------------------
sync_end_time <- Sys.time()
elapsed       <- as.numeric(difftime(sync_end_time, sync_start_time, units = "mins"))

cat("\n")
cat("========================================================\n")
cat("           POSEIDAT SYNC SUMMARY\n")
cat("========================================================\n")

# Records downloaded this run
downloaded <- results |>
  purrr::keep(~ nrow(.) > 0) |>
  purrr::map_dfr(~ tibble::tibble(n = nrow(.)), .id = "key") |>
  tidyr::separate(key, into = c("source", "journal_id", "device_id"),
                  sep = "__", extra = "merge") |>
  dplyr::left_join(
    vessel_config |> select(journal_id, vessel_name),
    by = "journal_id"
  ) |>
  dplyr::summarise(vessels = n_distinct(journal_id),
                   devices = n(),
                   records = sum(n))

cat(sprintf("  Downloaded : %s records across %d device(s) on %d vessel(s)\n",
            format(downloaded$records, big.mark = ","),
            downloaded$devices,
            downloaded$vessels))

# Most recent observation per vessel this run
cat("\n  Most recent observation per vessel (this run):\n")
vessel_summary <-
  results |>
  purrr::keep(~ nrow(.) > 0) |>
  purrr::map_dfr(~ tibble::tibble(last = max(.$datetime, na.rm = TRUE),
                                  n    = nrow(.)),
                 .id = "key") |>
  tidyr::separate(key, into = c("source", "journal_id", "device_id"),
                  sep = "__", extra = "merge") |>
  dplyr::left_join(vessel_config |> select(journal_id, vessel_name),
                   by = "journal_id") |>
  dplyr::group_by(vessel_name) |>
  dplyr::summarise(last_obs = max(last, na.rm = TRUE),
                   records  = sum(n),
                   .groups  = "drop") |>
  dplyr::arrange(vessel_name)

for (i in seq_len(nrow(vessel_summary))) {
  cat(sprintf("    %-20s  %s  (%s records)\n",
              vessel_summary$vessel_name[i],
              format(vessel_summary$last_obs[i], "%Y-%m-%d %H:%M:%S"),
              format(vessel_summary$records[i], big.mark = ",")))
}

# Total records available on disk
total <- arrow::open_dataset(
  file.path(.onedrive(), "raw")
) |>
  dplyr::count() |>
  dplyr::collect()

cat(sprintf("\n  Total on disk : %s records\n", format(total$n, big.mark = ",")))
cat(sprintf("  Started       : %s\n", format(sync_start_time, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Completed     : %s\n", format(sync_end_time,   "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Elapsed       : %.1f minutes\n", elapsed))
cat("========================================================\n\n")
