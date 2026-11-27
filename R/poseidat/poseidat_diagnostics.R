# ============================================================================
# poseidat_diagnostics.R
#
# Sandbox diagnostic script to troubleshoot failed Poseidat syncs.
# Run this interactively — it checks credentials, connectivity, and the
# device discovery endpoint for every configured vessel.
#
# Prerequisites: same libraries and .Renviron as Poseidat_sync_daily.R
# ============================================================================

library(dplyr)
library(httr2)
library(glue)
library(lubridate)
library(arrow)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

# ── helpers ─────────────────────────────────────────────────────────────────

.ok  <- function(msg) cat(glue("  \u2713 {msg}\n\n"))
.warn <- function(msg) cat(glue("  \u26a0 {msg}\n\n"))
.err <- function(msg) cat(glue("  \u2717 {msg}\n\n"))
.section <- function(title) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(glue(" {title}\n"))
  cat(strrep("=", 70), "\n")
}

# ============================================================================
# CHECK 1 — Environment variables
# ============================================================================
.section("CHECK 1 — Environment variables")

env_vars <- tibble::tribble(
  ~var,               ~needed_for,
  "EFICE_API_URL",    "efice source",
  "EFICE_TOKEN",      "efice source",
  "PEFA_API_URL",     "pefa source",
  "PEFA_TOKEN",       "pefa source",
  "DBMATIC_API_URL",  "dbmatic source",
  "DBMATIC_TOKEN",    "dbmatic source"
)

env_check <- env_vars |>
  dplyr::mutate(
    value = purrr::map_chr(var, Sys.getenv),
    set   = nchar(value) > 0
  )

env_check |>
  dplyr::rowwise() |>
  dplyr::group_walk(~ {
    if (.x$set) {
      # Show only first 6 chars of token for safety
      preview <- substr(.x$value, 1, 6)
      .ok(glue("{.x$var} is set  [{preview}...]  — needed for {.x$needed_for}"))
    } else {
      .warn(glue("{.x$var} NOT set — needed for {.x$needed_for}"))
    }
  })

# Identify which sources are usable
usable_sources <- env_check |>
  dplyr::mutate(source = stringr::str_extract(var, "^[A-Z]+") |> tolower()) |>
  dplyr::group_by(source) |>
  dplyr::summarise(all_set = all(set), .groups = "drop") |>
  dplyr::filter(all_set) |>
  dplyr::pull(source)

cat(glue("  Sources with complete credentials: {paste(usable_sources, collapse = ', ')}\n\n"))

# ============================================================================
# CHECK 2 — Basic network reachability (DNS + TCP)
# ============================================================================
.section("CHECK 2 — Network reachability")

# Extract hostnames from the API URL env vars
api_urls <- env_check |>
  dplyr::filter(set, stringr::str_ends(var, "_URL")) |>
  dplyr::select(var, value) |>
  dplyr::mutate(
    host = stringr::str_extract(value, "(?<=https?://)([^/]+)")
  ) |>
  dplyr::filter(!is.na(host))

if (nrow(api_urls) == 0) {
  .warn("No API URL env vars set — skipping network reachability checks")
} else {
  api_urls |>
    dplyr::rowwise() |>
    dplyr::group_walk(~ {
      host <- .x$host
      result <- tryCatch(
        { nslookup <- system2("ping", args = c("-n", "1", "-w", "3000", host),
                              stdout = TRUE, stderr = TRUE)
          if (any(stringr::str_detect(nslookup, "Reply from|bytes="))) "ok" else "timeout"
        },
        error = function(e) "error"
      )
      if (result == "ok") {
        .ok(glue("{host} is reachable"))
      } else {
        .err(glue("{host} — ping failed ({result}). Check VPN / firewall."))
      }
    })
}

# ============================================================================
# CHECK 3 — efice token validity (unauthenticated probe)
# ============================================================================
.section("CHECK 3 — efice API token validity")

efice_url   <- Sys.getenv("EFICE_API_URL")
efice_token <- Sys.getenv("EFICE_TOKEN")

if (nchar(efice_url) == 0 || nchar(efice_token) == 0) {

  .warn("EFICE_API_URL or EFICE_TOKEN not set — skipping token check")

} else {

  # Hit a lightweight endpoint (journals list) to verify auth
  probe_url <- glue("{efice_url}/journals")

  cat(glue("  Probing: {probe_url}\n"))

  probe_result <- tryCatch({
    resp <- httr2::request(probe_url) |>
      httr2::req_headers(Authorization = glue("Bearer {efice_token}")) |>
      httr2::req_timeout(15) |>
      httr2::req_perform()
    list(status = httr2::resp_status(resp), error = NULL)
  }, error = function(e) {
    list(status = NA_integer_, error = conditionMessage(e))
  })

  if (!is.null(probe_result$error)) {
    .err(glue("HTTP request failed: {probe_result$error}"))
    cat("  → Likely cause: network timeout, wrong URL, or API down.\n\n")
  } else if (probe_result$status == 200) {
    .ok(glue("efice API responded HTTP {probe_result$status} — token is valid"))
  } else if (probe_result$status == 401) {
    .err(glue("HTTP {probe_result$status} Unauthorized — token has expired or is wrong"))
    cat("  → Update EFICE_TOKEN in your .Renviron and restart R.\n\n")
  } else if (probe_result$status == 403) {
    .err(glue("HTTP {probe_result$status} Forbidden — token valid but lacks permission"))
  } else {
    .warn(glue("Unexpected HTTP {probe_result$status} — check API docs or contact efice support"))
  }
}

# ============================================================================
# CHECK 4 — Device discovery per vessel (the step that failed in the log)
# ============================================================================
.section("CHECK 4 — Device discovery per vessel")

setup_poseidat_sync()
config_dir    <- file.path(.onedrive(), "config")
vessel_config <- readRDS(file.path(config_dir, "poseidat_vessels.rds"))

efice_vessels <- vessel_config |>
  dplyr::filter(source == "efice", enabled == TRUE)

if (nrow(efice_vessels) == 0) {
  .warn("No enabled efice vessels found in config")
} else {
  cat(glue("  Testing device discovery for {nrow(efice_vessels)} vessel(s)...\n\n"))

  discovery_results <- efice_vessels |>
    dplyr::select(vessel_name, journal_id) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      discovery = list(tryCatch({
        t0  <- proc.time()["elapsed"]
        url <- glue("{efice_url}/journals/{journal_id}/devices")
        resp <- httr2::request(url) |>
          httr2::req_headers(Authorization = glue("Bearer {efice_token}")) |>
          httr2::req_timeout(30) |>
          httr2::req_perform()
        elapsed <- round(proc.time()["elapsed"] - t0, 1)
        body    <- httr2::resp_body_json(resp)
        n_dev   <- length(body)
        list(ok = TRUE, n_devices = n_dev, elapsed_s = elapsed,
             status = httr2::resp_status(resp), error = NULL)
      }, error = function(e) {
        list(ok = FALSE, n_devices = NA_integer_, elapsed_s = NA_real_,
             status = NA_integer_, error = conditionMessage(e))
      }))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ok        = purrr::map_lgl(discovery, "ok"),
      n_devices = purrr::map_int(discovery, "n_devices"),
      elapsed_s = purrr::map_dbl(discovery, "elapsed_s"),
      status    = purrr::map_int(discovery, "status"),
      error_msg = purrr::map_chr(discovery, ~ .x$error %||% "")
    ) |>
    dplyr::select(-discovery)

  # Print results row by row
  discovery_results |>
    dplyr::rowwise() |>
    dplyr::group_walk(~ {
      v <- .x
      if (!v$ok) {
        .err(glue("{v$vessel_name} ({v$journal_id}) — HTTP FAILED: {v$error_msg}"))
      } else if (v$n_devices == 0) {
        .warn(glue(
          "{v$vessel_name} ({v$journal_id}) — ",
          "API returned 0 devices (HTTP {v$status}, {v$elapsed_s}s)\n",
          "    This is what caused the sync to skip this vessel!"
        ))
      } else {
        .ok(glue(
          "{v$vessel_name} ({v$journal_id}) — ",
          "{v$n_devices} device(s) found (HTTP {v$status}, {v$elapsed_s}s)"
        ))
      }
    })

  # Summary table
  cat("\n  Discovery summary:\n")
  discovery_results |>
    dplyr::select(vessel_name, journal_id, ok, n_devices, elapsed_s, status, error_msg) |>
    dplyr::mutate(result = dplyr::case_when(
      !ok           ~ "HTTP ERROR",
      n_devices == 0 ~ "0 devices",
      TRUE           ~ glue("{n_devices} device(s)")
    )) |>
    dplyr::select(vessel_name, result, elapsed_s, status) |>
    print()
}

# ============================================================================
# CHECK 5 — Raw parquet currency (last record date per vessel x device)
# ============================================================================
.section("CHECK 5 — Raw parquet currency on disk")

raw_dir <- file.path(.onedrive(), "raw")

if (!dir.exists(raw_dir)) {
  .err(glue("Raw directory not found: {raw_dir}"))
} else {
  parquet_files <- list.files(raw_dir, pattern = "\\.parquet$",
                              recursive = TRUE, full.names = TRUE)

  if (length(parquet_files) == 0) {
    .warn("No parquet files found in raw directory")
  } else {

    cat(glue("  Found {length(parquet_files)} parquet file(s). Reading last-record timestamps...\n\n"))

    file_summary <- tibble::tibble(path = parquet_files) |>
      dplyr::mutate(
        filename    = basename(path),
        # File modified time as a fallback staleness check
        file_mtime  = file.mtime(path),
        days_old    = as.numeric(difftime(Sys.time(), file_mtime, units = "days"))
      ) |>
      dplyr::arrange(desc(days_old))

    stale_threshold_days <- 3

    stale <- file_summary |> dplyr::filter(days_old > stale_threshold_days)
    fresh <- file_summary |> dplyr::filter(days_old <= stale_threshold_days)

    if (nrow(fresh) > 0) {
      .ok(glue("{nrow(fresh)} file(s) updated within last {stale_threshold_days} day(s)"))
    }

    if (nrow(stale) > 0) {
      .warn(glue("{nrow(stale)} file(s) NOT updated in last {stale_threshold_days} day(s):"))
      stale |>
        dplyr::select(filename, file_mtime, days_old) |>
        dplyr::mutate(days_old = round(days_old, 1)) |>
        print()
    }

    # Also read last datetime from the actual parquet content
    cat("\n  Last record datetime per parquet file (from content):\n")
    arrow::open_dataset(raw_dir) |>
      dplyr::group_by(vessel_name, device_id) |>
      dplyr::summarise(
        last_record  = max(datetime, na.rm = TRUE),
        record_count = dplyr::n(),
        .groups      = "drop"
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        last_record = as.POSIXct(last_record, tz = "UTC"),
        days_since  = round(as.numeric(difftime(Sys.time(), last_record,
                                                units = "days")), 1),
        stale       = days_since > stale_threshold_days
      ) |>
      dplyr::arrange(desc(days_since)) |>
      print()
  }
}

# ============================================================================
# CHECK 6 — Thinned parquet currency
# ============================================================================
.section("CHECK 6 — Thinned parquet currency on disk")

thinned_dir <- file.path(.onedrive(), "processed", "thinned")

if (!dir.exists(thinned_dir)) {
  .warn(glue("Thinned directory not found: {thinned_dir}"))
} else {
  thinned_files <- list.files(thinned_dir, pattern = "\\.parquet$",
                              recursive = TRUE, full.names = TRUE)

  if (length(thinned_files) == 0) {
    .warn("No thinned parquet files found")
  } else {
    tibble::tibble(path = thinned_files) |>
      dplyr::mutate(
        filename   = basename(path),
        file_mtime = file.mtime(path),
        days_old   = round(as.numeric(difftime(Sys.time(), file_mtime,
                                               units = "days")), 1),
        status     = dplyr::if_else(days_old <= 1, "fresh (today)", "stale")
      ) |>
      dplyr::select(filename, file_mtime, days_old, status) |>
      dplyr::arrange(days_old) |>
      print()
  }
}

# ============================================================================
# SUMMARY
# ============================================================================
.section("DIAGNOSTIC SUMMARY")

cat(glue("  Run at : {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}\n\n"))
cat("  Review any \u2717 (error) or \u26a0 (warning) lines above.\n")
cat("  Most likely causes of today's failure:\n")
cat("    1. efice device discovery API returned 0 devices (token expired?)\n")
cat("    2. HTTP timeout on Willem Jacob — API unreachable for that vessel\n")
cat("    3. PEFA credentials not set — pefa source was silently skipped\n\n")
cat("  Next steps if device discovery (Check 4) still fails:\n")
cat("    - Verify your EFICE_TOKEN is current in .Renviron\n")
cat("    - Try running on a different network (not the hotspot)\n")
cat("    - Contact efice support with the journal_id and error message\n\n")
