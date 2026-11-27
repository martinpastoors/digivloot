# ============================================================================
# test_efice_get_devices.R
#
# Tests all three fallback tiers of the new .efice_get_devices() function.
#
# Run interactively, section by section. Each test temporarily monkey-patches
# .http_get() to simulate specific failure conditions, then restores it.
# No real API calls are made in tests 2 and 3.
#
# Prerequisites:
#   - Poseidat_functions.R sourced (with the new .efice_get_devices in place)
#   - setup_poseidat_sync() called
#   - At least one prior successful sync (for Tiers 2 and 3 to have data)
# ============================================================================

library(dplyr)
library(stringr)
library(glue)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

config_dir    <- file.path(.onedrive(), "config")
vessel_config <- readRDS(file.path(config_dir, "poseidat_vessels.rds"))

# Pick the first enabled efice vessel as the test subject
test_vessel <- vessel_config |>
  dplyr::filter(source == "efice", enabled == TRUE) |>
  dplyr::slice(1)

test_journal_id <- test_vessel$journal_id
test_vessel_name <- test_vessel$vessel_name

efice_url   <- Sys.getenv("EFICE_API_URL")
efice_token <- Sys.getenv("EFICE_TOKEN")

cat(glue("\nTest vessel : {test_vessel_name}\n"))
cat(glue("Journal ID  : {test_journal_id}\n\n"))

# ── helper ───────────────────────────────────────────────────────────────────

.pass <- function(msg) cat(glue("  \u2713 PASS  {msg}\n\n"))
.fail <- function(msg) cat(glue("  \u2717 FAIL  {msg}\n\n"))
.info <- function(msg) cat(glue("  \u2192 {msg}\n"))

.check <- function(label, result, min_devices = 1) {
  cat(glue("  Result : {nrow(result)} device(s)\n"))
  if (nrow(result) > 0) cat(glue("  IDs    : {paste(result$device_id, collapse = ', ')}\n"))
  cat("\n")
  if (nrow(result) >= min_devices) .pass(label) else .fail(label)
}

# Save the real .http_get so we can restore it after each test
.real_http_get <- .http_get

# ============================================================================
# TEST 1 — Tier 1: live API works normally
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" TEST 1 — Tier 1: live API (real network call)\n")
cat(strrep("=", 70), "\n")
cat("  Calling .efice_get_devices() with real credentials.\n")
cat("  Expect: devices returned via the live API probe.\n\n")

result_t1 <- .efice_get_devices(efice_url, efice_token, test_journal_id)
.check("Tier 1 (live API) returned at least 1 device", result_t1)

# Store what the live API returned so we can validate Tier 2 and 3 match it
live_device_ids <- sort(result_t1$device_id)


# ── Diagnose Tier 1: what does the sample fetch actually return? ─────────────

fmt_dt       <- function(t) format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
sample_start <- fmt_dt(Sys.time() - 90 * 86400)
end_dt       <- fmt_dt(Sys.time())

raw <- .http_get(efice_url, efice_token, "journals/entries",
                 params = list(
                   journalId     = test_journal_id,
                   startDate     = sample_start,
                   endDate       = end_dt,
                   limit         = 10L,
                   skip          = 0L,
                   sortBy        = "entry_datetime",
                   sortDirection = "descending"
                 ),
                 timeout_sec = 60)

cat(sprintf("total reported by API : %s\n", raw$total  %||% "NULL"))
cat(sprintf("items returned        : %d\n", length(raw$items %||% list())))
cat(sprintf("other keys in response: %s\n", paste(names(raw), collapse = ", ")))


# ── Diagnose Tier 2: what parquet files does the scan actually find? ─────────

efice_raw_root <- file.path(.onedrive(), "raw", "efice")
cat(sprintf("Scanning: %s\n",   efice_raw_root))
cat(sprintf("Dir exists: %s\n", dir.exists(efice_raw_root)))

all_parquet <- list.files(efice_raw_root, pattern = "\\.parquet$",
                          recursive = TRUE, full.names = FALSE)
cat(sprintf("Parquet files found: %d\n\n", length(all_parquet)))

# Show the first 10 filenames so we can see what the pattern looks like
cat("First 10 filenames:\n")
print(head(all_parquet, 10))

# ============================================================================
# TEST 2 — Tier 1 fails, Tier 2 (parquet) takes over
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" TEST 2 — Tier 2: parquet fallback (API simulated as broken)\n")
cat(strrep("=", 70), "\n")
cat("  Patching .http_get() to always throw a timeout error.\n")
cat("  Expect: same device IDs recovered from parquet filenames.\n\n")

# Monkey-patch .http_get in the global env to simulate a dead API
.http_get <<- function(...) stop("Simulated API timeout")

result_t2 <- .efice_get_devices(efice_url, efice_token, test_journal_id)

# Restore immediately
.http_get <<- .real_http_get

.check("Tier 2 (parquet) returned at least 1 device", result_t2)

if (nrow(result_t1) > 0 && nrow(result_t2) > 0) {
  # Tier 2 scans all efice parquets so may return a superset across vessels;
  # check that all live devices are present in the Tier 2 result
  missing <- setdiff(live_device_ids, sort(result_t2$device_id))
  if (length(missing) == 0) {
    .pass("Tier 2 result contains all devices found by Tier 1")
  } else {
    .fail(glue("Tier 2 missing devices that Tier 1 found: {paste(missing, collapse = ', ')}"))
    .info("This means the parquet filename pattern didn't match — check .safe() substitution")
  }
}

# ============================================================================
# TEST 3 — Tier 1 and Tier 2 both fail, Tier 3 (sync state) takes over
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" TEST 3 — Tier 3: sync state fallback (API + parquet both simulated broken)\n")
cat(strrep("=", 70), "\n")
cat("  Patching .http_get() to throw, and temporarily hiding the parquet dir.\n")
cat("  Expect: same device IDs recovered from sync state RDS.\n\n")

# Patch API
.http_get <<- function(...) stop("Simulated API timeout")

# Temporarily rename the efice raw dir so Tier 2 finds nothing
efice_raw_dir     <- file.path(.onedrive(), "raw", "efice")
efice_raw_dir_bak <- paste0(efice_raw_dir, "_TESTBAK")

dir_hidden <- FALSE
if (dir.exists(efice_raw_dir)) {
  file.rename(efice_raw_dir, efice_raw_dir_bak)
  dir_hidden <- TRUE
  cat("  (efice raw dir temporarily renamed to _TESTBAK)\n\n")
}

result_t3 <- tryCatch(
  .efice_get_devices(efice_url, efice_token, test_journal_id),
  error = function(e) {
    tibble(device_id = character(), device_name = character(),
           device_type = character())
  }
)

# Restore parquet dir and API
if (dir_hidden) file.rename(efice_raw_dir_bak, efice_raw_dir)
.http_get <<- .real_http_get

.check("Tier 3 (sync state) returned at least 1 device", result_t3)

if (nrow(result_t1) > 0 && nrow(result_t3) > 0) {
  missing <- setdiff(live_device_ids, sort(result_t3$device_id))
  if (length(missing) == 0) {
    .pass("Tier 3 result contains all devices found by Tier 1")
  } else {
    .fail(glue("Tier 3 missing: {paste(missing, collapse = ', ')}"))
    .info("Check that update_last_sync_time() was called after the last successful sync")
    .info("Inspect sync state with: names(get_sync_state()$vessels)")
  }
}

# ============================================================================
# TEST 4 — All tiers fail gracefully (no crash, returns empty tibble)
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" TEST 4 — All tiers exhausted: graceful empty return\n")
cat(strrep("=", 70), "\n")
cat("  Patching .http_get() to throw, hiding parquet dir,\n")
cat("  and using a fake journal_id not present in sync state.\n")
cat("  Expect: 0 devices returned, no crash.\n\n")

fake_journal_id <- "00000000-0000-0000-0000-000000000000"

.http_get <<- function(...) stop("Simulated API timeout")

if (dir.exists(efice_raw_dir)) {
  file.rename(efice_raw_dir, efice_raw_dir_bak)
  dir_hidden <- TRUE
}

result_t4 <- tryCatch(
  .efice_get_devices(efice_url, efice_token, fake_journal_id),
  error = function(e) {
    .fail(glue("Function threw an unhandled error: {conditionMessage(e)}"))
    tibble(device_id = character(), device_name = character(),
           device_type = character())
  }
)

if (dir_hidden) file.rename(efice_raw_dir_bak, efice_raw_dir)
.http_get <<- .real_http_get

if (nrow(result_t4) == 0 && is.data.frame(result_t4)) {
  .pass("All tiers exhausted — returned empty tibble without crashing")
} else {
  .fail(glue("Expected empty tibble, got {nrow(result_t4)} rows"))
}

# ============================================================================
# SUMMARY
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" SUMMARY\n")
cat(strrep("=", 70), "\n")
cat(glue("  Live API devices  : {paste(live_device_ids, collapse = ', ')}\n"))
cat(glue("  Tier 2 devices    : {paste(sort(result_t2$device_id), collapse = ', ')}\n"))
cat(glue("  Tier 3 devices    : {paste(sort(result_t3$device_id), collapse = ', ')}\n"))
cat("\n")
cat("  All four tests passing = the fallback chain is working correctly.\n")
cat("  If Tier 2 or 3 are missing devices, run:\n")
cat("    names(get_sync_state()$vessels)             # inspect sync state keys\n")
cat("    list.files(.onedrive(), recursive=TRUE,      # inspect parquet filenames\n")
cat("               pattern='.parquet$')[1:10]\n\n")
