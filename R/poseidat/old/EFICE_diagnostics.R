# ============================================================================
# EFICE_diagnostics.R
#
# Step-by-step diagnostics to establish a working EFICE API connection
# and discover the endpoint structure.
#
# Run one STEP at a time. Read the output before moving to the next step.
# Each step isolates one possible failure point.
# ============================================================================

library(httr2)
library(dplyr)
library(glue)


# ============================================================================
# STEP 1 — Verify credentials are loaded in this R session
#
# What to check: both values print as non-empty strings.
# If either shows "" → your .Renviron is not loaded. Fix before continuing.
# ============================================================================

efice_url   <- Sys.getenv("EFICE_API_URL")
efice_token <- Sys.getenv("EFICE_TOKEN")

cat("=== STEP 1: Credentials ===\n")
cat(sprintf("EFICE_API_URL : '%s'\n", efice_url))
cat(sprintf("EFICE_TOKEN   : '%s...%s' (%d chars)\n",
            substr(efice_token, 1, 6),
            substr(efice_token, nchar(efice_token) - 5, nchar(efice_token)),
            nchar(efice_token)))

# FIX if empty:
#   usethis::edit_r_environ()
#   Add:
#     EFICE_API_URL = https://...
#     EFICE_TOKEN   = your-bearer-token
#   Then restart R (Session → Restart R) and re-run from Step 1.


# ============================================================================
# STEP 2 — Verify the URL is reachable at the network level
#
# This uses curl (system tool) to check basic TCP connectivity,
# completely bypassing R and httr2. This tells you whether the host
# is reachable at all, independent of any auth or endpoint issues.
#
# Expected good output:  HTTP/... 200, 401, 403, or 404
#   (any HTTP response means the host is reachable)
# Bad output: "Could not resolve host" → DNS/URL problem
#             "Connection refused"      → wrong port or firewall
#             Command not found         → skip to Step 2b
# ============================================================================

cat("\n=== STEP 2a: Network reachability (curl) ===\n")
system(glue('curl -I --max-time 10 --silent --show-error "{efice_url}"'))

# STEP 2b — fallback if curl is not available (e.g. some Windows setups)
cat("\n=== STEP 2b: Network reachability (R socket) ===\n")
# url_parts <- httr2:::parse_url(efice_url)
# host      <- url_parts$hostname
# port      <- if (!is.null(url_parts$port)) url_parts$port else 443L

url_match <- regmatches(efice_url,
                        regexec("^https?://([^/:]+)(?::([0-9]+))?", efice_url))[[1]]
host <- url_match[2]
port <- if (length(url_match) >= 3 && nzchar(url_match[3])) as.integer(url_match[3]) else 443L
cat(sprintf("Parsed host: %s  port: %d\n", host, port))

tryCatch({
  con <- socketConnection(host = host, port = port,
                          open = "r+", timeout = 10, blocking = TRUE)
  close(con)
  cat(sprintf("✓ TCP connection to %s:%d succeeded\n", host, port))
}, error = function(e) {
  cat(sprintf("✗ TCP connection to %s:%d FAILED: %s\n", host, port, e$message))
  cat("  → Check that EFICE_API_URL is correct (typo? missing https://?).\n")
  cat("  → Check your network / VPN — is access restricted?\n")
})




# ===========================

# Confirmed base — no path prefix needed
confirmed_base <- efice_url   # https://poseidat-journal.m-catch.com

# Test 1: system version (no auth required — good sanity check)
resp <- request(paste0(confirmed_base, "/system/version")) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()
cat(sprintf("GET /system/version -> %d\n", resp_status(resp)))
cat(resp_body_string(resp), "\n")

# Test 2: journal list WITH auth
resp <- request(paste0(confirmed_base, "/journals")) |>
  req_auth_bearer_token(efice_token) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()
cat(sprintf("\nGET /journals -> %d\n", resp_status(resp)))
cat(substr(resp_body_string(resp), 1, 500), "\n")

# Test 3: journal entries WITH auth
resp <- request(paste0(confirmed_base, "/journals/entries")) |>
  req_auth_bearer_token(efice_token) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()
cat(sprintf("\nGET /journals/entries -> %d\n", resp_status(resp)))
cat(substr(resp_body_string(resp), 1, 500), "\n")

# ====================

# Pick the first journal ID from the /journals response
journal_id <- "6a5c162e-e967-41b6-9e8a-3cfddf5019b2"  # Lub Senior

# Test with correct parameter names
resp <- request(paste0(confirmed_base, "/journals/entries")) |>
  req_auth_bearer_token(efice_token) |>
  req_url_query(
    journalId   = journal_id,                  # camelCase — this was the bug
    startDate   = "2026-01-01T00:00:00Z",
    endDate     = "2026-03-01T00:00:00Z",
    entryType   = "device-measurement",
    limit       = 3,
    skip        = 0,
    sortBy      = "entry_datetime",
    sortDirection = "ascending"
  ) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()

cat(sprintf("Status: %d\n", resp_status(resp)))
cat(resp_body_string(resp), "\n")


# ============================================================================
# STEP 3 — Make a raw unauthenticated request to the root URL
#
# No token yet. This tells you whether the server responds to HTTP at all
# and what it says before authentication is even attempted.
#
# Expected:  status 401 (Unauthorized) or 403 (Forbidden) → server is alive,
#            just needs credentials
#            status 200 → public endpoint, note what it returns
#            status 404 → URL path may be wrong (check base URL)
#            Error/timeout → network issue (go back to Step 2)
# ============================================================================

cat("\n=== STEP 3: Raw unauthenticated GET to root URL ===\n")
tryCatch({
  resp <- request(efice_url) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()

  cat(sprintf("Status : %d %s\n", resp_status(resp), resp_status_desc(resp)))
  cat(sprintf("Headers:\n"))
  hdrs <- resp_headers(resp)
  for (h in c("content-type", "www-authenticate", "server", "x-powered-by")) {
    val <- hdrs[[h]]
    if (!is.null(val)) cat(sprintf("  %s: %s\n", h, val))
  }
  body <- tryCatch(resp_body_string(resp), error = \(e) "(unreadable body)")
  cat(sprintf("Body (first 500 chars):\n%s\n", substr(body, 1, 500)))

}, error = function(e) {
  cat(sprintf("✗ Request failed: %s\n", e$message))
  cat("  → This is a network/TLS error, not an auth error.\n")
  cat("  → Check Step 2 results.\n")
})


# ============================================================================
# STEP 4 — Test authentication with the bearer token
#
# Sends the token but hits a safe, known endpoint (root or /journals).
# This isolates whether the token itself is accepted.
#
# Expected good outcomes:
#   200 → authenticated, endpoint exists
#   401 → token rejected (wrong token, expired, or wrong auth scheme)
#   403 → token accepted but insufficient permissions
#   404 → authenticated but endpoint doesn't exist at this path (normal)
#
# If 401: check token value in Step 1, try Step 4b (alternative auth header)
# ============================================================================

cat("\n=== STEP 4a: Authenticated GET — root URL ===\n")
tryCatch({
  resp <- request(efice_url) |>
    req_auth_bearer_token(efice_token) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()

  cat(sprintf("Status  : %d %s\n", resp_status(resp), resp_status_desc(resp)))
  body <- tryCatch(resp_body_string(resp), error = \(e) "(unreadable)")
  cat(sprintf("Body    : %s\n", substr(body, 1, 300)))

}, error = function(e) {
  cat(sprintf("✗ Request error: %s\n", e$message))
})

# STEP 4b — Some APIs use a custom header instead of standard Bearer
# Uncomment and try if Step 4a returns 401:
cat("\n=== STEP 4b: Authenticated GET — custom header variants ===\n")

# Variant 1: X-API-Key header
tryCatch({
  resp <- request(efice_url) |>
    req_headers(`X-API-Key` = efice_token) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()
  cat(sprintf("X-API-Key header  → status: %d\n", resp_status(resp)))
}, error = function(e) cat(sprintf("X-API-Key error: %s\n", e$message)))

# Variant 2: Token prefix instead of Bearer
tryCatch({
  resp <- request(efice_url) |>
    req_headers(Authorization = paste("Token", efice_token)) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()
  cat(sprintf("Token prefix      → status: %d\n", resp_status(resp)))
}, error = function(e) cat(sprintf("Token prefix error: %s\n", e$message)))

# Variant 3: Basic auth with token as password (rare but exists)
tryCatch({
  resp <- request(efice_url) |>
    req_auth_basic("", efice_token) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()
  cat(sprintf("Basic auth        → status: %d\n", resp_status(resp)))
}, error = function(e) cat(sprintf("Basic auth error: %s\n", e$message)))



# ============================================================================
# STEP 5 — Probe common API base path variants
#
# APIs often have a version prefix in the path that must be included.
# Only run this after Step 4 confirms the token is accepted (200/403/404).
#
# Read the status for each — 404 means the path is wrong,
# 200/401/403 means you found a live endpoint.
# ============================================================================

cat("\n=== STEP 5: Base path variants ===\n")

# Fill in what EFICE told you the base URL is:
# e.g. "https://api.efice.com" with paths like /api/v1, /v1, /poseidat, etc.
base_paths <- c(
  "",          # as-is
  "/api",
  "/api/v1",
  "/api/v2",
  "/v1",
  "/v2",
  "/poseidat",
  "/poseidat/api/v1",
  "/rest",
  "/rest/v1"
)

for (path in base_paths) {
  url <- paste0(efice_url, path)
  tryCatch({
    resp <- request(url) |>
      req_auth_bearer_token(efice_token) |>
      req_error(is_error = \(r) FALSE) |>
      req_timeout(10) |>
      req_perform()
    cat(sprintf("  %-35s → %d %s\n",
                paste0(path, "/"), resp_status(resp), resp_status_desc(resp)))
  }, error = function(e) {
    cat(sprintf("  %-35s → ERROR: %s\n", paste0(path, "/"), e$message))
  })
  Sys.sleep(0.5)
}


# ============================================================================
# STEP 6 — Probe journal/vessel endpoint variants
#
# Once you know which base path works from Step 5, set `confirmed_base`
# below and run this to find the vessel listing endpoint.
# ============================================================================

cat("\n=== STEP 6: Vessel/journal endpoint variants ===\n")

confirmed_base <- efice_url   # ← replace with the working path from Step 5
                               # e.g. paste0(efice_url, "/api/v1")

vessel_endpoints <- c(
  "journals",
  "vessels",
  "trips",
  "fishermen",
  "registrations",
  "data",
  "records"
)

for (ep in vessel_endpoints) {
  url <- paste0(confirmed_base, "/", ep)
  tryCatch({
    resp <- request(url) |>
      req_auth_bearer_token(efice_token) |>
      req_error(is_error = \(r) FALSE) |>
      req_timeout(10) |>
      req_perform()

    status <- resp_status(resp)
    body_preview <- ""
    if (status == 200) {
      raw <- tryCatch(resp_body_string(resp), error = \(e) "")
      body_preview <- substr(raw, 1, 120)
    }
    cat(sprintf("  %-25s → %d%s\n",
                ep, status,
                if (nzchar(body_preview)) paste0("  ", body_preview) else ""))
  }, error = function(e) {
    cat(sprintf("  %-25s → ERROR: %s\n", ep, e$message))
  })
  Sys.sleep(0.5)
}


# ============================================================================
# STEP 7 — Inspect a successful response in full detail
#
# Once Step 6 finds a 200 endpoint, run this to see the full response
# structure so you can map the field names for the adapter.
# ============================================================================

cat("\n=== STEP 7: Full response inspection ===\n")

working_endpoint <- "journals"   # ← replace with whichever returned 200 in Step 6

tryCatch({
  resp <- request(paste0(confirmed_base, "/", working_endpoint)) |>
    req_auth_bearer_token(efice_token) |>
    req_error(is_error = \(r) FALSE) |>
    req_timeout(15) |>
    req_perform()

  cat(sprintf("Status  : %d %s\n", resp_status(resp), resp_status_desc(resp)))
  cat("Headers :\n")
  for (h in names(resp_headers(resp))) {
    cat(sprintf("  %s: %s\n", h, resp_headers(resp)[[h]]))
  }

  body_raw <- resp_body_string(resp)
  cat(sprintf("\nFull body (%d chars):\n", nchar(body_raw)))
  cat(body_raw, "\n")

  # If it's JSON, parse and show structure
  body_json <- tryCatch(resp_body_json(resp), error = \(e) NULL)
  if (!is.null(body_json)) {
    cat("\nParsed JSON structure:\n")
    cat(sprintf("  Top-level type  : %s\n", class(body_json)[1]))
    cat(sprintf("  Top-level length: %d\n", length(body_json)))
    if (is.list(body_json) && length(body_json) > 0) {
      first <- body_json[[1]]
      if (is.list(first)) {
        cat(sprintf("  First item keys : %s\n", paste(names(first), collapse = ", ")))
        cat("  First item values:\n")
        for (k in names(first)) {
          cat(sprintf("    %-30s: %s\n", k,
                      substr(as.character(first[[k]]), 1, 80)))
        }
      } else {
        cat(sprintf("  Top-level keys  : %s\n", paste(names(body_json), collapse = ", ")))
      }
    }
  }

}, error = function(e) {
  cat(sprintf("✗ %s\n", e$message))
})


# ============================================================================
# STEP 8 — Capture everything to a file for sharing or debugging
#
# If you need to share the output with someone for help, run this block.
# It writes a sanitised log (token is masked) to your desktop.
# ============================================================================

cat("\n=== STEP 8: Save diagnostic output ===\n")

log_file <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "efice_diagnostics.txt")

# Mask the token in the output
sink(log_file)
cat(sprintf("Diagnostic run: %s\n", Sys.time()))
cat(sprintf("EFICE_API_URL : %s\n", efice_url))
cat(sprintf("EFICE_TOKEN   : %s...%s (%d chars) [masked]\n",
            substr(efice_token, 1, 4),
            substr(efice_token, nchar(efice_token) - 3, nchar(efice_token)),
            nchar(efice_token)))
sink()

cat(sprintf("Log written to: %s\n", log_file))
cat("Share this file (not your actual token!) when asking for help.\n")
