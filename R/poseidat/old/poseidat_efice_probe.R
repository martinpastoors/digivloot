# ============================================================================
# poseidat_efice_probe.R
#
# Probes the efice API to find the correct endpoint structure.
# Run interactively after a 404 on device discovery.
# ============================================================================

library(dplyr)
library(httr2)
library(glue)
library(jsonlite)

# ── helpers ──────────────────────────────────────────────────────────────────

.ok   <- function(msg) cat(glue("  \u2713 {msg}\n\n"))
.warn <- function(msg) cat(glue("  \u26a0 {msg}\n\n"))
.err  <- function(msg) cat(glue("  \u2717 {msg}\n\n"))

.probe <- function(url, token, timeout = 15) {
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Authorization = glue("Bearer {token}")) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(r) FALSE) |>   # don't throw on 4xx/5xx
      httr2::req_perform()
    list(
      status = httr2::resp_status(resp),
      ok     = httr2::resp_status(resp) < 400,
      body   = tryCatch(httr2::resp_body_json(resp), error = \(e) NULL),
      error  = NULL
    )
  }, error = function(e) {
    list(status = NA_integer_, ok = FALSE, body = NULL,
         error  = conditionMessage(e))
  })
}

# Accepts the result list directly — no list-column unwrapping needed
.report <- function(label, url, result) {
  status <- if (is.na(result$status)) "no response" else result$status
  if (!is.null(result$error)) {
    .err( glue("[{status}] {label}\n       {url}\n       Error: {result$error}"))
  } else if (isTRUE(result$ok)) {
    .ok(  glue("[{status}] {label}\n       {url}"))
  } else {
    .warn(glue("[{status}] {label}\n       {url}"))
  }
  invisible(result)
}

# Iterates a tibble with columns (label, url) and a list-column (result),
# calling .report() for each row — avoids group_walk unwrapping issues
.report_tibble <- function(tbl) {
  for (i in seq_len(nrow(tbl))) {
    .report(
      label  = tbl$label[[i]],
      url    = tbl$url[[i]],
      result = tbl$result[[i]]
    )
  }
}

# ── credentials ──────────────────────────────────────────────────────────────

efice_url   <- Sys.getenv("EFICE_API_URL")
efice_token <- Sys.getenv("EFICE_TOKEN")

stopifnot(
  "EFICE_API_URL not set" = nchar(efice_url)   > 0,
  "EFICE_TOKEN not set"   = nchar(efice_token) > 0
)

# One known vessel for testing
test_journal_id <- "6a5c162e-e967-41b6-9e8a-3cfddf5019b2"  # Lub Senior

cat(glue("\n  efice_url   : {efice_url}\n"))
cat(glue("  test vessel : {test_journal_id}\n\n"))

# ============================================================================
# STEP 1 — Is the base URL itself alive?
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" STEP 1 — Base URL probe\n")
cat(strrep("=", 70), "\n")

base_candidates <- c(
  efice_url,
  sub("/v\\d+$",      "", efice_url),   # strip /v1
  sub("/api/v\\d+$",  "", efice_url)    # strip /api/v1
) |> unique()

base_results <- tibble::tibble(
  label = base_candidates,
  url   = base_candidates
) |>
  dplyr::mutate(result = lapply(url, .probe, token = efice_token))

.report_tibble(base_results)

# ── Quick domain structure probe ─────────────────────────────────────────────

# ── Fetch the OpenAPI spec from common Swagger/ReDoc paths ───────────────────

spec_candidates <- c(
  "https://poseidat-journal.m-catch.com/swagger/v1/swagger.json",
  "https://poseidat-journal.m-catch.com/swagger.json",
  "https://poseidat-journal.m-catch.com/openapi.json",
  "https://poseidat-journal.m-catch.com/openapi.yaml",
  "https://poseidat-journal.m-catch.com/api-docs",
  "https://poseidat-journal.m-catch.com/api-docs/swagger.json",
  "https://poseidat-journal.m-catch.com/docs/swagger.json"
)

spec_results <- tibble::tibble(url = spec_candidates) |>
  dplyr::mutate(result = lapply(url, .probe, token = efice_token)) |>
  dplyr::mutate(
    status = sapply(result, \(r) r$status),
    ok     = sapply(result, \(r) isTRUE(r$ok))
  )

# Print status overview
spec_results |>
  dplyr::select(url, status, ok) |>
  print(n = Inf)

# For any that returned 200, print the first 1000 chars of the body
spec_results |>
  dplyr::filter(ok) |>
  dplyr::rowwise() |>
  dplyr::group_walk(~ {
    cat(glue("\n\n=== {.x$url} ===\n"))
    body_text <- tryCatch(
      jsonlite::toJSON(.x$result[[1]]$body, auto_unbox = TRUE, pretty = TRUE),
      error = \(e) "(non-JSON body)"
    )
    cat(substr(body_text, 1, 1000), "\n")
  })

# ============================================================================
# STEP 2 — Common endpoint path variants for journal / vessel listing
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" STEP 2 — Journal / vessel listing endpoint variants\n")
cat(strrep("=", 70), "\n")

listing_paths <- c(
  "journals",
  "vessels",
  "journal",
  "vessel",
  "api/journals",
  "api/vessels"
)

listing_results <- tibble::tibble(label = listing_paths) |>
  dplyr::mutate(
    url    = glue("{efice_url}/{label}"),
    result = lapply(url, .probe, token = efice_token)
  )

.report_tibble(listing_results)

# If any listing endpoint returned 200, show what came back
working_listing <- listing_results |>
  dplyr::filter(sapply(result, \(r) isTRUE(r$ok)))

if (nrow(working_listing) > 0) {
  cat("  \u2713 Working listing endpoint(s) found — first response body:\n\n")
  cat(jsonlite::toJSON(working_listing$result[[1]]$body,
                       auto_unbox = TRUE, pretty = TRUE))
  cat("\n\n")
}

# ============================================================================
# STEP 3 — Device discovery path variants for the test vessel
# ============================================================================
cat(strrep("=", 70), "\n")
cat(glue(" STEP 3 — Device discovery variants for {test_journal_id}\n"))
cat(strrep("=", 70), "\n")

device_paths <- c(
  glue("journals/{test_journal_id}/devices"),
  glue("journals/{test_journal_id}/device"),
  glue("vessels/{test_journal_id}/devices"),
  glue("vessels/{test_journal_id}/device"),
  glue("journal/{test_journal_id}/devices"),
  glue("vessel/{test_journal_id}/devices"),
  glue("{test_journal_id}/devices"),
  glue("devices?journal_id={test_journal_id}"),
  glue("devices?vessel_id={test_journal_id}"),
  glue("devices?journalId={test_journal_id}")
)

device_results <- tibble::tibble(label = as.character(device_paths)) |>
  dplyr::mutate(
    url    = glue("{efice_url}/{label}"),
    result = lapply(url, .probe, token = efice_token)
  )

.report_tibble(device_results)

working_device <- device_results |>
  dplyr::filter(sapply(result, \(r) isTRUE(r$ok)))

if (nrow(working_device) > 0) {
  cat("  \u2713 Working device endpoint(s) found — first response body:\n\n")
  cat(jsonlite::toJSON(working_device$result[[1]]$body,
                       auto_unbox = TRUE, pretty = TRUE))
  cat("\n\n")
  cat(glue(
    "  \u2192 Update EFICE_API_URL in your .Renviron or fix the path in\n",
    "    Poseidat_functions.R to use:\n",
    "    {working_device$url[[1]]}\n\n"
  ))
} else {
  cat("  \u2717 No working device endpoint found in any tested variant.\n\n")
  cat("  Next steps:\n")
  cat("    1. Check efice API documentation for the correct URL structure\n")
  cat("    2. Run: cat(Sys.getenv('EFICE_API_URL')) — confirm no doubled segments\n")
  cat("    3. Check for any recent API change notes from efice\n\n")
}

# ============================================================================
# STEP 4 — URL sanity check (print the exact string being constructed)
# ============================================================================
cat(strrep("=", 70), "\n")
cat(" STEP 4 — URL sanity check\n")
cat(strrep("=", 70), "\n")

cat(glue("  EFICE_API_URL env var  : '{efice_url}'\n"))
cat(glue("  Constructed device URL : '{efice_url}/journals/{test_journal_id}/devices'\n"))
cat("\n  Common trap: if the base URL already ends in a path segment that the\n")
cat("  function also appends, you get a doubled path, e.g.:\n")
cat("    base  = https://api.efice.eu/api/v1/journals\n")
cat("    built = https://api.efice.eu/api/v1/journals/journals/{id}/devices  <- 404\n\n")
