# ============================================================================
# Poseidat_data_check.R
#
# Quick diagnostic: compare what is in raw parquet vs thinned parquet.
# Run sections manually to identify where data is missing.
# ============================================================================

library(arrow)
library(dplyr)
library(purrr)
library(lubridate)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

base <- .onedrive()

# ============================================================================
# 1. RAW PARQUET — file inventory
# ============================================================================

cat("\n=== RAW PARQUET FILES ===\n")

raw_files <- list.files(
  file.path(base, "raw"),
  pattern    = "\\.parquet$",
  recursive  = TRUE,
  full.names = TRUE
)

cat(sprintf("Total raw files: %d\n\n", length(raw_files)))

raw_inventory <- tibble(path = raw_files) |>
  mutate(
    vessel_name = basename(dirname(dirname(dirname(path)))),
    year_month  = paste(basename(dirname(dirname(path))),
                        basename(dirname(path)), sep = "/"),
    filename    = basename(path),
    size_kb     = round(file.size(path) / 1024, 1),
    modified    = file.mtime(path)
  ) |>
  arrange(vessel_name, year_month, filename)

raw_inventory |>
  select(vessel_name, year_month, filename, size_kb, modified) |>
  print(n = 100)

# ============================================================================
# 2. RAW PARQUET — date range per vessel from actual data
# ============================================================================

cat("\n=== RAW PARQUET — DATE RANGE PER VESSEL ===\n")

raw_ranges <- map_dfr(raw_files, function(f) {
  tryCatch({
    df <- read_parquet(f, col_select = c("journal_id", "datetime", "value_type"))
    tibble(
      path        = f,
      vessel_name = basename(dirname(dirname(dirname(f)))),
      n_records   = nrow(df),
      first_obs   = min(df$datetime, na.rm = TRUE),
      last_obs    = max(df$datetime, na.rm = TRUE),
      value_types = paste(sort(unique(df$value_type)), collapse = ", ")
    )
  }, error = function(e) {
    tibble(path = f, vessel_name = NA, n_records = NA,
           first_obs = NA, last_obs = NA, value_types = NA)
  })
})

raw_summary <- raw_ranges |>
  group_by(vessel_name) |>
  summarise(
    n_files     = n(),
    n_records   = sum(n_records, na.rm = TRUE),
    first_obs   = min(first_obs, na.rm = TRUE),
    last_obs    = max(last_obs, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(across(c(first_obs, last_obs), ~ format(., "%Y-%m-%d %H:%M")))

cat("\nSummary by vessel:\n")
print(raw_summary, n = 20)

# ============================================================================
# 3. THINNED PARQUET — file inventory
# ============================================================================

cat("\n=== THINNED PARQUET FILES ===\n")

thinned_dir   <- file.path(base, "processed", "thinned")
thinned_files <- list.files(
  thinned_dir,
  pattern    = "\\.parquet$",
  recursive  = TRUE,
  full.names = TRUE
)

cat(sprintf("Total thinned files: %d\n\n", length(thinned_files)))

if (length(thinned_files) > 0) {
  tibble(path = thinned_files) |>
    mutate(
      vessel_name = basename(dirname(path)),
      filename    = basename(path),
      size_kb     = round(file.size(path) / 1024, 1),
      modified    = file.mtime(path)
    ) |>
    select(vessel_name, filename, size_kb, modified) |>
    arrange(vessel_name, filename) |>
    print(n = 50)
}

# ============================================================================
# 4. THINNED PARQUET — date range per vessel from actual data
# ============================================================================

cat("\n=== THINNED PARQUET — DATE RANGE PER VESSEL ===\n")

if (length(thinned_files) > 0) {

  thinned_ranges <- map_dfr(thinned_files, function(f) {
    tryCatch({
      df <- read_parquet(f, col_select = c("vessel_name", "datetime", "value_type"))
      tibble(
        file        = basename(f),
        vessel_name = basename(dirname(f)),
        n_records   = nrow(df),
        first_obs   = min(df$datetime, na.rm = TRUE),
        last_obs    = max(df$datetime, na.rm = TRUE)
      )
    }, error = function(e) tibble(file = basename(f), vessel_name = NA,
                                   n_records = NA, first_obs = NA, last_obs = NA))
  })

  thinned_ranges |>
    mutate(across(c(first_obs, last_obs), ~ format(., "%Y-%m-%d %H:%M"))) |>
    arrange(vessel_name, file) |>
    print(n = 50)

}

# ============================================================================
# 5. COMPARISON — raw last_obs vs thinned last_obs per vessel
# ============================================================================

cat("\n=== COMPARISON: RAW vs THINNED last observation ===\n")

if (length(thinned_files) > 0) {

  thinned_latest <- thinned_ranges |>
    group_by(vessel_name) |>
    slice_max(last_obs, n = 1) |>
    summarise(thinned_last = max(last_obs, na.rm = TRUE), .groups = "drop")

  raw_latest <- raw_ranges |>
    group_by(vessel_name) |>
    summarise(raw_last = max(last_obs, na.rm = TRUE), .groups = "drop")

  full_join(raw_latest, thinned_latest, by = "vessel_name") |>
    mutate(
      gap_hours = round(as.numeric(difftime(raw_last, thinned_last,
                                            units = "hours")), 1),
      status    = case_when(
        is.na(thinned_last) ~ "NO THINNED FILE",
        gap_hours > 2       ~ sprintf("BEHIND by %.1f hours", gap_hours),
        TRUE                ~ "up to date"
      )
    ) |>
    mutate(across(c(raw_last, thinned_last), ~ format(., "%Y-%m-%d %H:%M"))) |>
    arrange(vessel_name) |>
    print(n = 20)

} else {
  cat("No thinned files found — run poseidat_thin(save = TRUE)\n")
}
