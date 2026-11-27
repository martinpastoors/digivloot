# ============================================================================
# Poseidat_sandbox.R
#
# Interactive exploration of the EFICE API and local Parquet data.
# Run sections manually — each section is self-contained.
#
# Structure:
#   STEP 0  — Setup and config load
#   STEP 1  — Explore the EFICE API (vessels, devices, value_types, availability)
#   STEP 2  — Download one device in memory and plot
#   STEP 3  — Sync one device to Parquet
#   STEP 4  — Full sync: all vessels, all devices
#   STEP 5  — Verify stored data (files, value_types, daily counts, summary)
#   STEP 5b — Compare API availability vs local Parquet
#   STEP 6  — Thin to 1-minute intervals and save
#   STEP 7  — Plots and overviews
#
# Prerequisites: .Renviron must contain EFICE_API_URL, EFICE_TOKEN,
#                ONEDRIVE_POSEIDAT. See Poseidat_config.R header.
# ============================================================================

library(httr2)
library(dplyr)
library(purrr)
library(arrow)
library(lubridate)
library(ggplot2)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))


# ============================================================================
# STEP 0 — Setup
# ============================================================================

setup_poseidat_sync()

# Load vessel config (journals + device labels)
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

# pefa_vessels <- get_vessels("pefa")
# print(pefa_vessels)

# ============================================================================
# STEP 1 — Explore the EFICE API
# ============================================================================

# 1a. List all journals (vessels) accessible with your token
efice_vessels <- get_vessels("efice")
print(efice_vessels)

# 1b. Inspect a single journal — returns vessel name, flag state, hull number
adp <- .get_adapter("efice")

# ============================================================
i <-3
journal_id   <- efice_vessels$journal_id[i]
sample_start <- format(Sys.time() - 30 * 86400, "%Y-%m-%dT%H:%M:%SZ")
end_dt       <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

# Fetch a sample to see what device IDs appear
resp <- request(paste0(adp$base_url, "/journals/entries")) |>
  req_auth_bearer_token(adp$token) |>
  req_url_query(
    journalId     = journal_id,
    startDate     = sample_start,
    endDate       = end_dt,
    limit         = 500L,
    sortBy        = "entry_datetime",
    sortDirection = "descending"
  ) |>
  req_timeout(60) |>
  req_perform()

data <- resp_body_json(resp)
cat("total entries:", data$total, "\n")

# Extract unique device IDs and value types seen
tibble(item = data$items) |>
  rowwise() |>
  mutate(
    device_id  = as.character(item$device_id %||% NA_character_),
    value_type = as.character(item$value$type %||% NA_character_)
  ) |>
  ungroup() |>
  select(device_id, value_type) |>
  distinct() |>
  arrange(device_id) |>
  print()


# ============================================================

resp <- request(paste0(adp$base_url, "/journals/",
                       efice_vessels$journal_id[i])) |>
  req_auth_bearer_token(adp$token) |>
  req_perform()
str(resp_body_json(resp), max.level = 2)

# 1c. List devices per vessel (sampled from recent entries)
efice_devices <- get_devices("efice", efice_vessels$journal_id[i])
print(efice_devices)

# 1d. Check which value_types each device produces
#     (samples 50 entries per device over a date range)
for (i in seq_len(nrow(efice_vessels))) {
  jid  <- efice_vessels$journal_id[i]
  name <- efice_vessels$vessel_name[i]
  cat(sprintf("\n=== %s (%s) ===\n", name, jid))

  devices <- get_devices("efice", jid)

  # Build label lookup for this vessel by journal_id
  cfg_row    <- which(vessel_config$journal_id == jid)
  dev_labels <- if (length(cfg_row) == 1) vessel_config$device_labels[[cfg_row]] else list()

  for (j in seq_len(nrow(devices))) {
    did   <- devices$device_id[j]
    label <- dev_labels[[did]]
    label <- if (is.null(label)) "(unlabelled)" else label

    fmt_dt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ")

    # Sample 50 records for value_type detection + total count
    resp <- request(paste0(adp$base_url, "/journals/entries")) |>
      req_auth_bearer_token(adp$token) |>
      req_url_query(
        journalId = jid,
        deviceId  = did,
        startDate = "2020-01-01T00:00:00Z",
        endDate   = fmt_dt(Sys.time()),
        limit     = 50,
        skip      = 0
      ) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()

    data  <- resp_body_json(resp)
    types <- unique(sapply(data$items, \(x) x$value$type))
    total <- data$total %||% 0L

    parse_dt <- function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # API returns descending by default: skip=0 = most recent = last_dt
    last_dt <- tryCatch({
      r <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(journalId = jid, deviceId = did,
                      startDate = "2020-01-01T00:00:00Z",
                      endDate   = fmt_dt(Sys.time()),
                      limit = 1L, skip = 0L) |>
        req_error(is_error = \(r) FALSE) |> req_timeout(15) |> req_perform()
      items <- resp_body_json(r)$items
      if (length(items) > 0) parse_dt(items[[1]]$entry_datetime) else NA
    }, error = function(e) NA)

    # skip=total-1 = oldest record = first_dt
    first_dt <- tryCatch({
      skip_to <- max(0L, as.integer(total) - 1L)
      r <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(journalId = jid, deviceId = did,
                      startDate = "2020-01-01T00:00:00Z",
                      endDate   = fmt_dt(Sys.time()),
                      limit = 1L, skip = skip_to) |>
        req_error(is_error = \(r) FALSE) |> req_timeout(15) |> req_perform()
      items <- resp_body_json(r)$items
      if (length(items) > 0) parse_dt(items[[1]]$entry_datetime) else NA
    }, error = function(e) NA)

    # Sanity check — if first > last the API ordering assumption was wrong; swap
    if (!is.na(first_dt) && !is.na(last_dt) && first_dt > last_dt) {
      tmp <- first_dt; first_dt <- last_dt; last_dt <- tmp
    }

    cat(sprintf("  ...%s  %-20s  types: %-20s  total: %s  [%s -> %s]\n",
                substr(did, nchar(did) - 7, nchar(did)),
                label,
                paste(types, collapse = ", "),
                if (is.na(total) || is.null(total)) "?" else format(as.integer(total), big.mark = ","),
                if (is.na(first_dt) || !inherits(first_dt, "POSIXct")) "?" else format(first_dt, "%Y-%m-%d %H:%M"),
                if (is.na(last_dt)  || !inherits(last_dt,  "POSIXct")) "?" else format(last_dt,  "%Y-%m-%d %H:%M")))
    Sys.sleep(0.3)
  }
}

# 1e. Check monthly data availability for one device
check_monthly <- function(source, journal_id, device_id, year = 2026) {
  adp    <- .get_adapter(source)
  fmt_dt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ")

  cat(sprintf("\nData availability: %s / ...%s / %d\n",
              source,
              substr(device_id, nchar(device_id) - 7, nchar(device_id)),
              year))
  cat(sprintf("%-10s %10s\n", "Month", "Records"))
  cat(strrep("-", 24), "\n")

  for (mo in 1:12) {
    start <- as.POSIXct(sprintf("%d-%02d-01", year, mo), tz = "UTC")
    end   <- as.POSIXct(sprintf("%d-%02d-01",
                                if (mo == 12) year + 1 else year,
                                if (mo == 12) 1L else mo + 1L), tz = "UTC")

    total <- tryCatch({
      resp <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(journalId = journal_id, deviceId = device_id,
                      startDate = fmt_dt(start), endDate = fmt_dt(end),
                      limit = 1L, skip = 0L) |>
        req_error(is_error = \(r) FALSE) |>
        req_timeout(15) |>
        req_perform()
      resp_body_json(resp)$total %||% 0L
    }, error = function(e) NA_integer_)

    bar <- if (!is.na(total) && total > 0)
      strrep("|", min(20L, ceiling(total / 5000L))) else ""
    cat(sprintf("%-10s %10s  %s\n",
                month.abb[mo],
                if (is.na(total)) "error" else format(total, big.mark = ","),
                bar))
    Sys.sleep(0.2)
  }
}

# Example: check availability for Willem Jacob TRAWL_TENSION
check_monthly(
  source     = "efice",
  journal_id = get_journal_id(vessel_config, "Willem Jacob"),
  device_id  = get_device_by_label(vessel_config, "Willem Jacob", "TRAWL_TENSION"),
  year       = 2026
)


# ============================================================================
# STEP 2 — Download data for one device (in memory, no parquet save)
# ============================================================================

t_start <- Sys.time()

trawl <- download_measurements(
  source              = "efice",
  journal_id          = get_journal_id(vessel_config, "Willem Jacob"),
  device_id           = get_device_by_label(vessel_config, "Willem Jacob", "TRAWL_TENSION"),
  start_time          = as.POSIXct("2026-02-01", tz = "UTC"),
  end_time            = as.POSIXct("2026-03-04", tz = "UTC"),
  page_size           = 10000,
  sleep_between_pages = 0
)

cat(sprintf("Downloaded %d records in %s\n",
            nrow(trawl),
            format(round(Sys.time() - t_start, 1))))

glimpse(trawl)
summary(trawl[, c("traction_port", "traction_center", "traction_starboard",
                  "shooted_length_port", "shooted_length_starboard")])

# Quick trawl tension plot — towing periods only (traction_center > 500 kg)
trawl |>
  mutate(time_bin = floor(as.numeric(datetime) / 60)) |>
  group_by(time_bin) |>
  slice(1) |>
  ungroup() |>
  filter(traction_center > 500) |>
  tidyr::pivot_longer(
    cols      = c(traction_port, traction_center, traction_starboard),
    names_to  = "warp",
    values_to = "traction_kg"
  ) |>
  ggplot(aes(x = datetime, y = traction_kg, colour = warp)) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Willem Jacob — Trawl tension (towing only)",
       x = NULL, y = "Traction (kg)") +
  theme_bw()


# ============================================================================
# STEP 3 — Sync one device to parquet
# ============================================================================

t_start <- Sys.time()

sync_device(
  source              = "efice",
  journal_id          = get_journal_id(vessel_config, "Willem Jacob"),
  device_id           = get_device_by_label(vessel_config, "Willem Jacob", "POSITION"),
  vessel_name         = "Willem Jacob",
  lookback_days       = 30,
  page_size           = 5000,
  sleep_between_pages = 0
)

cat(sprintf("Sync completed in %s\n", format(round(Sys.time() - t_start, 1))))

# Verify files landed in the correct YYYY/MM folders
list.files(file.path(.onedrive(), "raw", "efice"),
           recursive = TRUE, pattern = "\\.parquet$")


# ============================================================================
# STEP 4 — Full sync: all vessels, all devices
# ============================================================================

t_start <- Sys.time()

sync_all_vessels(
  vessel_config       = vessel_config,
  sources             = "efice",
  lookback_days       = 30,       # ignored when force_full = TRUE
  force_full          = FALSE,    # set TRUE only for initial backfill
  page_size           = 5000,
  sleep_between_pages = 0
)

cat(sprintf("Full sync completed in %s\n", format(round(Sys.time() - t_start, 1))))

# Check sync state — last sync time and record counts per vessel/device
print_sync_state()


# ============================================================================
# STEP 5 — Verify stored data
# ============================================================================

# What files exist and how large?
poseidat_files() |> print(n = 50)

# What value_types are stored per vessel / device?
poseidat_value_types()

# Daily record counts — useful for spotting gaps or unexpected spikes
poseidat_daily(source = "efice") |> print(n = 60)

# Statistical summary (min/mean/max) per value_type
poseidat_summary(source = "efice")


# ============================================================================
# STEP 5b — Compare API availability vs local parquet
#
# Shows first/last datetime from the API (what is available to download)
# alongside first/last from local parquet (what has already been synced).
# Useful for spotting gaps between what the API holds and what is stored.
# ============================================================================

# --- Local parquet summary with readable labels ---
cfg_labels <- vessel_config |>
  dplyr::select(journal_id, vessel_name, device_labels) |>
  tidyr::unnest_longer(device_labels,
                       indices_to = "device_id",
                       values_to  = "label") |>
  dplyr::select(vessel_name, device_id, label)

parquet_range <- poseidat_value_types(source = "efice") |>
  left_join(cfg_labels, by = c("vessel_name", "device_id")) |>
  mutate(label = coalesce(label,
                          substr(device_id, nchar(device_id) - 7, nchar(device_id)))) |>
  dplyr::select(vessel_name, label, value_type,
                parquet_n     = n_records,
                parquet_first = first_obs,
                parquet_last  = last_obs)

# --- API summary (first/last per device) ---
adp      <- .get_adapter("efice")
fmt_dt   <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ")
parse_dt <- function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

api_range <- map_dfr(seq_len(nrow(efice_vessels)), function(i) {
  jid        <- efice_vessels$journal_id[i]
  name       <- efice_vessels$vessel_name[i]
  cfg_row    <- which(vessel_config$journal_id == jid)
  dev_labels <- if (length(cfg_row) == 1) vessel_config$device_labels[[cfg_row]] else list()
  devices    <- get_devices("efice", jid)

  map_dfr(seq_len(nrow(devices)), function(j) {
    did   <- devices$device_id[j]
    label <- dev_labels[[did]]
    label <- if (is.null(label)) substr(did, nchar(did) - 7, nchar(did)) else label

    # Total count + value_types from a small sample
    resp  <- request(paste0(adp$base_url, "/journals/entries")) |>
      req_auth_bearer_token(adp$token) |>
      req_url_query(journalId = jid, deviceId = did,
                    startDate = "2020-01-01T00:00:00Z",
                    endDate   = fmt_dt(Sys.time()),
                    limit = 10L, skip = 0L) |>
      req_error(is_error = \(r) FALSE) |> req_timeout(15) |> req_perform()
    data  <- resp_body_json(resp)
    total <- data$total %||% 0L
    types <- unique(sapply(data$items, \(x) x$value$type))

    # Most recent (skip=0, API returns descending by default)
    api_last <- tryCatch({
      r <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(journalId = jid, deviceId = did,
                      startDate = "2020-01-01T00:00:00Z",
                      endDate   = fmt_dt(Sys.time()),
                      limit = 1L, skip = 0L) |>
        req_error(is_error = \(r) FALSE) |> req_timeout(15) |> req_perform()
      items <- resp_body_json(r)$items
      if (length(items) > 0) parse_dt(items[[1]]$entry_datetime) else NA
    }, error = function(e) NA)

    # Oldest (skip = total - 1)
    api_first <- tryCatch({
      r <- request(paste0(adp$base_url, "/journals/entries")) |>
        req_auth_bearer_token(adp$token) |>
        req_url_query(journalId = jid, deviceId = did,
                      startDate = "2020-01-01T00:00:00Z",
                      endDate   = fmt_dt(Sys.time()),
                      limit = 1L, skip = max(0L, as.integer(total) - 1L)) |>
        req_error(is_error = \(r) FALSE) |> req_timeout(15) |> req_perform()
      items <- resp_body_json(r)$items
      if (length(items) > 0) parse_dt(items[[1]]$entry_datetime) else NA
    }, error = function(e) NA)

    # Guarantee chronological order
    if (!is.na(api_first) && !is.na(api_last) && api_first > api_last) {
      tmp <- api_first; api_first <- api_last; api_last <- tmp
    }

    tibble(vessel_name = name, label = label,
           value_type  = paste(types, collapse = ", "),
           api_n       = as.integer(total),
           api_first   = api_first,
           api_last    = api_last)
  })
})

# --- Parquet summary aggregated to device level (sum across value_types) ---
parquet_by_device <- parquet_range |>
  group_by(vessel_name, label) |>
  summarise(
    parquet_types = paste(sort(unique(value_type)), collapse = ", "),
    parquet_n     = sum(parquet_n, na.rm = TRUE),
    parquet_first = min(parquet_first, na.rm = TRUE),
    parquet_last  = max(parquet_last,  na.rm = TRUE),
    .groups = "drop"
  )

# --- Side-by-side comparison joined at device level ---
comparison <- full_join(
  api_range,
  parquet_by_device,
  by = c("vessel_name", "label")
) |>
  mutate(
    n_missing   = api_n - coalesce(parquet_n, 0L),
    behind_days = round(as.numeric(difftime(
      api_last, coalesce(parquet_last, api_first), units = "days")), 1)
  ) |>
  dplyr::select(vessel_name, label, value_type, parquet_types,
                api_n, api_first, api_last,
                parquet_n, parquet_first, parquet_last,
                n_missing, behind_days) |>
  arrange(vessel_name, label)

cat("\n=== API vs Parquet comparison (device level) ===\n")
comparison |>
  mutate(across(c(api_first, api_last, parquet_first, parquet_last),
                \(x) format(x, "%Y-%m-%d %H:%M"))) |>
  print(n = 50, width = 160)


# ============================================================================
# STEP 6 — Thin to 1-minute intervals and map
# ============================================================================

# Thin all raw data to 1-minute averages and save to processed/thinned/
poseidat_thin(save = TRUE)

# Interactive vessel track map — all vessels, thinned parquet, 5-min display thinning
poseidat_map()

# Map specific vessels over a date range — using thinned parquet (default)
pos <- poseidat_positions(
  vessel_name  = c("Lub Senior", "Willem Jacob"),
  data_source  = "thinned",
  start_date   = as.Date("2026-02-20"),
  end_date     = Sys.Date(),
  thin_minutes = 5
)
poseidat_map(pos)

# Map using raw parquet for maximum positional detail
poseidat_map(
  data_source  = "raw",
  vessel_name  = "Willem Jacob",
  start_date   = as.Date("2026-04-01"),
  end_date     = as.Date("2026-04-07"),
  thin_minutes = 2
)

# ============================================================================
# STEP 7 — Plots and overviews
# ============================================================================

# Daily data availability calendar heatmap (all vessels x value_types)
poseidat_availability()


# ── Operational overview: fuel, speed, traction, depth ──────────────────────
# All vessels, thinned data (default)
poseidat_plot_operational()

# Single vessel
poseidat_plot_operational(vessel_name = "Willem Jacob")

# Multiple vessels, date filter
poseidat_plot_operational(
  vessel_name = c("Polar", "Lub Senior"),
  start_date  = as.Date("2026-05-01")
)

# Short window — x-axis auto-switches to daily labels
poseidat_plot_operational(
  start_date = as.Date("2026-05-08"),
  end_date   = as.Date("2026-05-12")
)

# Using raw parquet for full ~10-sec resolution
poseidat_plot_operational(
  data_source = "raw",
  vessel_name = "Lub Senior",
  start_date  = as.Date("2026-05-10"),
  end_date    = as.Date("2026-05-12")
)


# ── Scalar overview: all value_types x all vessels ──────────────────────────
poseidat_plot_overview()
poseidat_plot_overview(vessel_name = "Willem Jacob")

# With raw data
poseidat_plot_overview(
  data_source = "raw",
  vessel_name = "Lub Senior",
  start_date  = as.Date("2026-05-01")
)

vessel <- "Willem Jacob"
vessel <- "Lub Senior"
vessel <- "Polar"
poseidat_plot_overview(
  vessel_name = c(vessel),
  start_date  = as.Date("2026-04-13"),
  value_types = c("FUEL_CONSUMPTION", "TEMPERATURE", "DEPTH", "Speed (SOG)", "Course (COG)",
                  "Traction center","Traction port", "Traction starboard",
                  "Shooted length center","Shooted length port","Shooted length starboard",
                  "Latitude", "Longitude"),
  by_device   = TRUE
)

poseidat_plot_overview(
  vessel_name = c("Lub Senior","Willem Jacob", "Polar"),
  start_date  = as.Date("2026-05-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

poseidat_plot_overview(
  vessel_name = c("Willem Jacob"),
  start_date  = as.Date("2026-04-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

# ── Trawl tension ────────────────────────────────────────────────────────────
# Towing periods only (traction_center > 500 kg), thinned parquet (default)
poseidat_plot_trawl()

# With wire length panel, specific vessel
poseidat_plot_trawl(
  vessel_name = "Willem Jacob",
  show_length = TRUE
)

# Multiple vessels, faceted side by side
poseidat_plot_trawl(
  vessel_name  = c("Lub Senior", "Polar"),
  towing_only  = TRUE,
  min_traction = 300,
  scales       = "free_y",
  ncol         = 2
)

# Raw parquet — full resolution during a short trip
poseidat_plot_trawl(
  data_source  = "raw",
  vessel_name  = "Willem Jacob",
  start_date   = as.Date("2026-04-01"),
  end_date     = as.Date("2026-04-03"),
  towing_only  = FALSE
)


# ── Fuel consumption time series ─────────────────────────────────────────────
# All vessels, thinned parquet (default)
poseidat_plot_timeseries("FUEL_CONSUMPTION")

# One vessel with raw/thinned overlay
poseidat_plot_timeseries(
  "FUEL_CONSUMPTION",
  vessel_name = c("Willem Jacob", "Lub Senior"),
  start_date  = as.Date("2026-05-01"),
  data_source = "thinned",
  by_device = TRUE,
  # device_label = "FUEL_1",
  facet_by  = "device_x_vessel"
)

# Multiple vessels, fixed y-axis, 2-column layout, recent period
poseidat_plot_timeseries(
  "FUEL_CONSUMPTION",
  vessel_name = c("Polar", "Lub Senior", "Willem Jacob"),
  start_date  = as.Date("2026-05-11"),
  data_source = "thinned",
  facet_by    = "vessel",
  scales      = "fixed",
  ncol        = 2
)

# Per-device breakdown for one vessel
poseidat_plot_timeseries(
  "FUEL_CONSUMPTION",
  data_source = "thinned",
  vessel_name = c("Lub Senior", "Polar", "Willem Jacob"),
  start_date  = as.Date("2026-04-13"),
  facet_by    = "device_x_vessel",
  by_device   = TRUE
)

# Sub-daily window — use raw for full detail; x-axis auto-shows HH:MM labels
poseidat_plot_timeseries(
  "FUEL_CONSUMPTION",
  data_source = "raw",
  vessel_name = "Lub Senior",
  start_date  = as.Date("2026-03-05"),
  end_date    = as.Date("2026-03-05"),
  by_device   = TRUE
)


# ── Position time series (lat / lon over time) ───────────────────────────────
# Latitude only (default)
poseidat_plot_timeseries(
  "POSITION",
  vessel_name = "Lub Senior",
  start_date  = as.Date("2026-05-01"),
  end_date    = as.Date("2026-05-12")
)

# Both coordinates stacked
poseidat_plot_timeseries(
  c("FUEL_CONSUMPTION", "DEPTH"),
  # c("FUEL_CONSUMPTION", "POSITION"),
  position_component = "both",
  start_date = as.Date("2026-05-01"),
  end_date   = as.Date("2026-05-18"),
  scales     = "fixed",
  ncol       = 2
)

# Longitude across all vessels, fixed y-scale
poseidat_plot_timeseries(
  "POSITION",
  position_component = "longitude",
  scales = "fixed",
  ncol   = 3
)

# Latitude across all vessels
poseidat_plot_timeseries(
  "POSITION",
  position_component = "latitude",
  scales = "fixed",
  ncol   = 3
)

poseidat_plot_timeseries(
  value_type   = c("DEPTH", "TRAWL_TENSION", "FUEL_CONSUMPTION", "TEMPERATURE"),
  vessel_name  = "Willem Jacob",   # or whichever vessel this is
  by_device    = TRUE,              # show each fuel/temp sensor as its own line
  facet_by     = "value_type",      # one panel per value type
  scales       = "free_y",
  ncol         = 1,
  start_date   = Sys.Date() - 30, 
  data_source  = "raw"
)

# ── Overviews ─────────────────────────────────────────────────────────────────────

poseidat_plot_overview(
  vessel_name = c("Lub Senior", "Polar", "Willem Jacob"),
  start_date  = as.Date("2026-04-13"),
  value_types = c("FUEL_CONSUMPTION", "DEPTH", "Latitude", "Longitude"),
  by_device   = TRUE
)

# ── Maps ─────────────────────────────────────────────────────────────────────
# All vessels, thinned parquet, 5-minute display thinning (default)
poseidat_map()

# Specific vessels and period
poseidat_map(
  vessel_name  = c("Lub Senior", "Polar", "Willem Jacob"),
  start_date   = as.Date("2026-05-01"),
  end_date     = as.Date("2026-06-15"),
  thin_minutes = 5
)

poseidat_map(
  vessel_name  = c("Polar"),
  start_date   = as.Date("2026-05-11"),
  end_date     = as.Date("2026-06-01"),
  thin_minutes = 5
)

# Maximum detail from raw parquet for a single short trip
poseidat_map(
  data_source  = "raw",
  vessel_name  = "Willem Jacob",
  start_date   = as.Date("2026-04-01"),
  end_date     = as.Date("2026-04-03"),
  thin_minutes = 1
)

# Pre-build positions and pass to map (useful when reusing same data)
pos <- poseidat_positions(
  vessel_name  = "Lub Senior",
  data_source  = "thinned",
  start_date   = as.Date("2026-05-01"),
  end_date     = Sys.Date(),
  thin_minutes = 5
)
poseidat_map(pos)


# ── Observation frequency analysis ───────────────────────────────────────────
freq <- build_poseidat_frequency(source = "efice")
plot_poseidat_frequency(freq)
table_poseidat_frequency(freq)


# ── New sensor audit ─────────────────────────────────────────────────────────
new_sensors <- audit_new_sensors(vessel_config, source = "efice")
new_sensors <- audit_new_sensors(vessel_config, source = "pefa")

new_sensors |>
  filter(!is.na(device_id)) |>
  select(vessel_name, device_id, value_types, first_seen, n_records) |>
  print()

# Once identified, add to vessel_config:
# device_labels[[<vessel_row>]][[<new_uuid>]] <- "TRAWL_TENSION"
# saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))


# ── Device label inspection ───────────────────────────────────────────────────
vessel_config |>
  dplyr::select(vessel_name, device_labels) |>
  rowwise() |>
  mutate(labels = paste(names(device_labels), device_labels,
                        sep = " = ", collapse = "\n")) |>
  dplyr::select(vessel_name, labels) |>
  print()



# check devices on aravis
journal_id_aravis <- "940acddd-b61c-4d9a-a20f-ff2cd87d9156"
dev <- get_devices("pefa", journal_id_aravis)
print(dev, n=100)

dev |> count(device_type, sort = TRUE)   # is there a SCALE type?

library(stringr)
kandidaten <- dev |>
  filter(str_detect(str_to_lower(paste(device_type, device_name, equipment_name)),
                    "scale|weeg|weigh|marelec"))
print(kandidaten)


adp   <- .get_adapter("pefa")
einde <- as.POSIXct(Sys.time(), tz = "UTC")
start <- einde - 90 * 24 * 3600

for (i in seq_len(nrow(dev))) {
  m <- adp$get_meas(adp$base_url, adp$token, journal_id_aravis,
                    dev$device_id[i], start, einde)
  cat(dev$device_name[i], "| n =", nrow(m),
      "| types:", paste(unique(m$value_type), collapse = ", "), "\n")
}

resp <- httr2::request(adp$base_url) |>
  httr2::req_url_path_append("api", "v1", "Journals", journal_id_aravis,
                             "measurements", scale_device_id) |>
  httr2::req_auth_bearer_token(adp$token) |>
  httr2::req_url_query(start = as.integer(as.numeric(start)),
                       end   = as.integer(as.numeric(einde))) |>
  httr2::req_perform()

str(httr2::resp_body_json(resp)[[1]], max.level = 4)

# ── One-time migration to monthly files ──────────────────────────────────────
# source("R/poseidat/Poseidat_functions.R")
# setup_poseidat_sync()
# vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
# migrate_to_monthly(vessel_config, dry_run = TRUE)   # check output first
# migrate_to_monthly(vessel_config, dry_run = FALSE)  # then execute


# ── Memory cleanup ────────────────────────────────────────────────────────────
# gc()
# gc()  # run twice — Arrow lazy objects sometimes need two passes


# Get the fuel device for UK158
adp <- .get_adapter("efice")

resp <- request(paste0(adp$base_url, "/journals/entries")) |>
  req_auth_bearer_token(adp$token) |>
  req_url_query(
    journalId  = "f4fc3064-fd63-4c9a-9635-0b292d534827",
    deviceId   = "61326139-7379-3463-0000-000200000008",
    startDate  = "2024-01-01T00:00:00Z",
    endDate    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    limit      = 5
  ) |>
  req_perform()

data <- resp_body_json(resp)
cat("Total entries for this device:", data$total, "\n")



vessel_config |>
  filter(source == "efice", enabled) |>
  mutate(
    devices = lapply(journal_id, \(jid) {
      cat(sprintf("Fetching devices for %s ...\n", vessel_name))
      .efice_get_devices(adp$base_url, adp$token, jid)
    })
  ) |>
  select(vessel_name, devices) |>
  tidyr::unnest(devices) |>
  mutate(
    suffix = substr(device_id, nchar(device_id) - 11, nchar(device_id)),
    label  = map_device_label(device_id, device_labels)  # if you have this helper
  )



# Reload the updated config first
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

# Get the Willem Jacob row
wj <- vessel_config |> filter(vessel_name == "Willem Jacob")

# View full state — find Willem Jacob entries
state <- get_sync_state()
wj_keys <- names(state$vessels) |> grep("f4fc3064", x = _, value = TRUE)
state$vessels[wj_keys]

# Set starting point for the new sensor
update_last_sync_time(
  source       = "efice",
  journal_id   = "f4fc3064-fd63-4c9a-9635-0b292d534827",
  device_id    = "61326139-7379-3463-0000-000200000008",
  sync_time    = as.POSIXct("2026-02-15 00:00:00", tz = "UTC"),
  record_count = 0
)

vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

journal_id <- "f4fc3064-fd63-4c9a-9635-0b292d534827"
device_ids <- names(wj$device_labels[[1]])

for (did in device_ids[4]) {
  label <- wj$device_labels[[1]][[did]]
  cat(sprintf("Syncing %s [%s] ...\n", label, did))
  sync_device(
    source      = "efice",
    journal_id  = journal_id,
    device_id   = did,
    vessel_name = "Willem Jacob",
    start_time  = as.POSIXct("2026-02-15 00:00:00", tz = "UTC"),
    end_time    = Sys.time()
  )
}




# ── Confirm the prefix length fix ───────────────────────────────────────────

items      <- raw$items
sample_ids <- items |>
  lapply(\(x) as.character(x$device_id %||% NA_character_)) |>
  unlist() |>
  (\(x) x[!is.na(x)])()

cat(sprintf("First sample_id  : '%s'  (nchar = %d)\n",
            sample_ids[[1]], nchar(sample_ids[[1]])))

prefix_24 <- substr(sample_ids[[1]], 1, 24)
prefix_25 <- substr(sample_ids[[1]], 1, 25)

cat(sprintf("substr(id, 1, 24): '%s'\n", prefix_24))
cat(sprintf("substr(id, 1, 25): '%s'\n", prefix_25))

# Show what candidate 1 looks like with each prefix
cat(sprintf("\nCandidate with prefix 24: '%s'\n",
            paste0(prefix_24, sprintf("0002%08d", 1))))
cat(sprintf("Candidate with prefix 25: '%s'\n",
            paste0(prefix_25, sprintf("0002%08d", 1))))

# Compare against a known-good device ID from sync state
cat(sprintf("\nKnown good device  : '%s'\n", live_device_ids[[1]]))



# ── Probe the first candidate and inspect the raw response ──────────────────

prefix     <- substr(sample_ids[[1]], 1, 24)
candidates <- paste0(prefix, sprintf("0002%08d", 1:16))

cat("Probing first 3 candidates:\n\n")

for (cid in candidates[1:3]) {
  resp <- tryCatch(
    .http_get(efice_url, efice_token, "journals/entries",
              params = list(
                journalId = test_journal_id,
                deviceId  = cid,
                startDate = "2020-01-01T00:00:00Z",
                endDate   = end_dt,
                limit     = 1L,
                skip      = 0L
              ),
              timeout_sec = 10,
              max_tries   = 1L),
    error = function(e) list(error = conditionMessage(e))
  )
  
  if (!is.null(resp$error)) {
    cat(sprintf("  %s  →  ERROR: %s\n", cid, resp$error))
  } else {
    cat(sprintf("  %s  →  total: %s,  items: %d,  keys: %s\n",
                cid,
                resp$total %||% "NULL",
                length(resp$items %||% list()),
                paste(names(resp), collapse = ", ")))
  }
}

# Also check: does the API accept deviceId at all?
# Try without deviceId to confirm the endpoint responds normally
cat("\nControl — same call without deviceId:\n")
resp_nodev <- .http_get(efice_url, efice_token, "journals/entries",
                        params = list(
                          journalId = test_journal_id,
                          startDate = "2026-05-01T00:00:00Z",
                          endDate   = end_dt,
                          limit     = 1L,
                          skip      = 0L
                        ),
                        timeout_sec = 10)
cat(sprintf("  total: %s,  items: %d\n",
            resp_nodev$total %||% "NULL",
            length(resp_nodev$items %||% list())))

# And check what device_id field looks like in that one item
if (length(resp_nodev$items) > 0) {
  item <- resp_nodev$items[[1]]
  cat(sprintf("  item$device_id   : %s\n", item$device_id   %||% "NULL"))
  cat(sprintf("  item keys        : %s\n", paste(names(item), collapse = ", ")))
}



print_devices ("efice", vessel_config)












freq <- build_poseidat_frequency(
  sources    = "efice",
  start_date = as.Date("2026-04-25"),
  end_date   = Sys.Date(),
  vessel_config = vessel_config
)
plot_poseidat_frequency(freq, metric = "avg_interval")


# Step 1 — look at the raw value distribution around the spike dates
# to confirm spikes are in the stored parquet, not a plotting artefact
library(dplyr)
library(arrow)

fuel_raw <- open_dataset(file.path(.onedrive(), "raw")) |>
  filter(value_type == "FUEL_CONSUMPTION") |>
  collect() |>
  filter(
    journal_id == "6a5c162e-e967-41b6-9e8a-3cfddf5019b2",
    # vessel_name == "Lub Senior",   # if vessel_name is stored; else filter journal_id
    datetime >= as.POSIXct("2026-05-01", tz = "UTC")
  ) |>
  arrange(datetime)

# Check the actual max values — are they really in the parquet?
fuel_raw |>
  summarise(
    n          = n(),
    min_val    = min(value_numeric, na.rm = TRUE),
    p99        = quantile(value_numeric, 0.99, na.rm = TRUE),
    max_val    = max(value_numeric, na.rm = TRUE)
  )

# Step 2 — look at the timestamps of the largest spikes
fuel_raw |>
  filter(value_numeric > 100) |>
  select(datetime, value_numeric, entry_id) |>
  arrange(desc(value_numeric)) %>% 
  print(n=100)












# Load raw fuel data for device 00000005 on Lub Senior
# fuel_05 <- open_dataset(file.path(.onedrive(), "raw")) |>
#   filter(value_type == "FUEL_CONSUMPTION") |>
#   collect() |>
#   filter(
#     vessel_name == "Lub Senior",
#     stringr::str_ends(device_id, "00000005")
#   ) |>
#   arrange(datetime)

# Load raw fuel data for device 00000005 on Lub Senior
fuel_05 <- fuel_raw|>
  filter(
    stringr::str_ends(device_id, "00000005")
  ) |>
  arrange(datetime)

# Quick look at how the level changes over time using daily medians
fuel_05 |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarise(
    n          = n(),
    median_val = median(value_numeric, na.rm = TRUE),
    p10        = quantile(value_numeric, 0.10, na.rm = TRUE),
    p90        = quantile(value_numeric, 0.90, na.rm = TRUE),
    max_val    = max(value_numeric,      na.rm = TRUE),
    .groups    = "drop"
  ) |>
  print(n = 50)


# Adjust the window once Step 2 tells you the exact day
shift_window <- fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-12 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-16 00:00:00", tz = "UTC")
  ) |>
  select(datetime, value_numeric, entry_id) |>
  arrange(datetime)

# Look at a rolling window around the jump
shift_window |>
  mutate(
    prev_val   = lag(value_numeric),
    step_size  = value_numeric - lag(value_numeric),
    ratio      = value_numeric / lag(value_numeric)
  ) |>
  filter(abs(step_size) > 30) |>   # threshold — adjust to taste
  print(n = 40)


fuel_05 |>
  mutate(
    date          = as.Date(datetime),
    interval_sec  = as.numeric(difftime(datetime, lag(datetime), units = "secs"))
  ) |>
  filter(interval_sec > 0, interval_sec < 300) |>   # exclude gaps/restarts
  group_by(date) |>
  summarise(
    median_interval = median(interval_sec, na.rm = TRUE),
    median_value    = median(value_numeric, na.rm = TRUE),
    .groups         = "drop"
  ) |>
  print(n = 50)


# Step 1 — check for duplicate datetimes on device 00000005
fuel_05 |>
  group_by(datetime) |>
  summarise(
    n_records   = n(),
    n_entry_ids = n_distinct(entry_id),
    values      = paste(sort(unique(round(value_numeric, 2))), collapse = " | "),
    .groups     = "drop"
  ) |>
  filter(n_records > 1) |>
  arrange(desc(n_records)) |>
  print(n = 30)


# How bad is the timestamp collision problem over time?
fuel_05 |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarise(
    n_records            = n(),
    n_unique_timestamps  = n_distinct(datetime),
    duplication_ratio    = round(n_records / n_distinct(datetime), 2),
    median_records_per_ts = median(
      tabulate(match(datetime, unique(datetime)))
    ),
    max_records_per_ts   = max(
      tabulate(match(datetime, unique(datetime)))
    ),
    median_value         = round(median(value_numeric, na.rm = TRUE), 1),
    .groups              = "drop"
  ) |>
  print(n = 50)

# Does the value drift continuously upward within an operating period,
# or does it stay flat/oscillate around a level?
# Plot a short window around the first shift to see the shape clearly

fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-13 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-16 00:00:00", tz = "UTC"),
    value_numeric > 0
  ) |>
  ggplot(aes(x = datetime, y = value_numeric)) +
  geom_line(colour = "firebrick", linewidth = 0.3) +
  labs(
    title    = "Lub Senior / 00000005 — zoom around May 14 shift",
    subtitle = "Raw ~10-sec data",
    x        = NULL, y = "Value"
  ) +
  theme_minimal()

# And check: does the value ever decrease between consecutive records?
# A pure rate should go up and down. A cumulative counter only goes up
# (until reset).
fuel_05 |>
  filter(value_numeric > 0) |>
  arrange(datetime) |>
  mutate(delta = value_numeric - lag(value_numeric)) |>
  summarise(
    pct_increasing = round(100 * mean(delta > 0,  na.rm = TRUE), 1),
    pct_decreasing = round(100 * mean(delta < 0,  na.rm = TRUE), 1),
    pct_flat       = round(100 * mean(delta == 0, na.rm = TRUE), 1),
    median_delta   = round(median(delta,           na.rm = TRUE), 3),
    min_delta      = round(min(delta,              na.rm = TRUE), 3),
    max_delta      = round(max(delta,              na.rm = TRUE), 3)
  )


# Step 1 — zoom into May 27 around the burst period
# Look at a 10-minute window to see the context
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-27 19:10:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-27 19:30:00", tz = "UTC")
  ) |>
  arrange(datetime) |>
  mutate(
    interval_to_next = as.numeric(difftime(lead(datetime), datetime, units = "secs")),
    delta_value      = value_numeric - lag(value_numeric)
  ) |>
  print(n = 100)


# Step 2 — what does the timestamp distribution look like around the burst?
# How many records per second, across the whole burst window
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-27 19:10:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-27 19:30:00", tz = "UTC")
  ) |>
  mutate(second = format(datetime, "%H:%M:%S")) |>
  group_by(datetime) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n)) |>
  print(n = 30)


# Step 3 — look at the revision timestamps, not just entry_datetime
# The 'revision' field records when EFICE received/processed the record
# If many records share an entry_datetime but have spread-out revision times,
# they were submitted in separate API calls — i.e. resubmissions
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-27 19:14:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-27 19:22:00", tz = "UTC")
  ) |>
  arrange(datetime, revision) |>
  select(datetime, revision, value_numeric, entry_id) |>
  print(n = 60)

# Confirm the systematic offset between revision and entry_datetime
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-27 19:14:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-27 19:22:00", tz = "UTC")
  ) |>
  mutate(
    offset_hours = as.numeric(difftime(datetime, revision, units = "hours"))
  ) |>
  summarise(
    mean_offset_hours   = round(mean(offset_hours,   na.rm = TRUE), 2),
    median_offset_hours = round(median(offset_hours, na.rm = TRUE), 2),
    sd_offset_hours     = round(sd(offset_hours,     na.rm = TRUE), 2),
    min_offset_hours    = round(min(offset_hours,    na.rm = TRUE), 2),
    max_offset_hours    = round(max(offset_hours,    na.rm = TRUE), 2)
  )


# How many distinct submission batches hit EFICE, and how large were they?
# The revision timestamp advances ~10 seconds per batch
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-27 19:14:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-27 19:22:00", tz = "UTC")
  ) |>
  mutate(
    # Round revision to nearest 10 seconds to group into submission batches
    revision_batch = lubridate::floor_date(revision, unit = "10 seconds")
  ) |>
  group_by(revision_batch) |>
  summarise(
    n_records        = n(),
    entry_dt_min     = min(datetime),
    entry_dt_max     = max(datetime),
    entry_dt_span_s  = as.numeric(difftime(max(datetime), min(datetime), units = "secs")),
    .groups          = "drop"
  ) |>
  arrange(revision_batch) |>
  print(n = 40)

# Finally — is this resubmission problem confined to May 27,
# or does it affect the whole dataset?
# Check the offset between revision and datetime across all dates
fuel_05 |>
  mutate(
    date         = as.Date(datetime),
    offset_hours = as.numeric(difftime(datetime, revision, units = "hours"))
  ) |>
  group_by(date) |>
  summarise(
    median_offset_h = round(median(offset_hours, na.rm = TRUE), 2),
    pct_with_offset = round(100 * mean(abs(offset_hours) > 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  print(n = 50)


# Find the exact transition records around May 14
# First establish the before/after medians to set context
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-10 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-18 00:00:00", tz = "UTC"),
    value_numeric > 5   # exclude idle/zero records
  ) |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarise(
    n          = n(),
    p10        = round(quantile(value_numeric, 0.10, na.rm = TRUE), 1),
    median_val = round(median(value_numeric,         na.rm = TRUE), 1),
    p90        = round(quantile(value_numeric, 0.90, na.rm = TRUE), 1),
    .groups    = "drop"
  )

# Now zoom to the hour of the actual transition
# Look at consecutive records crossing the boundary
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-05 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-15 00:00:00", tz = "UTC"),
    value_numeric > 5
  ) |>
  arrange(datetime) |>
  mutate(
    delta      = value_numeric - lag(value_numeric),
    ratio      = round(value_numeric / lag(value_numeric), 3)
  ) |>
  # Find the record where the big jump occurs
  # filter(abs(delta) > 20) |>
  select(datetime, revision, value_numeric, delta, ratio, entry_id) %>% 
  ggplot(aes(x=datetime, y = delta)) +
  geom_point()

# On the transition days, do the values cluster into two distinct modes?
# A bimodal distribution would confirm two discrete states
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-13 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-15 00:00:00", tz = "UTC"),
    value_numeric > 5
  ) |>
  ggplot(aes(x = value_numeric)) +
  geom_histogram(binwidth = 5, fill = "firebrick", colour = "white") +
  geom_vline(xintercept = c(45, 134), linetype = "dashed", colour = "steelblue") +
  labs(
    title    = "Lub Senior / 00000005 — value distribution during transition (May 13–15)",
    subtitle = "Two modes would confirm switching between two signal sources",
    x        = "value_numeric", y = "count"
  ) +
  theme_minimal()

# More precisely: classify each record as "low regime" or "high regime"
# and see how the mix evolves hour by hour through the transition
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-13 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-15 00:00:00", tz = "UTC"),
    value_numeric > 5
  ) |>
  mutate(
    hour   = lubridate::floor_date(datetime, unit = "hour"),
    regime = if_else(value_numeric < 90, "low (~45)", "high (~134)")
  ) |>
  group_by(hour, regime) |>
  summarise(n = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = regime, values_from = n, values_fill = 0) |>
  mutate(
    pct_high = round(100 * `high (~134)` / (`high (~134)` + `low (~45)`), 1)
  ) |>
  arrange(hour) |>
  print(n = 50)


# Same analysis for the June transition
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-06-06 00:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-06-09 00:00:00", tz = "UTC"),
    value_numeric > 5
  ) |>
  mutate(
    hour   = lubridate::floor_date(datetime, unit = "hour"),
    regime = case_when(
      value_numeric < 90  ~ "low",
      value_numeric < 200 ~ "mid",
      TRUE                ~ "high"
    )
  ) |>
  group_by(hour, regime) |>
  summarise(n = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = regime, values_from = n, values_fill = 0) |>
  arrange(hour) |>
  print(n = 50)


fuel_05 |>
  filter(value_numeric > 5) |>
  mutate(
    week = paste0(
      lubridate::isoyear(datetime), "-W",
      formatC(lubridate::isoweek(datetime), width = 2, flag = "0")
    )
  ) |>
  ggplot(aes(x = value_numeric)) +
  geom_histogram(binwidth = 5, fill = "firebrick", colour = "white", linewidth = 0.2) +
  facet_wrap(~ week, ncol = 4) +
  labs(
    title    = "Lub Senior / 00000005 — fuel consumption distribution by week",
    subtitle = "Values > 5 only | binwidth = 5",
    x        = "Fuel consumption (value_numeric)",
    y        = "Count"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 7),
    axis.text.y      = element_text(size = 7)
  )


# Run these to get the exact numbers for the report
# before generating it

# 1. Transition timing
fuel_05 |>
  filter(
    datetime >= as.POSIXct("2026-05-13 18:00:00", tz = "UTC"),
    datetime <= as.POSIXct("2026-05-13 21:00:00", tz = "UTC"),
    value_numeric > 5
  ) |>
  mutate(regime = if_else(value_numeric < 90, "low", "high")) |>
  group_by(lubridate::floor_date(datetime, "30 minutes"), regime) |>
  summarise(n = n(), .groups = "drop")

# 2. Regime medians
fuel_05 |>
  filter(value_numeric > 5) |>
  mutate(
    era = case_when(
      datetime < as.POSIXct("2026-05-13 19:00:00", tz = "UTC") ~ "Pre-transition (W19)",
      datetime >= as.POSIXct("2026-05-15 00:00:00", tz = "UTC") &
        datetime < as.POSIXct("2026-06-07 22:00:00", tz = "UTC") ~ "Post-transition 1 (W21-W23)",
      datetime >= as.POSIXct("2026-06-07 22:00:00", tz = "UTC") ~ "Post-transition 2 (W24)",
      TRUE ~ "Transition (W20)"
    )
  ) |>
  group_by(era) |>
  summarise(
    n          = n(),
    median_val = round(median(value_numeric, na.rm = TRUE), 1),
    p10        = round(quantile(value_numeric, 0.10, na.rm = TRUE), 1),
    p90        = round(quantile(value_numeric, 0.90, na.rm = TRUE), 1),
    .groups    = "drop"
  )

# 3. May 27 clock failure extent  
fuel_05 |>
  filter(as.Date(datetime) == "2026-05-27") |>
  mutate(offset_hours = as.numeric(difftime(datetime, revision, units = "hours"))) |>
  filter(offset_hours > 1) |>
  summarise(
    n_affected     = n(),
    revision_start = min(revision),
    revision_end   = max(revision),
    entry_dt_start = min(datetime),
    entry_dt_end   = max(datetime)
  )

# Save the weekly histogram plot to a file
fuel_05 |>
  filter(value_numeric > 5) |>
  mutate(
    week = paste0(
      lubridate::isoyear(datetime), "-W",
      formatC(lubridate::isoweek(datetime), width = 2, flag = "0")
    )
  ) |>
  ggplot(aes(x = value_numeric)) +
  geom_histogram(binwidth = 5, fill = "firebrick", colour = "white", linewidth = 0.2) +
  facet_wrap(~ week, ncol = 4) +
  coord_cartesian(xlim = c(0, 350)) +
  labs(
    title    = "Lub Senior / 00000005 — fuel consumption distribution by week",
    subtitle = "Values > 5 only | binwidth = 5",
    x        = "Fuel consumption (value_numeric)",
    y        = "Count"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 7),
    axis.text.y      = element_text(size = 7)
  )

ggsave(
  filename = "fuel_histogram_by_week.png",
  width    = 10,
  height   = 7,
  dpi      = 150
)







# Visualisatie: entry_datetime vs. revisietijdstempel — klokstoring 27 mei 2026
may27 <- fuel_05 |>
  filter(as.Date(datetime) == "2026-05-27") |>
  mutate(
    offset_hours = as.numeric(difftime(datetime, revision, units = "hours"))
  ) 

# Referentielijn: perfecte 1:1 relatie (entry = revisie)
ref_min <- min(may27$revision)
ref_max <- max(may27$datetime)

ggplot(may27, aes(x = revision, y = datetime)) +
  # 1:1 referentielijn
  geom_abline(
    slope     = 1,
    intercept = as.numeric(as.POSIXct("2026-05-27 00:00:00", tz = "UTC")) -
      as.numeric(as.POSIXct("2026-05-27 00:00:00", tz = "UTC")),
    colour    = "grey60",
    linetype  = "dashed",
    linewidth = 0.6
  ) +
  # Datapunten
  geom_point(
    colour = "firebrick",
    alpha  = 0.25,
    size   = 0.8
  ) +
  # Annotatie: echte meettijd vs. toegekende tijd
  annotate(
    "segment",
    x    = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    xend = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    y    = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    yend = as.POSIXct("2026-05-27 19:13:00", tz = "UTC"),
    arrow     = arrow(length = unit(0.25, "cm"), ends = "both", type = "open"),
    colour    = "steelblue",
    linewidth = 0.7
  ) +
  annotate(
    "label",
    x     = as.POSIXct("2026-05-27 10:30:00", tz = "UTC"),
    y     = as.POSIXct("2026-05-27 14:30:00", tz = "UTC"),
    label = "~6 uur offset\n(klok kwijt)",
    colour    = "steelblue",
    fill      = "white",
    size      = 3.2,
    label.size = 0.3
  ) +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    name        = "Revisietijdstempel — werkelijke inzendingtijd (UTC)"
  ) +
  scale_y_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    name        = "entry_datetime — door DBM toegekend tijdstempel (UTC)"
  ) +
  labs(
    title    = "Klokstoring Lub Senior / 00000005 — 27 mei 2026",
    subtitle = paste0(
      "3.307 records kregen entry_datetime ~19:07\u201319:20 UTC,\n",
      "terwijl de revisietijdstempels lopen van 08:59 tot 18:20 UTC.\n",
      "Bij een correcte klok liggen alle punten op de gestippelde diagonaal."
    ),
    caption  = "Gestippelde lijn = verwacht bij correcte tijdstempels (entry = revisie)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey40", lineheight = 1.4),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )








may27_anomaly <- fuel_05 |>
  filter(as.Date(datetime) == "2026-05-27") |>
  mutate(
    offset_hours = as.numeric(difftime(datetime, revision, units = "hours")),
    is_affected  = offset_hours > 1
  ) |>
  filter(is_affected)

# Referentielijn: perfecte 1:1 relatie (entry = revisie)
ref_min <- min(may27_anomaly$revision)
ref_max <- max(may27_anomaly$datetime)

ggplot(may27_anomaly, aes(x = revision, y = datetime)) +
  # 1:1 referentielijn
  geom_abline(
    slope     = 1,
    intercept = as.numeric(as.POSIXct("2026-05-27 00:00:00", tz = "UTC")) -
      as.numeric(as.POSIXct("2026-05-27 00:00:00", tz = "UTC")),
    colour    = "grey60",
    linetype  = "dashed",
    linewidth = 0.6
  ) +
  # Datapunten
  geom_point(
    colour = "firebrick",
    alpha  = 0.25,
    size   = 0.8
  ) +
  # Annotatie: echte meettijd vs. toegekende tijd
  annotate(
    "segment",
    x    = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    xend = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    y    = as.POSIXct("2026-05-27 10:00:00", tz = "UTC"),
    yend = as.POSIXct("2026-05-27 19:13:00", tz = "UTC"),
    arrow     = arrow(length = unit(0.25, "cm"), ends = "both", type = "open"),
    colour    = "steelblue",
    linewidth = 0.7
  ) +
  annotate(
    "label",
    x     = as.POSIXct("2026-05-27 10:30:00", tz = "UTC"),
    y     = as.POSIXct("2026-05-27 14:30:00", tz = "UTC"),
    label = "~6 uur offset\n(klok kwijt)",
    colour    = "steelblue",
    fill      = "white",
    size      = 3.2,
    label.size = 0.3
  ) +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    name        = "Revisietijdstempel — werkelijke inzendingtijd (UTC)"
  ) +
  scale_y_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    name        = "entry_datetime — door DBM toegekend tijdstempel (UTC)"
  ) +
  labs(
    title    = "Klokstoring Lub Senior / 00000005 — 27 mei 2026",
    subtitle = paste0(
      "3.307 records kregen entry_datetime ~19:07\u201319:20 UTC,\n",
      "terwijl de revisietijdstempels lopen van 08:59 tot 18:20 UTC.\n",
      "Bij een correcte klok liggen alle punten op de gestippelde diagonaal."
    ),
    caption  = "Gestippelde lijn = verwacht bij correcte tijdstempels (entry = revisie)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey40", lineheight = 1.4),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )




# 
journal_id <- "f4fc3064-fd63-4c9a-9635-0b292d534827"

new_devices <- list(
  # "61326139-7379-3463-0000-000200000006" = "FUEL_2",
  "61326139-7379-3463-0000-000200000007" = "FUEL_3"
)

for (did in names(new_devices)) {
  cat(sprintf("Syncing %s [%s] ...\n", new_devices[[did]], did))
  sync_device_parallel(
    source      = "efice",
    journal_id  = journal_id,
    device_id   = did,
    vessel_name = "Willem Jacob",
    start_time  = as.POSIXct("2026-02-15 00:00:00", tz = "UTC"),
    chunk_by    = "1 month",
    page_size   = 10000,
    n_workers   = 4
  )
}






# 13/7/2026 checking daily sync functioning
library(dplyr)
library(arrow)
library(fs)
library(stringr)
library(lubridate)

raw_root     <- file.path(.onedrive(), "raw", "efice")
thinned_root <- file.path(.onedrive(), "processed", "thinned")

# ---- 1. Latest raw record per vessel/device, read from parquet metadata ----
raw_files <- fs::dir_ls(raw_root, recurse = TRUE, glob = "*.parquet") |>
  tibble::as_tibble_col("path") |>
  dplyr::mutate(
    file_mtime = fs::file_info(path)$modification_time,
    vessel     = str_extract(path, "(?<=efice/)[^/]+"),
    device_id  = str_extract(path, "(?<=_)[0-9a-f-]{36}(?=_[0-9]{6}\\.parquet$)")
  )

raw_latest <- raw_files |>
  dplyr::rowwise() |>
  dplyr::mutate(
    max_datetime = arrow::open_dataset(path) |>
      dplyr::summarise(m = max(datetime, na.rm = TRUE)) |>
      dplyr::collect() |>
      dplyr::pull(m)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(vessel, device_id, file_mtime, max_datetime)

# ---- 2. Latest thinned file per vessel ----
thinned_latest <- fs::dir_ls(thinned_root, recurse = TRUE, glob = "*.parquet") |>
  tibble::as_tibble_col("path") |>
  dplyr::mutate(
    vessel      = str_extract(path, "(?<=thinned/)[^/]+"),
    file_date   = str_extract(path, "[0-9]{8}") |> lubridate::ymd(),
    file_mtime  = fs::file_info(path)$modification_time
  ) |>
  dplyr::group_by(vessel) |>
  dplyr::slice_max(file_date, n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(vessel, thinned_file_date = file_date, thinned_mtime = file_mtime)

# ---- 3. Compare: how stale is thinning relative to raw? ----
raw_latest |>
  dplyr::group_by(vessel) |>
  dplyr::summarise(raw_max_datetime = max(max_datetime, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(thinned_latest, by = "vessel") |>
  dplyr::mutate(staleness_days = as.numeric(difftime(Sys.time(), thinned_mtime, units = "days")))


library(dplyr)
library(fs)

fs::dir_ls(raw_root, recurse = 1, type = "directory")
fs::dir_ls(thinned_root, recurse = 1, type = "directory")


library(dplyr)
library(stringr)

raw_summary <- raw_latest |>
  dplyr::group_by(vessel) |>
  dplyr::summarise(raw_max_datetime = max(max_datetime, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(vessel_key = stringr::str_replace_all(vessel, " ", "_"))

thinned_latest2 <- thinned_latest |>
  dplyr::mutate(vessel_key = stringr::str_replace_all(vessel, " ", "_"))

raw_summary |>
  dplyr::left_join(thinned_latest2, by = "vessel_key") |>
  dplyr::mutate(staleness_days = as.numeric(difftime(Sys.time(), thinned_mtime, units = "days"))) |>
  dplyr::select(vessel, raw_max_datetime, thinned_file_date, thinned_mtime, staleness_days)







library(dplyr)
library(arrow)
library(fs)
library(stringr)
library(tidyr)
library(lubridate)

# ============================================================================
# Step 1 / 6 helper — raw sensor/date inventory for one vessel (lazy arrow scan)
# ============================================================================
poseidat_raw_inventory <- function(vessel_name, source = "efice") {
  
  raw_files <- fs::dir_ls(.raw_dir(source, vessel_name),
                          recurse = TRUE, glob = "*.parquet", fail = FALSE)
  
  if (length(raw_files) == 0) {
    return(tibble(value_type = character(), raw_n = integer(),
                  raw_first = as.POSIXct(character()),
                  raw_last  = as.POSIXct(character())))
  }
  
  arrow::open_dataset(raw_files) |>
    dplyr::group_by(value_type) |>
    dplyr::summarise(
      raw_n     = dplyr::n(),
      raw_first = min(datetime, na.rm = TRUE),
      raw_last  = max(datetime, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(value_type)
}

# ============================================================================
# Step 1 / 6 helper — thinned sensor/date inventory (reads only latest file)
# ============================================================================
poseidat_thinned_inventory <- function(vessel_name) {
  
  vessel_dir <- file.path(.onedrive(), "processed", "thinned", .safe(vessel_name))
  
  files <- if (dir.exists(vessel_dir))
    sort(list.files(vessel_dir, pattern = "\\.parquet$", full.names = TRUE))
  else character(0)
  
  if (length(files) == 0) {
    return(tibble(value_type = character(), thinned_n = integer(),
                  thinned_first = as.POSIXct(character()),
                  thinned_last  = as.POSIXct(character()),
                  thinned_file_date = as.Date(character())))
  }
  
  latest_file <- tail(files, 1)
  file_date   <- stringr::str_extract(latest_file, "[0-9]{8}") |> lubridate::ymd()
  
  arrow::read_parquet(latest_file) |>
    dplyr::group_by(value_type) |>
    dplyr::summarise(
      thinned_n     = dplyr::n(),
      thinned_first = min(datetime, na.rm = TRUE),
      thinned_last  = max(datetime, na.rm = TRUE),
      .groups       = "drop"
    ) |>
    dplyr::mutate(thinned_file_date = file_date) |>
    dplyr::arrange(value_type)
}

# ============================================================================
# Combined raw-vs-thinned overview for one vessel (steps 1 & 6)
# ============================================================================
poseidat_vessel_overview <- function(vessel_name, source = "efice") {
  
  raw     <- poseidat_raw_inventory(vessel_name, source)
  thinned <- poseidat_thinned_inventory(vessel_name)
  
  raw |>
    dplyr::full_join(thinned, by = "value_type") |>
    dplyr::mutate(
      vessel_name = vessel_name,
      lag_days    = as.numeric(difftime(raw_last, thinned_last, units = "days"))
    ) |>
    dplyr::relocate(vessel_name, value_type) |>
    dplyr::arrange(value_type)
}

# ============================================================================
# Step 3 — check EFICE API for the most recent data available per sensor,
# WITHOUT saving anything to disk. A bounded recent window (peek_days) keeps
# this cheap — it's a diagnostic peek, not a sync.
# ============================================================================
poseidat_check_api_latest <- function(vessel_name, vessel_config,
                                      source     = "efice",
                                      peek_days  = 3,
                                      page_size  = 5000) {
  
  journal_id <- get_journal_id(vessel_config, vessel_name)
  devices    <- get_devices(source, journal_id, vessel_name)
  
  end_t   <- Sys.time()
  start_t <- end_t - peek_days * 86400
  
  devices |>
    dplyr::rowwise() |>
    dplyr::mutate(
      api_latest = list(
        tryCatch({
          peek <- download_measurements(source, journal_id, device_id,
                                        start_t, end_t,
                                        page_size           = page_size,
                                        sleep_between_pages = 0)
          if (nrow(peek) == 0) as.POSIXct(NA) else max(peek$datetime, na.rm = TRUE)
        }, error = function(e) as.POSIXct(NA))
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(api_latest) |>
    dplyr::mutate(vessel_name = vessel_name) |>
    dplyr::relocate(vessel_name, device_id, device_name, device_type, api_latest)
}

# ============================================================================
# Master stepwise runner — one vessel, your six steps in order
# ============================================================================
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
  
  # ---- 3. Retrieve new data -------------------------------------------------
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


result <- poseidat_sync_vessel_stepwise("Lub Senior", vessel_config, source = "efice")
result <- poseidat_sync_vessel_stepwise("Polar", vessel_config, source = "efice")


# fill the FUEL1 gap of 27 days

journal_id <- get_journal_id(vessel_config, "Willem Jacob")
fuel1_id   <- get_device_by_label(vessel_config, "Willem Jacob", "FUEL_1")

sync_device_parallel(
  source      = "efice",
  journal_id  = journal_id,
  device_id   = fuel1_id,
  vessel_name = "Willem Jacob",
  start_time  = as.POSIXct("2026-06-11 05:12:04", tz = "UTC"),  # from disk_latest
  chunk_by    = "1 week",
  page_size   = 10000,
  n_workers   = 4
)

journal_id <- get_journal_id(vessel_config, "Willem Jacob")
fuel3_id   <- get_device_by_label(vessel_config, "Willem Jacob", "FUEL_3")

sync_device_parallel(
  source      = "efice",
  journal_id  = journal_id,
  device_id   = fuel3_id,
  vessel_name = "Willem Jacob",
  start_time  = as.POSIXct("2026-02-20 00:00:00", tz = "UTC"),  # matches other devices' history
  chunk_by    = "1 day",
  page_size   = 10000,
  n_workers   = 4
)

# rund the stepwise sync again for Willem Jacob
result_wj <- poseidat_sync_vessel_stepwise("Willem Jacob", vessel_config, source = "efice")

# Daily data availability calendar heatmap (all vessels x value_types)
poseidat_availability()

poseidat_plot_overview(
  vessel_name = c("Willem Jacob"),
  start_date  = as.Date("2026-05-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)








# 3 August 2026 - Testing PEFA Poseidat API

# ---- PEFA token acquisition (Azure AD B2C ROPC) ----------------------------
# PEFA authenticates via an Azure AD B2C Resource Owner Password Credentials
# flow rather than a static bearer token. Access tokens expire (typically
# ~1 hour) so this must be called fresh -- or from a cache -- rather than
# read once from .Renviron like DBMATIC_TOKEN/EFICE_TOKEN.
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


usethis::edit_r_environ()

library(httr2)
library(glue)

# paste the .pefa_get_token() definition above, or source Poseidat_functions.R
# once you've added it there

token <- .pefa_get_token()

cat("Token type   :", token$token_type, "\n")
cat("Expires in   :", token$expires_in, "seconds\n")
cat("Fetched at   :", format(token$fetched_at), "\n")
cat("Token preview:", substr(token$access_token, 1, 12), "...(truncated)\n")

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

setup_poseidat_sync()
adp <- .get_adapter("pefa")
cat("Adapter token:", substr(adp$token, 1, 12), "...\n")

vessels <- get_vessels("pefa")
vessels

setup_poseidat_sync()

# once you have a real journal_id from the vessel list:
devices <- get_devices("pefa", journal_id = vessels$journal_id[1])
devices

# add to vessel_config
vessel_config <- vessel_config |>
  bind_rows(
    tibble(
      source        = "pefa",
      journal_id    = "<journal_id from get_vessels() output>",
      vessel_name   = "<vessel_name from get_vessels() output>",
      cfr           = "<cfr from get_vessels() output>",
      enabled       = FALSE,   # keep off until measurements are confirmed working
      device_labels = list(list(
        "386b3671-796f-7977-0000-000100000001" = "POSITION",      # Furuno gps
        "386b3671-796f-7977-0000-000100000018" = "FUEL_BB",       # BB Hulpmotor
        "386b3671-796f-7977-0000-000100000019" = "FUEL_STB",      # STB Hulpmotor
        "386b3671-796f-7977-0000-00010000001f" = "TRAWL_TENSION", # Trekkrachtmeter
        "386b3671-796f-7977-0000-000100000020" = "DEPTH",         # dieptemeter
        "386b3671-796f-7977-0000-000100000025" = "FUEL_MAIN"      # Hoofdmotor
      ))
    )
  )


vessel_config |> filter(vessel_name == "Catherina") |> pull(journal_id)
vessel_config |> filter(vessel_name == "Catherina") |> pull(device_labels)

library(httr2)

adp <- .get_adapter("pefa")

start_epoch <- as.integer(as.numeric(Sys.time() - 7*86400))
end_epoch   <- as.integer(as.numeric(Sys.time()))

resp <- request(adp$base_url) |>
  req_url_path_append("api", "v1", "Journals",
                      "59e2c628-6b6f-49dd-be24-6e73c9221e25", "measurements",
                      "386b3671-796f-7977-0000-000200000001") |>
  req_auth_bearer_token(adp$token) |>
  req_url_query(start = start_epoch, end = end_epoch) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()

cat("Status:", resp_status(resp), "\n")
data <- resp_body_json(resp)
cat("Number of records:", length(data), "\n")
if (length(data) > 0) data[[1]] |> str(max.level = 4)


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

t0 <- Sys.time()
result <- sync_device(
  source      = "pefa",
  journal_id  = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
  device_id   = "386b3671-796f-7977-0000-000200000001",
  vessel_name = "Catherina",
  start_time  = Sys.time() - 7*86400,   # explicit 7-day window, same as the original test
  end_time    = Sys.time()
)
cat("Elapsed:", format(Sys.time() - t0), "\n")






source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
source(file.path(here::here(), "R", "poseidat", "Poseidat_config.R"))

setup_poseidat_sync()

devices <- vessel_config |>
  filter(vessel_name == "Catherina") |>
  pull(device_labels) |>
  pluck(1)

backfill_start <- as.POSIXct("2026-02-15 00:00:00", tz = "UTC")

for (device_id in names(devices)) {
  cat(sprintf("\n=== Backfilling %s (%s) ===\n", devices[[device_id]], device_id))
  
  sync_device_parallel(
    source      = "pefa",
    journal_id  = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
    device_id   = device_id,
    vessel_name = "Catherina",
    start_time  = backfill_start,
    chunk_by    = "1 week",
    page_size   = 10000,   # irrelevant for PEFA (no true pagination) but harmless
    n_workers   = 4
  )
}


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

one_june_week <- download_measurements(
  source     = "pefa",
  journal_id = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
  device_id  = "386b3671-796f-7977-0000-000200000001",
  start_time = as.POSIXct("2026-06-16 00:00:00", tz = "UTC"),
  end_time   = as.POSIXct("2026-06-23 00:00:00", tz = "UTC")
)

nrow(one_june_week)



source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

# Free check first
devices <- get_devices("pefa", journal_id = "59e2c628-6b6f-49dd-be24-6e73c9221e25")
devices |> select(device_name, complete_up_to)

boundaries <- list(
  POSITION      = "386b3671-796f-7977-0000-000200000001",
  TRAWL_TENSION = "386b3671-796f-7977-0000-00020000001f",
  DEPTH         = "386b3671-796f-7977-0000-000200000020",
  FUEL_MAIN     = "386b3671-796f-7977-0000-000200000025"
)

for (label in names(boundaries)) {
  cat(sprintf("\n=== %s ===\n", label))
  find_earliest_data(
    source       = "pefa",
    journal_id   = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
    device_id    = boundaries[[label]],
    empty_date   = as.POSIXct("2026-05-23", tz = "UTC"),
    present_date = as.POSIXct("2026-08-03", tz = "UTC")
  )
}





journal_id <- "59e2c628-6b6f-49dd-be24-6e73c9221e25"

live_devices <- list(
  POSITION      = "386b3671-796f-7977-0000-000200000001",
  TRAWL_TENSION = "386b3671-796f-7977-0000-00020000001f",
  DEPTH         = "386b3671-796f-7977-0000-000200000020",
  FUEL_MAIN     = "386b3671-796f-7977-0000-000200000025"
)

backfill_start <- as.POSIXct("2026-06-10 00:00:00", tz = "UTC")

t0 <- Sys.time()

for (label in names(live_devices)) {
  cat(sprintf("\n=== Backfilling %s (%s) ===\n", label, live_devices[[label]]))
  
  sync_device_parallel(
    source      = "pefa",
    journal_id  = journal_id,
    device_id   = live_devices[[label]],
    vessel_name = "Catherina",
    start_time  = backfill_start,
    chunk_by    = "1 week",
    n_workers   = 4
  )
}

cat(sprintf("\nTotal backfill time: %s\n", format(Sys.time() - t0)))

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
poseidat_thin(source = "pefa", vessel_name = "Catherina", save = TRUE)

library(httr2)

adp <- .get_adapter("pefa")

resp <- request(adp$base_url) |>
  req_url_path_append("api", "v1", "Journals",
                      "59e2c628-6b6f-49dd-be24-6e73c9221e25", "measurements",
                      "386b3671-796f-7977-0000-000200000025") |>
  req_auth_bearer_token(adp$token) |>
  req_url_query(
    start = as.integer(as.numeric(as.POSIXct("2026-08-01", tz = "UTC"))),
    end   = as.integer(as.numeric(as.POSIXct("2026-08-03", tz = "UTC")))
  ) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()

data <- resp_body_json(resp)
cat("Number of records:", length(data), "\n")
data[[1]] |> str(max.level = 5)


bad_files <- list.files(
  file.path(.onedrive(), "raw", "pefa", "Catherina"),
  pattern = "386b3671-796f-7977-0000-000200000025.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)
bad_files   # inspect before deleting
file.remove(bad_files)


sync_device_parallel(
  source      = "pefa",
  journal_id  = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
  device_id   = "386b3671-796f-7977-0000-000200000025",
  vessel_name = "Catherina",
  start_time  = as.POSIXct("2026-06-10", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 4
)

poseidat_availability()

poseidat_availability(vessel_name = "Catherina")

poseidat_plot_overview(
  vessel_name = c("Catherina"),
  start_date  = as.Date("2026-05-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

poseidat_plot_overview(
  vessel_name = c("Catherina"),
  start_date  = as.Date("2026-06-24"),
  end_date  = as.Date("2026-06-24"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

poseidat_plot_overview(
  vessel_name = c("Catherina"),
  start_date  = as.Date("2026-06-24"),
  end_date  = as.Date("2026-06-24"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

sync_device_parallel(
  source      = "pefa",
  journal_id  = "59e2c628-6b6f-49dd-be24-6e73c9221e25",
  device_id   = "386b3671-796f-7977-0000-000200000025",
  vessel_name = "Catherina",
  start_time  = as.POSIXct("2026-07-21", tz = "UTC"),
  end_time    = Sys.time(),
  chunk_by    = "1 week",
  n_workers   = 4
)


poseidat_thin(source = "pefa", vessel_name = "Catherina", save = TRUE)

raw_fuel <- arrow::open_dataset(file.path(.onedrive(), "raw", "pefa", "Catherina")) |>
  dplyr::filter(device_id == "386b3671-796f-7977-0000-000200000025") |>
  dplyr::collect()

max(raw_fuel$datetime, na.rm = TRUE)

daily_presence <- arrow::open_dataset(file.path(.onedrive(), "raw", "pefa", "Catherina")) |>
  dplyr::collect() |>
  dplyr::mutate(day = as.Date(datetime)) |>
  dplyr::distinct(day, value_type) |>
  dplyr::mutate(present = TRUE) |>
  tidyr::pivot_wider(names_from = value_type, values_from = present, values_fill = FALSE)

# Days where DEPTH is missing but TRAWL_TENSION is present (or vice versa) --
# if this is genuinely empty, the two sensors track together and the gaps
# are operational (gear not deployed), not a sync problem.
daily_presence |>
  dplyr::filter(TRAWL_TENSION != DEPTH) |>
  dplyr::arrange(day)



suggestions <- suggest_quality_rules()
print_quality_suggestions(suggestions)


setup_poseidat_sync()   # confirms DBMATIC_API_URL/DBMATIC_TOKEN are still set

dbm_vessels <- get_vessels("dbmatic")
dbm_vessels

efice_vessels <- get_vessels("efice")
efice_vessels

devices_anna     <- get_devices("dbmatic", journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab")
devices_immanuel <- get_devices("dbmatic", journal_id = "d63c867e-df5c-4e2f-a88d-5ef75454d6cf")

devices_anna
devices_immanuel

vessel_config |>
  dplyr::filter(vessel_name %in% c("ANNA HENDRIKA", "IMMANUEL", "Anna Hendrika", "Immanuel")) |>
  dplyr::select(source, vessel_name, cfr, journal_id, enabled)

vessel_config |>
  dplyr::filter(vessel_name %in% c("ANNA HENDRIKA", "IMMANUEL", "Anna Hendrika", "Immanuel")) |>
  dplyr::select(source, vessel_name, cfr, journal_id, enabled)





# One device per vessel is enough to establish the boundary — pick whichever
# sensor you're most confident is core/always-on (e.g. POSITION-equivalent),
# same logic as using Furuno gps for Catherina.

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

# extract the structure of the measurements of dbmatic vessels
adp <- .get_adapter("dbmatic")

raw <- .http_get(
  adp$base_url, adp$token,
  glue::glue("journals/05447236-b531-4c62-b22c-59fb7b2490ab/measurements/{devices_anna$device_id[1]}"),
  params = list(
    start  = as.integer(as.numeric(Sys.time() - 2*86400)),
    end    = as.integer(as.numeric(Sys.time())),
    limit  = 5,
    offset = 0
  )
)

length(raw)
raw[[1]] |> str(max.level = 5)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

week_start <- as.POSIXct("2026-07-01", tz = "UTC")
week_end   <- week_start + 7*86400

result <- download_measurements(
  source     = "dbmatic",
  journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id  = devices_anna[["device_id"]][1],
  start_time = week_start,
  end_time   = week_end
)

nrow(result)

adp <- .get_adapter("dbmatic")

# Time page 1 (offset 0) alone, with a much bigger page size
t0 <- Sys.time()
p1 <- .dbmatic_get_measurements(adp$base_url, adp$token,
                                "05447236-b531-4c62-b22c-59fb7b2490ab",
                                devices_anna[["device_id"]][1],
                                start_time = week_start, end_time = week_end,
                                page_size = 20000, offset = 0)
cat("Page 1 (offset 0):", format(Sys.time() - t0), "-", nrow(p1), "rows\n")

# Time a page much further in, to see if cost grows with offset


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

week_start <- as.POSIXct("2026-04-01", tz = "UTC")
week_end   <- week_start + 7*86400

t0 <- Sys.time()
result <- download_measurements(
  source     = "dbmatic",
  journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id  = devices_anna[["device_id"]][1],
  start_time = week_start,
  end_time   = week_end
)
cat("Elapsed:", format(Sys.time() - t0), "-", nrow(result), "rows\n")

week_start <- as.POSIXct("2026-03-01", tz = "UTC")
week_end   <- week_start + 7*86400

vn  <- "Immanuel"
jid <- get_journal_id(vessel_config, vn)[1]
did <- get_device_by_label(vessel_config, vn, "POSITION")

t0 <- Sys.time()
result <- download_measurements(
  source     = "dbmatic",
  journal_id = jid,
  device_id  = did,
  start_time = week_start,
  end_time   = week_end
)
cat("Elapsed:", format(Sys.time() - t0), "-", nrow(result), "rows\n")

glimpse(result)






adp <- .get_adapter("dbmatic")

probe_all_devices <- function(vessel_name, days_back = 2) {
  jid    <- get_journal_id(vessel_config, vessel_name, source = "dbmatic")
  labels <- vessel_config |>
    dplyr::filter(vessel_name == !!vessel_name, source == "dbmatic") |>
    dplyr::pull(device_labels) |>
    purrr::pluck(1)
  
  for (did in names(labels)) {
    label <- labels[[did]]
    raw <- .http_get(
      adp$base_url, adp$token,
      glue::glue("journals/{jid}/measurements/{did}"),
      params = list(
        start = as.integer(as.numeric(Sys.time() - days_back*86400)),
        end   = as.integer(as.numeric(Sys.time())),
        limit = 5, offset = 0
      ),
      timeout_sec = 300
    )
    cat(sprintf("\n=== %s / %s (%s) ===\n", vessel_name, label, did))
    if (length(raw) == 0) {
      cat("  (no data in this window -- try a wider days_back or a known-active date)\n")
    } else {
      str(raw[[1]], max.level = 5)
    }
  }
}

probe_all_devices("Immanuel")
probe_all_devices("Anna Hendrika")





jid <- get_journal_id(vessel_config, "Immanuel", source = "dbmatic")
did <- get_device_by_label(vessel_config, "Immanuel", "FUEL_MAIN", source = "dbmatic")

sample_fuel <- download_measurements(
  source     = "dbmatic",
  journal_id = jid,
  device_id  = did,
  start_time = as.POSIXct(Sys.Date() - 3, tz = "UTC"),
  end_time   = as.POSIXct(Sys.Date(), tz = "UTC")
)

nrow(sample_fuel)
summary(sample_fuel$value_numeric)
sum(sample_fuel$value_numeric != 0, na.rm = TRUE)   # how many non-zero readings

sample_fuel |>
  dplyr::filter(value_numeric != 0) |>
  dplyr::select(datetime, value_numeric) |>
  dplyr::arrange(datetime) |>
  head(10)



options(future.globals.maxSize = 2000 * 1024^2)  # 2GB, plenty of headroom

backfill_plan <- tibble::tibble(
  vessel_name = c("Immanuel",      "Anna Hendrika"),
  start_date  = as.POSIXct(c("2026-04-01", "2026-05-01"), tz = "UTC")
)

for (i in seq_len(nrow(backfill_plan))) {
  
  vn    <- backfill_plan$vessel_name[i]
  start <- backfill_plan$start_date[i]
  jid   <- get_journal_id(vessel_config, vn, source = "dbmatic")
  
  labels <- vessel_config |>
    dplyr::filter(vessel_name == vn, source == "dbmatic") |>
    dplyr::pull(device_labels) |>
    purrr::pluck(1)
  
  for (device_id in names(labels)) {
    label <- labels[[device_id]]
    cat(sprintf("\n=== %s: backfilling %s (%s) ===\n", vn, label, device_id))
    
    sync_device_parallel(
      source      = "dbmatic",
      journal_id  = jid,
      device_id   = device_id,
      vessel_name = vn,
      start_time  = start,
      chunk_by    = "1 week",
      n_workers   = 4
    )
  }
}

poseidat_thin(source = "dbmatic", vessel_name = "Immanuel", save = TRUE)
poseidat_thin(source = "dbmatic", vessel_name = "Anna Hendrika", save = TRUE)

poseidat_value_types("dbmatic")

poseidat_availability()

poseidat_map()

poseidat_plot_overview(
  vessel_name = "Willem Jacob",
  start_date  = as.Date("2026-07-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE
)

poseidat_device_labels(vessel_config = vessel_config, vessel_name = "Willem Jacob")




jid <- get_journal_id(vessel_config, "Anna Hendrika", source = "dbmatic")

current_devices <- get_devices("dbmatic", journal_id = jid)
current_devices

# What's already labeled for Anna Hendrika
known_labels <- vessel_config |>
  dplyr::filter(vessel_name == "Anna Hendrika", source == "dbmatic") |>
  dplyr::pull(device_labels) |>
  purrr::pluck(1)

known_ids <- names(known_labels)

# The new one(s) -- present in the API, not yet in your config
new_devices <- current_devices |>
  dplyr::filter(!device_id %in% known_ids)

new_devices

adp <- .get_adapter("dbmatic")
jid <- get_journal_id(vessel_config, "Anna Hendrika", source = "dbmatic")
scale_id <- "70756278-3132-3931-0000-000200000007"   # full UUID from new_devices$device_id[1] -- double-check against print(new_devices, width = Inf), it looked truncated in the console output

raw <- .http_get(
  adp$base_url, adp$token,
  glue::glue("journals/{jid}/measurements/{scale_id}"),
  params = list(
    start = as.integer(as.numeric(Sys.time() - 7*86400)),   # wider window -- scale readings are likely event-driven (per catch/basket weighed), not continuous like GPS
    end   = as.integer(as.numeric(Sys.time())),
    limit = 5, offset = 0
  ),
  timeout_sec = 300
)

length(raw)
if (length(raw) == 0) cat("No data in the last 7 days -- try widening further\n") else raw[[1]] |> str(max.level = 6)



source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

sync_device_parallel(
  source      = "dbmatic",
  journal_id  = get_journal_id(vessel_config, "Anna Hendrika", source = "dbmatic"),
  device_id   = "70756278-3132-3931-0000-000200000007",
  vessel_name = "Anna Hendrika",
  start_time  = as.POSIXct("2026-05-15", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 4
)


result <- tryCatch(
  download_measurements(
    source     = "dbmatic",
    journal_id = get_journal_id(vessel_config, "Anna Hendrika", source = "dbmatic"),
    device_id  = "70756278-3132-3931-0000-000200000007",
    start_time = as.POSIXct("2026-05-15", tz = "UTC"),
    end_time   = as.POSIXct("2026-05-22", tz = "UTC")
  ),
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  }
)

nrow(result)

sync_device_parallel(
  source      = "dbmatic",
  journal_id  = get_journal_id(vessel_config, "Anna Hendrika", source = "dbmatic"),
  device_id   = "70756278-3132-3931-0000-000200000007",
  vessel_name = "Anna Hendrika",
  start_time  = as.POSIXct("2026-05-15", tz = "UTC"),
  end_time    = as.POSIXct("2026-05-22", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 1   # single worker this time, closer to the sequential path that worked
)



scale_raw <- arrow::open_dataset(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika")) |>
  dplyr::filter(device_id == "70756278-3132-3931-0000-000200000007") |>
  dplyr::collect()
glimpse(scale_raw)

# Basic shape
nrow(scale_raw)
range(scale_raw$datetime)
scale_raw |> dplyr::count(scale_category)   # sanity check -- expect "FISH" only, or are there others?

# Catch composition by species
catch_by_product <- scale_raw |>
  dplyr::group_by(scale_product) |>
  dplyr::summarise(
    total_weight = sum(scale_weight, na.rm = TRUE),
    n_weighings  = dplyr::n(),
    n_hauls      = dplyr::n_distinct(scale_haul_number),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(total_weight))

catch_by_product

# Per-haul summary, chronological
catch_by_haul <- scale_raw |>
  dplyr::group_by(scale_haul_number) |>
  dplyr::summarise(
    haul_start      = min(datetime),
    haul_end        = max(datetime),
    total_weight    = sum(scale_weight, na.rm = TRUE),
    n_species       = dplyr::n_distinct(scale_product),
    n_weighings     = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(haul_start)

catch_by_haul

# Daily totals
daily_catch <- scale_raw |>
  dplyr::mutate(day = as.Date(datetime)) |>
  dplyr::group_by(day) |>
  dplyr::summarise(
    total_weight = sum(scale_weight, na.rm = TRUE),
    n_hauls      = dplyr::n_distinct(scale_haul_number),
    .groups = "drop"
  ) |>
  dplyr::arrange(day)

daily_catch


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

adp <- .get_adapter("dbmatic")
test <- .dbmatic_get_measurements(
  adp$base_url, adp$token,
  journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id  = "70756278-3132-3931-0000-000200000007",
  start_time = as.POSIXct("2026-05-22", tz = "UTC"),
  end_time   = as.POSIXct("2026-05-23", tz = "UTC")
)

test |> dplyr::select(datetime, value_type, value_numeric, scale_category, scale_product, scale_haul_number)


bad_files <- list.files(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  pattern = "70756278-3132-3931-0000-000200000007.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)
bad_files
file.remove(bad_files)

sync_device_parallel(
  source      = "dbmatic",
  journal_id  = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id   = "70756278-3132-3931-0000-000200000007",
  vessel_name = "Anna Hendrika",
  start_time  = as.POSIXct("2026-05-01", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 4
)







# 1. Re-source, explicitly, right before anything else
source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

# 2. Confirm the fix is actually live in THIS session
adp <- .get_adapter("dbmatic")
test <- .dbmatic_get_measurements(
  adp$base_url, adp$token,
  journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id  = "70756278-3132-3931-0000-000200000007",
  start_time = as.POSIXct("2026-05-22", tz = "UTC"),
  end_time   = as.POSIXct("2026-05-23", tz = "UTC")
)
test |> dplyr::select(datetime, value_numeric, scale_category, scale_product, scale_haul_number)

# 3. Confirm the old files actually get found and removed
bad_files <- list.files(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  pattern = "70756278-3132-3931-0000-000200000007.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)
print(bad_files)             # should list 4 files
file.remove(bad_files)       # should print TRUE TRUE TRUE TRUE

# confirm they're actually gone
list.files(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  pattern = "70756278-3132-3931-0000-000200000007.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)   # should be character(0)

# 4. Re-backfill
sync_device_parallel(
  source      = "dbmatic",
  journal_id  = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id   = "70756278-3132-3931-0000-000200000007",
  vessel_name = "Anna Hendrika",
  start_time  = as.POSIXct("2026-05-01", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 4
)

# 5. Check the result fresh
scale_raw <- arrow::open_dataset(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika")) |>
  dplyr::filter(device_id == "70756278-3132-3931-0000-000200000007") |>
  dplyr::collect()
glimpse(scale_raw)






manual_chunk <- download_measurements(
  source     = "dbmatic",
  journal_id = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id  = "70756278-3132-3931-0000-000200000007",
  start_time = as.POSIXct("2026-05-22", tz = "UTC"),
  end_time   = as.POSIXct("2026-05-29", tz = "UTC")
)

names(manual_chunk)






source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

bad_files <- list.files(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  pattern = "70756278-3132-3931-0000-000200000007.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)
bad_files
file.remove(bad_files)

sync_device_parallel(
  source      = "dbmatic",
  journal_id  = "05447236-b531-4c62-b22c-59fb7b2490ab",
  device_id   = "70756278-3132-3931-0000-000200000007",
  vessel_name = "Anna Hendrika",
  start_time  = as.POSIXct("2026-05-01", tz = "UTC"),
  chunk_by    = "1 week",
  n_workers   = 4
)

scale_raw <- arrow::open_dataset(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika")) |>
  dplyr::filter(device_id == "70756278-3132-3931-0000-000200000007") |>
  dplyr::collect()
glimpse(scale_raw)




scale_file_direct <- arrow::read_parquet(
  "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data/raw/dbmatic/Anna Hendrika/2026/06/Anna_Hendrika_70756278-3132-3931-0000-000200000007_202606.parquet"
)

head(scale_file_direct)
names(scale_file_direct)

scale_file_direct |>
  dplyr::select(datetime, scale_category, scale_product, scale_weight, scale_haul_number) |>
  head(10)


scale_raw <- arrow::open_dataset(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  unify_schemas = TRUE
) |>
  dplyr::filter(device_id == "70756278-3132-3931-0000-000200000007") |>
  dplyr::collect()

glimpse(scale_raw)







scale_files <- list.files(
  file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
  pattern = "70756278-3132-3931-0000-000200000007.*\\.parquet$",
  recursive = TRUE, full.names = TRUE
)

scale_raw <- purrr::map_dfr(scale_files, arrow::read_parquet)

nrow(scale_raw)
scale_raw |> dplyr::select(datetime, scale_category, scale_product, scale_weight, scale_haul_number) |> head(10)






source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()
migrate_fix_column_types(dry_run = FALSE)




check_cols <- c("latitude", "longitude", "course_made_good", "speed_over_ground",
                "traction_port", "traction_center", "traction_starboard",
                "shooted_length_port", "shooted_length_center", "shooted_length_starboard",
                "fuel_avg_consumption", "fuel_peak_consumption",
                "fuel_last_hour_consumption", "fuel_last_day_consumption",
                "scale_weight", "n_satellites", "scale_haul_number")

files <- list.files(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
                    pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

type_report <- tibble::tibble()

for (f in files) {
  tmp <- tempfile(fileext = ".parquet")
  file.copy(f, tmp, overwrite = TRUE)
  df <- arrow::read_parquet(tmp)
  unlink(tmp)
  
  present <- intersect(check_cols, names(df))
  row <- tibble::tibble(file = basename(f))
  for (col in present) {
    row[[col]] <- class(df[[col]])[1]
  }
  type_report <- dplyr::bind_rows(type_report, row)
}

print(type_report, width = Inf, n = Inf)





files <- list.files(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
                    pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

for (f in files) {
  ds <- arrow::open_dataset(f)
  field <- ds$schema$GetFieldByName("traction_port")
  cat(sprintf("%-70s %s\n", basename(f), field$type$ToString()))
}





# Confirm .bak files are actually there
all_files <- list.files(file.path(.onedrive(), "raw", "dbmatic", "Anna Hendrika"),
                        recursive = TRUE, full.names = TRUE)
all_files[grepl("\\.bak$", all_files)]

bak_files <- list.files(file.path(.onedrive(), "raw"), pattern = "\\.parquet\\.bak$",
                        recursive = TRUE, full.names = TRUE)
length(bak_files)
file.remove(bak_files)




glimpse(scale_raw)   # quick sanity check on the columns first

# Catch composition by species
catch_by_product <- scale_raw |>
  dplyr::group_by(scale_product) |>
  dplyr::summarise(
    total_weight = sum(scale_weight, na.rm = TRUE),
    n_weighings  = dplyr::n(),
    n_hauls      = dplyr::n_distinct(scale_haul_number),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(total_weight))

catch_by_product

# Per-haul summary, chronological
catch_by_haul <- scale_raw |>
  dplyr::group_by(scale_haul_number) |>
  dplyr::summarise(
    haul_start   = min(datetime),
    haul_end     = max(datetime),
    total_weight = sum(scale_weight, na.rm = TRUE),
    n_species    = dplyr::n_distinct(scale_product),
    n_weighings  = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(haul_start)

catch_by_haul

# Daily totals
daily_catch <- scale_raw |>
  dplyr::mutate(day = as.Date(datetime)) |>
  dplyr::group_by(day) |>
  dplyr::summarise(
    total_weight = sum(scale_weight, na.rm = TRUE),
    n_hauls      = dplyr::n_distinct(scale_haul_number),
    .groups = "drop"
  ) |>
  dplyr::arrange(day)

daily_catch





source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

poseidat_availability()






# Load the same thinned data poseidat_availability() itself reads for Willem Jacob
thinned_dir <- file.path(.onedrive(), "processed", "thinned", "Willem_Jacob")
files <- sort(list.files(thinned_dir, pattern = "\\.parquet$", full.names = TRUE))
df <- arrow::read_parquet(tail(files, 1))

fuel_daily <- df |>
  dplyr::filter(value_type == "FUEL_CONSUMPTION") |>
  dplyr::mutate(date = as.Date(datetime)) |>
  dplyr::filter(date >= as.Date("2026-06-29"), date <= as.Date("2026-08-12")) |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    n_total   = dplyr::n(),
    n_nonzero = sum(!is.na(value_numeric) & value_numeric != 0, na.rm = TRUE),
    .groups = "drop"
  )

# Fill in every day explicitly -- days with ZERO records won't appear as rows
# at all after group_by(), so without this step they'd silently vanish from
# the table rather than showing as an explicit "no data" row.
all_dates <- seq(as.Date("2026-06-29"), as.Date("2026-08-12"), by = "day")

fuel_daily_full <- tibble::tibble(date = all_dates) |>
  dplyr::left_join(fuel_daily, by = "date") |>
  dplyr::mutate(
    n_total   = tidyr::replace_na(n_total, 0L),
    n_nonzero = tidyr::replace_na(n_nonzero, 0L),
    status    = dplyr::case_when(
      n_nonzero > 0 ~ "data",
      n_total   > 0 ~ "zero / NA",
      TRUE          ~ "no data"
    )
  )

print(fuel_daily_full, n = Inf)








# Every thinned file that exists for Willem Jacob, not just the most recent
thinned_dir <- file.path(.onedrive(), "processed", "thinned", "Willem_Jacob")
files <- sort(list.files(thinned_dir, pattern = "\\.parquet$", full.names = TRUE))

data.frame(
  file = basename(files),
  modified = file.info(files)$mtime
)

# Which one does tail(sort(...), 1) actually pick?
tail(files, 1)

# And separately: does RAW data actually exist through Aug 12?
raw_fuel <- arrow::open_dataset(file.path(.onedrive(), "raw", "efice", "Willem Jacob")) |>
  dplyr::collect() |>
  dplyr::filter(value_type == "FUEL_CONSUMPTION") |>
  dplyr::mutate(date = as.Date(datetime)) |>
  dplyr::filter(date == as.Date("2026-07-16"))

# raw_fuel |> dplyr::count(date) |> print(n = Inf)
hist(raw_fuel$value_numeric)



adp <- .get_adapter("efice")
fuel_device_id <- get_device_by_label(vessel_config, "Willem Jacob", "FUEL_1", source = "efice")   # adjust label if it's a different name
jid <- get_journal_id(vessel_config, "Willem Jacob", source = "efice")

recent <- download_measurements(
  source     = "efice",
  journal_id = jid,
  device_id  = fuel_device_id,
  start_time = as.POSIXct("2026-08-05", tz = "UTC"),
  end_time   = as.POSIXct("2026-08-12", tz = "UTC")
)

nrow(recent)





# ==============================================================================
# test_trip_identification.R
#
# Sandbox script to test poseidat_identify_trips() against real DIGIvloot
# vessel data already synced to Parquet. No synthetic data -- this loads
# whatever POSITION data you already have via poseidat_positions().
# ==============================================================================

source(file.path(here::here(), "R/poseidat", "Poseidat_functions.R"))   # adjust path if needed
setup_poseidat_sync()

# ---- 1. Pick vessels + a date range you know reasonably well ----------------
# Start narrow (one vessel, a couple of weeks) so the sanity checks below are
# easy to eyeball. Widen once you trust the output.

vessel <- "Willem Jacob"     # must match vessel_name in vessel_config
start  <- as.Date("2026-04-01")
end    <- as.Date("2026-08-10")

pos <- poseidat_positions(
  vessel_name = vessel,
  data_source = "thinned",
  start_date  = start,
  end_date    = end
)

nrow(pos)
range(pos$datetime)


# ---- 2. Run trip identification ----------------------------------------------
# Defaults are untuned guesses (see the function docs) -- if the harbour your
# vessel actually uses isn't in .default_fishing_harbours(), pass your own
# `harbours` tibble here (columns: name, lat, lon).

result <- poseidat_identify_trips(pos)

trips <- result$trips
pos_tripped <- result$positions


# ---- 2b. All vessels at once --------------------------------------------------
# No loop needed -- every step inside poseidat_identify_trips() is grouped by
# vessel_name internally (run detection, trip numbering, the position join),
# so passing multi-vessel data through in one call works out of the box:
#
pos_all    <- poseidat_positions(vessel_name = NULL, data_source = "thinned",
                                 start_date = start, end_date = end)
result_all <- poseidat_identify_trips(pos_all)
result_all$trips |> dplyr::count(vessel_name)   # trips found per vessel

# The one thing that scales with total row count is the harbour-distance
# matrix (geosphere::distm computes nrow(pos_all) x nrow(harbours) values
# in one shot). For a full fleet over many months this can get into the
# tens of millions of cells -- still fine for a few dozen harbours, but if
# you ever hit a memory wall, fall back to a per-vessel loop and bind_rows()
# the results:
#
# vessels    <- unique(pos_all$vessel_name)
# per_vessel <- vector("list", length(vessels))
# for (i in seq_along(vessels)) {
#   per_vessel[[i]] <- poseidat_identify_trips(
#     dplyr::filter(pos_all, vessel_name == vessels[i])
#   )$trips
# }
# trips_all <- dplyr::bind_rows(per_vessel)

trips_all <- result_all$trips
pos_tripped_all <- result_all$positions


# ---- 3. Sanity checks ---------------------------------------------------------

# a) How much of the position data actually got assigned to a trip?
#    Low coverage usually means harbour_radius_km/speed_threshold_knots need
#    tuning, or the vessel's home harbour isn't in the harbours table.
pos_tripped_all |>
  dplyr::summarise(
    n_total       = dplyr::n(),
    n_in_trip     = sum(!is.na(trip_id)),
    pct_in_trip   = round(100 * n_in_trip / n_total, 1)
  )

# b) Do trip durations look plausible for this fleet? Flag anything
#    suspiciously short/long for a manual look.
trips_all |>
  dplyr::arrange(duration_hours) |>
  dplyr::select(vessel_name, trip_id, departure_harbour, departure_time,
                arrival_harbour, arrival_time, duration_hours, n_positions)

# c) Departure/arrival harbour should almost always be the same harbour
#    (day trips) or a small, known set of harbours (multi-port trips).
#    Anything landing on a harbour you don't recognise is worth checking
#    against .default_fishing_harbours() / your own harbours table.
trips_all |> dplyr::count(departure_harbour, arrival_harbour, sort = TRUE)


# ---- 4. Visual check: plot one trip on the map --------------------------------
# Pick the longest trip and eyeball that it actually looks like a single
# coherent voyage (leaves harbour, goes out, comes back) rather than two
# trips stitched together or a trip cut short.

check_id <- trips_all |> dplyr::slice_max(duration_hours, n = 1) |> dplyr::pull(trip_id)

poseidat_map(
  pos = pos_tripped_all |> dplyr::filter(trip_id == check_id),
  thin_minutes = NULL,   # already at display resolution
  show_lines = FALSE
)

poseidat_map(
  pos = pos_tripped_all |> dplyr::filter(is.na(trip_id)),
  thin_minutes = NULL,   # already at display resolution
  show_lines = FALSE
)

# points only (good for eyeballing gaps/thinning), or lines only (good for
# a clean route overview without marker clutter):
# poseidat_map(pos = pos_tripped |> dplyr::filter(trip_id == check_id),
#              thin_minutes = NULL, show_lines = FALSE)
# poseidat_map(pos = pos_tripped |> dplyr::filter(trip_id == check_id),
#              thin_minutes = NULL, show_points = FALSE)


# ---- 5. If something looks off ------------------------------------------------
# - Trips too short / too fragmented -> increase bridge_gap_mins or
#   harbour_radius_km.
# - Two real trips merged into one -> decrease bridge_gap_mins, or check
#   whether the vessel is genuinely stopping close to a harbour mid-trip
#   (e.g. sheltering) that's being misread as "in harbour".
# - Trip missing entirely -> check min_trip_hours isn't filtering out a
#   genuinely short trip, and that speed_threshold_knots isn't too strict
#   for this vessel's harbour-manoeuvring speed.
#
# Example of re-running with adjusted thresholds:
#
# result2 <- poseidat_identify_trips(
#   pos,
#   harbour_radius_km     = 5,
#   speed_threshold_knots = 2,
#   min_trip_hours        = 0.5,
#   bridge_gap_mins       = 15
# )


# ---- 6. Once you trust it: persist trip_id into the thinned files -----------
# poseidat_write_trip_ids() defaults to dry_run = TRUE -- always look at
# $coverage first. interval_min must match whatever your thinned files
# already use (poseidat_files() shows the filenames, e.g. "..._1min_...").

preview <- poseidat_write_trip_ids(vessel_name = NULL, interval_min = 1)
preview$coverage
preview$trips_all |> dplyr::select(vessel_name, trip_id, trip_uid, duration_hours)

# # once satisfied:
# poseidat_write_trip_ids(vessel_name = vessel, interval_min = 1, dry_run = FALSE)



vc <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
vc$cfr[vc$vessel_name == "Lub Senior"]   <- "UK153"
vc$cfr[vc$vessel_name == "Polar"]        <- "UK150"
vc$cfr[vc$vessel_name == "Willem Jacob"] <- "UK158"
saveRDS(vc, file.path(.onedrive(), "config", "poseidat_vessels.rds"))


vc <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
names(vc)[names(vc) == "cfr"] <- "vessel_code"
saveRDS(vc, file.path(.onedrive(), "config", "poseidat_vessels.rds"))






source(file.path(here::here(), "R/poseidat", "Poseidat_functions.R"))   # adjust path if needed
setup_poseidat_sync()

# ---- 1. Pick vessels + a date range you know reasonably well ----------------
# Start narrow (one vessel, a couple of weeks) so the sanity checks below are
# easy to eyeball. Widen once you trust the output.

vessel <- "Willem Jacob"     # must match vessel_name in vessel_config
start  <- as.Date("2026-01-01")
end    <- as.Date("2026-08-13")

pos <- poseidat_positions(
  vessel_name = vessel,
  data_source = "thinned",
  start_date  = start,
  end_date    = end
)

nrow(pos)
range(pos$datetime)


# ---- 2. Run trip identification ----------------------------------------------
# Defaults are untuned guesses (see the function docs) -- if the harbour your
# vessel actually uses isn't in .default_fishing_harbours(), pass your own
# `harbours` tibble here (columns: name, lat, lon).

result <- poseidat_identify_trips(pos)

trips <- result$trips
pos_tripped <- result$positions


# ---- 2b. All vessels at once --------------------------------------------------
# No loop needed -- every step inside poseidat_identify_trips() is grouped by
# vessel_name internally (run detection, trip numbering, the position join),
# so passing multi-vessel data through in one call works out of the box:
#
pos_all    <- poseidat_positions(vessel_name = NULL, data_source = "thinned",
                                 start_date = start, end_date = end)
result_all <- poseidat_identify_trips(pos_all)
result_all$trips |> dplyr::count(vessel_name)   # trips found per vessel

# The one thing that scales with total row count is the harbour-distance
# matrix (geosphere::distm computes nrow(pos_all) x nrow(harbours) values
# in one shot). For a full fleet over many months this can get into the
# tens of millions of cells -- still fine for a few dozen harbours, but if
# you ever hit a memory wall, fall back to a per-vessel loop and bind_rows()
# the results:
#
# vessels    <- unique(pos_all$vessel_name)
# per_vessel <- vector("list", length(vessels))
# for (i in seq_along(vessels)) {
#   per_vessel[[i]] <- poseidat_identify_trips(
#     dplyr::filter(pos_all, vessel_name == vessels[i])
#   )$trips
# }
# trips_all <- dplyr::bind_rows(per_vessel)

trips_all <- result_all$trips
pos_tripped_all <- result_all$positions


# ---- 3. Sanity checks ---------------------------------------------------------

# a) How much of the position data actually got assigned to a trip?
#    Low coverage usually means harbour_radius_km/speed_threshold_knots need
#    tuning, or the vessel's home harbour isn't in the harbours table.
pos_tripped_all |>
  dplyr::summarise(
    n_total       = dplyr::n(),
    n_in_trip     = sum(!is.na(trip_id)),
    pct_in_trip   = round(100 * n_in_trip / n_total, 1)
  )

# b) Do trip durations look plausible for this fleet? Flag anything
#    suspiciously short/long for a manual look.
trips_all |>
  dplyr::arrange(duration_hours) |>
  dplyr::select(vessel_name, trip_id, departure_harbour, departure_time,
                arrival_harbour, arrival_time, duration_hours, n_positions) %>% 
  arrange(vessel_name, departure_time) %>% 
  View()

# c) Departure/arrival harbour should almost always be the same harbour
#    (day trips) or a small, known set of harbours (multi-port trips).
#    Anything landing on a harbour you don't recognise is worth checking
#    against .default_fishing_harbours() / your own harbours table.
trips_all |> dplyr::count(departure_harbour, arrival_harbour, sort = TRUE)


# ---- 4. Visual check: plot one trip on the map --------------------------------
# Pick the longest trip and eyeball that it actually looks like a single
# coherent voyage (leaves harbour, goes out, comes back) rather than two
# trips stitched together or a trip cut short.

check_id <- trips_all |> dplyr::slice_max(duration_hours, n = 1) |> dplyr::pull(trip_id)

# poseidat_map(
#   pos = pos_tripped_all |> dplyr::filter(trip_id == check_id),
#   thin_minutes = NULL,   # already at display resolution
#   show_lines = FALSE
# )

poseidat_map(
  pos = pos_tripped_all |> dplyr::filter(!is.na(trip_id)),
  thin_minutes = NULL,   # already at display resolution
  show_lines = FALSE
)

poseidat_map(
  pos = pos_tripped_all |> dplyr::filter(!is.na(trip_id), speed_over_ground <=7),
  thin_minutes = NULL,   # already at display resolution
  show_lines = FALSE
)

poseidat_map(
  pos = pos_tripped_all |> dplyr::filter(is.na(trip_id)),
  thin_minutes = NULL,   # already at display resolution
  show_lines = FALSE
)

# points only (good for eyeballing gaps/thinning), or lines only (good for
# a clean route overview without marker clutter):
# poseidat_map(pos = pos_tripped |> dplyr::filter(trip_id == check_id),
#              thin_minutes = NULL, show_lines = FALSE)
# poseidat_map(pos = pos_tripped |> dplyr::filter(trip_id == check_id),
#              thin_minutes = NULL, show_points = FALSE)



# ---- 5. If something looks off ------------------------------------------------
# - Trips too short / too fragmented -> increase bridge_gap_mins or
#   harbour_radius_km.
# - Two real trips merged into one -> decrease bridge_gap_mins, or check
#   whether the vessel is genuinely stopping close to a harbour mid-trip
#   (e.g. sheltering) that's being misread as "in harbour".
# - Trip missing entirely -> check min_trip_hours isn't filtering out a
#   genuinely short trip, and that speed_threshold_knots isn't too strict
#   for this vessel's harbour-manoeuvring speed.
#
# Example of re-running with adjusted thresholds:
#
# result2 <- poseidat_identify_trips(
#   pos,
#   harbour_radius_km     = 5,
#   speed_threshold_knots = 2,
#   min_trip_hours        = 0.5,
#   bridge_gap_mins       = 15
# )


# ---- 6. Once you trust it: persist trip_id into the thinned files -----------
# trip_uid is built from vessel_code (e.g. "UK158_0007") when your
# vessel_config has a vessel_code column, falling back to a sanitised
# vessel_name for any vessel without one. Check first:
#
# vc <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
# "vessel_code" %in% names(vc)   # FALSE -> add a vessel_code column to
#                                 # vessel_config before running this, e.g.:
# # vc$vessel_code[vc$vessel_name == "Willem Jacob"] <- "UK158"
# # saveRDS(vc, file.path(.onedrive(), "config", "poseidat_vessels.rds"))
#
# poseidat_write_trip_ids() defaults to dry_run = TRUE -- always look at
# $coverage first. interval_min must match whatever your thinned files
# already use (poseidat_files() shows the filenames, e.g. "..._1min_...").
#
# Pass the `result` object from step 2 as `detected` so this persists
# EXACTLY the trips you already inspected above, instead of silently
# re-running poseidat_identify_trips() with a possibly different set of
# thresholds:

preview <- poseidat_write_trip_ids(detected = result_all, interval_min = 1)
preview$coverage
preview$trips |> dplyr::select(vessel_name, trip_id, trip_uid, duration_hours)

source(file.path(here::here(), "R/poseidat", "Poseidat_functions.R"))   # adjust path if needed
setup_poseidat_sync()

# # once satisfied:
poseidat_write_trip_ids(detected = result_all, interval_min = 1, dry_run = FALSE)

# trips_all %>% filter(vessel_name=="Catherina") %>% View()

check <- arrow::read_parquet(
  file.path(.onedrive(), "processed", "thinned", "Willem_Jacob",
            "Willem_Jacob_thinned_1min_20260813.parquet")
)
"trip_id" %in% names(check)
"trip_uid" %in% names(check)
check |> dplyr::filter(!is.na(trip_id)) |> dplyr::slice_head(n = 3) %>% View()





vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
names(vessel_config)
names(vessel_config)[duplicated(names(vessel_config))]

vessel_config[, 7]
vessel_config <- vessel_config[, -7]
saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))


# Maximum detail from raw parquet for a single short trip
poseidat_map(
  data_source  = "thinned",
  start_date   = as.Date("2026-08-12"),
  end_date     = as.Date("2026-08-13"),
  thin_minutes = 1,
  show_lines = FALSE
)







# ==============================================================================
# test_fishing_operations.R
#
# Sandbox script to test poseidat_identify_fishing_operations() against a
# real, already-identified trip. Thresholds are untuned fleet-wide guesses
# (see the function docs) -- this script is as much about calibrating them
# against what you can see on the map as it is about testing the mechanics.
# ==============================================================================

source(file.path(here::here(), "R/poseidat", "Poseidat_functions.R"))   # adjust path if needed
setup_poseidat_sync()


# ---- 1. Pick a trip you already trust from poseidat_trips.parquet -----------

trips <- arrow::read_parquet(
  file.path(.onedrive(), "processed", "trips", "poseidat_trips.parquet")
)

vessel <- "Catherina"

trips |>
  dplyr::filter(vessel_name == vessel) |>
  dplyr::arrange(dplyr::desc(duration_hours)) |>
  dplyr::select(trip_id, trip_uid, departure_time, arrival_time, duration_hours)

# pick one -- a multi-day trip with a real fishing period is more useful to
# eyeball than a short steaming-only trip
check_trip_id <- 7   # <- set from the table above


# ---- 2. Load positions + trawl tension for that trip -------------------------

pos <- poseidat_positions(vessel_name = vessel, data_source = "thinned") |>
  dplyr::mutate(trip_id = check_trip_id)   # positions don't carry trip_id by
# default unless read from a file
# already tagged by
# poseidat_write_trip_ids()/
# poseidat_update_trip_ids()

trip_row <- trips |> dplyr::filter(vessel_name == vessel, trip_id == check_trip_id)

pos_trip <- pos |>
  dplyr::filter(dplyr::between(datetime, trip_row$departure_time, trip_row$arrival_time)) |>
  dplyr::mutate(trip_id = check_trip_id)

trawl <- .poseidat_load_data("thinned", vessel_name = vessel) |>
  dplyr::filter(value_type == "TRAWL_TENSION",
                dplyr::between(datetime, trip_row$departure_time, trip_row$arrival_time))

nrow(pos_trip)
nrow(trawl)
range(trawl$traction_center, na.rm = TRUE)   # sanity check the scale before
# trusting the 1500 kg default


# ---- 3. Run each signal separately first -- easier to debug than the combined
#         orchestrator, and shows you which signal is actually doing anything

speed_ops <- poseidat_flag_operations_speed(pos_trip)
speed_ops

traction_ops <- poseidat_flag_operations_traction(trawl, trip_row)
traction_ops

# If speed_ops / traction_ops come back empty or absurdly long/short, that's
# your first calibration signal -- adjust speed_min/speed_max or
# traction_threshold_kg and re-run before trying the combined version.


# ---- 3b. Calibrate the speed window from an obviously-fishing area ----------
# If the map shows dense parallel "mowing the lawn" tracks that mostly ISN'T
# getting flagged red -- especially if red only shows up on the curved
# turns, not the straight tow legs -- the actual tow speed is probably
# outside the current speed_min/speed_max window (very often above 5 kn).
# Rather than guess a new number, pull the real speed distribution from
# inside that area/period and set the window from the data.
#
# Set a bounding box (or time window) around the obviously-fishing area you
# can see on the map:

fishing_area <- pos_trip |>
  dplyr::filter(
    dplyr::between(latitude,  52.4, 53.30),   # <- adjust to the box you see
    dplyr::between(longitude, 2.5,  4.70)      #    on the map
  )

nrow(fishing_area)
summary(fishing_area$speed_over_ground)
quantile(fishing_area$speed_over_ground, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

hist(fishing_area$speed_over_ground, breaks = 30,
     main = "Speed inside the known-fishing area", xlab = "knots")

# Set speed_min/speed_max to bracket the bulk of that distribution (e.g.
# roughly the 5th-95th percentile), not the current 1-5 default, then
# re-run:

speed_ops2 <- poseidat_flag_operations_speed(
  pos_trip,
  speed_min = 3,   # <- replace with your quantile(0.05) result
  speed_max = 7.1    # <- replace with your quantile(0.95) result
)
speed_ops2


# ---- 4. Combined detection ----------------------------------------------------

result <- poseidat_identify_fishing_operations(
  positions  = pos_trip,
  trips      = trip_row,
  trawl_data = trawl,
  speed_min  = 3,   # <- use the same calibrated values as speed_ops2 above
  speed_max  = 7.1
  # catch_data = your_kisten_data   # add once available -- this is the
  # signal that actually gives ground truth
)

result$operations |>
  dplyr::select(operation_id, op_start, op_end, duration_min, sources, confidence)


# ---- 5. Visual check: plot the trip with operations coloured differently ----
# poseidat_map() doesn't currently colour by operation_id (only by vessel) --
# a quick manual leaflet call shows fishing-flagged points distinctly:

pos_ops <- pos_trip |>
  dplyr::left_join(
    result$operations |> dplyr::select(operation_id, op_start, op_end),
    by = dplyr::join_by(dplyr::between(datetime, op_start, op_end))
  ) |>
  dplyr::mutate(
    is_fishing = !is.na(operation_id),
    # shown on hover -- quick coordinate readout without needing to click
    .label = sprintf("%.4f, %.4f", latitude, longitude),
    # shown on click -- fuller detail for comparing against known events
    .popup = sprintf(
      "<b>%.5f, %.5f</b><br>%s<br>SOG: %.1f kn%s",
      latitude, longitude, format(datetime, "%Y-%m-%d %H:%M"),
      speed_over_ground,
      ifelse(is_fishing, sprintf("<br>Operation %d", operation_id), "")
    )
  )

leaflet::leaflet(pos_ops) |>
  leaflet::addTiles() |>
  # grey layer FIRST -- low fillOpacity because at 1-min resolution over a
  # multi-day trip there are thousands of overlapping circles; at 0.8 those
  # stack into near-black, at ~0.2 they stay visibly grey
  leaflet::addCircleMarkers(
    data        = dplyr::filter(pos_ops, !is_fishing),
    lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE,
    fillColor = "grey60", fillOpacity = 0.2,
    label  = ~.label,
    popup  = ~.popup
  ) |>
  # red layer SECOND -- added after, so it draws on top of the grey layer
  # rather than being buried under it
  leaflet::addCircleMarkers(
    data        = dplyr::filter(pos_ops, is_fishing),
    lng = ~longitude, lat = ~latitude, radius = 3, stroke = FALSE,
    fillColor = "red", fillOpacity = 0.9,
    label  = ~.label,
    popup  = ~.popup
  )



# Grey = steaming/other, red = flagged as a fishing operation. Compare this
# against what you know actually happened on this trip.



# ---- 6. If it looks wrong -----------------------------------------------------
# - No operations detected at all -> traction_threshold_kg is probably too
#   high for this vessel's gear; check range(trawl$traction_center) above
#   and set the threshold well below the observed towing plateau.
# - Way too many/long operations -> speed window or traction threshold too
#   permissive; also check bridge_gap_min isn't stitching together separate
#   hauls that had a real gap between them.
# - Operations look plausible in number but boundaries are off -> this is
#   exactly what catch_data (real haul start/weighing times) would fix --
#   worth prioritising getting that hooked up over further threshold tuning.







library(httr2)
library(dplyr)
library(purrr)
library(arrow)
library(lubridate)
library(ggplot2)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

setup_poseidat_sync()
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))



# calendar heatmap, scoped to each vessel's own sensors, filtered by date
poseidat_sensor_availability(vessel_name = "Willem Jacob",
                             start_date  = "2026-08-01",
                             end_date    = "2026-08-20",
                             by          = "device_id")

# same, but return the table instead of the plot (e.g. to pipe into gt)
cov <- poseidat_sensor_availability(vessel_name = "Willem Jacob", start_date = "2026-08-01", by="device_id", return_data = TRUE)

# spot-check the actual values, 5 rows per vessel/sensor
poseidat_sensor_sample(vessel_name = "Willem Jacob",
                       start_date = "2026-08-01", end_date = "2026-08-20", n = 10, by="device_id") %>% View()

poseidat_plot_overview(
  vessel_name = c("Willem Jacob"),
  start_date  = as.Date("2026-04-01"),
  value_types = c("FUEL_CONSUMPTION"),
  by_device   = TRUE,
  nonzero    = FALSE,
  geom_type = "both"
)



digivloot_root <- here::here()
rmarkdown::render(file.path(here::here(),"R/poseidat", "digivloot_vessel_overview.Rmd"),
                  output_options = list(reference_docx = file.path(digivloot_root, "report_template.docx")))


source(file.path(here::here(),"R/poseidat", "Poseidat_get_device_details.R"))

get_device_details("dbmatic", journal_id = "d63c867e-df5c-4e2f-a88d-5ef75454d6cf")
get_device_details("pefa", journal_id = some_pefa_journal_id)
get_all_device_details("dbmatic", vessel_config)  # every enabled vessel at once
get_all_device_details("pefa", vessel_config)  # every enabled vessel at once
get_all_device_details("", vessel_config)  # every enabled vessel at once


getwd()
list.files(pattern = "report_template", full.names = TRUE, all.files = TRUE)
file.exists("report_template.dotx")
file.size("report_template.dovx")




debug_tbl <- fleet_sensor_debug("Willem Jacob", outlier_k = 3)

debug_tbl |>
  dplyr::filter(sensor == "Fuel consumption") |>
  dplyr::arrange(date, device_id) |>
  View()










# ============================================================================
# Gemiddeld totaal brandstofverbruik per schip en per uur, laatste 2 maanden,
# alleen tijdens een actieve visreis
# ============================================================================

start_date <- Sys.Date() - 60
start_date <- dmy("1/8/2026")
end_date   <- Sys.Date()

# 1. Thinned brandstofdata, exacte duplicaten eruit
fuel_raw <- .poseidat_load_data("thinned", source = NULL, vessel_name = NULL) |>
  filter(value_type == "FUEL_CONSUMPTION", !is.na(value_numeric)) |>
  filter(as.Date(datetime) >= start_date, as.Date(datetime) <= end_date) |>
  select(vessel_name, device_id, datetime, value_numeric) |>
  distinct()

# 2. Reizen in de periode
trips <- arrow::read_parquet(
  file.path(.onedrive(), "processed", "trips", "poseidat_trips.parquet")
) |>
  filter(as.Date(arrival_time) >= start_date, as.Date(departure_time) <= end_date)

# 3. Eerst koppelen aan actieve reisperiodes (semi_join voorkomt duplicatie
#    door eventueel overlappende reizen)
fuel_on_trip_raw <- fuel_raw |>
  dplyr::semi_join(
    trips,
    by = dplyr::join_by(vessel_name, dplyr::between(datetime, departure_time, arrival_time))
  )

# 4. Tukey's fence per schip + device, alleen op waarden tijdens het varen
tukey_bounds <- fuel_on_trip_raw |>
  group_by(vessel_name, device_id) |>
  summarise(
    q1    = quantile(value_numeric, 0.25, na.rm = TRUE),
    q3    = quantile(value_numeric, 0.75, na.rm = TRUE),
    iqr   = q3 - q1,
    lower = q1 - 3 * iqr,
    upper = q3 + 3 * iqr,
    .groups = "drop"
  )

fuel_on_trip <- fuel_on_trip_raw |>
  left_join(tukey_bounds, by = c("vessel_name", "device_id")) |>
  filter(value_numeric >= lower, value_numeric <= upper) |>
  select(vessel_name, device_id, datetime, value_numeric)

# 5. Korte device-namen ophalen uit vessel_config$device_labels
cfg <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

label_rows <- vector("list", nrow(cfg))
for (i in seq_len(nrow(cfg))) {
  known <- cfg$device_labels[[i]]
  if (length(known) == 0) next
  label_rows[[i]] <- tibble(
    vessel_name  = cfg$vessel_name[i],
    device_id    = names(known),
    device_label = unname(unlist(known))
  )
}
device_label_lookup <- bind_rows(label_rows)

fuel_on_trip <- fuel_on_trip |>
  left_join(device_label_lookup, by = c("vessel_name", "device_id")) |>
  mutate(device_label = dplyr::coalesce(device_label, device_id))

# 6a. Gemiddelde per sensor (korte naam), zichtbaar per schip
fuel_per_device <- fuel_on_trip |>
  group_by(vessel_name, device_label) |>
  summarise(fuel_gemiddeld = mean(value_numeric, na.rm = TRUE), .groups = "drop") |>
  arrange(vessel_name, device_label)

# 6b. Totaal per schip: eerst gemiddelde per device, dan optellen
fuel_per_vessel <- fuel_per_device |>
  group_by(vessel_name) |>
  summarise(fuel_gemiddeld_totaal = sum(fuel_gemiddeld, na.rm = TRUE), .groups = "drop")

fuel_per_device
fuel_per_vessel

# fuel_wide <- fuel_per_device |>
#   tidyr::pivot_wider(
#     names_from  = device_label,
#     values_from = fuel_gemiddeld,
#     values_fill = 0
#   ) |>
fuel_wide <- fuel_per_device |>
  tidyr::pivot_wider(
    names_from  = device_label,
    values_from = fuel_gemiddeld
  ) |>
  rowwise() |>
  mutate(Totaal = sum(dplyr::c_across(-vessel_name), na.rm = TRUE)) |>
  ungroup()

fuel_wide_display <- fuel_wide |>
  mutate(across(-vessel_name, ~ ifelse(is.na(.x), ".", format(round(.x, 1), nsmall = 1))))

fuel_wide_display

fuel_wide
.pefa_get_vessels()














nieuw_journal_id   <- pefa_vessels$journal_id[pefa_vessels$vessel_name == "ARAVIS"]
nieuw_vessel_naam <- "Aravis"
wj  <- vessel_config |> filter(vessel_name == nieuw_vessel_naam)
labels <- wj$device_labels[[1]]

for (did in names(labels)) {
  label <- labels[[did]]
  cat(sprintf("Syncing %s [%s] ...\n", label, did))
  
  sync_device(
    source              = "pefa",
    journal_id           = nieuw_journal_id,
    device_id            = did,
    vessel_name          = nieuw_vessel_naam,
    start_time          = as.POSIXct(Sys.Date() - 30, tz = "UTC"),
    end_time            = as.POSIXct(Sys.Date(), tz = "UTC"),
    # force_full           = TRUE,     # eerste keer: hele geschiedenis
    page_size            = 5000,
    sleep_between_pages  = 0
  )
  
  Sys.sleep(1)
}

# Controle
poseidat_files() |> filter(vessel_name == nieuw_vessel_naam)
poseidat_value_types() |> filter(vessel_name == nieuw_vessel_naam)








raw_new <- .poseidat_load_report_raw(nieuw_vessel_naam, Sys.Date() - 30, Sys.Date())

# Overzicht per device/label: value_type, aantal, bereik
raw_new |>
  left_join(
    tibble(device_id = names(labels), device_label = unlist(labels)),
    by = "device_id"
  ) |>
  group_by(device_label, value_type) |>
  summarise(
    n         = n(),
    first_obs = min(datetime, na.rm = TRUE),
    last_obs  = max(datetime, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  arrange(device_label)

# Statistische samenvatting van de numerieke sensoren (fuel, STW, heading)
raw_new |>
  left_join(
    tibble(device_id = names(labels), device_label = unlist(labels)),
    by = "device_id"
  ) |>
  filter(!is.na(value_numeric)) |>
  group_by(device_label, value_type) |>
  summarise(
    n    = n(),
    min  = min(value_numeric, na.rm = TRUE),
    mean = mean(value_numeric, na.rm = TRUE),
    max  = max(value_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# Voor POSITION en TRAWL_TENSION -- die hebben hun waarde in aparte
# kolommen, niet in value_numeric
raw_new |> filter(value_type == "POSITION") |>
  summarise(n = n(),
            lat_range = paste(round(range(latitude, na.rm=TRUE), 3), collapse=" - "),
            lon_range = paste(round(range(longitude, na.rm=TRUE), 3), collapse=" - "))

raw_new |> filter(value_type == "TRAWL_TENSION") |>
  summarise(n = n(),
            traction_center_range = paste(round(range(traction_center, na.rm=TRUE), 1), collapse=" - "))












# vind eerste datum bij PEFA
find_first_month_pefa <- function(journal_id, device_id,
                                  vanaf       = Sys.Date(),
                                  max_maanden = 36) {
  
  maand           <- lubridate::floor_date(as.Date(vanaf), "month")
  laatste_met_data <- NA
  
  for (i in 0:max_maanden) {
    start_t <- as.POSIXct(maand, tz = "UTC")
    end_t   <- as.POSIXct(maand %m+% months(1), tz = "UTC")
    
    d <- tryCatch(
      download_measurements("pefa", journal_id, device_id, start_t, end_t, page_size = 5000),
      error = function(e) tibble()
    )
    
    cat(sprintf("  %s: %d records\n", format(maand, "%Y-%m"), nrow(d)))
    
    if (nrow(d) > 0) {
      laatste_met_data <- maand
    } else if (!is.na(laatste_met_data)) {
      break   # eerste lege maand ná data gezien -> we zijn voorbij het begin
    }
    
    maand <- maand %m-% months(1)
    Sys.sleep(0.3)
  }
  
  laatste_met_data
}

eerste_gps <- find_first_month_pefa(
  nieuw_journal_id,
  "8df60832-990b-4fb2-8a1f-ef41eebc0274"   # GPS device_id
)
cat("Eerste maand met data (GPS):", format(eerste_gps, "%Y-%m"), "\n")

# Backfill per device, in maandblokken -- voorkomt dat 1 request 6 jaar
# aan data in 1 keer moet ophalen (en dus timeout)
backfill_chunked <- function(did, label, vanaf = as.Date("2024-01-01")) {
  
  maand_starts <- seq(vanaf, Sys.Date(), by = "1 month")
  
  for (i in seq_along(maand_starts)) {
    m <- maand_starts[i]      # [ behoudt de Date-klasse; direct itereren over
    # de vector met `for (m in maand_starts)` doet dat NIET
    # en levert stilletjes een kaal getal op
    
    start_t <- as.POSIXct(m, tz = "UTC")
    end_t   <- min(as.POSIXct(m, tz = "UTC") + months(1), Sys.time())
    
    cat(sprintf("  %s: %s -> %s ...\n", label, format(start_t, "%Y-%m"), format(end_t, "%Y-%m-%d")))
    
    tryCatch({
      sync_device(
        source              = "pefa",
        journal_id          = nieuw_journal_id,
        device_id           = did,
        vessel_name         = nieuw_vessel_naam,
        start_time          = start_t,
        end_time            = end_t,
        page_size           = 5000,
        sleep_between_pages = 0
      )
    }, error = function(e) {
      cat(sprintf("    FOUT bij %s: %s\n", format(start_t, "%Y-%m"), conditionMessage(e)))
    })
    
    Sys.sleep(1)
  }
}

vanaf <- as.Date("2025-09-01")

for (did in names(labels)) {
  label <- labels[[did]]
  cat(sprintf("Backfill %s ...\n", label))
  backfill_chunked(did, label, vanaf = vanaf)
}


lijstje <- c("d1a55252-cb35-4462-989f-8a6f9b97ab78", "9aba560b-2efe-4a11-b71f-b1807a68cfd7",
             "bbfe79e7-9452-46dd-bbf4-4b4a1a99de06", "c511bf10-1445-4962-8f51-c9deff4f7b67")

for (did in lijstje) {
  label <- labels[[did]]
  cat(sprintf("Backfill %s ...\n", label))
  backfill_chunked(did, label, vanaf = vanaf)
}


poseidat_files() |> filter(vessel_name == nieuw_vessel_naam)
poseidat_value_types() |> filter(vessel_name == nieuw_vessel_naam)
poseidat_thin(save = TRUE)


vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

catherina_journal_id <- get_journal_id(vessel_config, "Catherina", source = "pefa")
catherina_gps_id      <- get_device_by_label(vessel_config, "Catherina", "POSITION", source = "pefa")

catherina_journal_id
catherina_gps_id






catherina_gps_id_v2 <- sub("0000-000100000001", "0000-000200000001", catherina_gps_id)
catherina_gps_id_v2

test <- download_measurements(
  "pefa", catherina_journal_id, catherina_gps_id_v2,
  as.POSIXct("2026-09-01", tz = "UTC"),
  as.POSIXct("2026-09-21", tz = "UTC"),
  page_size = 5000
)
nrow(test)





vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

catherina_journal_id <- get_journal_id(vessel_config, "Catherina", source = "pefa")
i <- which(vessel_config$vessel_name == "Catherina")
oude_labels <- vessel_config$device_labels[[i]]

resultaten <- vector("list", length(oude_labels))

for (j in seq_along(oude_labels)) {
  oud_id   <- names(oude_labels)[j]
  label    <- oude_labels[[j]]
  nieuw_id <- sub("0000-0001", "0000-0002", oud_id)
  
  test <- tryCatch(
    download_measurements(
      "pefa", catherina_journal_id, nieuw_id,
      as.POSIXct(Sys.Date() - 14, tz = "UTC"),
      as.POSIXct(Sys.Date(), tz = "UTC"),
      page_size = 5000
    ),
    error = function(e) tibble()
  )
  
  resultaten[[j]] <- tibble(label = label, oud_id = oud_id, nieuw_id = nieuw_id, n = nrow(test))
  cat(sprintf("  %-15s  %s -> %s : %d records (laatste 14 dagen)\n",
              label, substr(oud_id, nchar(oud_id)-5, nchar(oud_id)),
              substr(nieuw_id, nchar(nieuw_id)-5, nchar(nieuw_id)), nrow(test)))
  
  Sys.sleep(0.3)
}

overzicht <- bind_rows(resultaten)
overzicht


nieuw_te_labelen <- overzicht |> filter(n > 0)

for (r in seq_len(nrow(nieuw_te_labelen))) {
  vessel_config$device_labels[[i]][[nieuw_te_labelen$nieuw_id[r]]] <- nieuw_te_labelen$label[r]
}

saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))

# Controle
vessel_config |> filter(vessel_name == "Catherina") |> pull(device_labels)

for (r in seq_len(nrow(nieuw_te_labelen))) {
  nieuw_id <- nieuw_te_labelen$nieuw_id[r]
  label    <- nieuw_te_labelen$label[r]
  
  eerste <- find_first_month_pefa(catherina_journal_id, nieuw_id)
  cat(sprintf("%s -> data vanaf %s\n", label, format(eerste, "%Y-%m")))
  
  backfill_chunked(nieuw_id, label, vanaf = eerste)
}

poseidat_thin(save = TRUE)



# Bekijk wat er per ongeluk onder Aravis is beland
list.files(file.path(.onedrive(), "raw", "pefa", "Aravis"), recursive = TRUE, pattern = "\\.parquet$")

# Verplaats de bestanden die van Catherina's devices zijn (herken ze aan het
# device_id in de bestandsnaam -- de nieuwe 0002-IDs die je net hebt gelabeld)
verkeerd_geplaatst <- list.files(file.path(.onedrive(), "raw", "pefa", "Aravis"),
                                 full.names = TRUE, pattern = "\\.parquet$")
catherina_ids <- nieuw_te_labelen$nieuw_id

for (f in verkeerd_geplaatst) {
  if (any(sapply(catherina_ids, function(id) grepl(id, f)))) {
    doel_dir <- file.path(.onedrive(), "raw", "pefa", "Catherina",
                          sub(".*/([0-9]{4})/([0-9]{2})/[^/]+$", "\\1/\\2", f))
    dir.create(doel_dir, recursive = TRUE, showWarnings = FALSE)
    file.rename(f, file.path(doel_dir, basename(f)))
    cat("Verplaatst:", basename(f), "\n")
  }
}


alle_raw_files <- list.files(file.path(.onedrive(), "raw"), recursive = TRUE,
                             pattern = "\\.parquet$", full.names = TRUE)

# Zoek alle bestanden die het GPS-device_id van Catherina bevatten
catherina_gps_bestanden <- alle_raw_files[grepl("386b3671-796f-7977-0000-000200000001", alle_raw_files)]
catherina_gps_bestanden


mei_pad <- catherina_gps_bestanden[1]   # de Aravis/2026/05 file
doel_dir <- file.path(.onedrive(), "raw", "pefa", "Catherina", "2026", "05")
dir.create(doel_dir, recursive = TRUE, showWarnings = FALSE)
doel_pad <- file.path(doel_dir, sub("^Aravis_", "Catherina_", basename(mei_pad)))

file.rename(mei_pad, doel_pad)


juni_aravis    <- arrow::read_parquet(catherina_gps_bestanden[2])  # Aravis/2026/06
juni_catherina <- arrow::read_parquet(catherina_gps_bestanden[3])  # Catherina/2026/06

nrow(juni_aravis)
nrow(juni_catherina)

# Zelfde tijdsbereik en ongeveer evenveel records?
range(juni_aravis$datetime)
range(juni_catherina$datetime)


samengevoegd <- dplyr::bind_rows(juni_aravis, juni_catherina) |>
  dplyr::distinct(entry_id, .keep_all = TRUE) |>   # dedup op entry_id, net als .save_monthly_additive() doet
  dplyr::arrange(datetime)

arrow::write_parquet(samengevoegd, catherina_gps_bestanden[3])  # overschrijf de Catherina-versie
file.remove(catherina_gps_bestanden[1])                         # verwijder de foutieve Aravis-versie








backfill_chunked <- function(did, label, vessel_name, vanaf = as.Date("2024-01-01")) {
  
  maand_starts <- seq(vanaf, Sys.Date(), by = "1 month")
  
  for (i in seq_along(maand_starts)) {
    m <- maand_starts[i]
    start_t <- as.POSIXct(m, tz = "UTC")
    end_t   <- min(as.POSIXct(m, tz = "UTC") + months(1), Sys.time())
    
    cat(sprintf("  %s: %s -> %s ...\n", label, format(start_t, "%Y-%m"), format(end_t, "%Y-%m-%d")))
    
    tryCatch({
      sync_device(
        source              = "pefa",
        journal_id          = catherina_journal_id,   # let op: ook journal_id expliciet, zelfde risico
        device_id           = did,
        vessel_name         = vessel_name,             # nu een parameter, geen globale variabele
        start_time          = start_t,
        end_time            = end_t,
        page_size           = 5000,
        sleep_between_pages = 0
      )
    }, error = function(e) {
      cat(sprintf("    FOUT bij %s: %s\n", format(start_t, "%Y-%m"), conditionMessage(e)))
    })
    
    Sys.sleep(1)
  }
}

for (r in seq_len(nrow(nieuw_te_labelen))) {
  nieuw_id <- nieuw_te_labelen$nieuw_id[r]
  label    <- nieuw_te_labelen$label[r]
  
  eerste <- find_first_month_pefa(catherina_journal_id, nieuw_id)
  cat(sprintf("%s -> data vanaf %s\n", label, format(eerste, "%Y-%m")))
  
  backfill_chunked(nieuw_id, label, vessel_name = "Catherina", vanaf = eerste)
}

poseidat_thin(save=TRUE)




# ==============================================================================
# 19 October 2026
# ==============================================================================

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

setup_poseidat_sync()

# Load vessel config (journals + device labels)
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

# ── STAP 1: vind het nieuwe schip bij PEFA ──────────────────────────────────
pefa_vessels <- get_vessels("pefa")
print(pefa_vessels)   # zoek de rij met het nieuwe schip

nieuw_journal_id   <- pefa_vessels$journal_id[pefa_vessels$vessel_name == "ARAVIS"]
nieuw_vessel_naam  <- "ARAVIS"   # exacte naam zoals bij PEFA

# ── STAP 2: welke devices heeft dit schip? ──────────────────────────────────
pefa_devices <- get_devices("pefa", nieuw_journal_id)
print(pefa_devices, n=100)
# kolommen: equipment_id, equipment_name, device_id, device_name, device_type, complete_up_to

adp <- .get_adapter("pefa")

resultaten <- vector("list", nrow(pefa_devices))

# for (i in seq_len(nrow(pefa_devices))) {
for (i in 27:32) {
  did  <- pefa_devices$device_id[i]
  name <- pefa_devices$device_name[i]
  
  sample <- tryCatch(
    download_measurements(
      source     = "pefa",
      journal_id = nieuw_journal_id,
      device_id  = did,
      start_time = Sys.time() - 7 * 86400,
      end_time   = Sys.time(),
      page_size  = 100
    ),
    error = function(e) tibble()
  )
  
  resultaten[[i]] <- tibble(
    device_name = name,
    device_id   = did,
    value_types = if (nrow(sample) > 0) paste(unique(sample$value_type), collapse = ", ") else NA_character_,
    n           = nrow(sample)
  )
  
  cat(sprintf("  %-30s ...%s  ->  %-20s  (n=%d)\n",
              name, substr(did, nchar(did) - 7, nchar(did)),
              resultaten[[i]]$value_types %||% "(geen data)", nrow(sample)))
  
  Sys.sleep(0.2)
}

device_overzicht <- bind_rows(resultaten)
device_overzicht %>% View()





# hekgalgsensor overzicht

hek <- download_measurements(
  source = "pefa", journal_id = nieuw_journal_id,
  device_id = pefa_devices$device_id[32],
  start_time = Sys.time() - 7 * 86400, end_time = Sys.time(), page_size = 100
)

# hek |>
#   summarise(across(everything(), ~ sum(!is.na(.x)))) |>
#   pivot_longer(everything(), names_to = "kolom", values_to = "n_gevuld") |>
#   filter(n_gevuld > 0)

hek_plot <- hek |>
  filter(!is.na(traction_center)) |>
  arrange(datetime) |>
  mutate(datetime_nl = lubridate::with_tz(datetime, "Europe/Amsterdam")) %>% 
  filter()

ggplot(hek_plot, aes(x = datetime_nl, y = traction_center)) +
  geom_line(linewidth = 0.3, colour = "steelblue") +
  labs(title = "Hekgalgmeter (Aravis): traction_center",
       x = NULL, y = "traction_center") +
  theme_minimal()

van <- as.POSIXct(paste(dmy("19-10-2026"), "10:30"), tz = "Europe/Amsterdam")
tot <- as.POSIXct(paste(dmy("20-10-2026"), "10:00"), tz = "Europe/Amsterdam")

van <- as.POSIXct(paste(dmy("19-10-2026"), "12:00"), tz = "Europe/Amsterdam")
tot <- as.POSIXct(paste(dmy("19-10-2026"), "13:00"), tz = "Europe/Amsterdam")

ggplot(hek_plot, aes(x = datetime_nl, y = traction_center)) +
  geom_line(linewidth = 0.3, colour = "steelblue") +
  coord_cartesian(xlim = c(van, tot)) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M",
                   timezone = "Europe/Amsterdam") +
  labs(title = "Hekgalgmeter (Aravis)",
       x = NULL, y = "kg") +
  theme_minimal()




library(dplyr)
library(stringr)

# source("Poseidat_functions.R")
setup_poseidat_sync()
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

vessels  <- vessel_config |> filter(enabled) |> select(source, vessel_name)
inv_list <- vector("list", nrow(vessels))

for (i in seq_len(nrow(vessels))) {
  inv_list[[i]] <- poseidat_raw_inventory(vessels$vessel_name[i], vessels$source[i]) |>
    mutate(source = vessels$source[i], vessel_name = vessels$vessel_name[i])
}

id_check <- bind_rows(inv_list) |>
  mutate(
    id_scheme = if_else(
      str_detect(device_id, "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-0000-[0-9a-f]{12}$"),
      "structured", "random"),
    box_prefix = if_else(id_scheme == "structured", str_sub(device_id, 1, 18), NA_character_),
    group      = if_else(id_scheme == "structured", str_sub(device_id, 25, 28), NA_character_),
    slot       = if_else(id_scheme == "structured", strtoi(str_sub(device_id, -8), 16L), NA_integer_)
  ) |>
  group_by(source, vessel_name, id_scheme, box_prefix, group, slot, device_id) |>
  summarise(value_types = paste(sort(unique(value_type)), collapse = ","),
            n           = sum(raw_n),
            first       = as.Date(min(raw_first)),
            last        = as.Date(max(raw_last)),
            .groups     = "drop") |>
  arrange(source, vessel_name, box_prefix, slot)

# Vessels with more than one box prefix -> a logger was replaced at some point
id_check |>
  filter(id_scheme == "structured") |>
  group_by(source, vessel_name) |>
  summarise(n_prefixes = n_distinct(box_prefix), .groups = "drop") |>
  filter(n_prefixes > 1)

print(id_check, n = Inf)




library(dplyr)
library(tidyr)

# ---- (a) EFICE: which device IDs are actually transmitting now? ------------
# Un-filtered sample (no deviceId). sortDirection is ignored by the API, so this
# returns the oldest `limit` entries of the window -- fine for listing devices.
efice_transmitting <- function(journal_id, hours = 24, limit = 10000L) {
  adp <- .get_adapter("efice")
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  resp <- httr2::request(paste0(adp$base_url, "/journals/entries")) |>
    httr2::req_auth_bearer_token(adp$token) |>
    httr2::req_url_query(journalId = journal_id,
                         startDate = fmt(Sys.time() - hours * 3600),
                         endDate   = fmt(Sys.time()),
                         limit     = limit, skip = 0L) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()
  
  tibble(item = httr2::resp_body_json(resp)$items) |>
    hoist(item, device_id  = "device_id",
          value_type = list("value", "type"),
          t          = "entry_datetime") |>
    count(device_id, value_type, name = "n_sample")
}

efice_vessels <- vessel_config |> filter(source == "efice", enabled)
now_list      <- vector("list", nrow(efice_vessels))

for (i in seq_len(nrow(efice_vessels))) {
  now_list[[i]] <- efice_transmitting(efice_vessels$journal_id[i]) |>
    mutate(vessel_name = efice_vessels$vessel_name[i], .before = 1)
}

efice_now <- bind_rows(now_list) |>
  left_join(id_check |> select(device_id, on_disk_last = last), by = "device_id")

print(efice_now, n = Inf)   # on_disk_last = NA  ->  device we have never synced


# ---- (b) PEFA: what does the inventory list today? -------------------------
pefa_inv <- bind_rows(
  get_devices("pefa", get_journal_id(vessel_config, "Catherina", "pefa"), "Catherina") |>
    mutate(vessel_name = "Catherina"),
  get_devices("pefa", get_journal_id(vessel_config, "Aravis", "pefa"), "Aravis") |>
    mutate(vessel_name = "Aravis")
) |>
  left_join(id_check |> select(device_id, value_types, first, last), by = "device_id") |>
  select(vessel_name, equipment_name, device_name, device_id, value_types, first, last)

print(pefa_inv, n = Inf)


# ---- (c) Catherina: does the old 0001 ID still return data, and which ID
#          does the API itself put in the records? (bypasses the overwrite
#          in download_measurements)
adp     <- .get_adapter("pefa")
jid_cat <- get_journal_id(vessel_config, "Catherina", "pefa")
t_end   <- Sys.time()
t_start <- t_end - 2 * 86400

bind_rows(
  adp$get_meas(adp$base_url, adp$token, jid_cat,
               "386b3671-796f-7977-0000-000100000025", t_start, t_end) |>
    count(returned_id = device_id) |> mutate(requested = "0001 / slot 25"),
  adp$get_meas(adp$base_url, adp$token, jid_cat,
               "386b3671-796f-7977-0000-000200000025", t_start, t_end) |>
    count(returned_id = device_id) |> mutate(requested = "0002 / slot 25")
)







library(dplyr)
library(tidyr)
library(stringr)

# Helper: what does the API itself put in device_id for a requested ID?
# (bypasses the overwrite in download_measurements; handles empty results)
probe_returned_id <- function(source, journal_id, device_id, hours = 2) {
  adp <- .get_adapter(source)
  m   <- tryCatch(
    adp$get_meas(adp$base_url, adp$token, journal_id, device_id,
                 Sys.time() - hours * 3600, Sys.time()),
    error = function(e) tibble()
  )
  tibble(
    source       = source,
    requested_id = device_id,
    n_records    = nrow(m),
    returned_ids = if (nrow(m) == 0) NA_character_
    else paste(sort(unique(m$device_id)), collapse = ", ")
  )
}

# ---- (c) redo: Catherina 0001 vs 0002 -------------------------------------
jid_cat <- get_journal_id(vessel_config, "Catherina", "pefa")
bind_rows(
  probe_returned_id("pefa", jid_cat, "386b3671-796f-7977-0000-000100000025", hours = 48),
  probe_returned_id("pefa", jid_cat, "386b3671-796f-7977-0000-000200000025", hours = 48)
)

# ---- (d) DBMatic: inventory today vs disk ---------------------------------
dbm_vessels <- vessel_config |> filter(source == "dbmatic", enabled)
dbm_list    <- vector("list", nrow(dbm_vessels))

for (i in seq_len(nrow(dbm_vessels))) {
  dbm_list[[i]] <- get_devices("dbmatic", dbm_vessels$journal_id[i],
                               dbm_vessels$vessel_name[i]) |>
    mutate(vessel_name = dbm_vessels$vessel_name[i])
}

dbm_inv <- bind_rows(dbm_list) |>
  full_join(id_check |> filter(source == "dbmatic") |>
              select(vessel_name, device_id, value_types, first, last),
            by = c("vessel_name", "device_id")) |>
  select(vessel_name, equipment_name, device_name, device_id, value_types, first, last) |>
  arrange(vessel_name, device_id)

print(dbm_inv, n = Inf)   # device_name NA -> on disk but no longer in inventory
# value_types NA -> in inventory but never synced

# ---- (e) DBMatic: does the API return the ID we asked for? -----------------
# 1-hour window -- a full week for Anna Hendrika is ~26 MB
bind_rows(
  probe_returned_id("dbmatic", get_journal_id(vessel_config, "Immanuel", "dbmatic"),
                    "32307764-6734-7a74-0000-000200000001", hours = 1),
  probe_returned_id("dbmatic", get_journal_id(vessel_config, "Anna Hendrika", "dbmatic"),
                    "70756278-3132-3931-0000-000200000001", hours = 1)
)

# ---- (f) Cross-platform: same box on EFICE? -------------------------------
# The EFICE journals for Immanuel / Anna Hendrika are disabled in config but
# can still be sampled. Uses efice_transmitting() from the previous check.
cross <- bind_rows(
  efice_transmitting(get_journal_id(vessel_config, "Immanuel",      "efice"), hours = 6) |>
    mutate(vessel_name = "Immanuel"),
  efice_transmitting(get_journal_id(vessel_config, "Anna Hendrika", "efice"), hours = 6) |>
    mutate(vessel_name = "Anna Hendrika")
) |>
  mutate(box_prefix = str_sub(device_id, 1, 18), slot = str_sub(device_id, -8)) |>
  left_join(id_check |> filter(source == "dbmatic") |>
              select(vessel_name, device_id, dbmatic_value_types = value_types),
            by = c("vessel_name", "device_id"))

print(cross, n = Inf)   # dbmatic_value_types filled -> identical IDs on both APIs

# ---- (g) Willem Jacob: conclusive check (6 h window stays under the limit) --
efice_transmitting(get_journal_id(vessel_config, "Willem Jacob", "efice"), hours = 6)







library(dplyr)
library(tidyr)
library(stringr)

efice_transmitting <- function(journal_id, hours = 24, limit = 10000L,
                               start = NULL) {
  adp <- .get_adapter("efice")
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  t0  <- if (is.null(start)) Sys.time() - hours * 3600 else as.POSIXct(start, tz = "UTC")
  
  resp <- httr2::request(paste0(adp$base_url, "/journals/entries")) |>
    httr2::req_auth_bearer_token(adp$token) |>
    httr2::req_url_query(journalId = journal_id,
                         startDate = fmt(t0),
                         endDate   = fmt(Sys.time()),
                         limit     = limit, skip = 0L) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()
  
  body  <- httr2::resp_body_json(resp)
  items <- body$items %||% list()
  
  if (length(items) == 0)
    return(tibble(device_id = NA_character_, value_type = NA_character_,
                  n_sample = 0L, first = as.POSIXct(NA), last = as.POSIXct(NA),
                  api_total = as.integer(body$total %||% 0L),
                  http_status = httr2::resp_status(resp)))
  
  tibble(item = items) |>
    hoist(item, device_id  = "device_id",
          value_type = list("value", "type"),
          t          = "entry_datetime") |>
    mutate(t = lubridate::ymd_hms(t, tz = "UTC", quiet = TRUE)) |>
    group_by(device_id, value_type) |>
    summarise(n_sample = n(), first = min(t), last = max(t), .groups = "drop") |>
    mutate(api_total   = as.integer(body$total %||% NA_integer_),
           http_status = httr2::resp_status(resp))
}

# ---- (f) Cross-platform: any data on the EFICE journals of the DBMatic boxes?
# Long window (from 2025-01-01) -- ascending order means we see the OLDEST
# entries, which is fine for "does this journal have data, and which IDs?"
cross <- bind_rows(
  efice_transmitting(get_journal_id(vessel_config, "Immanuel", "efice"),
                     start = "2025-01-01", limit = 5000L) |>
    mutate(vessel_name = "Immanuel"),
  efice_transmitting(get_journal_id(vessel_config, "Anna Hendrika", "efice"),
                     start = "2025-01-01", limit = 5000L) |>
    mutate(vessel_name = "Anna Hendrika")
) |>
  mutate(box_prefix = str_sub(device_id, 1, 18), slot = str_sub(device_id, -8)) |>
  left_join(id_check |> filter(source == "dbmatic") |>
              select(vessel_name, device_id, dbmatic_value_types = value_types),
            by = c("vessel_name", "device_id")) |>
  relocate(vessel_name)

print(cross, n = Inf)
# api_total = 0               -> journal has no data on EFICE at all
# dbmatic_value_types filled  -> identical device IDs on both platforms
# same box_prefix, no match   -> same box, different ID scheme per platform

# ---- (g) Willem Jacob: are fuel/depth really silent? -----------------------
efice_transmitting(get_journal_id(vessel_config, "Willem Jacob", "efice"), hours = 6)





# ==============================================================================
# 20 October 2026
# ==============================================================================

thin_file <- file.path(.onedrive(), "processed", "thinned", "Lub_Senior",
                       "Lub_Senior_thinned_1min_20261019.parquet")

open_dataset(thin_file) |>
  group_by(value_type) |>
  summarise(first = min(datetime), last = max(datetime), n = n()) |>
  collect() |>
  arrange(last)


fuel_ids <- unique(chk$device_id)

open_dataset(file.path(.onedrive(), "raw", "efice"), unify_schemas = TRUE) |>
  filter(device_id %in% fuel_ids,
         datetime >= as.POSIXct("2026-09-01", tz = "UTC")) |>
  group_by(device_id, value_type) |>
  summarise(last = max(datetime), n = n()) |>
  collect()

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))

setup_poseidat_sync()

# Load vessel config (journals + device labels)
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))






start_date <- as.Date("2026-08-01")
end_date   <- Sys.Date() - 1

# 1. Which devices are fuel sensors? Check this table before trusting sums.
fuel_devices <- fuel_device_lookup(vessel_config)
fuel_devices

# 1b. Diagnostic: which thinned files deliver fuel data for which days?
chk <- check_fuel_files("Lub Senior", start_date = start_date)
chk |>
  group_by(file) |>
  summarise(first = min(date), last = max(date), rows = sum(n_rows))

# 2. Load per-minute thinned data (fast) ...
fuel_obs <- load_fuel_obs(fuel_devices,
                          resolution = "thinned",
                          start_date = start_date,
                          end_date   = end_date)

# ... or raw high-res data (slower, for validation)
# fuel_obs <- load_fuel_obs(fuel_devices, resolution = "raw",
#                           start_date = start_date, end_date = end_date)

# 3. Daily litres per sensor
daily_device <- fuel_daily_by_device(fuel_obs,
                                     flow_unit   = "L_per_h",
                                     max_gap_min = 5,
                                     tz          = "Europe/Amsterdam",
                                     start_date  = start_date,
                                     end_date    = end_date)

# 4. Daily litres per vessel
daily_vessel <- fuel_daily_by_vessel(daily_device)
# e.g. if a device turns out to be a return-flow meter:
# daily_vessel <- fuel_daily_by_vessel(daily_device,
#                                      return_devices = "<device_id>")

# Wide table: one row per day, one column per vessel
daily_vessel |>
  select(date, vessel_name, fuel_L) |>
  pivot_wider(names_from = vessel_name, values_from = fuel_L) |>
  arrange(date)

# 5. Plots
plot_fuel_daily(daily_device, by_device = TRUE, min_coverage = 50)
plot_fuel_daily(daily_vessel)

chk <- check_fuel_files("Lub Senior", start_date = as.Date("2026-09-01"))
chk |>
  group_by(file) |>
  summarise(first = min(date), last = max(date), rows = sum(n_rows))

# 6. Export
writexl::write_xlsx(daily_vessel,
                    file.path(.onedrive(), "outputs",
                              paste0("fuel_daily_", format(Sys.Date(), "%Y%m%d"), ".xlsx")))
# write.csv2(daily_vessel,
#            file.path(.onedrive(), "outputs",
#                      paste0("fuel_daily_", format(Sys.Date(), "%Y%m%d"), ".csv")),
#            row.names = FALSE)



# ==============================================================================
# 20 October 2026
# ==============================================================================

# 1. Trim the Aravis config to the six sensors

setup_poseidat_sync()
config_pad    <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
vessel_config <- readRDS(config_pad)

pefa_devices <- get_devices("pefa", "940acddd-b61c-4d9a-a20f-ff2cd87d9156")

keep_names <- tribble(
  ~device_name,              ~label,
  "FC-Ltr-Eng01-ME [l/h]",   "FUEL_1",
  "FC-Ltr-Eng02-AE 1 [l/h]", "FUEL_2",
  "FC-Ltr-Eng03-AE 2 [l/h]", "FUEL_3",
  "GPS",                     "POSITION",
  "Trekkrachtmeter",         "TRAWL_TENSION",
  "Hekgalgmeter",            "HEKGALGMETER"
)

keep <- pefa_devices |>
  inner_join(keep_names, by = "device_name") |>
  select(device_id, label)
stopifnot(nrow(keep) == 6)

labels <- setNames(keep$label, keep$device_id)
i <- which(vessel_config$vessel_name == "Aravis" & vessel_config$source == "pefa")
vessel_config$device_labels[[i]] <- labels
saveRDS(vessel_config, config_pad)

# 3. Remove the unwanted devices from the raw files already on disk

raw_root <- .raw_dir("pefa", "Aravis")

bestanden <- tibble(pad = list.files(raw_root, pattern = "\\.parquet$",
                                     recursive = TRUE, full.names = TRUE)) |>
  mutate(device_id = sub("^[^_]+_(.+)_[0-9]{6}\\.parquet$", "\\1", basename(pad)))

te_verplaatsen <- bestanden |> filter(!device_id %in% names(labels))
count(te_verplaatsen, device_id)    # check the list first
View(te_verplaatsen)

doel <- file.path(.onedrive(), "_to_delete_aravis")
dir.create(doel, showWarnings = FALSE)
ok <- file.rename(te_verplaatsen$pad, file.path(doel, basename(te_verplaatsen$pad)))
table(ok)    # all TRUE; delete the folder yourself once the report looks right

# 4. Rebuild the thinned file for Aravis

poseidat_thin(source = "pefa", vessel_name = "Aravis",
              exclude_value_types = "TEMPERATURE", save = TRUE)




# ==============================================================================
# 9 november 2026
# ==============================================================================

setup_poseidat_sync()
.onedrive()                                   # note this path
config_pad <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
vessel_config <- readRDS(config_pad)

pefa_devices <- get_devices("pefa", "940acddd-b61c-4d9a-a20f-ff2cd87d9156")

keep_names <- tribble(
  ~device_name,              ~label,
  "FC-Ltr-Eng01-ME [l/h]",   "FUEL_1",
  "FC-Ltr-Eng02-AE 1 [l/h]", "FUEL_2",
  "FC-Ltr-Eng03-AE 2 [l/h]", "FUEL_3",
  "GPS",                     "POSITION",
  "Trekkrachtmeter",         "TRAWL_TENSION",
  "Hekgalgmeter",            "HEKGALGMETER"
)

keep <- pefa_devices |>
  inner_join(keep_names, by = "device_name") |>
  select(device_id, label)
print(keep)                      # must show 6 rows, one per label
stopifnot(nrow(keep) == 6)

# Same storage format as the existing entries: a named list, not a named vector
labels <- as.list(setNames(keep$label, keep$device_id))
i <- which(vessel_config$vessel_name == "Aravis" & vessel_config$source == "pefa")
stopifnot(length(i) == 1)
vessel_config$device_labels[[i]] <- labels
saveRDS(vessel_config, config_pad)

# Proof: read it back from disk
file.info(config_pad)$mtime
readRDS(config_pad) |> filter(vessel_name == "Aravis") |> pull(device_labels) |> first() |> names()




raw_root  <- .raw_dir("pefa", "Aravis")
bestanden <- tibble(pad = list.files(raw_root, pattern = "\\.parquet$",
                                     recursive = TRUE, full.names = TRUE)) |>
  mutate(device_id = sub("^[^_]+_(.+)_[0-9]{6}.*\\.parquet$", "\\1", basename(pad)))

te_verplaatsen <- bestanden |> filter(!device_id %in% names(labels))
nrow(te_verplaatsen)
count(te_verplaatsen, device_id)

length(list.files(file.path(.onedrive(), "_to_delete_aravis")))   # files moved earlier


# verplaatsen
doel <- file.path(.onedrive(), "_to_delete_aravis")
dir.create(doel, showWarnings = FALSE)

ok <- file.rename(te_verplaatsen$pad,
                  file.path(doel, basename(te_verplaatsen$pad)))
table(ok)    # all should be TRUE; delete the folder yourself once the report is fine


# Same file names in the move folder? Then these are restored copies
doel <- file.path(.onedrive(), "_to_delete_aravis")
sum(basename(te_verplaatsen$pad) %in% list.files(doel))   # ~80 = restored copies

range(file.info(te_verplaatsen$pad)$mtime)                # recent dates = written back recently

poseidat_thin(source = "pefa", vessel_name = "Aravis", save = TRUE)





# 11 november
config_pad <- file.path(.onedrive(), "config", "poseidat_vessels.rds")
keep_ids <- readRDS(config_pad) |>
  filter(vessel_name == "Aravis", source == "pefa") |>
  pull(device_labels) |> first() |> names()
stopifnot(is.character(keep_ids), length(keep_ids) == 6)

doel <- file.path(.onedrive(), "_to_delete_aravis")

terug <- te_verplaatsen |>
  filter(device_id %in% keep_ids) |>
  mutate(van = file.path(doel, basename(pad)))

count(terug, device_id)               # check: only your six sensors
stopifnot(all(file.exists(terug$van)))

for (d in unique(dirname(terug$pad))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
ok <- file.rename(terug$van, terug$pad)
table(ok)                             # all TRUE



bestanden <- tibble(pad = list.files(.raw_dir("pefa", "Aravis"), pattern = "\\.parquet$",
                                     recursive = TRUE, full.names = TRUE)) |>
  mutate(device_id = sub("^[^_]+_(.+)_[0-9]{6}.*\\.parquet$", "\\1", basename(pad)))

count(bestanden, device_id)                       # exactly your six ids
sum(!bestanden$device_id %in% keep_ids)           # 0




#backfill chunks
backfill_chunks <- function(device_id, vessel_name, journal_id,
                            van, tot = Sys.time(), uren = 6) {
  van    <- as.POSIXct(van, tz = "UTC")
  tot    <- as.POSIXct(tot, tz = "UTC")
  starts <- seq(van, tot, by = uren * 3600)
  res    <- vector("list", length(starts))
  
  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- min(s + uren * 3600, tot)
    cat(sprintf("[%d/%d] %s -> %s\n", k, length(starts),
                format(s, "%Y-%m-%d %H:%M"), format(e, "%Y-%m-%d %H:%M")))
    out <- tryCatch(
      sync_device(source = "pefa", journal_id = journal_id, device_id = device_id,
                  vessel_name = vessel_name, start_time = s, end_time = e,
                  force_full = FALSE, page_size = 5000, sleep_between_pages = 0),
      error = function(err) { message("  failed: ", conditionMessage(err)); NULL }
    )
    res[[k]] <- tibble(van = s, tot = e, n = if (is.null(out)) 0L else nrow(out))
    Sys.sleep(1)
  }
  bind_rows(res)
}

jid    <- "940acddd-b61c-4d9a-a20f-ff2cd87d9156"
trawl  <- "c511bf10-1445-4962-8f51-c9deff4f7b67"

options(poseidat.pefa_timeout = 120)

uit1 <- backfill_chunks(trawl, "Aravis", jid, van = "2026-09-29", tot = "2026-10-06", uren = 6)
uit2 <- backfill_chunks(trawl, "Aravis", jid, van = "2026-10-13", tot = "2026-11-10", uren = 6)

bind_rows(uit1, uit2) |> filter(n == 0)    # chunks that failed or were empty

mislukt <- bind_rows(uit1, uit2) |> filter(n == 0)
nrow(mislukt)

retry <- vector("list", nrow(mislukt))
for (k in seq_len(nrow(mislukt))) {
  retry[[k]] <- backfill_chunks(trawl, "Aravis", jid,
                                van = mislukt$van[k], tot = mislukt$tot[k], uren = 1)
}
bind_rows(retry) |> filter(n == 0)    # hours that still fail





dagtelling <- function(device_id, naam) {
  bestanden <- list.files(.raw_dir("pefa", "Aravis"),
                          pattern = paste0(device_id, ".*\\.parquet$"),
                          recursive = TRUE, full.names = TRUE)
  lijst <- vector("list", length(bestanden))
  for (k in seq_along(bestanden)) {
    lijst[[k]] <- arrow::read_parquet(bestanden[k], col_select = "datetime") |>
      distinct(datetime) |>
      transmute(datum = as.Date(datetime))
  }
  bind_rows(lijst) |> count(datum, name = "n") |> mutate(sensor = naam)
}

dagen <- bind_rows(
  dagtelling("c511bf10-1445-4962-8f51-c9deff4f7b67", "TRAWL"),
  dagtelling("6117b7b4-17ef-4285-8dc0-8585e078cf0c", "HEK"),
  dagtelling("d1a55252-cb35-4462-989f-8a6f9b97ab78", "FUEL_1")
) |>
  filter(datum >= as.Date("2026-09-15")) |>
  tidyr::pivot_wider(names_from = sensor, values_from = n) |>
  arrange(datum)

print(dagen, n = Inf)

hek <- "6117b7b4-17ef-4285-8dc0-8585e078cf0c"
options(poseidat.pefa_timeout = 120)
uit3 <- backfill_chunks(hek, "Aravis", jid, van = "2026-10-20", uren = 6)
filter(uit3, n == 0)


backfill_chunks(hek, "Aravis", jid, van = "2026-10-20 00:00", tot = "2026-10-20 06:00", uren = 1)

dagen <- bind_rows(
  dagtelling("c511bf10-1445-4962-8f51-c9deff4f7b67", "TRAWL"),
  dagtelling("6117b7b4-17ef-4285-8dc0-8585e078cf0c", "HEK"),
  dagtelling("d1a55252-cb35-4462-989f-8a6f9b97ab78", "FUEL_1")
) |>
  filter(datum >= as.Date("2026-10-15")) |>
  tidyr::pivot_wider(names_from = sensor, values_from = n) |>
  arrange(datum)
print(dagen, n = Inf)


poseidat_thin(source = "pefa", vessel_name = "Aravis",
              only_labelled = TRUE, full_rebuild = TRUE, save = TRUE)





library(dplyr)

thinned_dir <- file.path(.onedrive(), "processed", "thinned")
bestanden   <- list.files(thinned_dir, pattern = "_thinned_1min_.*\\.parquet$",
                          recursive = TRUE, full.names = TRUE)

for (f in seq_along(bestanden)) {
  tmp <- tempfile(fileext = ".parquet")
  file.copy(bestanden[f], tmp)
  x <- arrow::read_parquet(tmp)
  unlink(tmp)
  
  if (!"n_satellites" %in% names(x)) next
  oud_type <- class(x$n_satellites)[1]
  
  x <- x |>
    mutate(n_satellites = if_else(is.finite(n_satellites), n_satellites, NA_real_),
           n_satellites = as.integer(n_satellites))
  
  arrow::write_parquet(x, bestanden[f])
  cat(sprintf("%-60s %s -> integer\n", basename(bestanden[f]), oud_type))
}



# ==========================================================================
# 12 november 2026
# ==========================================================================

source(file.path(here::here(), "R/poseidat/Poseidat_check_renumbering.R"))
vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
chk <- check_device_renumbering(vessel_config)
chk$pairs |> print(n = Inf)

probe <- probe_after_silence(vessel_config, chk$dead)


# ==========================================================================
# 19 november 2026
# ==========================================================================

vn <- "Anna Hendrika"
fuel <- .poseidat_load_data("thinned", vessel_name = vn, value_types = "FUEL_CONSUMPTION",
                            start_date = as.Date("2026-11-01"), cols = "value_numeric")

# 1. Which devices, and how do their values look?
fuel |>
  group_by(device_id) |>
  summarise(n = n(), first = min(datetime), last = max(datetime),
            min = min(value_numeric, na.rm = TRUE),
            median = median(value_numeric, na.rm = TRUE),
            max = max(value_numeric, na.rm = TRUE),
            n_zero = sum(value_numeric == 0, na.rm = TRUE),
            .groups = "drop") |>
  left_join(.poseidat_device_labels(), by = "device_id") |>
  print(n = Inf, width = Inf)

# 2. The spike itself
fuel |> filter(value_numeric > 1e5) |> arrange(datetime) |> print(n = 20)

# 3. The name of the device on the API (id starts with 70756278)
cfg <- vessel_config |> filter(vessel_name == vn, source == "dbmatic")
get_devices("dbmatic", cfg$journal_id, vn) |>
  filter(grepl("^70756278", device_id)) |>
  print(width = Inf)

# 4. What the config has for this vessel
cfg |> pull(device_labels) |> first()





new <- fuel |> filter(grepl("0008$", device_id), value_numeric > -1000, value_numeric < 1e5)
quantile(new$value_numeric, c(0, .01, .05, .25, .5, .75, .95, .99, 1))
ggplot(new, aes(datetime, value_numeric)) + geom_line()


dev <- "70756278-3132-3931-0000-000200000008"
raw <- .poseidat_load_data("raw", vessel_name = "Anna Hendrika", device_ids = dev,
                           value_types = "FUEL_CONSUMPTION", cols = "value_numeric",
                           start_date = as.Date("2026-11-12"), end_date = as.Date("2026-11-12"))
raw |>
  filter(datetime >= as.POSIXct("2026-11-12 10:30", tz = "UTC"),
         datetime <= as.POSIXct("2026-11-12 10:40", tz = "UTC")) |>
  arrange(datetime) |> select(datetime, value_numeric) |> print(n = 60)


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
source(file.path(here::here(), "R", "poseidat", "Poseidat_get_device_details.R"))  # adjust path to where you saved it

dd_pefa    <- get_all_device_details("pefa",    vessel_config)
dd_dbmatic <- get_all_device_details("dbmatic", vessel_config)
dd_efice   <- get_all_device_details("efice", vessel_config)

names(dd_pefa)
names(dd_dbmatic)



bind_rows(dd_pefa |> mutate(source = "pefa"),
          dd_dbmatic |> mutate(source = "dbmatic")) |>
  group_by(source, vessel_name) |>
  summarise(devices    = n(),
            brand      = sum(!is.na(brand)      & brand      != ""),
            product_no = sum(!is.na(product_no) & product_no != ""),
            serial_no  = sum(!is.na(serial_no)  & serial_no  != ""),
            .groups = "drop")



rev <- poseidat_review_devices(vessel_config)


# ── Operational overview: fuel, speed, traction, depth ──────────────────────
# All vessels, thinned data (default)
poseidat_plot_operational(variables = "fuel", start_date = Sys.time() - lubridate::days(4))

poseidat_plot_operational(variables = "fuel", vessel="Anna Hendrika", start_date = Sys.time() - lubridate::days(4))



apply_device_renumbering(
  vessel_config,
  tibble(vessel_name   = "Anna Hendrika",
         new_device_id = "70756278-3132-3931-0000-000200000008",
         label         = "FUEL_MAIN"),
  save = TRUE)


rev$inventory |>
  filter(vessel_name == "Catherina") |>
  select(label, device_name, in_api, last_data, device_id) |>
  print(n = Inf, width = Inf)


vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))
i   <- which(vessel_config$vessel_name == "Catherina" & vessel_config$enabled)
bad <- c("386b3671-796f-7977-0000-000100000001", "386b3671-796f-7977-0000-000100000018",
         "386b3671-796f-7977-0000-000100000019", "386b3671-796f-7977-0000-00010000001f",
         "386b3671-796f-7977-0000-000100000020", "386b3671-796f-7977-0000-000100000025")
lab <- vessel_config$device_labels[[i]]
vessel_config$device_labels[[i]] <- lab[!names(lab) %in% bad]
vessel_config$device_labels[[i]]          # should show exactly 4 entries
# saveRDS(vessel_config, file.path(.onedrive(), "config", "poseidat_vessels.rds"))



rev$inventory |> filter(vessel_name == "Polar") |>
  select(label, value_type, first_data, last_data) |> print(n = Inf, width = Inf)




poseidat_ignore_devices("Aravis",
                        rev$flags |> filter(vessel_name == "Aravis", flag == "UNLABELLED_ACTIVE") |> pull(device_id),
                        reason = "only the 6 key sensors are used")






# 26/11/2026

leeg <- .poseidat_open_dataset("raw", sources = "efice",
                               start_date = as.Date("2026-09-01")) |>
  filter(value_type == "FUEL_CONSUMPTION") |>
  mutate(date = as.Date(datetime)) |>
  group_by(journal_id, device_id, date) |>
  summarise(n = n(), n_leeg = sum(is.na(value_numeric))) |>
  collect() |>
  filter(n_leeg > 0) |>
  left_join(vessel_config |> distinct(journal_id, vessel_name), by = "journal_id") |>
  arrange(vessel_name, device_id, date)

leeg |> group_by(vessel_name, device_id) |> summarise(van = min(date), tot = max(date))


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

test <- download_measurements(
  "efice", "a9294c48-ab38-4679-9dd5-f34003ba6b14",
  "3864776a-3075-3271-0000-000200000005",
  Sys.time() - 3600, Sys.time(), page_size = 5000)

test |> count(value_type, leeg = is.na(value_numeric))


poseidat_merge_conflict_copies()                  # eerst bekijken
poseidat_merge_conflict_copies(dry_run = FALSE)   # dan uitvoeren





alle <- tibble(pad = list.files(.onedrive(), recursive = TRUE, full.names = TRUE)) |>
  mutate(bestand   = basename(pad),
         gewijzigd = file.info(pad)$mtime)

# Wanneer zijn de oktober-hoofdbestanden voor het laatst geschreven?
alle |>
  filter(grepl("_202610\\.parquet$", bestand)) |>
  arrange(desc(gewijzigd)) |>
  select(bestand, gewijzigd)

# Conflictkopieën buiten raw/ (logbestanden, config, thinned)
alle |>
  filter(!grepl("/raw/", pad), grepl("-[A-Za-z0-9]+-[A-Za-z0-9]+\\.[a-z]+$", bestand)) |>
  select(pad, gewijzigd)


source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()



cfg_dir <- file.path(.onedrive(), "config")
f_main  <- file.path(cfg_dir, "poseidat_vessels.rds")
f_copy  <- list.files(cfg_dir, pattern = "^poseidat_vessels-LAPTOP", full.names = TRUE)

file.info(c(f_main, f_copy))[, "mtime", drop = FALSE]

a <- readRDS(f_main)
b <- readRDS(f_copy)

labels_long <- function(cfg) {
  out <- vector("list", nrow(cfg))
  for (i in seq_len(nrow(cfg))) {
    lab <- cfg$device_labels[[i]]
    out[[i]] <- tibble(
      source      = cfg$source[i],
      vessel_name = cfg$vessel_name[i],
      device_id   = names(lab) %||% character(),
      label       = as.character(unlist(lab, use.names = FALSE)) %||% character()
    )
  }
  bind_rows(out)
}

anti_join(labels_long(b), labels_long(a), by = c("source", "vessel_name", "device_id", "label"))  # alleen in de kopie
anti_join(labels_long(a), labels_long(b), by = c("source", "vessel_name", "device_id", "label"))  # alleen in het hoofdbestand

waldo::compare(select(a, -device_labels), select(b, -device_labels))





test <- download_measurements(
  "efice", "a9294c48-ab38-4679-9dd5-f34003ba6b14",
  "3864776a-3075-3271-0000-000200000005",
  Sys.time() - 3600, Sys.time(), page_size = 5000)

test |> count(value_type, leeg = is.na(value_numeric))



herstel <- leeg |>
  group_by(vessel_name, journal_id, device_id) |>
  summarise(van = min(date), .groups = "drop")

for (i in seq_len(nrow(herstel))) {
  sync_device_parallel(
    source      = "efice",
    journal_id  = herstel$journal_id[i],
    device_id   = herstel$device_id[i],
    vessel_name = herstel$vessel_name[i],
    start_time  = as.POSIXct(herstel$van[i], tz = "UTC"),
    chunk_by    = "1 week",
    page_size   = 5000
  )
}
