# ============================================================================
# Poseidat_fuel_daily.R
# Daily fuel consumption per DIGIvloot vessel (and per fuel sensor)
#
# Reads the per-minute thinned parquet files (processed/thinned/<vessel>/)
# or, optionally, the raw high-res parquet files (raw/<source>/<vessel>/),
# integrates the flow rate over time and returns litres per day.
#
# Integration:
#   each observation counts for the time until the next observation of the
#   same device, capped at `max_gap_min`. Longer gaps (sensor offline,
#   harbour, outage) contribute nothing and are reported via `coverage_pct`.
#   litres = flow (L/h) * minutes / 60
#
# Style: dplyr pipelines only; no apply-family, no purrr::map. All vessels are
# read in one go via arrow::open_dataset(), which is faster than looping over
# files and keeps filtering lazy (only fuel devices are pulled into memory).
#
# Dependencies: arrow, dplyr, tidyr, lubridate, ggplot2, scales
# ============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))


# ============================================================================
# 1.  FUEL DEVICE LOOKUP
#     One row per vessel x fuel device, taken from the device_labels in
#     vessel_config. Also serves to attach vessel_name to the sensor data
#     (device_id is unique per vessel).
# ============================================================================

fuel_device_lookup <- function(vessel_config,
                               fuel_keywords = c("brandstof", "fuel",
                                                 "flow", "verbruik")) {
  vessel_config |>
    select(vessel_name, device_labels) |>
    unnest_longer(device_labels,
                  indices_to = "device_id",
                  values_to  = "device_name") |>
    mutate(device_name = as.character(device_name)) |>
    filter(grepl(paste(fuel_keywords, collapse = "|"),
                 device_name, ignore.case = TRUE)) |>
    distinct(vessel_name, device_id, device_name)
}


# ============================================================================
# 2.  LOAD FUEL OBSERVATIONS
# ============================================================================

#' Load fuel flow observations for all vessels
#'
#' @param fuel_devices  Output of fuel_device_lookup()
#' @param resolution    "thinned" (per minute, default) or "raw" (high-res)
#' @param sources       Raw data source folder(s), only used for "raw"
#' @param start_date, end_date  Date range (inclusive)
#' @return tibble: vessel_name, device_id, device_name, datetime, flow
load_fuel_obs <- function(fuel_devices,
                          resolution = c("thinned", "raw"),
                          sources    = "efice",
                          start_date = Sys.Date() - 30,
                          end_date   = Sys.Date(),
                          fuel_value_type = "FUEL_CONSUMPTION") {

  resolution <- match.arg(resolution)

  # Pad by one day so that local-time day boundaries are complete
  t_start <- as.POSIXct(as.Date(start_date) - 1, tz = "UTC")
  t_end   <- as.POSIXct(as.Date(end_date)   + 2, tz = "UTC")

  ds <- .open_fuel_dataset(resolution, sources)

  # All candidate flow columns present in the schema. Files written by
  # different versions of the thinning code use different names; with
  # unify_schemas the missing one is NA, so we coalesce across all of them.
  value_cols <- intersect(c("value_numeric", "value_mean", "value"), names(ds))
  if (length(value_cols) == 0) {
    stop("No flow column found. Columns in the ", resolution, " files: ",
         paste(names(ds), collapse = ", "))
  }

  # Select fuel rows by value_type when available (catches sensors whose
  # device_id is not, or no longer, in vessel_config$device_labels)
  use_type <- "value_type" %in% names(ds)
  q <- if (use_type) {
    filter(ds, value_type == fuel_value_type)
  } else {
    filter(ds, device_id %in% fuel_devices$device_id)
  }
  message(sprintf("Fuel rows selected by %s; flow from: %s",
                  if (use_type) "value_type" else "device_labels",
                  paste(value_cols, collapse = " > ")))

  obs <- q |>
    filter(datetime >= t_start, datetime < t_end) |>
    select(any_of(c("vessel_name", "device_id", "datetime", value_cols))) |>
    collect() |>
    mutate(flow = coalesce(!!!syms(value_cols))) |>
    filter(!is.na(flow)) |>
    distinct(device_id, datetime, .keep_all = TRUE)

  # Vessel name: from the data if present, otherwise via the device lookup
  if (!"vessel_name" %in% names(obs)) {
    obs <- left_join(obs, distinct(fuel_devices, device_id, vessel_name),
                     by = "device_id")
  }

  obs |>
    left_join(distinct(fuel_devices, device_id, device_name), by = "device_id") |>
    mutate(device_name = coalesce(device_name, device_id)) |>
    filter(!is.na(vessel_name)) |>
    select(vessel_name, device_id, device_name, datetime, flow) |>
    arrange(vessel_name, device_id, datetime)
}


.open_fuel_dataset <- function(resolution = "thinned", sources = "efice") {
  if (resolution == "thinned") {
    open_dataset(file.path(.onedrive(), "processed", "thinned"),
                 unify_schemas = TRUE)
  } else {
    open_dataset(file.path(.onedrive(), "raw", sources),
                 unify_schemas = TRUE)
  }
}


#' Diagnostic: which files deliver fuel rows for which days?
#'
#' One row per file x device x day, with the number of rows and how many of
#' them have a value in each candidate flow column. Use it to spot stale
#' files, schema differences or device_ids missing from vessel_config.
check_fuel_files <- function(vessel = NULL,
                             resolution = "thinned",
                             sources    = "efice",
                             start_date = Sys.Date() - 60,
                             fuel_value_type = "FUEL_CONSUMPTION") {

  ds <- .open_fuel_dataset(resolution, sources)
  value_cols <- intersect(c("value_numeric", "value_mean", "value"), names(ds))

  q <- ds |>
    filter(value_type == fuel_value_type,
           datetime >= as.POSIXct(as.Date(start_date), tz = "UTC"))
  if (!is.null(vessel) && "vessel_name" %in% names(ds)) {
    q <- filter(q, vessel_name == vessel)
  }

  q |>
    mutate(file = add_filename()) |>
    select(file, device_id, datetime, any_of(value_cols)) |>
    collect() |>
    mutate(file = basename(file),
           date = as.Date(datetime)) |>
    group_by(file, device_id, date) |>
    summarise(n_rows = n(),
              across(any_of(value_cols), \(x) sum(!is.na(x)), .names = "n_{.col}"),
              .groups = "drop") |>
    arrange(device_id, date, file)
}


# ============================================================================
# 3.  DAILY CONSUMPTION PER VESSEL x DEVICE
# ============================================================================

#' Integrate flow rate to litres per day
#'
#' @param fuel_obs     Output of load_fuel_obs()
#' @param flow_unit    "L_per_h" (default), "L_per_min" or "kg_per_h"
#' @param max_gap_min  Max minutes a single observation may represent.
#'                     Use 1 for thinned data if you want a strict sum of
#'                     minute bins; 5 bridges short dropouts.
#' @param tz           Time zone defining "a day" (default Europe/Amsterdam;
#'                     use "UTC" to match the parquet timestamps)
#' @param start_date, end_date  Trim the output to this range
fuel_daily_by_device <- function(fuel_obs,
                                 flow_unit   = c("L_per_h", "L_per_min", "kg_per_h"),
                                 max_gap_min = 5,
                                 tz          = "Europe/Amsterdam",
                                 start_date  = NULL,
                                 end_date    = NULL) {

  flow_unit <- match.arg(flow_unit)
  diesel_density <- 0.84   # kg/L, only used for kg_per_h

  out <- fuel_obs |>
    group_by(vessel_name, device_id, device_name) |>
    arrange(datetime, .by_group = TRUE) |>
    mutate(
      flow_L_per_h = case_when(
        flow_unit == "L_per_min" ~ flow * 60,
        flow_unit == "kg_per_h"  ~ flow / diesel_density,
        TRUE                     ~ flow
      ),
      step_min = as.numeric(difftime(lead(datetime), datetime, units = "mins")),
      step_min = pmin(coalesce(step_min, 1), max_gap_min),
      litres   = coalesce(flow_L_per_h, 0) * step_min / 60,
      date     = as.Date(with_tz(datetime, tz))
    ) |>
    group_by(vessel_name, device_id, device_name, date) |>
    summarise(
      fuel_L         = sum(litres),
      minutes_cov    = sum(step_min),
      coverage_pct   = pmin(100, minutes_cov / 1440 * 100),
      active_hours   = sum(step_min[flow_L_per_h > 1], na.rm = TRUE) / 60,
      mean_flow_L_h  = mean(flow_L_per_h[flow_L_per_h > 1], na.rm = TRUE),
      max_flow_L_h   = max(flow_L_per_h, na.rm = TRUE),
      n_obs          = n(),
      .groups        = "drop"
    ) |>
    mutate(across(c(fuel_L, coverage_pct, active_hours, mean_flow_L_h),
                  \(x) round(x, 1)))

  if (!is.null(start_date)) out <- filter(out, date >= as.Date(start_date))
  if (!is.null(end_date))   out <- filter(out, date <= as.Date(end_date))
  out
}


# ============================================================================
# 4.  DAILY CONSUMPTION PER VESSEL
#     Sums all fuel sensors of a vessel. Use `exclude_devices` to drop sensors
#     that should not be added (e.g. a return-flow meter), or `return_devices`
#     to subtract them (supply - return = net consumption).
# ============================================================================

fuel_daily_by_vessel <- function(daily_device,
                                 exclude_devices = character(0),
                                 return_devices  = character(0)) {
  daily_device |>
    filter(!device_id %in% exclude_devices) |>
    mutate(sign = if_else(device_id %in% return_devices, -1, 1)) |>
    group_by(vessel_name, date) |>
    summarise(
      fuel_L           = sum(sign * fuel_L),
      n_devices        = n_distinct(device_id),
      min_coverage_pct = min(coverage_pct),
      active_hours     = max(active_hours),
      .groups          = "drop"
    ) |>
    mutate(fuel_L = round(fuel_L, 1))
}


# ============================================================================
# 5.  PLOT
# ============================================================================

plot_fuel_daily <- function(daily, by_device = FALSE, min_coverage = 0) {

  df <- daily
  if ("coverage_pct" %in% names(df)) df <- filter(df, coverage_pct >= min_coverage)

  p <- ggplot(df, aes(x = date, y = fuel_L,
                      fill = if (by_device) device_name else vessel_name)) +
    geom_col(width = 0.8) +
    facet_wrap(~ vessel_name, scales = "free_y", ncol = 2) +
    scale_x_date(date_labels = "%d %b") +
    scale_y_continuous(labels = label_comma()) +
    labs(title = "Daily fuel consumption per DIGIvloot vessel",
         subtitle = "Flow rate integrated over time  |  gaps not filled",
         x = NULL, y = "Fuel (L/day)", fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = if (by_device) "bottom" else "none",
          strip.text      = element_text(face = "bold"),
          plot.title      = element_text(face = "bold"))
  p
}


# ============================================================================
# EXAMPLE USAGE
# ============================================================================
if (FALSE) {

  start_date <- as.Date("2026-09-01")
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
  write.csv2(daily_vessel,
             file.path(.onedrive(), "output",
                       paste0("fuel_daily_", format(Sys.Date(), "%Y%m%d"), ".csv")),
             row.names = FALSE)
}
