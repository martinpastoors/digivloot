# =============================================================================
# Poseidat_fuel_trip.R
#
# Fuel consumption per trip, per vessel and per fuel sensor.
#
# Builds on two existing pieces of the Poseidat code:
#   - poseidat_fuel_obs()  (fuel flow observations: vessel_name, device_id,
#                           device_name, datetime, flow)
#   - processed/trips/poseidat_trips.parquet, the trip table written by
#     poseidat_write_trip_ids() / poseidat_update_trip_ids() in the daily
#     sync (vessel_name, trip_id, trip_uid, departure_harbour,
#     departure_time, arrival_harbour, arrival_time, ...).
#     Trips are NOT re-identified here; poseidat_load_trips() just reads
#     that table, so trip_id / trip_uid match the tags on the thinned files.
#
# Logic
#   1. The flow rate is integrated to litres per observation, exactly as in
#      poseidat_fuel_daily(): each reading counts for the time until the next
#      reading, capped at max_gap_min.
#   2. Each observation is assigned to a trip when its datetime falls between
#      departure_time and arrival_time of that vessel. This is the same
#      between-join poseidat_write_trip_ids() uses to tag the thinned rows,
#      so it gives the same trip_id -- and it also works for raw data,
#      which carries no trip tags.
#   3. Observations outside a trip are fuel used in harbour. They are assigned
#      to the trip that arrived just before (rolling join), so every trip gets
#      "fuel at sea" plus "fuel in harbour after the trip".
#   4. Litres are summed per vessel x device x trip x phase.
#
# Requires: dplyr >= 1.1.0 (join_by() with between() / closest()),
#           tidyr, lubridate, ggplot2, scales
# Source Poseidat_functions.R first.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


# ---- load the stored trip table -----------------------------------------------

#' Read the trip table written by the daily sync
#'
#' @param vessel_name  Character vector to restrict to, or NULL for all
#' @param start_date, end_date  Keep trips that overlap this date range
#'                     (NULL = no limit). Open trips (no arrival yet) are
#'                     treated as running until now.
#' @return tibble with one row per trip, as stored in poseidat_trips.parquet
#' @export
poseidat_load_trips <- function(vessel_name = NULL,
                                start_date  = NULL,
                                end_date    = NULL) {

  trips_file <- file.path(.onedrive(), "processed", "trips", "poseidat_trips.parquet")
  if (!file.exists(trips_file)) {
    stop("No trip table found at ", trips_file,
         ". Run poseidat_update_trip_ids(dry_run = FALSE) first.")
  }

  vn_filter <- vessel_name
  trips     <- arrow::read_parquet(trips_file)

  if (!is.null(vn_filter)) trips <- filter(trips, vessel_name %in% vn_filter)
  if (!is.null(start_date)) {
    trips <- filter(trips, coalesce(arrival_time, Sys.time()) >=
                             as.POSIXct(as.Date(start_date), tz = "UTC"))
  }
  if (!is.null(end_date)) {
    trips <- filter(trips, departure_time <
                             as.POSIXct(as.Date(end_date) + 1, tz = "UTC"))
  }

  arrange(trips, vessel_name, departure_time)
}


# ---- fuel per trip per device -------------------------------------------------

#' Integrate fuel flow to litres per trip, per vessel x device
#'
#' @param fuel_obs     Output of poseidat_fuel_obs()
#' @param trips        Output of poseidat_load_trips() (needs vessel_name,
#'                     trip_id, departure_time, arrival_time; trip_uid and
#'                     the harbour columns are carried through if present).
#'                     Open trips (arrival_time NA) run until the last fuel
#'                     observation and are flagged complete_trip = FALSE.
#' @param flow_unit    "L_per_h" (default), "L_per_min" or "kg_per_h"
#' @param max_gap_min  Max minutes a single observation may represent.
#'                     1 = strict sum of 1-min bins; 5 (default) bridges
#'                     short dropouts. Longer gaps add nothing.
#' @param include_harbour  If TRUE (default), also return the fuel used in
#'                     harbour after each trip (until the next departure).
#' @param max_flow_L_h Readings above this flow (after unit conversion) are
#'                     treated as sensor errors and removed before
#'                     integration. Default 1000 L/h. Negative readings are
#'                     always removed. A removed reading leaves a gap that is
#'                     bridged by the previous reading up to max_gap_min.
#' @param stale_trip_days  Warn when a vessel's last trip in `trips` ends more
#'                     than this many days before its last fuel reading,
#'                     which usually means the trip table was not updated.
#' @return tibble per vessel x device x trip x phase ("at_sea" /
#'   "harbour_after"): fuel_L, minutes_cov, period_hours, coverage_pct,
#'   active_hours, mean_flow_L_h, max_flow_L_h, n_obs, plus the trip
#'   details from `trips`.
#' @export
poseidat_fuel_trip <- function(fuel_obs,
                               trips,
                               flow_unit       = c("L_per_h", "L_per_min", "kg_per_h"),
                               max_gap_min     = 5,
                               include_harbour = TRUE,
                               max_flow_L_h    = 1000,
                               stale_trip_days = 1) {

  flow_unit      <- match.arg(flow_unit)
  diesel_density <- 0.84   # kg/L, only used for kg_per_h

  if (nrow(trips) == 0) stop("`trips` is empty. Check poseidat_load_trips().")
  if (nrow(fuel_obs) == 0) stop("`fuel_obs` is empty. Run poseidat_fuel_obs() first.")

  # ── 0. Normalise the trip table ────────────────────────────────────────────
  #    Open trips (still at sea at the last trip update) get the vessel's last
  #    fuel observation as provisional arrival time. Trips of vessels without
  #    fuel data are dropped.
  last_obs <- fuel_obs |>
    group_by(vessel_name) |>
    summarise(last_obs_time = max(datetime), .groups = "drop")

  if (!"trip_uid" %in% names(trips)) {
    trips <- mutate(trips, trip_uid = paste(vessel_name, trip_id, sep = "_"))
  }
  if (!"departure_harbour" %in% names(trips)) trips <- mutate(trips, departure_harbour = NA_character_)
  if (!"arrival_harbour"   %in% names(trips)) trips <- mutate(trips, arrival_harbour   = NA_character_)

  trips <- trips |>
    inner_join(last_obs, by = "vessel_name") |>
    mutate(
      open_trip           = is.na(arrival_time),
      arrival_time        = coalesce(arrival_time, last_obs_time),
      trip_duration_hours = as.numeric(difftime(arrival_time, departure_time,
                                                units = "hours"))
    ) |>
    filter(!is.na(arrival_time), arrival_time >= departure_time) |>
    select(-last_obs_time)

  if (nrow(trips) == 0) stop("None of the trips belong to a vessel in `fuel_obs`.")

  # Stale trip table: the last trip ends well before the fuel data does.
  # Everything after the last arrival is then counted as "harbour_after",
  # even if the vessel is at sea.
  stale <- trips |>
    group_by(vessel_name) |>
    summarise(last_trip_end = max(arrival_time), .groups = "drop") |>
    inner_join(last_obs, by = "vessel_name") |>
    filter(as.numeric(difftime(last_obs_time, last_trip_end, units = "days")) >
             stale_trip_days)

  if (nrow(stale) > 0) {
    warning(paste0(
      "Trip table may be out of date -- last trip ends long before the fuel data:\n",
      paste(sprintf("  %s: last trip ends %s, fuel data runs to %s",
                    stale$vessel_name,
                    format(stale$last_trip_end, "%Y-%m-%d %H:%M"),
                    format(stale$last_obs_time, "%Y-%m-%d %H:%M")),
            collapse = "\n"),
      "\nFuel after the last trip is counted as harbour_after. ",
      "Run poseidat_update_trip_ids(dry_run = FALSE) and reload the trips."
    ), call. = FALSE)
  }

  # ── 1. Convert units and remove implausible readings ─────────────────────
  obs_conv <- fuel_obs |>
    mutate(
      flow_L_per_h = case_when(
        flow_unit == "L_per_min" ~ flow * 60,
        flow_unit == "kg_per_h"  ~ flow / diesel_density,
        TRUE                     ~ flow
      ),
      bad_reading = is.na(flow_L_per_h) | flow_L_per_h < 0 |
                    flow_L_per_h > max_flow_L_h
    )

  bad_summary <- obs_conv |>
    filter(bad_reading) |>
    group_by(vessel_name, device_id) |>
    summarise(n_bad     = n(),
              first_bad = min(datetime),
              last_bad  = max(datetime),
              max_value = suppressWarnings(max(flow_L_per_h, na.rm = TRUE)),
              .groups   = "drop")

  if (nrow(bad_summary) > 0) {
    message("Removed implausible fuel readings (< 0 or > ", max_flow_L_h, " L/h):")
    message(paste(sprintf("  %s / ...%s: %d reading(s) between %s and %s (max %s)",
                          bad_summary$vessel_name,
                          substr(bad_summary$device_id,
                                 nchar(bad_summary$device_id) - 2,
                                 nchar(bad_summary$device_id)),
                          bad_summary$n_bad,
                          format(bad_summary$first_bad, "%Y-%m-%d %H:%M"),
                          format(bad_summary$last_bad,  "%Y-%m-%d %H:%M"),
                          format(round(bad_summary$max_value), big.mark = ",")),
                  collapse = "\n"))
  }

  # ── 2. Litres per observation (same integration as poseidat_fuel_daily) ────
  #    Done on the full stream before splitting into trips, so step lengths
  #    are not cut off at trip boundaries.
  obs <- obs_conv |>
    filter(!bad_reading) |>
    select(-bad_reading) |>
    group_by(vessel_name, device_id, device_name) |>
    arrange(datetime, .by_group = TRUE) |>
    mutate(
      step_min = as.numeric(difftime(lead(datetime), datetime, units = "mins")),
      step_min = pmin(coalesce(step_min, 1), max_gap_min),
      litres   = flow_L_per_h * step_min / 60
    ) |>
    ungroup()

  # ── 3. Trip windows ────────────────────────────────────────────────────────
  #    The harbour period after trip N runs from its arrival_time until the
  #    next departure_time of the same vessel (NA for the last trip = open).
  trip_windows <- trips |>
    arrange(vessel_name, departure_time) |>
    group_by(vessel_name) |>
    mutate(next_departure_time = lead(departure_time)) |>
    ungroup() |>
    select(vessel_name, trip_id, departure_time, arrival_time,
           next_departure_time)

  # ── 4. Observations at sea: datetime within [departure, arrival] ─────────
  at_sea <- obs |>
    inner_join(
      trip_windows |> select(vessel_name, trip_id, departure_time, arrival_time),
      by       = join_by(vessel_name, between(datetime, departure_time, arrival_time)),
      multiple = "first"
    ) |>
    mutate(phase = "at_sea") |>
    select(-departure_time, -arrival_time)

  # ── 5. Observations in harbour: attach to the last trip that arrived ──────
  #    Rolling join on the most recent arrival_time before the observation.
  #    Observations before the first trip of a vessel get no trip and are
  #    dropped. Observations after the last trip are kept as harbour_after
  #    of that last trip.
  fuel_by_obs <- at_sea

  if (include_harbour) {
    in_harbour <- obs |>
      anti_join(at_sea |> select(vessel_name, device_id, datetime),
                by = c("vessel_name", "device_id", "datetime")) |>
      inner_join(
        trip_windows |> select(vessel_name, trip_id, arrival_time, next_departure_time),
        by = join_by(vessel_name, closest(datetime > arrival_time))
      ) |>
      filter(is.na(next_departure_time) | datetime < next_departure_time) |>
      mutate(phase = "harbour_after") |>
      select(-arrival_time, -next_departure_time)

    fuel_by_obs <- bind_rows(at_sea, in_harbour)
  }

  # ── 6. Period length per trip x phase (for coverage) ──────────────────────
  #    at_sea:        departure -> arrival
  #    harbour_after: arrival -> next departure (or last observation of the
  #                   vessel after the final trip)
  periods <- trip_windows |>
    left_join(last_obs, by = "vessel_name") |>
    mutate(
      at_sea        = as.numeric(difftime(arrival_time, departure_time, units = "hours")),
      harbour_after = as.numeric(difftime(coalesce(next_departure_time, last_obs_time),
                                          arrival_time, units = "hours"))
    ) |>
    select(vessel_name, trip_id, at_sea, harbour_after) |>
    pivot_longer(c(at_sea, harbour_after),
                 names_to = "phase", values_to = "period_hours") |>
    mutate(period_hours = pmax(period_hours, 0))

  # ── 7. Sum per vessel x device x trip x phase ─────────────────────────────
  #    complete_trip: trip has ended (not open) and both harbours are known
  #    (a missing departure harbour usually means the trip started before
  #    the data did).
  trip_info <- trips |>
    select(vessel_name, trip_id, trip_uid, departure_harbour, departure_time,
           arrival_harbour, arrival_time, trip_duration_hours, open_trip) |>
    mutate(complete_trip = !open_trip &
                           !is.na(departure_harbour) & !is.na(arrival_harbour))

  fuel_by_obs |>
    group_by(vessel_name, device_id, device_name, trip_id, phase) |>
    summarise(
      fuel_L        = sum(litres, na.rm = TRUE),
      minutes_cov   = sum(step_min),
      active_hours  = sum(step_min[flow_L_per_h > 1], na.rm = TRUE) / 60,
      mean_flow_L_h = mean(flow_L_per_h[flow_L_per_h > 1], na.rm = TRUE),
      max_flow_L_h  = max(flow_L_per_h, na.rm = TRUE),
      n_obs         = n(),
      .groups       = "drop"
    ) |>
    left_join(periods, by = c("vessel_name", "trip_id", "phase")) |>
    mutate(
      coverage_pct  = if_else(period_hours > 0,
                              pmin(100, minutes_cov / (period_hours * 60) * 100),
                              NA_real_),
      mean_flow_L_h = if_else(is.nan(mean_flow_L_h), NA_real_, mean_flow_L_h)
    ) |>
    left_join(trip_info, by = c("vessel_name", "trip_id")) |>
    mutate(across(c(fuel_L, period_hours, coverage_pct, active_hours, mean_flow_L_h),
                  \(x) round(x, 1))) |>
    select(vessel_name, trip_id, trip_uid, departure_harbour, departure_time,
           arrival_harbour, arrival_time, trip_duration_hours, open_trip,
           complete_trip, phase, device_id, device_name,
           fuel_L, period_hours, minutes_cov, coverage_pct,
           active_hours, mean_flow_L_h, max_flow_L_h, n_obs) |>
    arrange(vessel_name, trip_id, phase, device_id)
}


# ---- fuel per trip per vessel -------------------------------------------------

#' Sum trip fuel over the fuel devices of each vessel
#'
#' Returns one row per vessel x trip, with fuel at sea and in harbour side by
#' side, and fuel per hour / per day at sea for comparing trips and vessels.
#'
#' @param trip_device      Output of poseidat_fuel_trip()
#' @param exclude_devices  device_ids to leave out of the vessel total
#' @param return_devices   device_ids of return-flow meters, subtracted
#'                         instead of added (supply - return = net use)
#' @return tibble per vessel x trip: fuel_sea_L, fuel_harbour_L, fuel_total_L,
#'   fuel_sea_L_per_h, fuel_sea_L_per_day, n_devices, min_coverage_sea_pct
#' @export
poseidat_fuel_trip_vessel <- function(trip_device,
                                      exclude_devices = character(0),
                                      return_devices  = character(0)) {

  per_phase <- trip_device |>
    filter(!device_id %in% exclude_devices) |>
    mutate(sign = if_else(device_id %in% return_devices, -1, 1)) |>
    group_by(vessel_name, trip_id, trip_uid, departure_harbour, departure_time,
             arrival_harbour, arrival_time, trip_duration_hours,
             open_trip, complete_trip, phase) |>
    summarise(
      fuel_L           = sum(sign * fuel_L),
      n_devices        = n_distinct(device_id),
      min_coverage_pct = suppressWarnings(min(coverage_pct, na.rm = TRUE)),
      .groups          = "drop"
    ) |>
    mutate(min_coverage_pct = if_else(is.infinite(min_coverage_pct),
                                      NA_real_, min_coverage_pct))

  wide <- per_phase |>
    pivot_wider(
      id_cols     = c(vessel_name, trip_id, trip_uid, departure_harbour,
                      departure_time, arrival_harbour, arrival_time,
                      trip_duration_hours, open_trip, complete_trip),
      names_from  = phase,
      values_from = c(fuel_L, n_devices, min_coverage_pct)
    )

  # pivot_wider only creates columns for phases that exist
  # (no harbour columns when include_harbour = FALSE)
  if (!"fuel_L_harbour_after" %in% names(wide)) {
    wide <- wide |>
      mutate(fuel_L_harbour_after           = NA_real_,
             min_coverage_pct_harbour_after = NA_real_)
  }

  wide |>
    transmute(
      vessel_name, trip_id, trip_uid,
      departure_harbour, departure_time,
      arrival_harbour, arrival_time,
      trip_duration_hours = round(trip_duration_hours, 1),
      open_trip, complete_trip,
      fuel_sea_L           = round(fuel_L_at_sea, 1),
      fuel_harbour_L       = round(fuel_L_harbour_after, 1),
      fuel_total_L         = round(fuel_L_at_sea + coalesce(fuel_L_harbour_after, 0), 1),
      fuel_sea_L_per_h     = round(fuel_L_at_sea / trip_duration_hours, 1),
      fuel_sea_L_per_day   = round(fuel_L_at_sea / trip_duration_hours * 24, 0),
      n_devices            = n_devices_at_sea,
      min_coverage_sea_pct = min_coverage_pct_at_sea,
      min_coverage_harbour_pct = min_coverage_pct_harbour_after
    ) |>
    arrange(vessel_name, departure_time)
}


# ---- plot ---------------------------------------------------------------------

#' Bar chart of fuel per trip, one panel per vessel
#'
#' @param trip_data     Output of poseidat_fuel_trip() (by_device = TRUE
#'                      stacks the devices) or poseidat_fuel_trip_vessel()
#' @param by_device     Fill bars by device (requires device-level input)
#' @param min_coverage  Trips with sea coverage (%) below this are drawn
#'                      transparent, so incomplete data stands out
#' @param complete_only Drop trips without both a departure and arrival
#'                      harbour (trips cut off by the data window)
#' @export
poseidat_plot_fuel_trip <- function(trip_data,
                                    by_device     = FALSE,
                                    min_coverage  = 80,
                                    complete_only = TRUE) {

  device_level <- "device_id" %in% names(trip_data)

  df <- if (device_level) {
    trip_data |>
      filter(phase == "at_sea") |>
      group_by(vessel_name, trip_id) |>
      mutate(trip_coverage = suppressWarnings(min(coverage_pct, na.rm = TRUE)),
             trip_coverage = if_else(is.infinite(trip_coverage), NA_real_, trip_coverage)) |>
      ungroup() |>
      rename(fuel_sea_L = fuel_L)
  } else {
    trip_data |>
      mutate(trip_coverage = min_coverage_sea_pct)
  }

  if (complete_only) df <- filter(df, complete_trip)
  if (by_device && !device_level) by_device <- FALSE

  df <- df |>
    mutate(
      trip_label = sprintf("%d\n%s", trip_id, format(departure_time, "%d %b")),
      fill_var   = if (by_device) device_name else vessel_name,
      ok_cov     = coalesce(trip_coverage >= min_coverage, FALSE)
    )

  ggplot(df, aes(x = reorder(trip_label, departure_time), y = fuel_sea_L,
                 fill = fill_var, alpha = ok_cov)) +
    geom_col(width = 0.8) +
    facet_wrap(~ vessel_name, scales = "free", ncol = 1) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      title    = "Fuel consumption at sea per trip",
      subtitle = sprintf("Flow rate integrated over time  |  faded bars: sea coverage < %s%%",
                         min_coverage),
      x = "Trip (number and departure date)", y = "Fuel at sea (L)", fill = NULL,
      caption  = sprintf("Generated: %s", Sys.Date())
    ) +
    theme_bw() +
    theme(
      legend.position = if (by_device) "bottom" else "none",
      strip.text      = element_text(face = "bold"),
      plot.title      = element_text(face = "bold")
    )
}


# ---- example usage ------------------------------------------------------------
if (FALSE) {

  source(file.path(here::here(), "R/poseidat/Poseidat_functions.R"))
  source(file.path(here::here(), "R/poseidat/Poseidat_fuel_trip.R"))
  setup_poseidat_sync()

  vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))

  start_date <- as.Date("2026-08-01")
  end_date   <- Sys.Date() - 1

  # 1. Stored trips (from the daily sync; no re-identification)
  trips <- poseidat_load_trips(start_date = start_date,
                               end_date   = end_date)

  # 2. Fuel observations covering those trips in full (a trip that started
  #    before start_date would otherwise get low coverage)
  fuel_obs <- poseidat_fuel_obs(vessel_config,
                                data_source = "thinned",
                                start_date  = min(as.Date(trips$departure_time)),
                                end_date    = end_date)

  # 3. Litres per trip per sensor (at sea + in harbour after the trip)
  trip_device <- poseidat_fuel_trip(fuel_obs, trips,
                                    flow_unit   = "L_per_h",
                                    max_gap_min = 5)

  # 4. Litres per trip per vessel
  trip_vessel <- poseidat_fuel_trip_vessel(trip_device)
  # if a device turns out to be a return-flow meter:
  # trip_vessel <- poseidat_fuel_trip_vessel(trip_device,
  #                                          return_devices = "<device_id>")

  trip_vessel |>
    filter(complete_trip) |>
    select(vessel_name, trip_uid, departure_time, trip_duration_hours,
           fuel_sea_L, fuel_harbour_L, fuel_sea_L_per_day, min_coverage_sea_pct)

  # Average per vessel over complete trips with good coverage
  trip_vessel |>
    filter(complete_trip, min_coverage_sea_pct >= 80) |>
    group_by(vessel_name) |>
    summarise(
      n_trips             = n(),
      mean_trip_days      = round(mean(trip_duration_hours) / 24, 1),
      mean_fuel_trip_L    = round(mean(fuel_sea_L)),
      mean_fuel_per_day_L = round(mean(fuel_sea_L_per_day)),
      .groups = "drop"
    )

  # 5. Plots
  poseidat_plot_fuel_trip(trip_device, by_device = TRUE)
  poseidat_plot_fuel_trip(trip_vessel)

  # 6. Export
  out_dir <- file.path(.onedrive(), "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  writexl::write_xlsx(
    list(
      per_vessel = trip_vessel,
      per_sensor = trip_device
    ),
    path = file.path(out_dir,
                     paste0("fuel_trip_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
  )
}
