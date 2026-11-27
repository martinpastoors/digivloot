# ============================================================================
# Poseidat_fuel_diagnostics.R
# Disentangling differences in fuel consumption between vessels / sensors
#
# All analysis is based on the thinned (1-minute) parquet files for
# consistency and performance. Run poseidat_thin(save = TRUE) first if
# thinned files do not yet exist.
#
# When two similar vessels show very different fuel readings the cause is
# almost always one of:
#   A. Unit mismatch       — L/h vs L/min vs kg/h vs pulses
#   B. Engine count        — one vessel reports per-engine, one reports total
#   C. Sensor calibration  — wrong pulse-per-litre factor
#   D. Zero/idle reporting — sensor goes offline vs. reports 0 when idle
#   E. Actual operations   — steaming vs. fishing, different trip patterns
#
# Functions:
#   build_fuel_thinned()          — load 1-min thinned fuel data
#   build_speed_thinned()         — load 1-min thinned SOG from POSITION fixes
#   diagnose_fuel_units()         — distribution plots to spot unit mismatches
#   diagnose_fuel_idle()          — zero/idle reporting behaviour
#   diagnose_fuel_operational()   — fuel rate vs. speed (SOG) cross-plot
#   diagnose_fuel_daily()         — daily total with active-hours normalisation
#   table_fuel_diagnostics()      — gt summary table of all diagnostic metrics
#
# Prerequisites: source Poseidat_functions.R first (provides .onedrive(),
#   .poseidat_load_data(), and vessel_config helpers).
#
# Dependencies: arrow, dplyr, tidyr, lubridate, ggplot2, scales, gt
# ============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(gt)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))


# ============================================================================
# FUEL-SPECIFIC INTERNAL HELPERS
# (general helpers .onedrive(), .poseidat_load_data() come from functions.R)
# ============================================================================

# Convenience shortcut: thinned parquet directory for one vessel
.thin_dir <- function(vn)
  file.path(.onedrive(), "processed", "thinned", vn)

# Pick the primary numeric value column from a parquet frame
.value_col <- function(df) {
  candidates <- intersect(c("value", "fuel_rate", "flow", "consumption"), names(df))
  if (length(candidates) > 0) return(candidates[1])
  nc <- setdiff(names(df)[sapply(df, is.numeric)],
                c("entry_id", "journal_id", "sequence_nr"))
  if (length(nc) > 0) nc[1] else NULL
}

# Identify fuel device IDs for one vessel from vessel_config labels
.fuel_device_ids <- function(vessel_config, vessel_name,
                              fuel_keywords = c("FUEL", "brandstof",
                                                "flow", "verbruik")) {
  row  <- vessel_config[vessel_config$vessel_name == vessel_name, ]
  lbls <- if (nrow(row) > 0 && !is.null(row$device_labels[[1]]))
    row$device_labels[[1]] else character(0)
  names(lbls)[grepl(paste(fuel_keywords, collapse = "|"), lbls,
                    ignore.case = TRUE)]
}

# Identify position/GPS device IDs — label is "POSITION" in EFICE data
.speed_device_ids <- function(vessel_config, vessel_name,
                               speed_keywords = c("POSITION", "position",
                                                  "speed", "sog", "gps")) {
  row  <- vessel_config[vessel_config$vessel_name == vessel_name, ]
  lbls <- if (nrow(row) > 0 && !is.null(row$device_labels[[1]]))
    row$device_labels[[1]] else character(0)
  names(lbls)[grepl(paste(speed_keywords, collapse = "|"), lbls,
                    ignore.case = TRUE)]
}


# ============================================================================
# 1.  BUILD FUEL DATA FROM THINNED FILES
#     Loads per-minute thinned parquet data for fuel devices.
#     Using thinned data keeps the analysis fast and consistent with the
#     rest of the pipeline. All downstream diagnostic functions work from
#     this output unchanged.
#
#     Thinned files must exist — run poseidat_thin(save = TRUE) first if needed.
# ============================================================================

#' Load per-minute thinned fuel data for all vessels
#'
#' Reads from the thinned parquet files (one row per device per minute)
#' rather than the raw parquet files. Produces the same output columns as
#' the former build_fuel_raw() so all diagnostic functions work unchanged.
#'
#' @param vessel_config   data.frame from readRDS("efice_vessels.rds")
#' @param start_date      Date
#' @param end_date        Date
#' @param fuel_keywords   Label substrings to identify fuel devices
#' @param flow_unit       "L_per_h" (default) or "L_per_min" — unit of
#'                        value_mean in the thinned files
#' @param gap_threshold_min  Consecutive missing minutes above this count
#'                           are flagged as gaps (default 5 min)
#'
#' @return tibble with one row per fuel device per minute, columns:
#'   vessel_name, device_id, device_name, datetime, obs_date, hour_of_day,
#'   interval_sec (always 60), is_gap, is_zero, flow_raw, flow_L_per_h,
#'   flow_sd, flow_min, flow_max, n_raw
#' @export
build_fuel_thinned <- function(vessel_config,
                                start_date        = Sys.Date() - 30,
                                end_date          = Sys.Date(),
                                fuel_keywords     = c("FUEL", "brandstof",
                                                      "flow", "verbruik"),
                                flow_unit         = "L_per_h",
                                gap_threshold_min = 5) {

  vessels <- vessel_config$vessel_name

  purrr::map_dfr(vessels, function(vn) {

    fuel_ids <- .fuel_device_ids(vessel_config, vn, fuel_keywords)
    if (length(fuel_ids) == 0) return(tibble())

    # Load thinned parquet files for this vessel
    thin_dir   <- .thin_dir(vn)
    thin_files <- if (dir.exists(thin_dir))
      list.files(thin_dir, pattern = "\\.parquet$",
                 recursive = TRUE, full.names = TRUE)
    else character(0)

    if (length(thin_files) == 0) {
      message(sprintf("  %s: no thinned files found — run poseidat_thin(save = TRUE) first", vn))
      return(tibble())
    }

    thinned <- purrr::map_dfr(thin_files, arrow::read_parquet) |>
      distinct(device_id, datetime, .keep_all = TRUE) |>
      filter(device_id %in% fuel_ids)

    if (!is.null(start_date))
      thinned <- filter(thinned, as.Date(datetime) >= as.Date(start_date))
    if (!is.null(end_date))
      thinned <- filter(thinned, as.Date(datetime) <= as.Date(end_date))

    if (nrow(thinned) == 0) return(tibble())

    message(sprintf("  %s: %d fuel minute-rows loaded (%d device(s))",
                    vn, nrow(thinned), n_distinct(thinned$device_id)))

    # Build label lookup for this vessel: named vector device_id -> label
    lbls <- if (!is.null(vessel_config[vessel_config$vessel_name == vn, ]$device_labels[[1]]))
      vessel_config[vessel_config$vessel_name == vn, ]$device_labels[[1]]
    else character(0)
    label_df <- tibble(
      device_id   = names(lbls),
      device_name = as.character(unlist(lbls))
    )

    thinned |>
      arrange(device_id, datetime) |>
      group_by(device_id) |>
      mutate(
        vessel_name  = vn,
        obs_date     = as.Date(datetime),
        hour_of_day  = hour(datetime),
        prev_dt      = lag(datetime),
        gap_mins     = as.numeric(difftime(datetime, prev_dt, units = "mins")),
        interval_sec = 60,
        is_gap       = !is.na(gap_mins) & gap_mins > gap_threshold_min,
        flow_raw     = value_mean,
        flow_L_per_h = case_when(
          flow_unit == "L_per_h"   ~ value_mean,
          flow_unit == "L_per_min" ~ value_mean * 60,
          TRUE                     ~ value_mean
        ),
        is_zero      = is.na(value_mean) | value_mean <= 0
      ) |>
      ungroup() |>
      left_join(label_df, by = "device_id") |>
      mutate(
        device_name = if_else(is.na(device_name), substr(device_id, 1, 8), device_name)
      ) |>
      select(vessel_name, device_id, device_name, datetime, obs_date,
             hour_of_day, interval_sec, is_gap, is_zero,
             flow_raw, flow_L_per_h,
             flow_sd = value_sd, flow_min = value_min,
             flow_max = value_max, n_raw)
  })
}


# ============================================================================
# 2.  UNIT MISMATCH DIAGNOSTICS
#     Distribution of instantaneous flow rate values.
#     A vessel with L/min instead of L/h would show values ~60x lower,
#     which is immediately visible in overlapping density plots.
# ============================================================================

#' Density and box plots of instantaneous flow rates across vessels
#'
#' The key diagnostic: if two similar vessels have very different
#' distributions the most likely cause is a unit mismatch.
#'
#' @param fuel_raw         Output of build_fuel_raw()
#' @param remove_zero      Logical — exclude zero/idle readings (default TRUE)
#' @param min_flow_L_per_h Minimum flow rate to include (L/h). Use this to
#'                         remove low near-idle values that distort the
#'                         comparison (e.g. harbour readings). Default 1 L/h.
#'                         Set to 0 to disable.
#' @param percentile_trim  Trim extreme percentiles to focus on typical range.
#'                         Default 0.01 removes bottom/top 1%.
#' @param log_x            Logical — log scale on x-axis (default TRUE)
#' @param log_y            Logical — log scale on y-axis of box plot (default FALSE)
#' @export
diagnose_fuel_units <- function(fuel_raw,
                                 remove_zero      = TRUE,
                                 min_flow_L_per_h = 1,
                                 percentile_trim  = 0.01,
                                 log_x            = TRUE,
                                 log_y            = FALSE) {

  df <- fuel_raw
  if (remove_zero) df <- filter(df, !is_zero, !is_gap)
  if (min_flow_L_per_h > 0)
    df <- filter(df, flow_L_per_h >= min_flow_L_per_h)
  if (nrow(df) == 0) { message("No data after filtering."); return(invisible(NULL)) }

  # Trim extremes per vessel to focus on typical operating range
  df <- df |>
    group_by(vessel_name, device_name) |>
    filter(flow_L_per_h >= quantile(flow_L_per_h, percentile_trim,  na.rm = TRUE),
           flow_L_per_h <= quantile(flow_L_per_h, 1 - percentile_trim, na.rm = TRUE)) |>
    ungroup() |>
    mutate(label = paste0(vessel_name, "\n", device_name))

  # Summary stats annotation
  stats <- df |>
    group_by(vessel_name, device_name, label) |>
    summarise(
      median_val = median(flow_L_per_h, na.rm = TRUE),
      p10        = quantile(flow_L_per_h, 0.10, na.rm = TRUE),
      p90        = quantile(flow_L_per_h, 0.90, na.rm = TRUE),
      .groups    = "drop"
    )

  # Build subtitle dynamically
  subtitle_parts <- c(
    "Dashed line = median",
    if (log_x) "log x-axis",
    if (min_flow_L_per_h > 0)
      paste0("values < ", min_flow_L_per_h, " L/h excluded")
    else
      "zeros and gaps excluded"
  )
  subtitle <- paste(subtitle_parts, collapse = "  |  ")

  # Density plot
  p1 <- ggplot(df, aes(x = flow_L_per_h, fill = vessel_name,
                        colour = vessel_name)) +
    geom_density(alpha = 0.30, linewidth = 0.7) +
    geom_vline(data = stats,
               aes(xintercept = median_val, colour = vessel_name),
               linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~ device_name, scales = "free_y") +
    labs(
      title    = "Instantaneous fuel flow rate distribution per sensor",
      subtitle = subtitle,
      x        = if (log_x) "Flow rate (L/h, log scale)" else "Flow rate (L/h)",
      y        = "Density",
      fill = NULL, colour = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title      = element_text(face = "bold"))

  if (log_x)
    p1 <- p1 + scale_x_log10(labels = label_comma())
  else
    p1 <- p1 + scale_x_continuous(labels = label_comma())

  # Box plot
  p2 <- ggplot(df, aes(x = label, y = flow_L_per_h, fill = vessel_name)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3, width = 0.5) +
    labs(
      title    = "Flow rate box plots per vessel × sensor",
      subtitle = if (log_y) "log y-axis" else "linear y-axis",
      x = NULL,
      y = if (log_y) "Flow rate (L/h, log scale)" else "Flow rate (L/h)",
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "none",
          axis.text.x      = element_text(size = 9),
          plot.title       = element_text(face = "bold"))

  if (log_y)
    p2 <- p2 + scale_y_log10(labels = label_comma())
  else
    p2 <- p2 + scale_y_continuous(labels = label_comma())

  # Print stats table to console as a quick reference
  message("\nFlow rate summary (L/h, low values excluded):")
  stats |>
    mutate(across(c(median_val, p10, p90), round, 1)) |>
    print()

  list(density = p1, boxplot = p2)
}


# ============================================================================
# 3.  IDLE / ZERO REPORTING BEHAVIOUR
#     How does each sensor behave when the vessel is not underway?
#     Sensor A may report 0, sensor B may go offline entirely.
#     This affects daily totals and averages differently.
# ============================================================================

#' Analyse zero/idle reporting behaviour per vessel × sensor
#'
#' Returns a plot and table showing:
#'   - fraction of time reporting zero vs. positive vs. offline (gap)
#'   - how zero-reporting correlates with hour of day
#'
#' @param fuel_raw  Output of build_fuel_raw()
#' @export
diagnose_fuel_idle <- function(fuel_raw) {

  if (nrow(fuel_raw) == 0) return(invisible(NULL))

  # Classify each observation
  df <- fuel_raw |>
    mutate(
      state = case_when(
        is_gap                    ~ "gap (offline)",
        is_zero                   ~ "zero (idle)",
        flow_L_per_h < 5          ~ "very low (<5 L/h)",
        flow_L_per_h < 20         ~ "low (5–20 L/h)",
        TRUE                      ~ "active (>20 L/h)"
      ) |> factor(levels = c("gap (offline)", "zero (idle)",
                              "very low (<5 L/h)", "low (5–20 L/h)",
                              "active (>20 L/h)")),
      label = paste0(vessel_name, "\n", device_name)
    )

  # Proportion plot
  state_colours <- c(
    "gap (offline)"    = "#bdbdbd",
    "zero (idle)"      = "#fee08b",
    "very low (<5 L/h)" = "#fdae61",
    "low (5–20 L/h)"   = "#f46d43",
    "active (>20 L/h)" = "#1a9641"
  )

  p_prop <- df |>
    count(label, vessel_name, device_name, state) |>
    group_by(label) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    ggplot(aes(x = label, y = pct, fill = state)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = state_colours, name = NULL) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title    = "Fuel sensor state proportions",
      subtitle = "Shows how each sensor behaves — gap = offline, zero = reporting idle",
      x = NULL, y = "% of observations"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title      = element_text(face = "bold"))

  # Hourly zero/gap rate — does it correlate with time of day?
  p_hourly <- df |>
    filter(!is.na(hour_of_day)) |>
    group_by(vessel_name, device_name, hour_of_day) |>
    summarise(
      pct_inactive = mean(is_zero | is_gap, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    mutate(label = paste0(vessel_name, " — ", device_name)) |>
    ggplot(aes(x = hour_of_day, y = pct_inactive, colour = vessel_name)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~ device_name, ncol = 2) +
    scale_x_continuous(breaks = seq(0, 23, 6)) +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
    labs(
      title    = "Inactivity rate by hour of day",
      subtitle = "High values during daytime may indicate harbour; flat = consistent at-sea pattern",
      x = "Hour (UTC)", y = "% inactive (zero or gap)", colour = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title      = element_text(face = "bold"))

  list(proportions = p_prop, hourly = p_hourly)
}


# ============================================================================
# 4.  LOAD POSITION DATA AND CALCULATE SOG FROM PARQUET
#     The POSITION device stores lat/lon fixes, not a speed value.
#     SOG is calculated from successive fixes using the Haversine formula.
# ============================================================================

# Haversine distance in metres between two lat/lon points (vectorised)
.haversine_m <- function(lat1, lon1, lat2, lon2) {
  r     <- 6371000  # Earth radius in metres
  phi1  <- lat1 * pi / 180
  phi2  <- lat2 * pi / 180
  dphi  <- (lat2 - lat1) * pi / 180
  dlam  <- (lon2 - lon1) * pi / 180
  a     <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

#' Load per-minute thinned position data and calculate SOG
#'
#' Reads from the thinned parquet files (one row per minute for the POSITION
#' device). Uses value_mean as mean lat or lon — but since thinned files
#' store a single value_mean per device per minute, and lat/lon are separate
#' devices or columns, this falls back to reading the raw position parquet
#' files and computing SOG at 1-minute resolution via Haversine.
#'
#' Note: the thinned files aggregate all numeric columns to a single
#' value_mean, so they cannot preserve separate lat/lon. SOG is therefore
#' computed from the raw POSITION parquet files but thinned to 1-minute
#' means before returning, keeping the output consistent with the fuel data.
#'
#' @param vessel_config    data.frame
#' @param sources          Character vector — default "efice"
#' @param start_date       Date
#' @param end_date         Date
#' @param position_keyword Label substring to identify the POSITION device
#' @param max_gap_sec      Gaps larger than this excluded from SOG (default 300 s)
#'
#' @return tibble with one row per vessel per minute:
#'   vessel_name, datetime (minute floor), speed_kn
#' @export
build_speed_thinned <- function(vessel_config,
                                 sources          = "efice",
                                 start_date       = Sys.Date() - 30,
                                 end_date         = Sys.Date(),
                                 position_keyword = "POSITION",
                                 max_gap_sec      = 300) {

  vessels <- vessel_config$vessel_name

  purrr::map_dfr(vessels, function(vn) {

    pos_ids <- .speed_device_ids(vessel_config, vn,
                                  speed_keywords = position_keyword)
    if (length(pos_ids) == 0) {
      message(sprintf("  %s: no POSITION device found", vn))
      return(tibble())
    }

    df <- .poseidat_load_data("raw", source = sources,
                               vessel_name = vn) |>
      distinct(entry_id, .keep_all = TRUE) |>
      arrange(datetime)
    if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= as.Date(start_date))
    if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= as.Date(end_date))
    if (nrow(df) == 0) return(tibble())

    lat_col <- intersect(c("latitude", "lat", "y"), names(df))
    lon_col <- intersect(c("longitude", "lon", "x"), names(df))

    if (length(lat_col) == 0 || length(lon_col) == 0) {
      message(sprintf("  %s: no lat/lon columns found. Available: %s",
                      vn, paste(names(df), collapse = ", ")))
      return(tibble())
    }
    lat_col <- lat_col[1]
    lon_col <- lon_col[1]

    message(sprintf("  %s: %d fixes loaded, computing SOG and thinning to 1 min ...",
                    vn, nrow(df)))

    # Compute SOG at raw resolution, then average to 1-minute bins
    df |>
      arrange(datetime) |>
      mutate(
        lat          = .data[[lat_col]],
        lon          = .data[[lon_col]],
        interval_sec = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
        dist_m       = .haversine_m(lag(lat), lag(lon), lat, lon),
        speed_kn     = if_else(
          !is.na(interval_sec) & interval_sec > 0 & interval_sec <= max_gap_sec,
          (dist_m / interval_sec) * 1.94384,
          NA_real_
        ),
        datetime_min = floor_date(datetime, unit = "minute")
      ) |>
      filter(!is.na(speed_kn)) |>
      group_by(datetime_min) |>
      summarise(
        speed_kn = mean(speed_kn, na.rm = TRUE),
        .groups  = "drop"
      ) |>
      mutate(vessel_name = vn) |>
      rename(datetime = datetime_min) |>
      select(vessel_name, datetime, speed_kn)
  })
}


# ============================================================================
# 5.  FUEL RATE vs. SPEED (SOG) CROSS-PLOT
#     Validates that fuel readings make operational sense.
#     Vessels at anchor/harbour should show low/zero fuel; steaming should
#     show higher values. A sensor with a calibration problem will show
#     a different slope from the other vessels.
# ============================================================================

#' Cross-plot fuel rate vs. speed (SOG) per vessel
#'
#' Both fuel_raw (from build_fuel_thinned) and speed_raw (from
#' build_speed_thinned) are at 1-minute resolution, so this is a
#' simple exact join on vessel_name + datetime minute.
#'
#' @param fuel_raw   Output of build_fuel_thinned()
#' @param speed_raw  Output of build_speed_thinned()
#' @export
diagnose_fuel_operational <- function(fuel_raw, speed_raw) {

  if (nrow(speed_raw) == 0) {
    message("speed_raw is empty. Run build_speed_thinned() first.")
    return(invisible(NULL))
  }

  # Both datasets are already at 1-minute resolution — direct exact join
  fuel_with_spd <- fuel_raw |>
    filter(!is_gap, !is_zero) |>
    mutate(datetime_min = floor_date(datetime, unit = "minute")) |>
    left_join(
      speed_raw |> rename(datetime_min = datetime),
      by = c("vessel_name", "datetime_min")
    ) |>
    filter(!is.na(speed_kn))

  if (nrow(fuel_with_spd) == 0) {
    message("No fuel-speed minute pairs found. Check date ranges overlap.")
    return(invisible(NULL))
  }

  message(sprintf("Joined %d fuel-speed minute pairs.", nrow(fuel_with_spd)))

  ggplot(fuel_with_spd, aes(x = speed_kn, y = flow_L_per_h, colour = vessel_name)) +
    geom_point(alpha = 0.08, size = 0.8) +
    geom_smooth(method = "loess", span = 0.4,
                se = TRUE, linewidth = 1.1, alpha = 0.15) +
    facet_wrap(~ device_name, scales = "free_y") +
    scale_colour_brewer(palette = "Set1", name = NULL) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title    = "Fuel rate vs. vessel speed (SOG)",
      subtitle = "Loess smoother per vessel  |  1-minute resolution  |  zeros and gaps excluded",
      x = "Speed over ground (knots)",
      y = "Fuel flow rate (L/h)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position   = "bottom",
          plot.title        = element_text(face = "bold"),
          panel.grid.minor  = element_blank())
}


# ============================================================================
# 5.  DAILY CONSUMPTION WITH NORMALISATION
#     Computes daily totals and normalises by active hours to make
#     vessels with different harbour/fishing ratios comparable.
# ============================================================================

#' Daily fuel consumption with active-hours normalisation
#'
#' "Active" is defined as flow_L_per_h > active_threshold and no gap.
#' This allows fair comparison between vessels with different harbour patterns.
#'
#' @param fuel_raw          Output of build_fuel_raw()
#' @param active_threshold  Flow rate (L/h) above which vessel is considered
#'                          active/underway (default: 5)
#' @param gap_threshold_sec Gaps larger than this excluded from integration
#' @export
diagnose_fuel_daily <- function(fuel_raw,
                                 active_threshold  = 5,
                                 gap_threshold_sec = 300) {

  if (nrow(fuel_raw) == 0) return(invisible(NULL))

  daily <- fuel_raw |>
    filter(!is_gap) |>
    group_by(vessel_name, device_name, obs_date) |>
    summarise(
      # Total consumption: integrate flow x time step
      n_obs              = n(),
      consumption_L      = sum(
        flow_L_per_h * pmin(interval_sec, gap_threshold_sec) / 3600,
        na.rm = TRUE
      ),
      # Active = above threshold and not a gap
      active_hours       = sum(
        if_else(!is_zero & flow_L_per_h > active_threshold,
                pmin(interval_sec, gap_threshold_sec), 0),
        na.rm = TRUE
      ) / 3600,
      mean_flow_active   = mean(
        if_else(!is_zero & flow_L_per_h > active_threshold,
                flow_L_per_h, NA_real_),
        na.rm = TRUE
      ),
      pct_zero           = mean(is_zero, na.rm = TRUE) * 100,
      pct_gap            = mean(is_gap,  na.rm = TRUE) * 100,
      .groups            = "drop"
    ) |>
    mutate(
      # Normalised consumption: L per active hour
      L_per_active_hour  = if_else(active_hours > 0,
                                    round(consumption_L / active_hours, 1),
                                    NA_real_),
      consumption_L      = round(consumption_L, 1),
      active_hours       = round(active_hours, 2),
      mean_flow_active   = round(mean_flow_active, 1)
    )

  # Plot: daily totals + normalised rate side by side
  p1 <- ggplot(daily, aes(x = obs_date, y = consumption_L,
                           colour = vessel_name)) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~ device_name, scales = "free_y", ncol = 2) +
    scale_colour_brewer(palette = "Set1", name = NULL) +
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title    = "Daily fuel consumption (total litres)",
      subtitle = "Includes all active periods; gaps > 5 min excluded",
      x = NULL, y = "Consumption (L/day)"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position  = "bottom",
          strip.text       = element_text(face = "bold"),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          plot.title       = element_text(face = "bold"))

  p2 <- ggplot(daily |> filter(!is.na(L_per_active_hour)),
               aes(x = obs_date, y = L_per_active_hour,
                   colour = vessel_name)) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~ device_name, scales = "free_y", ncol = 2) +
    scale_colour_brewer(palette = "Set1", name = NULL) +
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
    labs(
      title    = paste0("Fuel rate normalised by active hours (>",
                        active_threshold, " L/h)"),
      subtitle = "Removes harbour/idle time — comparable across vessels",
      x = NULL, y = "L per active hour"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position  = "bottom",
          strip.text       = element_text(face = "bold"),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          plot.title       = element_text(face = "bold"))

  list(daily_totals = p1, normalised_rate = p2, data = daily)
}


# ============================================================================
# 6.  SUMMARY DIAGNOSTIC TABLE
# ============================================================================

#' GT table summarising all key diagnostic metrics per vessel × sensor
#'
#' @param fuel_raw          Output of build_fuel_raw()
#' @param active_threshold  Same as in diagnose_fuel_daily() (default 5 L/h)
#' @export
table_fuel_diagnostics <- function(fuel_raw, active_threshold = 5) {

  df <- fuel_raw |>
    filter(!is_gap) |>
    group_by(vessel_name, device_name) |>
    summarise(
      n_days           = n_distinct(obs_date),
      n_obs            = n(),
      median_flow      = round(median(flow_L_per_h[!is_zero], na.rm = TRUE), 1),
      p10_flow         = round(quantile(flow_L_per_h[!is_zero], 0.10, na.rm = TRUE), 1),
      p90_flow         = round(quantile(flow_L_per_h[!is_zero], 0.90, na.rm = TRUE), 1),
      pct_zero         = round(mean(is_zero, na.rm = TRUE) * 100, 1),
      mean_active_flow = round(mean(
        flow_L_per_h[!is_zero & flow_L_per_h > active_threshold],
        na.rm = TRUE), 1),
      ratio_to_first   = NA_real_,   # filled below
      .groups          = "drop"
    )

  # Add ratio column: each vessel's median relative to the lowest median
  min_median <- min(df$median_flow, na.rm = TRUE)
  df <- df |>
    mutate(ratio_to_min = round(median_flow / min_median, 2))

  gt(df, groupname_col = "vessel_name") |>
    tab_header(
      title    = "Fuel sensor diagnostic summary",
      subtitle = paste0("Zeros excluded from flow statistics  |  ",
                        "active threshold: >", active_threshold, " L/h")
    ) |>
    cols_hide(columns = ratio_to_first) |>
    cols_label(
      device_name      = "Sensor",
      n_days           = "Days",
      n_obs            = "Obs (deduped)",
      median_flow      = "Median flow (L/h)",
      p10_flow         = "P10 (L/h)",
      p90_flow         = "P90 (L/h)",
      pct_zero         = "% zero/idle",
      mean_active_flow = "Mean active flow (L/h)",
      ratio_to_min     = "Ratio to lowest"
    ) |>
    fmt_number(columns = c(median_flow, p10_flow, p90_flow,
                            mean_active_flow),
               decimals = 1) |>
    fmt_number(columns = ratio_to_min, decimals = 2) |>
    data_color(
      columns = ratio_to_min,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = c(1, 3)
    ) |>
    data_color(
      columns = pct_zero,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = c(0, 80)
    ) |>
    tab_footnote(
      footnote  = paste0("Ratio > 1 means this sensor reads higher than the ",
                         "lowest-reading vessel. A ratio of ~60 suggests L/min vs L/h mismatch."),
      locations = cells_column_labels(columns = ratio_to_min)
    ) |>
    tab_footnote(
      footnote  = "High % zero may mean sensor goes offline when idle (vs. reporting 0)",
      locations = cells_column_labels(columns = pct_zero)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style = 6, color = "red") |>
    tab_options(table.font.size = "small")
}


# ============================================================================
# EXAMPLE USAGE (run interactively — vessel_config loaded at top of file)
# ============================================================================
if (FALSE) {

  # Filter to vessels that have data on disk
  efice_root           <- file.path(.onedrive(), "raw", "efice")
  vessel_config_active <- vessel_config |>
    filter(enabled, file.exists(file.path(efice_root, vessel_name)))

  # ── Step 0: ensure thinned files exist ────────────────────────────────────
  # poseidat_thin(save = TRUE)   # from Poseidat_functions.R

  # ── Step 1: load thinned fuel data ────────────────────────────────────────
  fuel_raw <- build_fuel_thinned(
    vessel_config     = vessel_config_active,
    start_date        = as.Date("2026-01-10"),
    end_date          = Sys.Date(),
    fuel_keywords     = c("FUEL", "brandstof", "flow", "verbruik"),
    flow_unit         = "L_per_h",
    gap_threshold_min = 5
  )
  glimpse(fuel_raw)

  # ── Step 2: unit mismatch check ───────────────────────────────────────────
  # Look at the density plot: two vessels with the same engine should overlap.
  # If one is shifted ~60x to the left, it's reporting L/min.
  # If shifted ~0.82x, it may be reporting kg/h (diesel density ~0.82 kg/L).
  plots_units <- diagnose_fuel_units(fuel_raw,
                                     min_flow_L_per_h = 1,
                                     log_x = TRUE)
  print(plots_units$density)
  print(plots_units$boxplot)

  # ── Step 3: idle/zero behaviour ───────────────────────────────────────────
  plots_idle <- diagnose_fuel_idle(fuel_raw)
  print(plots_idle$proportions)
  print(plots_idle$hourly)

  # ── Step 4: fuel vs. speed cross-plot ─────────────────────────────────────
  # SOG computed from raw POSITION fixes then thinned to 1-minute means.
  speed_raw <- build_speed_thinned(
    vessel_config    = vessel_config_active,
    sources          = "efice",
    start_date       = as.Date("2026-01-10"),
    end_date         = Sys.Date(),
    position_keyword = "POSITION",
    max_gap_sec      = 300
  )

  p_ops <- diagnose_fuel_operational(
    fuel_raw  = fuel_raw,
    speed_raw = speed_raw
  )
  print(p_ops)

  # ── Step 5: daily totals + normalised rate ────────────────────────────────
  daily <- diagnose_fuel_daily(fuel_raw, active_threshold = 5)
  print(daily$daily_totals)
  print(daily$normalised_rate)

  daily$data |>
    group_by(vessel_name, device_name) |>
    summarise(
      mean_L_per_active_hour = round(mean(L_per_active_hour, na.rm = TRUE), 1),
      mean_active_hours_day  = round(mean(active_hours,      na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    print()

  # ── Step 6: summary diagnostic table ─────────────────────────────────────
  tbl <- table_fuel_diagnostics(fuel_raw, active_threshold = 5)
  print(tbl)

  # ── Quick ratio interpretation ────────────────────────────────────────────
  # ratio_to_min ~60  -> L/min vs L/h unit mismatch
  # ratio_to_min ~2   -> single engine vs total (two engines)
  # ratio_to_min ~0.82 -> kg/h vs L/h (diesel density)
  # ratio_to_min ~3 with no clean factor -> calibration or operational
}
