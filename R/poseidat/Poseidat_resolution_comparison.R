# ============================================================================
# Poseidat_resolution_comparison.R
# Comparing high-resolution parquet data vs. thinned (per-minute) data
# for DIGIvloot / EFICE Poseidat vessels
#
# The "thinned" files are one-observation-per-minute summaries derived
# from the high-resolution parquet files via poseidat_thin() in
# Poseidat_functions.R. Run poseidat_thin(save = TRUE) before using
# this script.
#
# This script provides the ANALYTICAL layer only — data loading and thinning
# are handled by Poseidat_functions.R:
#   poseidat_thin()            — create/update thinned parquet (functions.R)
#   .poseidat_load_data()      — load raw or thinned parquet (functions.R)
#
# Functions in this file:
#   compare_resolution()          — information-loss metrics per device
#   plot_resolution_traces()      — side-by-side high-res vs. thinned traces
#   plot_resolution_density()     — value distribution comparison
#   plot_peak_detection()         — events missed in thinned data
#   plot_vessel_resolution_profile() — per-vessel sensor overview
#   table_resolution_comparison() — gt table of information-loss metrics
#   build_obs_comparison()        — daily observation count: raw vs. thinned
#   build_fuel_comparison()       — daily fuel estimate: raw vs. thinned
#   plot_fuel_comparison()        — line plot of fuel comparison
#   table_fuel_comparison()       — gt table of fuel comparison
#   build_speed_thinned()         — 1-min SOG from thinned POSITION fixes
#
# Prerequisites: source Poseidat_functions.R first.
#
# Dependencies: arrow, dplyr, tidyr, purrr, lubridate, ggplot2, scales, gt
# ============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)
library(gt)

source(file.path(here::here(), "R", "poseidat", "Poseidat_functions.R"))
setup_poseidat_sync()

vessel_config <- readRDS(file.path(.onedrive(), "config", "poseidat_vessels.rds"))


# ============================================================================
# RESOLUTION-SPECIFIC INTERNAL HELPERS
# (general helpers come from Poseidat_functions.R)
# ============================================================================

# Convenience path to thinned files for one vessel
.thin_dir <- function(vn)
  file.path(.onedrive(), "processed", "thinned", vn)

# Haversine distance in metres between two lat/lon points (vectorised)
.haversine_m <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371000
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Pick the primary numeric value column from a parquet frame
.value_col <- function(df) {
  candidates <- intersect(c("value", "speed", "fuel_rate", "tension",
                             "depth", "heading", "temperature"), names(df))
  if (length(candidates) > 0) candidates[1] else {
    nc <- names(df)[sapply(df, is.numeric)]
    nc <- setdiff(nc, c("journal_id", "entry_id"))
    if (length(nc) > 0) nc[1] else NULL
  }
}

# Load thinned parquet for one vessel (used by comparison functions)
.load_thinned <- function(vessel_name, start_date = NULL, end_date = NULL) {
  dir <- .thin_dir(vessel_name)
  if (!dir.exists(dir))
    stop("No thinned directory for ", vessel_name,
         ". Run poseidat_thin(save = TRUE) first.")
  files <- list.files(dir, pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0)
    stop("No thinned parquet files found for ", vessel_name)
  df <- map_dfr(files, arrow::read_parquet) |>
    distinct(device_id, datetime, .keep_all = TRUE) |>
    arrange(datetime)
  if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= as.Date(start_date))
  if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= as.Date(end_date))
  df
}

# Load raw parquet for one vessel, deduped on entry_id (resolution comparison
# needs entry_id dedup, not the broader .poseidat_load_data path)
.rc_load_raw <- function(vessel_name, device_id = NULL,
                          sources    = "efice",
                          start_date = NULL, end_date = NULL) {
  df <- .poseidat_load_data("raw", source = sources, vessel_name = vessel_name)
  if (nrow(df) == 0) return(df)
  df <- distinct(df, entry_id, .keep_all = TRUE) |> arrange(datetime)
  if (!is.null(device_id))  df <- filter(df, .data$device_id == device_id)
  if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= as.Date(start_date))
  if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= as.Date(end_date))
  df
}


  if (!dir.exists(dir)) stop("No thinned directory for ", vessel_name,
                              ". Run poseidat_thin(save = TRUE) first.")

  files <- list.files(dir, pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) stop("No thinned parquet files found for ", vessel_name)

  df <- map_dfr(files, arrow::read_parquet) |>
    distinct(device_id, datetime, .keep_all = TRUE) |>
    arrange(datetime)

  if (!is.null(start_date)) df <- filter(df, as.Date(datetime) >= as.Date(start_date))
  if (!is.null(end_date))   df <- filter(df, as.Date(datetime) <= as.Date(end_date))
  df
}


# ============================================================================
# 3.  COMPARE RESOLUTION METRICS
#     Returns a tibble with information-loss statistics per vessel × device
# ============================================================================

#' Compute information-loss metrics between high-res and thinned data
#'
#' Metrics returned per vessel × device:
#'   - native_interval_sec  : median sampling interval in raw data
#'   - compression_ratio    : raw rows per minute-bin on average
#'   - range_retention_pct  : how much of the within-minute value range
#'                            is preserved in the minute mean
#'   - peak_detection_rate  : fraction of within-minute maxima that lie
#'                            within ± 5 % of the thinned mean
#'                            (lower = more peaks smoothed away)
#'   - sd_within_minute     : average intra-minute standard deviation
#'                            (high = thinning loses variance)
#'   - n_minutes_covered    : total minutes with data
#'
#' @param vessel_config   data.frame
#' @param vessel_name     Character – vessel to analyse
#' @param sources         Character vector – e.g. "efice" or c("efice","pefa")
#' @param start_date      Date
#' @param end_date        Date
#'
#' @return tibble of metrics per device
#' @export
compare_resolution <- function(vessel_config,
                                vessel_name,
                                sources    = "efice",
                                start_date = Sys.Date() - 14,
                                end_date   = Sys.Date()) {

  raw    <- .rc_load_raw(vessel_name,
                                  sources    = sources,
                                  start_date = start_date,
                                  end_date   = end_date)
  if (nrow(raw) == 0) stop("No raw data for ", vessel_name)

  val_col <- .value_col(raw)
  if (is.null(val_col)) stop("No numeric value column found in raw data.")

  raw <- raw |>
    mutate(minute_floor = floor_date(datetime, unit = "minute")) |>
    group_by(device_id) |>
    arrange(datetime, .by_group = TRUE) |>
    mutate(interval_sec = as.numeric(difftime(datetime, lag(datetime),
                                              units = "secs"))) |>
    ungroup()

  metrics <- raw |>
    group_by(device_id, minute_floor) |>
    summarise(
      n_raw         = n(),
      val_mean      = mean(.data[[val_col]],  na.rm = TRUE),
      val_min       = min(.data[[val_col]],   na.rm = TRUE),
      val_max       = max(.data[[val_col]],   na.rm = TRUE),
      val_sd        = sd(.data[[val_col]],    na.rm = TRUE),
      val_range     = val_max - val_min,
      # peak_missed: max deviates more than 5 % from mean?
      peak_large    = abs(val_max - val_mean) > 0.05 * abs(val_mean + 1e-9),
      .groups       = "drop"
    ) |>
    group_by(device_id) |>
    summarise(
      n_minutes_covered    = n(),
      compression_ratio    = round(mean(n_raw), 2),
      sd_within_minute     = round(mean(val_sd,    na.rm = TRUE), 4),
      mean_range           = round(mean(val_range, na.rm = TRUE), 4),
      # range_retention: if within-minute range is zero mean is perfect;
      # otherwise range_retention = 1 - range/mean_abs_value
      range_retention_pct  = round(
        pmax(0, pmin(100,
                     100 * (1 - mean(val_range / (abs(val_mean) + 1e-9),
                                     na.rm = TRUE)))),
        1),
      peak_detection_rate  = round(100 * mean(!peak_large, na.rm = TRUE), 1),
      .groups = "drop"
    )

  # Add native interval
  native_interval <- raw |>
    filter(!is.na(interval_sec), interval_sec > 0) |>
    group_by(device_id) |>
    summarise(
      native_interval_sec = round(median(interval_sec, na.rm = TRUE), 1),
      .groups = "drop"
    )

  left_join(metrics, native_interval, by = "device_id") |>
    rowwise() |>
    mutate(device_name = .device_label(vessel_config, vessel_name, device_id)) |>
    ungroup() |>
    mutate(vessel_name = vessel_name) |>
    select(vessel_name, device_id, device_name,
           native_interval_sec, compression_ratio,
           n_minutes_covered, sd_within_minute, mean_range,
           range_retention_pct, peak_detection_rate)
}


# ============================================================================
# 4.  PLOTS
# ============================================================================

#' Side-by-side time-series traces: high-res vs. thinned
#'
#' @param vessel_name   Character
#' @param device_id     Character – single device UUID
#' @param vessel_config data.frame
#' @param sources       Character vector – default "efice"
#' @param start_date    Date
#' @param end_date      Date
#' @param hours         Integer – number of hours to plot (default: 6)
#'                      Set NULL to plot the full period
#' @export
plot_resolution_traces <- function(vessel_name,
                                    device_id,
                                    vessel_config,
                                    sources    = "efice",
                                    start_date = Sys.Date() - 7,
                                    end_date   = Sys.Date(),
                                    hours      = 6) {

  raw    <- .rc_load_raw(vessel_name, device_id,
                                  sources    = sources,
                                  start_date = start_date, end_date = end_date)
  if (nrow(raw) == 0) stop("No raw data.")

  val_col   <- .value_col(raw)
  dev_label <- .device_label(vessel_config, vessel_name, device_id)

  # Restrict to first N hours for readability
  if (!is.null(hours)) {
    t0  <- min(raw$datetime)
    raw <- filter(raw, datetime <= t0 + hours * 3600)
  }

  thinned <- raw |>
    mutate(minute_floor = floor_date(datetime, unit = "minute")) |>
    group_by(minute_floor) |>
    summarise(
      value_mean = mean(.data[[val_col]], na.rm = TRUE),
      value_min  = min(.data[[val_col]],  na.rm = TRUE),
      value_max  = max(.data[[val_col]],  na.rm = TRUE),
      .groups    = "drop"
    )

  # Plot
  p_raw <- ggplot(raw, aes(x = datetime, y = .data[[val_col]])) +
    geom_line(colour = "#2166ac", linewidth = 0.4, alpha = 0.8) +
    labs(title = "High-resolution (raw)",
         subtitle = paste0(dev_label, " — ", vessel_name),
         x = NULL, y = val_col) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

  p_thin <- ggplot(thinned, aes(x = minute_floor)) +
    geom_ribbon(aes(ymin = value_min, ymax = value_max),
                fill = "#4dac26", alpha = 0.25) +
    geom_line(aes(y = value_mean), colour = "#4dac26",
              linewidth = 0.6) +
    labs(title = "Thinned (1-minute mean ± range)",
         x = NULL, y = val_col) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

  # Stack with patchwork if available, otherwise print separately
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p_raw / p_thin +
      plot_annotation(
        title   = paste0("Resolution comparison — ", vessel_name, " / ", dev_label),
        caption = paste0("High-res: ", nrow(raw), " obs; thinned: ",
                         nrow(thinned), " minute bins")
      )
  } else {
    print(p_raw)
    print(p_thin)
  }
}


#' Density plot comparing value distributions: high-res vs. thinned
#'
#' @param vessel_name   Character
#' @param device_id     Character
#' @param vessel_config data.frame
#' @param sources       Character vector – default "efice"
#' @param start_date    Date
#' @param end_date      Date
#' @export
plot_resolution_density <- function(vessel_name,
                                     device_id,
                                     vessel_config,
                                     sources    = "efice",
                                     start_date = Sys.Date() - 14,
                                     end_date   = Sys.Date()) {

  raw     <- .rc_load_raw(vessel_name, device_id,
                                   sources    = sources,
                                   start_date = start_date, end_date = end_date)
  if (nrow(raw) == 0) stop("No raw data.")

  val_col   <- .value_col(raw)
  dev_label <- .device_label(vessel_config, vessel_name, device_id)

  thinned <- raw |>
    mutate(minute_floor = floor_date(datetime, unit = "minute")) |>
    group_by(minute_floor) |>
    summarise(value = mean(.data[[val_col]], na.rm = TRUE), .groups = "drop")

  # Combine into one long frame
  combined <- bind_rows(
    raw     |> select(value = all_of(val_col)) |> mutate(source = "High-res (raw)"),
    thinned |> select(value)                   |> mutate(source = "Thinned (1-min mean)")
  )

  ggplot(combined, aes(x = value, fill = source, colour = source)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values   = c("High-res (raw)" = "#2166ac",
                                   "Thinned (1-min mean)" = "#4dac26")) +
    scale_colour_manual(values = c("High-res (raw)" = "#2166ac",
                                   "Thinned (1-min mean)" = "#4dac26")) +
    labs(
      title    = paste0("Value distribution: high-res vs. thinned"),
      subtitle = paste0(vessel_name, " — ", dev_label, "  |  ",
                        format(as.Date(start_date), "%d %b"), " – ",
                        format(as.Date(end_date),   "%d %b %Y")),
      x = val_col, y = "Density", fill = NULL, colour = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title      = element_text(face = "bold"))
}


#' Scatter: within-minute SD vs. native sampling interval
#' Shows which sensors lose the most information when thinned.
#'
#' @param resolution_df  Output of compare_resolution() (one vessel)
#'                       or bind_rows() of multiple vessels
#' @export
plot_resolution_profile <- function(resolution_df) {

  df <- resolution_df |>
    mutate(
      label_colour = case_when(
        peak_detection_rate >= 90 ~ "good (>90%)",
        peak_detection_rate >= 70 ~ "moderate (70–90%)",
        TRUE                      ~ "poor (<70%)"
      ) |> factor(levels = c("good (>90%)", "moderate (70–90%)", "poor (<70%)"))
    )

  ggplot(df, aes(x = native_interval_sec,
                  y = sd_within_minute,
                  colour = label_colour,
                  size   = compression_ratio,
                  label  = paste0(vessel_name, "\n", device_name))) +
    geom_point(alpha = 0.8) +
    ggrepel::geom_text_repel(size = 3, max.overlaps = 10,
                              colour = "grey30", show.legend = FALSE) +
    scale_x_log10(labels = label_comma()) +
    scale_colour_manual(
      values = c("good (>90%)" = "#1a9641",
                 "moderate (70–90%)" = "#f4a582",
                 "poor (<70%)" = "#d73027"),
      name = "Peak detection rate"
    ) +
    scale_size_continuous(range = c(2, 10), name = "Compression\nratio") +
    labs(
      title    = "Sensor resolution profile",
      subtitle = "x = native sampling interval | y = within-minute SD | size = compression ratio",
      x = "Native sampling interval (seconds, log scale)",
      y = "Within-minute standard deviation"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title      = element_text(face = "bold")
    )
}


# ============================================================================
# 5.  GT RESOLUTION COMPARISON TABLE
# ============================================================================

#' GT table of information-loss metrics across all vessels and devices
#'
#' @param resolution_df  bind_rows() output of compare_resolution() calls
#' @export
table_resolution_comparison <- function(resolution_df) {

  gt(resolution_df, groupname_col = "vessel_name") |>
    tab_header(
      title    = "High-resolution vs. thinned data — information loss",
      subtitle = "One row per sensor; colour coding shows severity of information loss"
    ) |>
    cols_hide(columns = device_id) |>
    cols_label(
      device_name         = "Sensor",
      native_interval_sec = "Native interval (s)",
      compression_ratio   = "Compression ratio",
      n_minutes_covered   = "Minutes covered",
      sd_within_minute    = "Within-min SD",
      mean_range          = "Mean within-min range",
      range_retention_pct = "Range retention (%)",
      peak_detection_rate = "Peak detection rate (%)"
    ) |>
    data_color(
      columns = range_retention_pct,
      palette = c("#d73027", "#fee08b", "#1a9641"),
      domain  = c(0, 100)
    ) |>
    data_color(
      columns = peak_detection_rate,
      palette = c("#d73027", "#fee08b", "#1a9641"),
      domain  = c(0, 100)
    ) |>
    data_color(
      columns = sd_within_minute,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = NULL
    ) |>
    fmt_number(columns = c(native_interval_sec, compression_ratio,
                            sd_within_minute, mean_range),
               decimals = 2) |>
    fmt_number(columns = c(range_retention_pct, peak_detection_rate),
               decimals = 1) |>
    tab_footnote(
      footnote  = "Compression ratio = average raw observations per minute bin",
      locations = cells_column_labels(columns = compression_ratio)
    ) |>
    tab_footnote(
      footnote  = "Range retention: 100% = no within-minute variation (mean = full signal); lower = more information lost",
      locations = cells_column_labels(columns = range_retention_pct)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style = 6, color = "green") |>
    tab_options(table.font.size = "small")
}


# ============================================================================
# EXAMPLE USAGE (run interactively — vessel_config loaded at top of file)
# ============================================================================
if (FALSE) {

  library(ggrepel)    # for plot_resolution_profile()
  library(patchwork)  # for plot_resolution_traces()

  # ── Step 1: ensure thinned files exist ───────────────────────────────────
  poseidat_thin(save = TRUE)   # from Poseidat_functions.R

  # ── Step 2: compute resolution comparison metrics for each vessel ──────────
  res_list <- map(vessel_config$vessel_name, function(vn) {
    tryCatch(
      compare_resolution(
        vessel_config = vessel_config,
        vessel_name   = vn,
        sources       = "efice",
        start_date    = Sys.Date() - 14,
        end_date      = Sys.Date()
      ),
      error = function(e) {
        message("Skipping ", vn, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |> compact()

  res_all <- bind_rows(res_list)
  print(res_all)

  # ── Step 3: resolution profile scatter (all vessels × devices) ────────────
  p_profile <- plot_resolution_profile(res_all)
  print(p_profile)

  # ── Step 4: time-series traces for a specific device ──────────────────────
  # Replace with real vessel name and device_id from vessel_config
  p_traces <- plot_resolution_traces(
    vessel_name   = "Polar",
    device_id     = vessel_config |>
      filter(vessel_name == "Polar") |>
      pull(device_labels) |>
      pluck(1) |>
      names() |>
      first(),
    vessel_config = vessel_config,
    sources       = "efice",
    start_date    = Sys.Date() - 7,
    end_date      = Sys.Date(),
    hours         = 6     # zoom to 6 hours for readability
  )
  print(p_traces)

  # ── Step 5: value density comparison ──────────────────────────────────────
  p_density <- plot_resolution_density(
    vessel_name   = "Polar",
    device_id     = vessel_config |>
      filter(vessel_name == "Polar") |>
      pull(device_labels) |>
      pluck(1) |>
      names() |>
      first(),
    vessel_config = vessel_config,
    sources       = "efice",
    start_date    = Sys.Date() - 14,
    end_date      = Sys.Date()
  )
  print(p_density)

  # ── Step 6: GT table ──────────────────────────────────────────────────────
  tbl <- table_resolution_comparison(res_all)
  print(tbl)

  # ── Step 7: Cross-vessel interval comparison ───────────────────────────────
  # Useful for understanding natural variation across vessels BEFORE thinning
  res_all |>
    select(vessel_name, device_name, native_interval_sec, compression_ratio) |>
    arrange(vessel_name, device_name) |>
    print(n = 40)
}

# ============================================================================
# 6.  OBSERVATION COUNT COMPARISON: raw vs. thinned
# ============================================================================

#' Build a daily observation count comparison between raw and thinned data
#'
#' Returns one row per vessel x device x day with:
#'   - n_raw        : observations in the raw parquet files
#'   - n_thinned    : minute bins in the thinned files
#'   - compression  : n_raw / n_thinned (raw rows per minute bin)
#'   - coverage_pct : n_thinned / 1440 * 100 (% of minutes in a day covered)
#'
#' @param vessel_config  data.frame from readRDS("efice_vessels.rds")
#' @param sources        Character vector - default "efice"
#' @param start_date     Date
#' @param end_date       Date
#' @return tibble
#' @export
build_obs_comparison <- function(vessel_config,
                                  sources    = "efice",
                                  start_date = Sys.Date() - 30,
                                  end_date   = Sys.Date()) {

  vessels <- vessel_config$vessel_name

  map_dfr(vessels, function(vn) {

    # raw counts
    raw <- .rc_load_raw(vn, sources = sources,
                                 start_date = start_date, end_date = end_date)
    if (nrow(raw) == 0) return(tibble())

    raw_counts <- raw |>
      mutate(obs_date = as.Date(datetime)) |>
      count(device_id, obs_date, name = "n_raw")

    # thinned counts
    thin_dir <- .thin_dir(vn)
    if (!dir.exists(thin_dir)) {
      message(sprintf("  %s: no thinned directory - run poseidat_thin(save = TRUE) first", vn))
      return(raw_counts |>
               mutate(vessel_name = vn, n_thinned = NA_integer_,
                      compression = NA_real_, coverage_pct = NA_real_))
    }

    thin_files <- list.files(thin_dir, pattern = "\\.parquet$",
                              recursive = TRUE, full.names = TRUE)
    if (length(thin_files) == 0) {
      message(sprintf("  %s: thinned directory empty", vn))
      return(raw_counts |>
               mutate(vessel_name = vn, n_thinned = NA_integer_,
                      compression = NA_real_, coverage_pct = NA_real_))
    }

    thinned <- map_dfr(thin_files, arrow::read_parquet) |>
      distinct(device_id, datetime, .keep_all = TRUE)

    if (!is.null(start_date))
      thinned <- filter(thinned, as.Date(datetime) >= as.Date(start_date))
    if (!is.null(end_date))
      thinned <- filter(thinned, as.Date(datetime) <= as.Date(end_date))

    thin_counts <- thinned |>
      mutate(obs_date = as.Date(datetime)) |>
      count(device_id, obs_date, name = "n_thinned")

    full_join(raw_counts, thin_counts, by = c("device_id", "obs_date")) |>
      mutate(
        vessel_name  = vn,
        compression  = round(n_raw / pmax(n_thinned, 1), 1),
        coverage_pct = round(pmin(n_thinned / 1440 * 100, 100), 1)
      )
  }) |>
    rowwise() |>
    mutate(device_name = .device_label(vessel_config, vessel_name, device_id)) |>
    ungroup() |>
    select(vessel_name, device_id, device_name,
           obs_date, n_raw, n_thinned, compression, coverage_pct) |>
    arrange(vessel_name, device_name, obs_date)
}


#' Line plot of daily raw vs. thinned observation counts per vessel x device
#'
#' @param obs_comparison  Output of build_obs_comparison()
#' @param vessel_name     Optional: filter to one vessel
#' @export
plot_obs_comparison <- function(obs_comparison, vessel_name = NULL) {

  df <- obs_comparison
  if (!is.null(vessel_name))
    df <- filter(df, .data$vessel_name == vessel_name)

  df_long <- df |>
    select(vessel_name, device_name, obs_date, n_raw, n_thinned) |>
    pivot_longer(cols = c(n_raw, n_thinned),
                 names_to  = "source",
                 values_to = "n_obs") |>
    mutate(
      source      = recode(source,
                           n_raw     = "Raw (high-res)",
                           n_thinned = "Thinned (1-min)"),
      facet_label = paste0(vessel_name, " - ", device_name)
    )

  ggplot(df_long, aes(x = obs_date, y = n_obs, colour = source)) +
    geom_line(linewidth = 0.6, alpha = 0.85) +
    facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
    scale_colour_manual(
      values = c("Raw (high-res)" = "#2166ac", "Thinned (1-min)" = "#4dac26"),
      name   = NULL
    ) +
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title    = "Daily observation counts: raw vs. thinned",
      subtitle = paste0(format(min(df$obs_date), "%d %b %Y"), " - ",
                        format(max(df$obs_date), "%d %b %Y")),
      x = NULL, y = "Observations per day"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 8),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


#' GT table: observation counts raw vs. thinned, aggregated per vessel x device
#'
#' @param obs_comparison  Output of build_obs_comparison()
#' @export
table_obs_comparison <- function(obs_comparison) {

  df <- obs_comparison |>
    group_by(vessel_name, device_name) |>
    summarise(
      days             = n(),
      total_raw        = sum(n_raw,      na.rm = TRUE),
      total_thinned    = sum(n_thinned,  na.rm = TRUE),
      mean_compression = round(mean(compression,  na.rm = TRUE), 1),
      mean_coverage    = round(mean(coverage_pct, na.rm = TRUE), 1),
      .groups = "drop"
    )

  gt(df, groupname_col = "vessel_name") |>
    tab_header(
      title    = "Observation counts: raw vs. thinned",
      subtitle = paste0(format(min(obs_comparison$obs_date), "%d %b %Y"),
                        " - ",
                        format(max(obs_comparison$obs_date), "%d %b %Y"))
    ) |>
    cols_label(
      device_name      = "Sensor",
      days             = "Days",
      total_raw        = "Total raw obs",
      total_thinned    = "Total thinned (min)",
      mean_compression = "Mean compression ratio",
      mean_coverage    = "Mean daily coverage (%)"
    ) |>
    fmt_number(columns = c(total_raw, total_thinned), decimals = 0) |>
    fmt_number(columns = c(mean_compression, mean_coverage), decimals = 1) |>
    data_color(
      columns = mean_compression,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = NULL
    ) |>
    data_color(
      columns = mean_coverage,
      palette = c("#d73027", "#fee08b", "#1a9641"),
      domain  = c(0, 100)
    ) |>
    tab_footnote(
      footnote  = "Compression ratio = raw observations per minute bin; higher = more detail lost when thinning",
      locations = cells_column_labels(columns = mean_compression)
    ) |>
    tab_footnote(
      footnote  = "Daily coverage = % of the 1440 minutes in a day with at least one thinned observation",
      locations = cells_column_labels(columns = mean_coverage)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style = 6, color = "blue") |>
    tab_options(table.font.size = "small")
}


# ============================================================================
# 7.  FUEL CONSUMPTION: raw vs. thinned
#
# PoseiDAT fuel sensors report an instantaneous flow rate (litres/hour or
# litres/minute depending on device configuration). To derive daily
# consumption we integrate: consumption = flow_rate x time_step.
#
# For raw data:    sum(value x interval_sec / 3600) per day  [litres]
# For thinned:     sum(value_mean x 60 / 3600) per day       [litres]
#   (each thinned row represents one minute = 60 seconds)
#
# If your sensor reports in a different unit, adjust via flow_unit:
#   "L_per_h" (default) or "L_per_min"
# ============================================================================

#' Build daily fuel consumption estimates per vessel x sensor
#'
#' Scans for devices whose label contains common fuel-related keywords.
#' Computes consumption from both raw and thinned data and compares them.
#'
#' @param vessel_config      data.frame
#' @param sources            Character vector - default "efice"
#' @param start_date         Date
#' @param end_date           Date
#' @param fuel_keywords      Substrings to match in device labels (case-insensitive)
#' @param flow_unit          "L_per_h" (default) or "L_per_min"
#' @param gap_threshold_sec  Gaps larger than this are excluded from raw
#'                           integration to avoid inflating consumption during
#'                           harbour stops / outages. Default: 300 s (5 min)
#' @return tibble with daily consumption from raw and thinned, one row per
#'         vessel x device x day
#' @export
build_fuel_comparison <- function(vessel_config,
                                   sources           = "efice",
                                   start_date        = Sys.Date() - 30,
                                   end_date          = Sys.Date(),
                                   fuel_keywords     = c("brandstof", "fuel",
                                                         "flow", "verbruik"),
                                   flow_unit         = "L_per_h",
                                   gap_threshold_sec = 300) {

  stopifnot(flow_unit %in% c("L_per_h", "L_per_min"))
  vessels <- vessel_config$vessel_name

  map_dfr(vessels, function(vn) {

    row  <- vessel_config[vessel_config$vessel_name == vn, ]
    lbls <- if (!is.null(row$device_labels[[1]])) row$device_labels[[1]] else character(0)

    fuel_device_ids <- names(lbls)[
      grepl(paste(fuel_keywords, collapse = "|"), lbls, ignore.case = TRUE)
    ]

    if (length(fuel_device_ids) == 0) {
      message(sprintf("  %s: no fuel devices found (check device labels)", vn))
      return(tibble())
    }
    message(sprintf("  %s: %d fuel device(s): %s",
                    vn, length(fuel_device_ids),
                    paste(lbls[fuel_device_ids], collapse = ", ")))

    map_dfr(fuel_device_ids, function(dev_id) {

      dev_label <- lbls[[dev_id]]

      # raw integration
      raw <- .rc_load_raw(vn, device_id = dev_id, sources = sources,
                                   start_date = start_date, end_date = end_date)
      if (nrow(raw) == 0) return(tibble())

      val_col <- .value_col(raw)
      if (is.null(val_col)) return(tibble())

      raw_fuel <- raw |>
        arrange(datetime) |>
        mutate(
          interval_sec = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
          obs_date     = as.Date(datetime)
        ) |>
        filter(!is.na(interval_sec),
               interval_sec > 0,
               interval_sec <= gap_threshold_sec) |>
        mutate(
          litres = if (flow_unit == "L_per_h")
            .data[[val_col]] * interval_sec / 3600
          else
            .data[[val_col]] * interval_sec / 60
        ) |>
        group_by(obs_date) |>
        summarise(
          consumption_raw_L = round(sum(litres, na.rm = TRUE), 2),
          n_raw             = n(),
          .groups           = "drop"
        )

      # thinned integration
      thin_dir   <- .thin_dir(vn)
      thin_files <- if (dir.exists(thin_dir))
        list.files(thin_dir, pattern = "\\.parquet$",
                   recursive = TRUE, full.names = TRUE)
      else character(0)

      if (length(thin_files) > 0) {
        thinned <- map_dfr(thin_files, arrow::read_parquet) |>
          filter(device_id == dev_id) |>
          distinct(device_id, datetime, .keep_all = TRUE)

        if (!is.null(start_date))
          thinned <- filter(thinned, as.Date(datetime) >= as.Date(start_date))
        if (!is.null(end_date))
          thinned <- filter(thinned, as.Date(datetime) <= as.Date(end_date))

        thin_fuel <- thinned |>
          mutate(
            obs_date = as.Date(datetime),
            litres   = if (flow_unit == "L_per_h")
              value_mean * 60 / 3600
            else
              value_mean * 60 / 60
          ) |>
          group_by(obs_date) |>
          summarise(
            consumption_thin_L = round(sum(litres, na.rm = TRUE), 2),
            n_thinned          = n(),
            .groups            = "drop"
          )
      } else {
        thin_fuel <- tibble(obs_date           = as.Date(character()),
                            consumption_thin_L = numeric(),
                            n_thinned          = integer())
      }

      full_join(raw_fuel, thin_fuel, by = "obs_date") |>
        mutate(
          vessel_name = vn,
          device_id   = dev_id,
          device_name = dev_label,
          diff_pct    = round((consumption_thin_L - consumption_raw_L) /
                                pmax(consumption_raw_L, 0.01) * 100, 1)
        )
    })
  }) |>
    select(vessel_name, device_id, device_name, obs_date,
           consumption_raw_L, consumption_thin_L, diff_pct,
           n_raw, n_thinned) |>
    arrange(vessel_name, device_name, obs_date)
}


#' Line plot of daily fuel consumption: raw vs. thinned
#'
#' @param fuel_df     Output of build_fuel_comparison()
#' @param vessel_name Optional: filter to one vessel
#' @export
plot_fuel_comparison <- function(fuel_df, vessel_name = NULL) {

  df <- fuel_df
  if (!is.null(vessel_name))
    df <- filter(df, .data$vessel_name == vessel_name)
  if (nrow(df) == 0) { message("No fuel data to plot."); return(invisible(NULL)) }

  df_long <- df |>
    select(vessel_name, device_name, obs_date,
           consumption_raw_L, consumption_thin_L) |>
    pivot_longer(cols      = c(consumption_raw_L, consumption_thin_L),
                 names_to  = "source",
                 values_to = "litres") |>
    mutate(
      source      = recode(source,
                           consumption_raw_L  = "Raw (high-res)",
                           consumption_thin_L = "Thinned (1-min)"),
      facet_label = paste0(vessel_name, " - ", device_name)
    )

  ggplot(df_long, aes(x = obs_date, y = litres, colour = source)) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
    scale_colour_manual(
      values = c("Raw (high-res)" = "#2166ac", "Thinned (1-min)" = "#d95f02"),
      name   = NULL
    ) +
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title    = "Daily fuel consumption: raw vs. thinned",
      subtitle = paste0(format(min(df$obs_date), "%d %b %Y"), " - ",
                        format(max(df$obs_date), "%d %b %Y"),
                        "  |  integration method: flow rate x time step"),
      x = NULL, y = "Estimated consumption (litres)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 8),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


#' GT table: daily fuel consumption summary per vessel x sensor
#'
#' @param fuel_df  Output of build_fuel_comparison()
#' @export
table_fuel_comparison <- function(fuel_df) {

  df <- fuel_df |>
    group_by(vessel_name, device_name) |>
    summarise(
      days              = n(),
      total_raw_L       = round(sum(consumption_raw_L,  na.rm = TRUE), 0),
      total_thin_L      = round(sum(consumption_thin_L, na.rm = TRUE), 0),
      mean_daily_raw_L  = round(mean(consumption_raw_L,  na.rm = TRUE), 1),
      mean_daily_thin_L = round(mean(consumption_thin_L, na.rm = TRUE), 1),
      mean_diff_pct     = round(mean(diff_pct, na.rm = TRUE), 1),
      max_diff_pct      = round(max(abs(diff_pct), na.rm = TRUE), 1),
      .groups = "drop"
    )

  gt(df, groupname_col = "vessel_name") |>
    tab_header(
      title    = "Daily fuel consumption: raw vs. thinned",
      subtitle = paste0(format(min(fuel_df$obs_date), "%d %b %Y"),
                        " - ",
                        format(max(fuel_df$obs_date), "%d %b %Y"))
    ) |>
    cols_label(
      device_name       = "Sensor",
      days              = "Days",
      total_raw_L       = "Total raw (L)",
      total_thin_L      = "Total thinned (L)",
      mean_daily_raw_L  = "Mean daily raw (L)",
      mean_daily_thin_L = "Mean daily thinned (L)",
      mean_diff_pct     = "Mean diff (%)",
      max_diff_pct      = "Max |diff| (%)"
    ) |>
    fmt_number(columns = c(total_raw_L, total_thin_L), decimals = 0) |>
    fmt_number(columns = c(mean_daily_raw_L, mean_daily_thin_L,
                            mean_diff_pct, max_diff_pct),
               decimals = 1) |>
    data_color(
      columns = mean_diff_pct,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = c(-20, 20)
    ) |>
    tab_footnote(
      footnote  = "Consumption = integral of flow rate x time step; raw gaps > 5 min excluded",
      locations = cells_column_labels(columns = total_raw_L)
    ) |>
    tab_footnote(
      footnote  = "Mean diff = (thinned - raw) / raw x 100; positive = thinned overestimates",
      locations = cells_column_labels(columns = mean_diff_pct)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style = 6, color = "cyan") |>
    tab_options(table.font.size = "small")
}


# ============================================================================
# EXAMPLE USAGE for sections 6 and 7 (run interactively — vessel_config at top)
# ============================================================================
if (FALSE) {

  # Filter to vessels that have data on disk
  efice_root           <- file.path(.onedrive(), "raw", "efice")
  vessel_config_active <- vessel_config |>
    filter(enabled, file.exists(file.path(efice_root, vessel_name)))

  # Ensure thinned files exist first
  # poseidat_thin(save = TRUE)   # from Poseidat_functions.R

  # ── Observation count comparison ──────────────────────────────────────────
  obs <- build_obs_comparison(
    vessel_config = vessel_config_active,
    sources       = "efice",
    start_date    = as.Date("2026-01-10"),
    end_date      = Sys.Date()
  )

  p_obs <- plot_obs_comparison(obs)
  print(p_obs)

  tbl_obs <- table_obs_comparison(obs)
  print(tbl_obs)

  # Per vessel
  p_obs_polar <- plot_obs_comparison(obs, vessel_name = "Polar")
  print(p_obs_polar)

  # ── Fuel consumption comparison ────────────────────────────────────────────
  fuel <- build_fuel_comparison(
    vessel_config     = vessel_config_active,
    sources           = "efice",
    start_date        = as.Date("2026-01-10"),
    end_date          = Sys.Date(),
    fuel_keywords     = c("brandstof", "fuel", "flow", "verbruik"),
    flow_unit         = "L_per_h",   # adjust if your sensors report L/min
    gap_threshold_sec = 300
  )

  p_fuel <- plot_fuel_comparison(fuel)
  print(p_fuel)

  tbl_fuel <- table_fuel_comparison(fuel)
  print(tbl_fuel)

  # Inspect days where thinned deviates most from raw
  fuel |>
    filter(abs(diff_pct) > 10) |>
    select(vessel_name, device_name, obs_date,
           consumption_raw_L, consumption_thin_L, diff_pct) |>
    arrange(desc(abs(diff_pct)))
}
