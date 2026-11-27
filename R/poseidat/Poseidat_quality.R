# ============================================================================
# Poseidat_quality.R
# Sensor data quality assessment for the DIGIvloot / EFICE Poseidat pipeline
#
# Provides functions to assess:
#   1. Data availability / uptime per vessel × device × period
#   2. Gap detection (missing windows in the time series)
#   3. Outlier / anomaly detection per sensor
#   4. Summary plots and tables
#
# Prerequisites: source Poseidat_functions.R first (provides .onedrive(),
#   .poseidat_load_data(), and .device_label() used internally below).
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


# ── Internal helper: load raw parquet for one vessel (deduped) ───────────────
# Wraps the shared .poseidat_load_data() from Poseidat_functions.R, adding
# entry_id deduplication which is specific to quality assessment on raw data.
.qc_load_vessel <- function(vessel_name,
                             sources    = "efice",
                             start_date = NULL,
                             end_date   = NULL) {

  df <- .poseidat_load_data("raw", source = sources,
                             vessel_name = vessel_name)
  if (nrow(df) == 0) return(df)

  df <- df |>
    distinct(entry_id, .keep_all = TRUE) |>
    arrange(datetime)

  if (!is.null(start_date))
    df <- filter(df, as.Date(datetime) >= as.Date(start_date))
  if (!is.null(end_date))
    df <- filter(df, as.Date(datetime) <= as.Date(end_date))

  message(sprintf("  %s: %d records after dedup and date filter.", vessel_name, nrow(df)))
  df
}


# ============================================================================
# 1.  BUILD QUALITY SUMMARY
#     Returns one row per vessel × device × day with availability metrics.
# ============================================================================

#' Build a daily data-quality summary for all vessels and devices
#'
#' For each vessel × device × day the function computes:
#'   - n_obs          : number of observations
#'   - median_interval_sec : median gap between successive timestamps (seconds)
#'   - expected_interval   : mode of the expected interval (seconds)
#'   - n_gaps         : number of gaps > gap_threshold_mult × expected_interval
#'   - max_gap_min    : largest gap (minutes)
#'   - uptime_pct     : fraction of the day covered without large gaps
#'   - n_outliers     : values beyond outlier_sd standard deviations from mean
#'
#' @param vessel_config   data.frame from readRDS("efice_vessels.rds"),
#'                        must have columns: vessel_name, device_labels (list-col)
#' @param sources         Character vector of source subfolders to scan,
#'                        e.g. "efice" or c("efice", "pefa"). Default: "efice"
#' @param start_date      Date – first day to include
#' @param end_date        Date – last day to include (default: today)
#' @param gap_threshold_mult  Numeric – a gap is flagged when it is larger than
#'                            this multiple of the median interval (default: 5)
#' @param outlier_sd      Numeric – IQR-based fence multiplier (default: 3)
#'
#' @return tibble with quality summary, one row per vessel × device × day
#' @export
build_quality_summary <- function(vessel_config,
                                   sources            = "efice",
                                   start_date         = Sys.Date() - 30,
                                   end_date           = Sys.Date(),
                                   gap_threshold_mult = 5,
                                   outlier_sd         = 3) {

  vessels <- vessel_config$vessel_name
  message(sprintf("Building quality summary for %d vessels, %s to %s ...",
                  length(vessels),
                  format(as.Date(start_date), "%d %b %Y"),
                  format(as.Date(end_date),   "%d %b %Y")))

  all_rows <- map_dfr(vessels, function(vn) {
    df <- .qc_load_vessel(vn,
                          sources    = sources,
                          start_date = start_date,
                          end_date   = end_date)
    if (nrow(df) == 0) return(tibble())

    message(sprintf("  %s: %d records loaded", vn, nrow(df)))

    df <- df |>
      mutate(obs_date = as.Date(datetime)) |>
      group_by(device_id) |>
      arrange(datetime, .by_group = TRUE) |>
      mutate(
        interval_sec = as.numeric(difftime(datetime, lag(datetime),
                                           units = "secs"))
      ) |>
      ungroup()

    # Determine expected interval per device (mode-like: use 10th percentile
    # to avoid influence of intentional large gaps during harbour)
    exp_intervals <- df |>
      filter(!is.na(interval_sec), interval_sec > 0) |>
      group_by(device_id) |>
      summarise(
        expected_interval_sec = quantile(interval_sec, 0.10, na.rm = TRUE),
        .groups = "drop"
      )

    df <- left_join(df, exp_intervals, by = "device_id")

    # Per-device, per-day summary
    day_summary <- df |>
      group_by(device_id, obs_date) |>
      summarise(
        n_obs             = n(),
        median_interval_sec = median(interval_sec, na.rm = TRUE),
        expected_interval_sec = first(expected_interval_sec),
        max_gap_sec       = max(interval_sec, na.rm = TRUE),
        n_gaps            = sum(interval_sec > gap_threshold_mult *
                                  first(expected_interval_sec), na.rm = TRUE),
        # Uptime: fraction of 24 h that is "covered" by the sensor
        # Defined as 1 - (total gap time > threshold) / 86400
        total_gap_sec     = sum(pmax(interval_sec -
                                       gap_threshold_mult * first(expected_interval_sec),
                                     0), na.rm = TRUE),
        uptime_pct        = pmax(0, pmin(100,
                                         (1 - total_gap_sec / 86400) * 100)),
        .groups           = "drop"
      ) |>
      mutate(
        max_gap_min    = round(max_gap_sec / 60, 1),
        vessel_name    = vn
      )

    # Outlier detection — needs value column; scan for any numeric column
    # named "value" (common in PoseiDAT) or pick the first available numeric
    num_cols <- df |>
      select(where(is.numeric)) |>
      select(-any_of(c("interval_sec", "expected_interval_sec"))) |>
      names()

    if ("value" %in% num_cols) {
      val_col <- "value"
    } else if (length(num_cols) > 0) {
      val_col <- num_cols[1]
    } else {
      val_col <- NULL
    }

    if (!is.null(val_col)) {
      outlier_summary <- df |>
        group_by(device_id, obs_date) |>
        summarise(
          n_outliers = {
            v   <- .data[[val_col]]
            q25 <- quantile(v, 0.25, na.rm = TRUE)
            q75 <- quantile(v, 0.75, na.rm = TRUE)
            iqr <- q75 - q25
            sum(v < (q25 - outlier_sd * iqr) |
                  v > (q75 + outlier_sd * iqr), na.rm = TRUE)
          },
          .groups = "drop"
        )
      day_summary <- left_join(day_summary, outlier_summary,
                               by = c("device_id", "obs_date"))
    } else {
      day_summary <- mutate(day_summary, n_outliers = NA_integer_)
    }

    day_summary
  })

  if (nrow(all_rows) == 0) return(tibble())

  # Attach human-readable device labels
  all_rows <- all_rows |>
    rowwise() |>
    mutate(
      device_name = .device_label(vessel_config, vessel_name, device_id)
    ) |>
    ungroup() |>
    select(vessel_name, device_id, device_name,
           obs_date, n_obs, median_interval_sec, expected_interval_sec,
           max_gap_min, n_gaps, uptime_pct, n_outliers) |>
    arrange(vessel_name, device_name, obs_date)

  message(sprintf("Done. %d vessel × device × day rows.", nrow(all_rows)))
  all_rows
}


# ============================================================================
# 2.  GAP CATALOGUE
#     Returns every individual gap (start, end, duration) above a threshold.
# ============================================================================

#' Catalogue all data gaps per vessel × device
#'
#' @param vessel_config   See build_quality_summary()
#' @param sources         Character vector – e.g. "efice" or c("efice","pefa")
#' @param start_date      Date
#' @param end_date        Date
#' @param min_gap_minutes Minimum gap duration to include (default: 30 min)
#'
#' @return tibble with one row per gap event
#' @export
build_gap_catalogue <- function(vessel_config,
                                 sources         = "efice",
                                 start_date      = Sys.Date() - 30,
                                 end_date        = Sys.Date(),
                                 min_gap_minutes = 30) {

  vessels <- vessel_config$vessel_name
  min_gap_sec <- min_gap_minutes * 60

  map_dfr(vessels, function(vn) {
    df <- .qc_load_vessel(vn,
                          sources    = sources,
                          start_date = start_date,
                          end_date   = end_date)
    if (nrow(df) == 0) return(tibble())

    df |>
      group_by(device_id) |>
      arrange(datetime, .by_group = TRUE) |>
      mutate(
        gap_sec    = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
        gap_start  = lag(datetime)
      ) |>
      filter(!is.na(gap_sec), gap_sec >= min_gap_sec) |>
      transmute(
        vessel_name  = vn,
        device_id    = device_id,
        gap_start    = gap_start,
        gap_end      = datetime,
        gap_duration_hours = round(gap_sec / 3600, 2),
        gap_type     = case_when(
          gap_sec < 3600       ~ "short (<1h)",
          gap_sec < 6 * 3600   ~ "medium (1–6h)",
          gap_sec < 24 * 3600  ~ "long (6–24h)",
          TRUE                 ~ "very long (>24h)"
        )
      ) |>
      ungroup()
  }) |>
    rowwise() |>
    mutate(device_name = .device_label(vessel_config, vessel_name, device_id)) |>
    ungroup() |>
    arrange(vessel_name, device_name, gap_start)
}


# ============================================================================
# 3.  PLOTS
# ============================================================================

#' Heatmap of sensor uptime (%) per vessel × device × day
#'
#' @param quality_summary  Output of build_quality_summary()
#' @param metric           "uptime_pct" | "n_gaps" | "n_outliers" |
#'                         "max_gap_min" | "median_interval_sec"
#' @param max_days         Integer – show only the most recent N days
#' @export
plot_quality_heatmap <- function(quality_summary,
                                  metric   = "uptime_pct",
                                  max_days = 30) {

  metric_labels <- c(
    uptime_pct           = "Uptime (%)",
    n_gaps               = "Number of gaps",
    n_outliers           = "Number of outliers",
    max_gap_min          = "Largest gap (minutes)",
    median_interval_sec  = "Median interval (seconds)"
  )
  stopifnot(metric %in% names(metric_labels))

  # Recent window
  max_date  <- max(quality_summary$obs_date)
  min_date  <- max_date - max_days + 1

  df <- quality_summary |>
    filter(obs_date >= min_date) |>
    mutate(
      y_label  = paste0(vessel_name, " – ", device_name),
      value    = .data[[metric]]
    )

  if (nrow(df) == 0) {
    message("No data in the selected date range.")
    return(invisible(NULL))
  }

  # Colour direction: uptime_pct → high = good (green); others → low = good (red)
  good_high  <- metric == "uptime_pct"
  fill_low   <- if (good_high) "#d73027" else "#1a9641"
  fill_high  <- if (good_high) "#1a9641" else "#d73027"
  fill_na    <- "grey85"

  ggplot(df, aes(x = obs_date, y = y_label, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_x_date(
      date_breaks = if (max_days <= 14) "2 days" else "1 week",
      date_labels = "%d %b",
      expand      = expansion(add = 0.5)
    ) +
    scale_fill_gradient(
      low     = fill_low,
      high    = fill_high,
      na.value = fill_na,
      name    = metric_labels[[metric]]
    ) +
    labs(
      title    = paste0("Sensor quality — ", metric_labels[[metric]]),
      subtitle = paste0(format(min_date, "%d %b %Y"), " – ",
                        format(max_date, "%d %b %Y")),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 9),
      legend.position  = "right",
      panel.grid       = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}


#' Timeline strip-plot of gaps for one or all vessels
#'
#' @param gap_catalogue   Output of build_gap_catalogue()
#' @param vessel_name     Optional: filter to one vessel
#' @export
plot_gap_timeline <- function(gap_catalogue,
                               vessel_name = NULL) {

  df <- gap_catalogue
  if (!is.null(vessel_name))
    df <- filter(df, .data$vessel_name == vessel_name)
  if (nrow(df) == 0) {
    message("No gaps found.")
    return(invisible(NULL))
  }

  df <- df |>
    mutate(
      y_label   = paste0(vessel_name, " – ", device_name),
      gap_type  = factor(gap_type,
                         levels = c("short (<1h)", "medium (1–6h)",
                                    "long (6–24h)", "very long (>24h)"))
    )

  gap_colours <- c(
    "short (<1h)"     = "#fee08b",
    "medium (1–6h)"   = "#fc8d59",
    "long (6–24h)"    = "#d73027",
    "very long (>24h)"= "#4d0000"
  )

  ggplot(df, aes(x = gap_start, xend = gap_end,
                  y = y_label, yend = y_label,
                  colour = gap_type, linewidth = gap_duration_hours)) +
    geom_segment(lineend = "round", alpha = 0.85) +
    scale_colour_manual(values = gap_colours, name = "Gap duration") +
    scale_linewidth_continuous(range = c(1, 6), guide = "none") +
    labs(
      title    = "Data gaps by vessel and sensor",
      subtitle = paste0(format(min(df$gap_start), "%d %b %Y"), " – ",
                        format(max(df$gap_end),   "%d %b %Y")),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold")
    )
}


#' Box-plot of daily uptime per device across the analysis period
#'
#' @param quality_summary  Output of build_quality_summary()
#' @export
plot_uptime_distribution <- function(quality_summary) {

  df <- quality_summary |>
    mutate(y_label = paste0(vessel_name, " – ", device_name))

  ggplot(df, aes(x = uptime_pct, y = reorder(y_label, uptime_pct, median))) +
    geom_boxplot(fill = "#4575b4", colour = "#333333",
                 outlier.colour = "#d73027", outlier.size = 1.5,
                 width = 0.6) +
    geom_vline(xintercept = 90, linetype = "dashed",
               colour = "#d73027", linewidth = 0.5) +
    scale_x_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
    labs(
      title    = "Daily sensor uptime distribution",
      subtitle = paste0(format(min(quality_summary$obs_date), "%d %b %Y"),
                        " – ",
                        format(max(quality_summary$obs_date), "%d %b %Y"),
                        "  |  dashed line = 90 % threshold"),
      x = "Uptime (%)", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(face = "bold")
    )
}


# ============================================================================
# 4.  SUMMARY TABLE  (gt)
# ============================================================================

#' GT table: aggregated quality metrics per vessel × device
#'
#' @param quality_summary  Output of build_quality_summary()
#' @export
table_quality_summary <- function(quality_summary) {

  df <- quality_summary |>
    group_by(vessel_name, device_name) |>
    summarise(
      days_with_data   = n(),
      mean_uptime_pct  = round(mean(uptime_pct,  na.rm = TRUE), 1),
      median_uptime_pct = round(median(uptime_pct, na.rm = TRUE), 1),
      total_gaps       = sum(n_gaps,     na.rm = TRUE),
      max_gap_min      = max(max_gap_min, na.rm = TRUE),
      total_outliers   = sum(n_outliers,  na.rm = TRUE),
      mean_interval_sec = round(mean(median_interval_sec, na.rm = TRUE), 1),
      .groups = "drop"
    )

  gt(df, groupname_col = "vessel_name") |>
    tab_header(
      title    = "Poseidat sensor data quality summary",
      subtitle = paste0(format(min(quality_summary$obs_date), "%d %b %Y"),
                        " – ",
                        format(max(quality_summary$obs_date), "%d %b %Y"))
    ) |>
    cols_label(
      device_name       = "Sensor",
      days_with_data    = "Days",
      mean_uptime_pct   = "Mean uptime (%)",
      median_uptime_pct = "Median uptime (%)",
      total_gaps        = "Total gaps",
      max_gap_min       = "Max gap (min)",
      total_outliers    = "Outliers",
      mean_interval_sec = "Mean interval (s)"
    ) |>
    data_color(
      columns = mean_uptime_pct,
      palette = c("#d73027", "#fee08b", "#1a9641"),
      domain  = c(0, 100)
    ) |>
    data_color(
      columns = total_gaps,
      palette = c("#1a9641", "#fee08b", "#d73027"),
      domain  = NULL
    ) |>
    fmt_number(columns = c(mean_uptime_pct, median_uptime_pct,
                            mean_interval_sec),
               decimals = 1) |>
    fmt_number(columns = c(total_gaps, total_outliers), decimals = 0) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style = 6, color = "blue") |>
    tab_options(table.font.size = "small")
}


# ============================================================================
# EXAMPLE USAGE (run interactively — vessel_config loaded at top of file)
# ============================================================================
if (FALSE) {

  # Filter to vessels that have data on disk
  efice_root        <- file.path(.onedrive(), "raw", "efice")
  vessel_config_qc  <- vessel_config |>
    filter(enabled, file.exists(file.path(efice_root, vessel_name)))

  # ── 1. Build daily quality summary ──────────────────────────────────────────
  qs <- build_quality_summary(
    vessel_config      = vessel_config_qc,
    sources            = "efice",          # or c("efice", "pefa")
    start_date         = Sys.Date() - 60,
    end_date           = Sys.Date(),
    gap_threshold_mult = 5,
    outlier_sd         = 3
  )

  # ── 2. Uptime heatmap ───────────────────────────────────────────────────────
  p_up <- plot_quality_heatmap(qs, metric = "uptime_pct", max_days = 30)
  print(p_up)

  # ── 3. Gap count heatmap ────────────────────────────────────────────────────
  p_gaps <- plot_quality_heatmap(qs, metric = "n_gaps", max_days = 30)
  print(p_gaps)

  # ── 4. Uptime distribution (box-plots) ──────────────────────────────────────
  p_box <- plot_uptime_distribution(qs)
  print(p_box)

  # ── 5. Gap catalogue & timeline ─────────────────────────────────────────────
  gaps <- build_gap_catalogue(
    vessel_config   = vessel_config_qc,
    sources         = "efice",
    start_date      = Sys.Date() - 60,
    end_date        = Sys.Date(),
    min_gap_minutes = 30
  )

  p_gap_tl <- plot_gap_timeline(gaps)
  print(p_gap_tl)

  # Per vessel
  p_gap_polar <- plot_gap_timeline(gaps, vessel_name = "Polar")
  print(p_gap_polar)

  # ── 6. Summary GT table ─────────────────────────────────────────────────────
  tbl <- table_quality_summary(qs)
  print(tbl)

  # ── 7. Quick numeric inspection ─────────────────────────────────────────────
  qs |>
    filter(uptime_pct < 80) |>
    arrange(uptime_pct) |>
    select(vessel_name, device_name, obs_date, uptime_pct, n_gaps, max_gap_min) |>
    print(n = 30)
}
