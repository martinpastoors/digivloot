# =============================================================================
# poseidat_trip_plots.R
#
# Two additions to the trip-summary workflow:
#
#   poseidat_map_trips()      — faceted ggplot map of GPS tracks per trip,
#                               one panel per trip, coloured by trip_id,
#                               departure and arrival harbour marked.
#
#   poseidat_trip_sensors()   — per-trip average of every sensor type,
#                               split by device when by_device = TRUE.
#                               Uses the same value_type pivot logic as
#                               poseidat_plot_overview() so column names
#                               match ("Traction center", "Speed (SOG)", etc.)
#
# Requires:
#   Poseidat_functions.R          — poseidat_positions(), .onedrive(), etc.
#   poseidat_atsea_or_in_harbour.R — poseidat_trip_summary(), north_sea_harbours
#
# Additional dependencies: ggplot2, sf (optional, for better map tiles)
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(arrow)


# =============================================================================
# 1.  poseidat_map_trips()
# =============================================================================

#' Faceted map of vessel GPS tracks, one panel per trip
#'
#' Draws the 1-minute thinned track for each selected trip, coloured by
#' trip_id, with departure and arrival harbour locations annotated.
#' One facet per trip; facet header shows trip_id and date range.
#' Plot title shows vessel name and overall period.
#'
#' @param trips        Tibble from \code{poseidat_trip_summary()} — already
#'                     filtered to one vessel before passing in, or use
#'                     \code{vessel_name} below.
#' @param vessel_name  Character(1). Must match exactly one vessel in \code{trips}.
#' @param trip_ids     Integer vector of trip_ids to include.
#'                     NULL = all trips for the vessel (can be slow for large fleets).
#' @param harbours     Harbour reference table with harbour_name, lat, lon.
#'                     Defaults to \code{north_sea_harbours}.
#' @param ncol         Number of facet columns. Default 3.
#' @param point_size   Size of GPS track points. Default 0.4.
#' @param harbour_size Size of harbour annotation points. Default 3.
#'
#' @return A ggplot object (print or ggsave it).
#'
#' @examples
#' \dontrun{
#' trips <- poseidat_trip_summary(start_date = as.Date("2025-01-01"))
#'
#' # Map trips 1-9 of Lub Senior
#' p <- poseidat_map_trips(trips, vessel_name = "Lub Senior", trip_ids = 1:9)
#' print(p)
#' ggsave("lub_senior_trips.png", p, width = 14, height = 12, dpi = 150)
#' }
poseidat_map_trips <- function(trips,
                                vessel_name  = NULL,
                                trip_ids     = NULL,
                                harbours     = north_sea_harbours,
                                ncol         = 3,
                                point_size   = 0.4,
                                harbour_size = 3) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")

  # ── 0. Filter trips to the requested vessel ────────────────────────────────
  if (!is.null(vessel_name)) {
    trips <- trips |> filter(vessel_name == .env$vessel_name)
  }

  if (n_distinct(trips$vessel_name) > 1)
    stop("trips contains more than one vessel — supply vessel_name to select one.")

  if (nrow(trips) == 0)
    stop("No trips found for vessel '", vessel_name, "'.")

  vname <- unique(trips$vessel_name)

  if (!is.null(trip_ids))
    trips <- trips |> filter(trip_id %in% trip_ids)

  if (nrow(trips) == 0)
    stop("No trips remaining after filtering trip_ids.")


  # ── 1. Load thinned positions for the trip window ─────────────────────────
  window_start <- as.Date(min(trips$departure_time, na.rm = TRUE))
  window_end   <- as.Date(max(trips$arrival_time,   na.rm = TRUE)) + 1L

  message(sprintf("Loading positions for %s (%s to %s) ...",
                  vname,
                  format(window_start), format(window_end)))

  pos <- poseidat_positions(
    vessel_name = vname,
    data_source = "thinned",
    start_date  = window_start,
    end_date    = window_end
  )

  if (nrow(pos) == 0)
    stop("No thinned position data found for vessel '", vname, "'.")


  # ── 2. Assign each fix to a trip_id ───────────────────────────────────────
  #   For each trip, keep fixes that fall inside [departure_time, arrival_time].
  #   We expand trips into an interval lookup and join by time range.
  #   No apply — uses a left_join + filter on pre-built interval bounds.

  trip_windows <- trips |>
    select(trip_id, t_start = departure_time, t_end = arrival_time)

  # Cross-join pos with trip_windows, then keep only matches
  # (reasonable because n_trips per vessel is small, typically < 200)
  pos_trips <- pos |>
    cross_join(trip_windows) |>
    filter(datetime >= t_start, datetime <= t_end) |>
    select(-t_start, -t_end)


  if (nrow(pos_trips) == 0)
    stop("No position fixes fall within any of the selected trip windows.")


  # ── 3. Build facet labels: "Trip N  |  DD Mon – DD Mon YYYY" ──────────────
  trip_labels <- trips |>
    mutate(
      facet_label = sprintf(
        "Trip %d  |  %s \u2013 %s",
        trip_id,
        format(departure_time, "%d %b"),
        format(arrival_time,   "%d %b %Y")
      )
    ) |>
    select(trip_id, facet_label)

  pos_trips <- pos_trips |>
    left_join(trip_labels, by = "trip_id") |>
    mutate(facet_label = factor(facet_label,
                                levels = trip_labels$facet_label))


  # ── 4. Harbour annotation points ──────────────────────────────────────────
  #   One point per trip for departure harbour, one for arrival harbour.
  #   Joined from the harbours table via harbour name.

  dep_pts <- trips |>
    filter(!is.na(departure_harbour)) |>
    left_join(harbours |> select(harbour_name, lat, lon),
              by = c("departure_harbour" = "harbour_name")) |>
    left_join(trip_labels, by = "trip_id") |>
    mutate(
      facet_label  = factor(facet_label, levels = trip_labels$facet_label),
      harbour_role = "Departure"
    ) |>
    select(trip_id, facet_label, lat, lon, harbour_role,
           harbour_name = departure_harbour)

  arr_pts <- trips |>
    filter(!is.na(arrival_harbour)) |>
    left_join(harbours |> select(harbour_name, lat, lon),
              by = c("arrival_harbour" = "harbour_name")) |>
    left_join(trip_labels, by = "trip_id") |>
    mutate(
      facet_label  = factor(facet_label, levels = trip_labels$facet_label),
      harbour_role = "Arrival"
    ) |>
    select(trip_id, facet_label, lat, lon, harbour_role,
           harbour_name = arrival_harbour)

  harbour_pts <- bind_rows(dep_pts, arr_pts) |>
    mutate(harbour_role = factor(harbour_role, levels = c("Departure", "Arrival")))


  # ── 5. Build colour palette — one colour per trip_id ──────────────────────
  n_trips  <- n_distinct(pos_trips$trip_id)
  trip_colours <- setNames(
    scales::hue_pal()(n_trips),
    sort(unique(pos_trips$trip_id))
  )


  # ── 6. Assemble plot ───────────────────────────────────────────────────────
  period_str <- sprintf("%s \u2013 %s",
                        format(min(trips$departure_time, na.rm = TRUE), "%d %b %Y"),
                        format(max(trips$arrival_time,   na.rm = TRUE), "%d %b %Y"))

  p <- ggplot() +
    # GPS track points
    geom_point(
      data    = pos_trips,
      mapping = aes(x = longitude, y = latitude, colour = factor(trip_id)),
      size    = point_size,
      alpha   = 0.7
    ) +
    # Harbour markers
    geom_point(
      data    = harbour_pts,
      mapping = aes(x = lon, y = lat, shape = harbour_role),
      size    = harbour_size,
      colour  = "black",
      fill    = "white",
      stroke  = 1.2
    ) +
    # Harbour name labels (slightly offset to avoid overlap with point)
    geom_text(
      data    = harbour_pts,
      mapping = aes(x = lon, y = lat, label = harbour_name),
      size    = 2.5,
      hjust   = -0.15,
      vjust   = 0.5,
      colour  = "grey20"
    ) +
    scale_colour_manual(values = trip_colours, guide = "none") +
    scale_shape_manual(
      values = c("Departure" = 24L, "Arrival" = 25L),   # up / down triangle
      name   = NULL
    ) +
    facet_wrap(~ facet_label, ncol = ncol, scales = "free") +
    coord_quickmap() +
    labs(
      title    = sprintf("%s — trip tracks", vname),
      subtitle = period_str,
      x        = "Longitude",
      y        = "Latitude",
      caption  = sprintf("1-min thinned GPS  |  Generated: %s", Sys.Date())
    ) +
    theme_bw(base_size = 10) +
    theme(
      strip.text       = element_text(face = "bold", size = 8),
      strip.background = element_rect(fill = "grey92", colour = NA),
      axis.text        = element_text(size = 7),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )

  p
}


# =============================================================================
# 2.  poseidat_trip_sensors()
# =============================================================================

#' Per-trip average sensor values
#'
#' Loads thinned parquet data for one vessel, pivots non-scalar types
#' (POSITION, TRAWL_TENSION) into named rows using the same naming convention
#' as \code{poseidat_plot_overview()}, then calculates the mean value per
#' trip per sensor type (and optionally per device label).
#'
#' Non-zero filter is applied to all types except Longitude (sign matters).
#'
#' @param trips        Tibble from \code{poseidat_trip_summary()}.
#' @param vessel_name  Character(1). Vessel to analyse.
#' @param trip_ids     Integer vector of trip_ids to include. NULL = all.
#' @param value_types  Character vector of sensor names to include.
#'                     Uses the pivoted names: "FUEL_CONSUMPTION", "DEPTH",
#'                     "Speed (SOG)", "Traction center", "Traction port",
#'                     "Traction starboard", "Shooted length center",
#'                     "Shooted length port", "Shooted length starboard",
#'                     "Latitude", "Longitude".
#'                     NULL = all available.
#' @param by_device    If TRUE, split averages per device label. Default TRUE.
#' @param source       API source filter. NULL = all.
#'
#' @return A tibble with columns:
#'   vessel_name, trip_id, departure_harbour, departure_time, arrival_harbour,
#'   arrival_time, value_type, [device_label if by_device], mean_value, n_fixes.
#'
#' @examples
#' \dontrun{
#' trips  <- poseidat_trip_summary(start_date = as.Date("2025-01-01"))
#' sensor <- poseidat_trip_sensors(trips, vessel_name = "Lub Senior")
#'
#' # Wide format — one column per sensor type
#' sensor |>
#'   filter(!by_device) |>
#'   select(trip_id, value_type, mean_value) |>
#'   tidyr::pivot_wider(names_from = value_type, values_from = mean_value)
#' }
poseidat_trip_sensors <- function(trips,
                                   vessel_name = NULL,
                                   trip_ids    = NULL,
                                   value_types = c(
                                     "FUEL_CONSUMPTION",
                                     "DEPTH",
                                     "Speed (SOG)",
                                     "Traction center",
                                     "Traction port",
                                     "Traction starboard",
                                     "Shooted length center",
                                     "Shooted length port",
                                     "Shooted length starboard",
                                     "Latitude",
                                     "Longitude"
                                   ),
                                   by_device   = TRUE,
                                   source      = NULL) {

  # ── 0. Filter trips ────────────────────────────────────────────────────────
  if (!is.null(vessel_name))
    trips <- trips |> filter(vessel_name == .env$vessel_name)

  if (n_distinct(trips$vessel_name) > 1)
    stop("trips contains more than one vessel — supply vessel_name to select one.")

  if (nrow(trips) == 0)
    stop("No trips found for vessel '", vessel_name, "'.")

  vname <- unique(trips$vessel_name)

  if (!is.null(trip_ids))
    trips <- trips |> filter(trip_id %in% trip_ids)

  if (nrow(trips) == 0)
    stop("No trips remaining after filtering trip_ids.")


  # ── 1. Load thinned parquet directly (all value_types in one read) ─────────
  #   poseidat_positions() only returns POSITION rows; here we need all sensor
  #   types, so we read the thinned parquet files for this vessel ourselves.

  base        <- .onedrive()
  thinned_dir <- file.path(base, "processed", "thinned")

  # Locate the latest thinned file for this vessel
  vessel_safe <- gsub(" ", "_", vname)
  vessel_dir  <- file.path(thinned_dir, vessel_safe)

  if (!dir.exists(vessel_dir))
    stop("No thinned directory found for vessel '", vname,
         "'.  Run poseidat_thin(save = TRUE) first.")

  parquet_files <- sort(
    list.files(vessel_dir, pattern = "\\.parquet$", full.names = TRUE),
    decreasing = TRUE
  )

  if (length(parquet_files) == 0)
    stop("No thinned parquet files found for vessel '", vname, "'.")

  # Use only the most recent file (daily versioned, so this is always current)
  raw_df <- arrow::read_parquet(parquet_files[[1]])

  if (nrow(raw_df) == 0) {
    message("Thinned parquet is empty for vessel '", vname, "'.")
    return(tibble())
  }

  # Filter to the trip window
  window_start <- min(trips$departure_time, na.rm = TRUE)
  window_end   <- max(trips$arrival_time,   na.rm = TRUE)

  raw_df <- raw_df |>
    filter(datetime >= window_start, datetime <= window_end)

  if (!is.null(source))
    raw_df <- raw_df |> filter(source %in% .env$source)


  # ── 2. Load device label map ───────────────────────────────────────────────
  cfg_path  <- file.path(base, "config", "poseidat_vessels.rds")
  label_map <- NULL

  if (file.exists(cfg_path)) {
    cfg <- readRDS(cfg_path)
    if ("device_labels" %in% names(cfg)) {
      label_map <- cfg |>
        dplyr::select(vessel_name, device_labels) |>
        tidyr::unnest_longer(device_labels,
                             indices_to = "device_id",
                             values_to  = "label") |>
        dplyr::select(vessel_name, device_id, label)
    }
  }

  # Join vessel_name (not stored in thinned parquet, comes from config)
  vessel_lookup <- tryCatch({
    readRDS(cfg_path) |>
      dplyr::select(-device_labels) |>
      dplyr::select(journal_id, vessel_name)
  }, error = \(e) tibble(journal_id = character(), vessel_name = character()))

  raw_df <- raw_df |>
    left_join(vessel_lookup, by = "journal_id")

  if (!is.null(label_map))
    raw_df <- raw_df |>
      left_join(label_map |> select(device_id, label),
                by = "device_id")

  if (!"label" %in% names(raw_df))
    raw_df <- raw_df |> mutate(label = NA_character_)

  # Fall back to last 8 chars of device_id when no friendly label exists
  raw_df <- raw_df |>
    mutate(
      label = coalesce(label,
                       substr(device_id, nchar(device_id) - 7, nchar(device_id)))
    )


  # ── 3. Pivot non-scalar types into value_numeric rows ─────────────────────
  #   Mirrors the logic in poseidat_plot_overview() so names match exactly.

  shared_cols <- c("datetime", "vessel_name", "device_id",
                   "journal_id", "source", "label")

  .pivot_col <- function(df, col, new_type) {
    if (nrow(df) == 0 || !col %in% names(df)) return(tibble())
    df |>
      filter(!is.na(.data[[col]])) |>
      select(all_of(intersect(shared_cols, names(df))),
             value_numeric = all_of(col)) |>
      mutate(value_type = new_type)
  }

  df_scalar <- raw_df |>
    filter(!value_type %in% c("POSITION", "TRAWL_TENSION"),
           !is.na(value_numeric))

  df_pos   <- raw_df |> filter(value_type == "POSITION")
  df_trawl <- raw_df |> filter(value_type == "TRAWL_TENSION")

  pos_rows <- bind_rows(
    .pivot_col(df_pos,   "latitude",          "Latitude"),
    .pivot_col(df_pos,   "longitude",         "Longitude"),
    .pivot_col(df_pos,   "speed_over_ground", "Speed (SOG)")
  )

  trawl_rows <- bind_rows(
    .pivot_col(df_trawl, "traction_port",            "Traction port"),
    .pivot_col(df_trawl, "traction_center",          "Traction center"),
    .pivot_col(df_trawl, "traction_starboard",       "Traction starboard"),
    .pivot_col(df_trawl, "shooted_length_port",      "Shooted length port"),
    .pivot_col(df_trawl, "shooted_length_center",    "Shooted length center"),
    .pivot_col(df_trawl, "shooted_length_starboard", "Shooted length starboard")
  )

  df_combined <- bind_rows(df_scalar, pos_rows, trawl_rows)

  if (nrow(df_combined) == 0) {
    message("No sensor data found after pivoting.")
    return(tibble())
  }


  # ── 4. Filter to requested value_types ────────────────────────────────────
  if (!is.null(value_types))
    df_combined <- df_combined |>
      filter(value_type %in% value_types)

  if (nrow(df_combined) == 0) {
    message("No data remaining after value_type filter.")
    return(tibble())
  }


  # ── 5. Non-zero filter (Longitude excluded — its sign carries meaning) ─────
  df_combined <- df_combined |>
    filter(value_type == "Longitude" | value_numeric != 0)


  # ── 6. Assign each sensor fix to a trip ───────────────────────────────────
  trip_windows <- trips |>
    select(trip_id, departure_harbour, departure_time,
           arrival_harbour, arrival_time)

  # Cross-join then filter to matching time window
  df_trips <- df_combined |>
    cross_join(trip_windows) |>
    filter(datetime >= departure_time, datetime <= arrival_time) |>
    select(-departure_time, -arrival_time)


  if (nrow(df_trips) == 0) {
    message("No sensor fixes fall within any trip window.")
    return(tibble())
  }


  # ── 7. Summarise: mean per trip × value_type (× device if by_device) ──────
  # vessel_name may not survive the pivot if journal_id was absent from the
  # thinned parquet, so exclude it from group_vars and add it back as a
  # constant after summarising.
  group_vars <- if (by_device) {
    c("trip_id", "departure_harbour", "arrival_harbour", "value_type", "label")
  } else {
    c("trip_id", "departure_harbour", "arrival_harbour", "value_type")
  }

  sensor_summary <- df_trips |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      mean_value = mean(value_numeric, na.rm = TRUE),
      n_fixes    = n(),
      .groups    = "drop"
    ) |>
    mutate(vessel_name = vname)

  # Re-attach departure_time and arrival_time for context
  trip_meta <- trips |>
    select(trip_id, departure_time, arrival_time)

  sensor_summary <- sensor_summary |>
    left_join(trip_meta, by = "trip_id")

  # Clean column order
  leading_cols <- c("vessel_name", "trip_id",
                    "departure_harbour", "departure_time",
                    "arrival_harbour",   "arrival_time",
                    "value_type")
  if (by_device) leading_cols <- c(leading_cols, "label")

  sensor_summary |>
    select(all_of(leading_cols), mean_value, n_fixes) |>
    arrange(vessel_name, trip_id, value_type,
            if (by_device) label else NULL)
}


# =============================================================================
# EXAMPLE USAGE
# =============================================================================
if (FALSE) {

  source(file.path(here::here(), "R/poseidat/Poseidat_functions.R"))
  source(file.path(here::here(), "R/poseidat/poseidat_atsea_or_in_harbour.R"))
  setup_poseidat_sync()

  trips <- poseidat_trip_summary(start_date = as.Date("2025-01-01"))

  # ── Map ────────────────────────────────────────────────────────────────────

  # First 9 trips of Lub Senior, 3 columns
  p <- poseidat_map_trips(
    trips,
    vessel_name = "Lub Senior",
    trip_ids    = 1:9,
    ncol        = 3
  )
  print(p)

  # Save
  ggsave(
    file.path(here::here(), "outputs", "lub_senior_trips.png"),
    p, width = 14, height = 12, dpi = 150
  )

  # ── Sensor summary ─────────────────────────────────────────────────────────

  # Per-trip sensor averages, split by device
  sensor <- poseidat_trip_sensors(
    trips,
    vessel_name = "Lub Senior"
  )
  print(sensor)

  # Wide format for quick inspection
  sensor |>
    mutate(departure_date = as.Date(departure_time), arrival_date = as.Date(arrival_time)) |>
    select(trip_id, departure_harbour, departure_date, arrival_harbour, arrival_date, value_type, label, mean_value) |>
    tidyr::pivot_wider(
      names_from  = c(value_type, label),
      values_from = mean_value
    ) |>
    print(n = 20)

  # Without device split
  sensor_agg <- poseidat_trip_sensors(
    trips,
    vessel_name = "Lub Senior",
    by_device   = FALSE
  )

  sensor_agg |>
    tidyr::pivot_wider(
      id_cols     = c(trip_id, departure_time, arrival_time),
      names_from  = value_type,
      values_from = mean_value
    ) |>
    print(n = 20)
}
