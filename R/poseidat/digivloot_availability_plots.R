# ============================================================================
# DIGIvloot -- Data availability / quality / spatial visuals for the
# deelnemersbrief
#
# Three self-contained pieces, each with a data-building function and a
# matching plot_*() function:
#
#   1. fleet_weekly_availability()  + plot_weekly_availability()
#      -- per vessel/sensor/week: real data vs. zero/near-zero vs. NA vs.
#         nothing received at all
#   2. fleet_fuel_quality()         + plot_fuel_quality()
#      -- % zero / % outlier / % days-covered for FUEL_CONSUMPTION, per vessel
#   3. fleet_spatial_distribution() + plot_spatial_distribution()
#      -- where the fleet has been fishing (North Sea / Channel)
#
# DEPENDENCIES: this file assumes the SAME session has already sourced
# Poseidat_functions.R and the helper functions defined in the vessel
# overview Rmd's `setup`/`helpers` chunks -- specifically:
#   .poseidat_load_report_raw(), extra_pivoted_channels(), pivot_col(),
#   .bounds_for(), .tukey_bounds(), NO_OUTLIER_TYPES, .poseidat_device_labels()
# If you're running this standalone (not via the Rmd), pull those chunks
# into their own helpers.R and source() it before this file.
#
# Style: dplyr pipes throughout; per-vessel work uses an explicit for loop
# (same pattern the Rmd already uses in fleet_sensor_daily()) rather than
# apply()/sapply() or purrr::map(), so each vessel's raw data is loaded,
# summarised and discarded in turn and peak memory stays bounded. No
# purrr::map() is used anywhere here -- nothing in these three pieces reads
# enough files per call to make the ~parallel-iteration benefit worth the
# extra dependency.
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================================
# 1. Weekly data availability per vessel x sensor
# ============================================================================

# Same pivot idea as the Rmd's own pivot_col(), but deliberately does NOT
# drop NA rows -- pivot_col() filters !is.na(.data[[col]]) because it's
# meant to produce clean readings for the table/trend/histogram code. Here
# we want the opposite: a device that reported a row with an NA reading is
# a different situation from a device that reported nothing at all that
# week, and we can only tell them apart if the NA row survives to be
# counted. Kept private to this file (dot-prefixed, distinct name) so it
# never shadows pivot_col() used elsewhere.
.weekly_pivot_keep_na <- function(df, col, new_type) {
  if (nrow(df) == 0 || !col %in% names(df)) return(tibble())
  shared <- intersect(c("datetime", "device_id"), names(df))
  df |>
    select(all_of(shared), value_numeric = all_of(col)) |>
    mutate(value_type = new_type)
}

# "Close to zero" is sensor-specific -- a fuel reading of 0.3 L/h is
# plausibly idling, a depth of 0.3 m is not a plausible trawl depth. These
# are starting points, not measured thresholds; override via the
# `zero_eps` argument (a named list, same names as the `sensor` column
# below) if a sensor's real working range makes a different cutoff more
# sensible. Any sensor not named here falls back to eps = 0, i.e. only an
# exact 0 counts as "zero" for it.
DEFAULT_ZERO_EPS <- list(
  Brandstof  = 0.01,
  Trekkracht = 1,
  Diepte     = 0.5,
  Weegschaal = 1,
  GPS        = 0.001
)

# Per (vessel, sensor, ISO week), counts how many readings were real data,
# how many were zero/near-zero, how many were present-but-NA, and the raw
# row count -- instead of the earlier single "any data at all" count. This
# needs the full RAW archive, not the thinned one: an NA reading is exactly
# the kind of row thinning may legitimately drop, so classifying NA
# reliably means reading raw. Sensor grouping mirrors assign_row_group() in
# the Rmd (Fuel / Tow / Depth / Scale / GPS), so the picture lines up with
# how sensors are already described elsewhere in the report.
#
# `include_scale`: SCALE is deliberately excluded from thinning in the Rmd,
# but that doesn't affect this function (it already reads raw for
# everything) -- include_scale here just controls whether the Weegschaal
# row is computed at all, in case you want a faster run without it.
#
# `sensors` / `exclude_sensors`: which sensor groups end up in the result
# (and so in the figure). Names are the labels in the `sensor` column --
# Brandstof, Trekkracht, Diepte, Weegschaal, GPS, plus any other
# value_type under its own raw name (e.g. "POWER", "PRESSURE").
#   - `sensors` = DEFAULT_SENSORS (the five above): anything else, such as
#     POWER or PRESSURE, is left out. Add one to the vector to bring it
#     back, e.g. sensors = c(DEFAULT_SENSORS, "POWER"). Pass NULL to keep
#     every sensor that is found.
#   - `exclude_sensors`: dropped after `sensors` is applied -- handy with
#     sensors = NULL ("everything except ...").
DEFAULT_SENSORS <- c("Brandstof", "Trekkracht", "Diepte", "Weegschaal", "GPS")

fleet_weekly_availability <- function(vessels, start_date = NULL, end_date = NULL,
                                       include_scale = TRUE,
                                       zero_eps = DEFAULT_ZERO_EPS,
                                       sensors = DEFAULT_SENSORS,
                                       exclude_sensors = NULL) {

  eps_tbl <- tibble(sensor = names(zero_eps), eps = unlist(zero_eps, use.names = FALSE))

  weekly_list <- vector("list", length(vessels))

  for (v in seq_along(vessels)) {
    vessel <- vessels[v]
    raw_v  <- .poseidat_load_report_raw(vessel, start_date, end_date)
    if (nrow(raw_v) == 0) { weekly_list[[v]] <- tibble(); next }

    # No !is.na(value_numeric) filter here (unlike the Rmd's own scalar
    # filters) -- NA rows are kept on purpose so they can be classified as
    # "NA-waarden" below instead of silently disappearing before they're
    # ever counted.
    scalar_all <- raw_v |>
      filter(!value_type %in% c("POSITION", "TRAWL_TENSION", "SCALE"),
             !grepl("TEMP", value_type, ignore.case = TRUE)) |>
      select(device_id, value_type, datetime, value_numeric)

    trawl <- filter(raw_v, value_type == "TRAWL_TENSION")
    pos   <- filter(raw_v, value_type == "POSITION")

    pivoted_all <- bind_rows(
      .weekly_pivot_keep_na(trawl, "traction_port",      "Traction port"),
      .weekly_pivot_keep_na(trawl, "traction_center",    "Traction center"),
      .weekly_pivot_keep_na(trawl, "traction_starboard", "Traction starboard"),
      .weekly_pivot_keep_na(pos,   "latitude",           "Latitude"),
      .weekly_pivot_keep_na(pos,   "longitude",          "Longitude")
    )

    if (isTRUE(include_scale)) {
      scale_v <- filter(raw_v, value_type == "SCALE")
      pivoted_all <- bind_rows(pivoted_all, .weekly_pivot_keep_na(scale_v, "scale_weight", "SCALE"))
      rm(scale_v)
    }

    combined <- bind_rows(scalar_all, pivoted_all)
    rm(scalar_all, pivoted_all, trawl, pos)

    weekly_list[[v]] <- combined |>
      mutate(
        sensor = dplyr::case_when(
          value_type == "FUEL_CONSUMPTION" ~ "Brandstof",
          value_type %in% c("Traction port", "Traction center", "Traction starboard") ~ "Trekkracht",
          value_type == "DEPTH" ~ "Diepte",
          value_type == "SCALE" ~ "Weegschaal",
          value_type %in% c("Latitude", "Longitude") ~ "GPS",
          TRUE ~ value_type
        ),
        # Monday-anchored week bucket, same idea as base R's cut.Date("week")
        week_start = as.Date(cut(as.Date(datetime), "week"))
      ) |>
      filter(is.null(sensors) | sensor %in% sensors,
             !sensor %in% exclude_sensors) |>
      left_join(eps_tbl, by = "sensor") |>
      mutate(eps = tidyr::replace_na(eps, 0)) |>
      group_by(sensor, week_start) |>
      summarise(
        n_rows = dplyr::n(),
        n_na   = sum(is.na(value_numeric)),
        n_zero = sum(!is.na(value_numeric) & abs(value_numeric) <= eps),
        .groups = "drop"
      ) |>
      mutate(n_real = n_rows - n_na - n_zero,
             vessel = vessel)

    rm(raw_v, combined)
    gc(FALSE)
  }

  bind_rows(weekly_list)
}

# Heatmap: vessel (y) x week (x), faceted by sensor, with FOUR states per
# tile instead of the earlier binary data/no-data:
#   "Data ontvangen" -- at least one real (non-zero, non-NA) reading
#   "Nul/bijna-nul"  -- readings arrived but all were zero/near-zero
#   "NA-waarden"     -- readings arrived but all were NA
#   "Geen data"      -- nothing was received at all that week
# Priority order when a week mixes categories (e.g. mostly real readings
# with one NA row) is real > zero > NA > nothing -- a single real reading
# in a week is read as "this sensor worked that week", which is what
# matters for a letter to schippers; the quality drill-down (plot 2) is
# where partial problems get surfaced in detail.
#
# Missing (vessel, sensor, week) combinations -- i.e. genuinely no rows at
# all -- are filled in explicitly via expand_grid() so a full gap in
# delivery still shows up as "Geen data" rather than silently not
# appearing.
#
# Colours: blue / amber / red / light grey, chosen and validated with the
# dataviz skill's palette validator (`validate_palette.js`) for this exact
# four-way use -- CVD-safe adjacent and all-pairs, "Geen data" intentionally
# low-chroma grey since it represents absence, not a fourth category of
# reading. The amber tile is sub-3:1 contrast by design (per the skill's
# status-colour notes), which is why the legend stays on and is never
# optional for this plot.
plot_weekly_availability <- function(weekly) {
  if (nrow(weekly) == 0) return(NULL)

  all_weeks <- seq(min(weekly$week_start), max(weekly$week_start), by = "1 week")

  full_grid <- tidyr::expand_grid(
    vessel     = unique(weekly$vessel),
    sensor     = unique(weekly$sensor),
    week_start = all_weeks
  )

  plot_data <- full_grid |>
    left_join(weekly, by = c("vessel", "sensor", "week_start")) |>
    mutate(
      n_rows = tidyr::replace_na(n_rows, 0),
      n_na   = tidyr::replace_na(n_na, 0),
      n_zero = tidyr::replace_na(n_zero, 0),
      n_real = tidyr::replace_na(n_real, 0),
      status = dplyr::case_when(
        n_real > 0 ~ "Data ontvangen",
        n_zero > 0 ~ "Nul/bijna-nul",
        n_na   > 0 ~ "NA-waarden",
        TRUE       ~ "Geen data"
      ),
      status = factor(status, levels = c("Geen data", "NA-waarden", "Nul/bijna-nul", "Data ontvangen"))
    )

  status_colours <- c(
    "Geen data"     = "#e1e0d9",
    "NA-waarden"    = "#e34948",
    "Nul/bijna-nul" = "#eda100",
    "Data ontvangen" = "#2a78d6"
  )

  ggplot(plot_data, aes(x = week_start, y = vessel, fill = status)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    facet_wrap(~ sensor, ncol = 2) +
    scale_fill_manual(values = status_colours, name = NULL, drop = FALSE) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title    = "Databeschikbaarheid per schip per week",
         subtitle = "Echte data vs. nul/bijna-nul vs. NA-waarden vs. niets ontvangen",
         x = NULL, y = NULL) +
    theme_bw() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(face = "bold", size = 8),
      plot.title      = element_text(face = "bold"),
      legend.position = "bottom"
    )
}


# ============================================================================
# 2. Fuel data quality per vessel
# ============================================================================

# Three simple, non-technical quality metrics per vessel for
# FUEL_CONSUMPTION: % readings that are exactly zero (likely dropout, not a
# real reading), % readings flagged as outliers under the report's own
# Tukey's-fences rule (.bounds_for(), same outlier_k as the rest of the
# report), and % of the vessel's own date span that actually has at least
# one fuel reading.
fleet_fuel_quality <- function(vessels, start_date = NULL, end_date = NULL, outlier_k = 3) {

  quality_list <- vector("list", length(vessels))

  for (v in seq_along(vessels)) {
    vessel <- vessels[v]
    raw_v  <- .poseidat_load_report_raw(vessel, start_date, end_date)
    if (nrow(raw_v) == 0) { quality_list[[v]] <- tibble(); next }

    fuel <- raw_v |>
      filter(value_type == "FUEL_CONSUMPTION", !is.na(value_numeric))

    if (nrow(fuel) == 0) {
      quality_list[[v]] <- tibble()
      rm(raw_v, fuel)
      next
    }

    b <- .bounds_for("FUEL_CONSUMPTION", fuel$value_numeric, outlier_k)

    n_total    <- nrow(fuel)
    n_zero     <- sum(fuel$value_numeric == 0, na.rm = TRUE)
    n_outlier  <- sum(fuel$value_numeric < b["lower"] | fuel$value_numeric > b["upper"], na.rm = TRUE)
    n_days_obs <- length(unique(as.Date(fuel$datetime)))
    span_days  <- as.integer(max(as.Date(fuel$datetime)) - min(as.Date(fuel$datetime))) + 1L

    quality_list[[v]] <- tibble(
      vessel             = vessel,
      n_raw              = n_total,
      pct_zero           = 100 * n_zero / n_total,
      pct_outlier        = 100 * n_outlier / n_total,
      pct_days_with_data = 100 * n_days_obs / span_days
    )

    rm(raw_v, fuel)
    gc(FALSE)
  }

  bind_rows(quality_list)
}

# Horizontal grouped bar chart, one bar-group per vessel, one bar per
# metric -- deliberately simple (no facets, no jargon) since this is meant
# to go straight into a letter to schippers, not a technical appendix.
plot_fuel_quality <- function(quality) {
  if (nrow(quality) == 0) return(NULL)

  plot_data <- quality |>
    select(vessel, pct_zero, pct_outlier, pct_days_with_data) |>
    tidyr::pivot_longer(-vessel, names_to = "metric", values_to = "pct") |>
    mutate(metric = dplyr::recode(metric,
      pct_zero           = "% nulwaarden",
      pct_outlier        = "% uitschieters",
      pct_days_with_data = "% dagen met data"
    ))

  ggplot(plot_data, aes(x = vessel, y = pct, fill = metric)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_y_continuous(labels = \(x) paste0(x, "%")) +
    labs(title = "Kwaliteit brandstofdata per schip",
         x = NULL, y = NULL, fill = NULL) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
}


# ============================================================================
# 3. Spatial distribution of the fleet (North Sea / Channel)
# ============================================================================

# Pulls POSITION readings only (pivoted via pivot_col(), same helper the
# Rmd already uses for Latitude/Longitude), restricts to a plausible
# North Sea / Channel bounding box to drop GPS glitches, and optionally
# subsamples per vessel so the plot stays light even over a long history.
fleet_spatial_distribution <- function(vessels, start_date = NULL, end_date = NULL,
                                        sample_n = 20000) {

  pos_list <- vector("list", length(vessels))

  # Roughly: southern Channel to northern North Sea, French/Belgian coast
  # to Danish coast. Adjust if the fleet also fishes outside this box.
  lat_range <- c(48, 62)
  lon_range <- c(-6, 10)

  for (v in seq_along(vessels)) {
    vessel <- vessels[v]
    raw_v  <- .poseidat_load_report_raw(vessel, start_date, end_date)
    if (nrow(raw_v) == 0) { pos_list[[v]] <- tibble(); next }

    pos <- filter(raw_v, value_type == "POSITION")
    lat <- pivot_col(pos, "latitude",  "Latitude")  |> select(datetime, latitude  = value_numeric)
    lon <- pivot_col(pos, "longitude", "Longitude") |> select(datetime, longitude = value_numeric)

    coords <- lat |>
      inner_join(lon, by = "datetime") |>
      filter(
        latitude  > lat_range[1], latitude  < lat_range[2],
        longitude > lon_range[1], longitude < lon_range[2]
      )

    if (nrow(coords) > sample_n) {
      coords <- coords[sample(nrow(coords), sample_n), ]
    }

    pos_list[[v]] <- mutate(coords, vessel = vessel)
    rm(raw_v, pos, lat, lon, coords)
    gc(FALSE)
  }

  bind_rows(pos_list)
}

# Points coloured by vessel over a simple coastline (needs the `maps`
# package for ggplot2::map_data(); swap in sf/rnaturalearth for a nicer
# coastline if that's already part of the toolchain elsewhere).
#
# `facet_by`: "none" (default, one map), "year_quarter" (one panel per
# year + quarter, e.g. "2026 Q1"), "year", or "quarter" (Q1-Q4 pooled over
# years). Uses the `datetime` column that fleet_spatial_distribution()
# already returns. Panels follow chronological order; `facet_ncol` sets the
# number of panel columns.
#
# `eez`: optional EEZ boundaries, drawn as dashed lines. One of:
#   - NULL / FALSE (default): no EEZ lines
#   - TRUE: download the EEZ polygons for the map area from Marine Regions
#     via the `mregions2` package (needs internet; fetched once per call --
#     for repeated runs fetch once with load_eez() and pass the result in)
#   - a path to a shapefile / GeoPackage / GeoJSON (e.g. a Flanders Marine
#     Institute "World EEZ" download), or an `sf` object you already have
# Needs the `sf` package whenever eez is used. The map then switches from
# coord_quickmap() to coord_sf(), the only coord that can combine with sf
# layers; the look stays the same.
#
# (load_eez() below is the helper behind `eez`; plot_spatial_distribution()
# itself follows right after it.)
load_eez <- function(eez = TRUE, xlim = c(-6, 10), ylim = c(48, 62)) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Install sf: install.packages('sf')")

  if (inherits(eez, "sf")) {
    out <- eez
  } else if (is.character(eez) && length(eez) == 1) {
    out <- sf::st_read(eez, quiet = TRUE)
  } else if (isTRUE(eez)) {
    if (!requireNamespace("mregions2", quietly = TRUE))
      stop("Install mregions2 (install.packages('mregions2')) or pass a shapefile path / sf object as `eez`")

    # WFS request limited to the map's bounding box. The Marine Regions
    # geoserver can interpret EPSG:4326 boxes as lat/lon instead of
    # lon/lat, so if the lon/lat box returns nothing the lat/lon version
    # is tried too. Errors are swallowed per attempt so the second can run.
    fetch <- function(cql) {
      tryCatch(mregions2::mrp_get("eez", cql_filter = cql), error = function(e) NULL)
    }
    out <- fetch(sprintf("BBOX(the_geom, %s, %s, %s, %s)", xlim[1], ylim[1], xlim[2], ylim[2]))
    if (is.null(out) || nrow(out) == 0)
      out <- fetch(sprintf("BBOX(the_geom, %s, %s, %s, %s)", ylim[1], xlim[1], ylim[2], xlim[2]))
    if (is.null(out) || nrow(out) == 0)
      stop("No EEZ polygons returned by Marine Regions for this area. ",
           "Download the EEZ shapefile and pass its path as `eez` instead.")
  } else {
    stop("`eez` must be NULL/FALSE, TRUE, a file path, or an sf object")
  }

  if (is.na(sf::st_crs(out))) sf::st_crs(out) <- 4326

  # Does the layer actually fall inside the map window? If x/y came back
  # swapped (lat/lon order), the polygons sit far outside the map and
  # nothing is drawn -- swap them back in that case.
  overlaps <- function(x) {
    bb <- sf::st_bbox(sf::st_transform(x, 4326))
    isTRUE(unname(bb["xmax"] >= xlim[1] & bb["xmin"] <= xlim[2] &
                  bb["ymax"] >= ylim[1] & bb["ymin"] <= ylim[2]))
  }
  out <- sf::st_transform(out, 4326)

  if (!overlaps(out)) {
    swapped <- sf::st_geometry(out) * matrix(c(0, 1, 1, 0), 2, 2)
    sf::st_crs(swapped) <- 4326
    candidate <- out
    sf::st_geometry(candidate) <- swapped
    if (overlaps(candidate)) {
      out <- candidate
      message("EEZ: x/y coordinates were swapped; corrected.")
    } else {
      warning("EEZ layer does not overlap the map area (", paste(xlim, collapse = " to "),
              " E, ", paste(ylim, collapse = " to "), " N); nothing will be visible.")
    }
  }

  message(sprintf("EEZ: %d feature(s) loaded.", nrow(out)))
  out
}

plot_spatial_distribution <- function(positions,
                                       facet_by   = c("none", "year_quarter", "year", "quarter"),
                                       facet_ncol = 2,
                                       eez        = NULL) {
  if (nrow(positions) == 0) return(NULL)
  facet_by <- match.arg(facet_by)

  xlim <- c(-6, 10)
  ylim <- c(48, 62)

  coastline <- ggplot2::map_data("world", xlim = xlim, ylim = ylim)

  # Panel label built from the position timestamps; factor levels in
  # chronological order so panels read left-to-right, top-to-bottom in time
  if (facet_by != "none") {
    positions <- positions |>
      mutate(
        year    = format(datetime, "%Y"),
        quarter = paste0("Q", ceiling(as.integer(format(datetime, "%m")) / 3)),
        # facet_by is a single value, so a plain if/else (not case_when(),
        # which warns on scalar conditions in dplyr >= 1.2.0)
        panel   = if (facet_by == "year_quarter") paste(year, quarter)
                  else if (facet_by == "year") year
                  else quarter
      ) |>
      arrange(panel) |>
      mutate(panel = factor(panel, levels = unique(panel)))
  }

  use_eez <- !is.null(eez) && !isFALSE(eez)
  if (use_eez) eez_sf <- load_eez(eez, xlim, ylim)

  p <- ggplot() +
    geom_polygon(data = coastline, aes(x = long, y = lat, group = group),
                 fill = "grey85", colour = "grey60", linewidth = 0.2)

  if (use_eez) {
    p <- p +
      geom_sf(data = eez_sf, fill = NA, colour = "#2a78d6",
              linewidth = 0.4, linetype = "dashed", inherit.aes = FALSE)
  }

  p <- p +
    geom_point(data = positions, aes(x = longitude, y = latitude, colour = vessel),
               alpha = 0.15, size = 0.3)

  # geom_sf() layers can only be combined with coord_sf(); without EEZ the
  # original coord_quickmap() is kept
  p <- p +
    (if (use_eez) coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
     else coord_quickmap(xlim = xlim, ylim = ylim))

  if (facet_by != "none") p <- p + facet_wrap(~ panel, ncol = facet_ncol)

  p +
    labs(title    = "Ruimtelijke spreiding deelnemende schepen",
         subtitle = if (use_eez) "Noordzee en Kanaal, met EEZ-grenzen (gestippeld)" else "Noordzee en Kanaal",
         x = NULL, y = NULL, colour = NULL) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold")) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))
}


# ============================================================================
# Example usage (run interactively, not sourced automatically)
# ============================================================================
#
source(file.path(here::here(), "R/poseidat/Poseidat_functions.R")); setup_poseidat_sync()
source(file.path(here::here(), "R/poseidat/helpers.R"))   # the Rmd's setup/helpers chunks, extracted
# source("digivloot_availability_plots.R")

all_vt  <- poseidat_value_types()
vessels <- sort(unique(all_vt$vessel_name))

# One start date for all three figures (NULL = full history; end_date can
# be set the same way). lubridate::dmy() reads "1/3/2026" as 1 March 2026.
start_date <- lubridate::dmy("1/3/2026")
end_date   <- NULL

weekly <- fleet_weekly_availability(vessels, start_date = start_date, end_date = end_date)
ggsave("databeschikbaarheid.png", plot_weekly_availability(weekly),
       width = 9, height = 6, dpi = 200)

quality <- fleet_fuel_quality(vessels, start_date = start_date, end_date = end_date)
ggsave("brandstofkwaliteit.png", plot_fuel_quality(quality),
       width = 7, height = 5, dpi = 200)

positions <- fleet_spatial_distribution(vessels, start_date = start_date, end_date = end_date)

# EEZ fetched once and reused, so repeated plotting doesn't re-download it
eez_sf <- load_eez("C:/WORK/4_Data/GIS/World_EEZ_v12_20231025/eez_v12.shp")
p_spatial <- plot_spatial_distribution(positions, facet_by = "quarter", eez = eez_sf, facet_ncol=4)
p_spatial   # check on screen first

ggsave("ruimtelijke_spreiding.png", p_spatial,
       width = 8, height = 9, dpi = 200)
