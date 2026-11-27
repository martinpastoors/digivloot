# =============================================================================
# poseidat_atsea_or_in_harbour.R
#
# Derives a per-vessel trip overview (harbour departures & arrivals) from the
# 1-minute thinned GPS data produced by poseidat_thin() / poseidat_positions().
#
# Requires Poseidat_functions.R to be sourced first (for poseidat_positions()
# and the .onedrive() helper).
#
# Classification logic
#   A fix is labelled IN_HARBOUR when:
#     distance to nearest known harbour <= harbour_radius_km
#   All other fixes are AT_SEA.
#   Speed is deliberately NOT used: slow steaming, gear-setting, and anchoring
#   at sea would otherwise fragment real multi-day trips into short stints.
#
# Trip definition
#   A trip is a consecutive run of AT_SEA fixes bookended by a departure
#   harbour (the IN_HARBOUR run immediately before) and an arrival harbour
#   (the IN_HARBOUR run immediately after).
#   Trips shorter than min_trip_hours are dropped as artefacts (e.g. a vessel
#   briefly leaving and re-entering a harbour zone).
#
# Storage scheme reminder (from Poseidat_functions.R)
#   raw/       <source>/<vessel>/YYYY/MM/<vessel>_<device>_YYYYMM.parquet
#   processed/ thinned/<vessel>/<vessel>_thinned_1min_YYYYMMDD.parquet
#
# Dependencies (beyond those already loaded by Poseidat_functions.R)
#   geosphere   — vectorised haversine distances
#
# =============================================================================

library(dplyr)
library(lubridate)
library(geosphere)


# =============================================================================
# HARBOUR REFERENCE TABLE
# =============================================================================
# Coverage: Netherlands, Belgium, France (Channel coast), Denmark, Germany,
#           Norway (Skagerrak), UK (East coast & Channel).
#
# Coordinates are for the inner harbour / fish quay entrance — the point a
# vessel would be near when berthed.  WGS84 decimal degrees, accuracy ±200 m.
#
# Exported so callers can inspect or subset:
#   source("poseidat_atsea_or_in_harbour.R")
#   north_sea_harbours                        # full table
#   north_sea_harbours |> filter(country == "NLD")   # Netherlands only

north_sea_harbours <- tribble(
  ~harbour_name,      ~country,  ~lat,      ~lon,

  # ── Netherlands ─────────────────────────────────────────────────────────────
  "Den Helder",       "NLD",     52.9585,   4.7650,
  "IJmuiden",         "NLD",     52.4633,   4.5697,
  "Scheveningen",     "NLD",     52.1058,   4.2672,
  "Stellendam",       "NLD",     51.8224,   4.0314,
  "Colijnsplaat",     "NLD",     51.5993,   3.8541,
  "Yerseke",          "NLD",     51.4953,   4.0547,
  "Vlissingen",       "NLD",     51.4427,   3.5706,
  "Breskens",         "NLD",     51.4018,   3.5598,
  "Urk",              "NLD",     52.6642,   5.6003,
  "Harlingen",        "NLD",     53.1758,   5.4228,
  "Lauwersoog",       "NLD",     53.4088,   6.2008,
  "Eemshaven",        "NLD",     53.4420,   6.8437,
  "Delfzijl",         "NLD",     53.3298,   6.9277,

  # ── Belgium ──────────────────────────────────────────────────────────────────
  "Zeebrugge",        "BEL",     51.3381,   3.1983,
  "Oostende",         "BEL",     51.2288,   2.9152,
  "Nieuwpoort",       "BEL",     51.1290,   2.7286,

  # ── France (Channel coast) ───────────────────────────────────────────────────
  "Dunkerque",        "FRA",     51.0448,   2.3675,
  "Boulogne-sur-Mer", "FRA",     50.7262,   1.5994,
  "Dieppe",           "FRA",     49.9302,   1.0843,
  "Le Havre",         "FRA",     49.4892,   0.1078,

  # ── Germany ──────────────────────────────────────────────────────────────────
  "Bremerhaven",      "DEU",     53.5377,   8.5853,
  "Cuxhaven",         "DEU",     53.8714,   8.7044,
  "Büsum",            "DEU",     54.1289,   8.8607,
  "Husum",            "DEU",     54.4793,   9.0463,
  "Flensburg",        "DEU",     54.7949,   9.4338,

  # ── Denmark ──────────────────────────────────────────────────────────────────
  "Esbjerg",          "DNK",     55.4762,   8.4521,
  "Hvide Sande",      "DNK",     56.0004,   8.1231,
  "Thyborøn",         "DNK",     56.7020,   8.2102,
  "Hanstholm",        "DNK",     57.1178,   8.5952,
  "Hirtshals",        "DNK",     57.5909,   9.9566,
  "Skagen",           "DNK",     57.7289,  10.5842,
  "Frederikshavn",    "DNK",     57.4312,  10.5396,
  "Grenaa",           "DNK",     56.4168,  10.8875,

  # ── Norway (Skagerrak / SW coast) ────────────────────────────────────────────
  "Kristiansand",     "NOR",     58.1451,   7.9968,
  "Egersund",         "NOR",     58.4516,   6.0027,
  "Stavanger",        "NOR",     58.9700,   5.7331,

  # ── UK (East coast & Channel) ────────────────────────────────────────────────
  "Peterhead",        "GBR",     57.5024,  -1.7778,
  "Aberdeen",         "GBR",     57.1443,  -2.0785,
  "Fraserburgh",      "GBR",     57.6920,  -2.0030,
  "Montrose",         "GBR",     56.7066,  -2.4587,
  "Eyemouth",         "GBR",     55.8740,  -2.0900,
  "North Shields",    "GBR",     55.0100,  -1.4430,
  "Scarborough",      "GBR",     54.2799,  -0.3942,
  "Grimsby",          "GBR",     53.5770,  -0.0693,
  "Lowestoft",        "GBR",     52.4684,   1.7482,
  "Great Yarmouth",   "GBR",     52.6089,   1.7354,
  "Ramsgate",         "GBR",     51.3303,   1.4198,
  "Dover",            "GBR",     51.1181,   1.3104,
  "Folkestone",       "GBR",     51.0790,   1.1893
)


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# -----------------------------------------------------------------------------
# .nearest_harbour_km()
#
# Vectorised haversine pass over all fixes × all harbours.
# Returns pos_data with two new columns:
#   .dist_to_harbour_km  — distance in km to the nearest harbour
#   .nearest_harbour     — name of that harbour
# -----------------------------------------------------------------------------
.nearest_harbour_km <- function(pos_data, harbours) {

  ping_matrix    <- cbind(pos_data$longitude, pos_data$latitude)
  harbour_matrix <- cbind(harbours$lon,       harbours$lat)

  # n_pings × n_harbours distance matrix (metres)
  dist_m <- geosphere::distm(ping_matrix, harbour_matrix,
                              fun = geosphere::distHaversine)

  # Row-wise minimum in km — do.call(pmin) avoids the apply family
  dist_km     <- do.call(pmin, as.data.frame(dist_m)) / 1000

  # Index of nearest harbour per fix
  nearest_idx <- max.col(-dist_m, ties.method = "first")

  pos_data |>
    mutate(
      .dist_to_harbour_km = dist_km,
      .nearest_harbour    = harbours$harbour_name[nearest_idx]
    )
}


# -----------------------------------------------------------------------------
# .classify_fixes()
#
# Adds a .state column ("IN_HARBOUR" / "AT_SEA") based solely on proximity.
# -----------------------------------------------------------------------------
.classify_fixes <- function(pos_data, harbours, harbour_radius_km) {

  pos_data |>
    .nearest_harbour_km(harbours) |>
    mutate(
      .state = if_else(.dist_to_harbour_km <= harbour_radius_km,
                       "IN_HARBOUR", "AT_SEA")
    )
}


# =============================================================================
# MAIN FUNCTION — poseidat_trip_summary()
# =============================================================================

#' Per-vessel trip summary from Poseidat 1-minute GPS data
#'
#' Loads thinned POSITION fixes via \code{poseidat_positions()}, classifies
#' each fix as IN_HARBOUR or AT_SEA based on proximity to known harbours, and
#' collapses the result into one row per sea trip per vessel.
#'
#' Speed is not used for classification: slow steaming, trawling, and
#' anchoring at sea would otherwise fragment multi-day trips into many short
#' stints.
#'
#' @param harbours          data.frame / tibble with columns harbour_name (chr),
#'                          lat (dbl), lon (dbl).  Defaults to the built-in
#'                          \code{north_sea_harbours} table.
#' @param source            Passed to \code{poseidat_positions()}.
#'                          NULL = all sources.
#' @param vessel_name       Character vector of vessel names, or NULL for all.
#' @param start_date        Date or NULL. Filters fixes on or after this date.
#' @param end_date          Date or NULL. Filters fixes on or before this date.
#' @param harbour_radius_km A fix within this distance of any harbour is
#'                          classified IN_HARBOUR.  Default 3 km.  Increase to
#'                          5 km for large port complexes (IJmuiden, Esbjerg).
#' @param min_trip_hours    AT_SEA episodes shorter than this are dropped
#'                          (e.g. vessel briefly leaves then re-enters harbour
#'                          zone).  Default 12 hours.
#'
#' @return A tibble with one row per trip:
#'   vessel_name, trip_id, departure_harbour, departure_time,
#'   arrival_harbour, arrival_time, trip_duration_hours,
#'   n_fixes_at_sea, mean_sog_knots.
#'
#' @examples
#' \dontrun{
#' source(file.path(here::here(), "R/poseidat/Poseidat_functions.R"))
#' setup_poseidat_sync()
#'
#' # Use built-in harbour table, all vessels
#' trips <- poseidat_trip_summary(start_date = as.Date("2025-01-01"))
#'
#' # Single vessel
#' trips <- poseidat_trip_summary(
#'   vessel_name = "Lub Senior",
#'   start_date  = as.Date("2025-01-01")
#' )
#'
#' # Widen harbour zone for large ports
#' trips <- poseidat_trip_summary(
#'   harbour_radius_km = 5,
#'   start_date        = as.Date("2025-01-01")
#' )
#'
#' overview <- poseidat_harbour_overview(trips)
#' print(overview)
#' }
poseidat_trip_summary <- function(harbours          = north_sea_harbours,
                                  source            = NULL,
                                  vessel_name       = NULL,
                                  start_date        = NULL,
                                  end_date          = NULL,
                                  harbour_radius_km = 3,
                                  min_trip_hours    = 12) {

  # ── 0. Validate harbour table ──────────────────────────────────────────────
  required_hrb <- c("harbour_name", "lat", "lon")
  missing_hrb  <- setdiff(required_hrb, names(harbours))
  if (length(missing_hrb) > 0)
    stop("harbours is missing columns: ", paste(missing_hrb, collapse = ", "))
  if (nrow(harbours) == 0)
    stop("`harbours` is empty — supply at least one harbour location.")


  # ── 1. Load 1-minute thinned GPS fixes ────────────────────────────────────
  message("Loading thinned position data ...")

  pos <- poseidat_positions(
    source      = source,
    vessel_name = vessel_name,
    data_source = "thinned",
    start_date  = start_date,
    end_date    = end_date
  )

  if (nrow(pos) == 0) {
    message("No position data found — returning empty tibble.")
    return(tibble(
      vessel_name         = character(),
      trip_id             = integer(),
      departure_harbour   = character(),
      departure_time      = as.POSIXct(NA),
      arrival_harbour     = character(),
      arrival_time        = as.POSIXct(NA),
      trip_duration_hours = numeric(),
      n_fixes_at_sea      = integer(),
      mean_sog_knots      = numeric()
    ))
  }

  message(sprintf("  %s fixes loaded for %d vessel(s).",
                  format(nrow(pos), big.mark = ","),
                  n_distinct(pos$vessel_name)))


  # ── 2. Classify every fix as IN_HARBOUR / AT_SEA ──────────────────────────
  message("Classifying fixes by harbour proximity ...")

  classified <- pos |>
    arrange(vessel_name, datetime) |>
    .classify_fixes(harbours, harbour_radius_km)

  pct_harbour <- mean(classified$.state == "IN_HARBOUR") * 100
  message(sprintf("  %.1f%% of fixes classified as IN_HARBOUR (radius <= %.1f km).",
                  pct_harbour, harbour_radius_km))


  # ── 3. Run-length encode state per vessel ─────────────────────────────────
  #   Consecutive fixes with the same .state → same .run_id.
  #   cumsum() of the change flag increments the run ID at every state flip.
  #   Grouped by vessel so run IDs reset per vessel.
  with_runs <- classified |>
    group_by(vessel_name) |>
    mutate(
      .state_change = .state != lag(.state, default = first(.state)),
      .run_id       = cumsum(.state_change)
    ) |>
    ungroup()


  # ── 4. Collapse runs into one row per episode ──────────────────────────────
  episodes <- with_runs |>
    group_by(vessel_name, .run_id, .state) |>
    summarise(
      start_time    = min(datetime),
      end_time      = max(datetime),
      n_fixes       = n(),
      mean_sog      = mean(speed_over_ground, na.rm = TRUE),
      .harbour_name = first(.nearest_harbour),
      .groups       = "drop"
    ) |>
    arrange(vessel_name, start_time) |>
    mutate(
      # harbour name is only meaningful for IN_HARBOUR episodes
      .harbour_name = if_else(.state == "IN_HARBOUR", .harbour_name, NA_character_)
    )


  # ── 5. Pair each AT_SEA episode with its adjacent harbour episodes ─────────
  #   join_by() does not support arithmetic on join keys, so we pre-compute
  #   the shifted run IDs in the lookup tables before joining.
  #
  #   departure harbour: the IN_HARBOUR run at (.run_id + 1) == this AT_SEA run
  #   arrival   harbour: the IN_HARBOUR run at (.run_id - 1) == this AT_SEA run

  at_sea <- episodes |>
    filter(.state == "AT_SEA") |>
    select(vessel_name, .run_id,
           departure_time = start_time,
           arrival_time   = end_time,
           n_fixes_at_sea = n_fixes,
           mean_sog_knots = mean_sog)

  harbour_lookup <- episodes |>
    filter(.state == "IN_HARBOUR") |>
    select(vessel_name, .h_run_id = .run_id, .harbour_name)

  departure_lookup <- harbour_lookup |>
    mutate(.next_run_id = .h_run_id + 1L) |>
    select(vessel_name, .next_run_id, departure_harbour = .harbour_name)

  arrival_lookup <- harbour_lookup |>
    mutate(.prev_run_id = .h_run_id - 1L) |>
    select(vessel_name, .prev_run_id, arrival_harbour = .harbour_name)

  trips <- at_sea |>
    left_join(departure_lookup,
              by = c("vessel_name", ".run_id" = ".next_run_id")) |>
    left_join(arrival_lookup,
              by = c("vessel_name", ".run_id" = ".prev_run_id"))


  # ── 6. Duration and noise filter ──────────────────────────────────────────
  trips <- trips |>
    mutate(
      trip_duration_hours = as.numeric(
        difftime(arrival_time, departure_time, units = "hours")
      )
    ) |>
    filter(trip_duration_hours >= min_trip_hours)


  # ── 7. Sequential trip ID per vessel, tidy column order ───────────────────
  trips <- trips |>
    group_by(vessel_name) |>
    mutate(trip_id = row_number()) |>
    ungroup() |>
    select(
      vessel_name,
      trip_id,
      departure_harbour,
      departure_time,
      arrival_harbour,
      arrival_time,
      trip_duration_hours,
      n_fixes_at_sea,
      mean_sog_knots
    ) |>
    arrange(vessel_name, departure_time)

  message(sprintf("Done. %d trip(s) found across %d vessel(s).",
                  nrow(trips), n_distinct(trips$vessel_name)))

  trips
}


# =============================================================================
# CONVENIENCE WRAPPER — poseidat_harbour_overview()
# =============================================================================

#' Summarise time in harbour and at sea per vessel
#'
#' Collapses the trip-level output of \code{poseidat_trip_summary()} into one
#' summary row per vessel.
#'
#' @param trips Tibble returned by \code{poseidat_trip_summary()}.
#' @return A tibble with one row per vessel.
poseidat_harbour_overview <- function(trips) {

  if (nrow(trips) == 0) {
    message("No trips to summarise.")
    return(tibble())
  }

  trips |>
    group_by(vessel_name) |>
    summarise(
      n_trips           = n(),
      total_sea_hours   = sum(trip_duration_hours,    na.rm = TRUE),
      mean_trip_hours   = mean(trip_duration_hours,   na.rm = TRUE),
      median_trip_hours = median(trip_duration_hours, na.rm = TRUE),
      first_departure   = min(departure_time,         na.rm = TRUE),
      last_arrival      = max(arrival_time,           na.rm = TRUE),
      .groups           = "drop"
    ) |>
    mutate(
      observation_days = as.numeric(
        difftime(last_arrival, first_departure, units = "days")
      ),
      pct_time_at_sea  = round(
        total_sea_hours / (observation_days * 24) * 100, 1
      )
    ) |>
    select(
      vessel_name,
      n_trips,
      total_sea_hours,
      mean_trip_hours,
      median_trip_hours,
      observation_days,
      pct_time_at_sea,
      first_departure,
      last_arrival
    ) |>
    arrange(vessel_name)
}


# =============================================================================
# EXAMPLE USAGE
# =============================================================================
if (FALSE) {

  source(file.path(here::here(), "R/poseidat/Poseidat_functions.R"))
  setup_poseidat_sync()

  # Default: built-in harbour table, 3 km radius, trips >= 12 h
  trips <- poseidat_trip_summary(
    start_date = as.Date("2025-01-01"),
    end_date   = Sys.Date()
  )

  # Single vessel
  trips_lub <- poseidat_trip_summary(
    vessel_name = "Lub Senior",
    start_date  = as.Date("2025-01-01")
  )

  # Larger harbour zone (good for IJmuiden / Esbjerg)
  trips_wide <- poseidat_trip_summary(
    harbour_radius_km = 5,
    start_date        = as.Date("2025-01-01")
  )

  # Fleet overview
  overview <- poseidat_harbour_overview(trips)
  print(overview)

  # Inspect one vessel
  trips_lub |>
    filter(vessel_name == "Lub Senior") |>
    select(trip_id, departure_harbour, departure_time,
           arrival_harbour,  arrival_time, trip_duration_hours) |>
    print(n = 40)

  # Departure port frequencies
  trips |> count(vessel_name, departure_harbour, sort = TRUE)
  

}
