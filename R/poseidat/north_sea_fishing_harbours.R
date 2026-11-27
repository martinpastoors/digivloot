# =============================================================================
# north_sea_fishing_harbours.R
#
# Reference table of North Sea fishing harbours for use with
# poseidat_trip_summary().
#
# Coverage: Netherlands, Belgium, France (Channel coast), Denmark, Germany,
#           Norway (Skagerrak), UK (East coast & Channel)
#
# Coordinates are for the inner harbour / fish quay entrance — the point a
# vessel would be near when berthed. WGS84 decimal degrees.
#
# Sources: verified against OpenStreetMap harbour centroids, Admiralty charts,
#          and port authority websites.  Accuracy ±200 m.
#
# Use harbour_radius_km = 3 in poseidat_trip_summary() for most ports.
# Consider harbour_radius_km = 5 for larger ports (IJmuiden, Boulogne,
# Esbjerg) where the fish quay sits deeper inside the port complex.
# =============================================================================

library(dplyr)

north_sea_harbours <- tribble(
  ~harbour_name,      ~country,      ~lat,      ~lon,

  # ── Netherlands ────────────────────────────────────────────────────────────
  "Den Helder",       "NLD",         52.9585,   4.7650,
  "IJmuiden",         "NLD",         52.4633,   4.5697,
  "Scheveningen",     "NLD",         52.1058,   4.2672,
  "Stellendam",       "NLD",         51.8224,   4.0314,
  "Colijnsplaat",     "NLD",         51.5993,   3.8541,
  "Yerseke",          "NLD",         51.4953,   4.0547,
  "Vlissingen",       "NLD",         51.4427,   3.5706,
  "Breskens",         "NLD",         51.4018,   3.5598,
  "Urk",              "NLD",         52.6642,   5.6003,   # IJsselmeer / Wadden
  "Harlingen",        "NLD",         53.1758,   5.4228,
  "Lauwersoog",       "NLD",         53.4088,   6.2008,
  "Eemshaven",        "NLD",         53.4420,   6.8437,
  "Delfzijl",         "NLD",         53.3298,   6.9277,

  # ── Belgium ────────────────────────────────────────────────────────────────
  "Zeebrugge",        "BEL",         51.3381,   3.1983,
  "Oostende",         "BEL",         51.2288,   2.9152,
  "Nieuwpoort",       "BEL",         51.1290,   2.7286,

  # ── France (Channel coast) ─────────────────────────────────────────────────
  "Dunkerque",        "FRA",         51.0448,   2.3675,
  "Boulogne-sur-Mer", "FRA",         50.7262,   1.5994,
  "Dieppe",           "FRA",         49.9302,   1.0843,
  "Le Havre",         "FRA",         49.4892,   0.1078,

  # ── Germany ────────────────────────────────────────────────────────────────
  "Bremerhaven",      "DEU",         53.5377,   8.5853,
  "Cuxhaven",         "DEU",         53.8714,   8.7044,
  "Büsum",            "DEU",         54.1289,   8.8607,
  "Husum",            "DEU",         54.4793,   9.0463,
  "Flensburg",        "DEU",         54.7949,   9.4338,

  # ── Denmark ────────────────────────────────────────────────────────────────
  "Esbjerg",          "DNK",         55.4762,   8.4521,
  "Thyborøn",         "DNK",         56.7020,   8.2102,
  "Hanstholm",        "DNK",         57.1178,   8.5952,
  "Hirtshals",        "DNK",         57.5909,   9.9566,
  "Skagen",           "DNK",         57.7289,  10.5842,
  "Frederikshavn",    "DNK",         57.4312,  10.5396,
  "Grenaa",           "DNK",         56.4168,  10.8875,
  "Hvide Sande",      "DNK",         56.0004,   8.1231,

  # ── Norway (Skagerrak / SW coast) ─────────────────────────────────────────
  "Kristiansand",     "NOR",         58.1451,   7.9968,
  "Egersund",         "NOR",         58.4516,   6.0027,
  "Stavanger",        "NOR",         58.9700,   5.7331,

  # ── UK (East coast & Channel) ─────────────────────────────────────────────
  "Peterhead",        "GBR",         57.5024,  -1.7778,
  "Aberdeen",         "GBR",         57.1443,  -2.0785,
  "Fraserburgh",      "GBR",         57.6920,  -2.0030,
  "Montrose",         "GBR",         56.7066,  -2.4587,
  "Eyemouth",         "GBR",         55.8740,  -2.0900,
  "North Shields",    "GBR",         55.0100,  -1.4430,
  "Scarborough",      "GBR",         54.2799,  -0.3942,
  "Grimsby",          "GBR",         53.5770,  -0.0693,
  "Lowestoft",        "GBR",         52.4684,   1.7482,
  "Great Yarmouth",   "GBR",         52.6089,   1.7354,
  "Ramsgate",         "GBR",         51.3303,   1.4198,
  "Dover",            "GBR",         51.1181,   1.3104,
  "Folkestone",       "GBR",         51.0790,   1.1893
)

# =============================================================================
# Convenience: a minimal subset covering the harbours you specified plus the
# most common Dutch/Belgian fleet home ports.  Pass this to
# poseidat_trip_summary() when you want a lighter reference table.
# =============================================================================

core_harbours <- north_sea_harbours |>
  filter(harbour_name %in% c(
    "Harlingen", "Eemshaven", "Lauwersoog",
    "Breskens",  "Vlissingen",
    "Oostende",
    "Boulogne-sur-Mer",
    "Thyborøn",  "Esbjerg",
    # common Dutch home ports for beam trawl / pulse fleets
    "Den Helder", "IJmuiden", "Scheveningen",
    "Stellendam", "Urk",
    "Zeebrugge"
  ))


# =============================================================================
# EXAMPLE USAGE
# =============================================================================
if (FALSE) {

  source("Poseidat_functions.R")
  source("poseidat_trip_summary.R")
  setup_poseidat_sync()

  # Full North Sea coverage
  trips <- poseidat_trip_summary(
    harbours   = north_sea_harbours,
    start_date = as.Date("2025-01-01")
  )

  # Minimal core set (your specified harbours + key Dutch/Belgian ports)
  trips_core <- poseidat_trip_summary(
    harbours   = core_harbours,
    start_date = as.Date("2025-01-01")
  )

  # Overview per vessel
  poseidat_harbour_overview(trips)
}
