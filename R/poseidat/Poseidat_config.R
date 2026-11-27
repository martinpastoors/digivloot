# ============================================================================
# Poseidat_config.R
#
# Run once to initialise, or re-run to update vessel/device configuration.
# Saves vessel_config as an RDS file to OneDrive for use by all other scripts.
#
# Supported sources:
#   "dbmatic" — DBMatic API (test / sandbox data)
#   "efice"   — EFICE API  (production, EFICE-registered vessels)
#   "pefa"    — PEFA API   (production, PEFA-registered vessels)
#
# .Renviron entries required (usethis::edit_r_environ()):
#   DBMATIC_API_URL   = https://api.dbmatic.eu/api/v1
#   DBMATIC_TOKEN     = your-dbmatic-bearer-token
#   EFICE_API_URL     = https://poseidat-journal.m-catch.com
#   EFICE_TOKEN       = your-efice-bearer-token
#   PEFA_API_URL      = https://...   (fill once confirmed)
#   PEFA_TOKEN        = your-pefa-bearer-token
#   ONEDRIVE_POSEIDAT = C:/Users/<you>/OneDrive/Poseidat
# ============================================================================

library(dplyr)

# ----------------------------------------------------------------------------
# OneDrive root — read from .Renviron
# ----------------------------------------------------------------------------
onedrive_base <- Sys.getenv("ONEDRIVE_POSEIDAT", unset = "")
if (!nzchar(onedrive_base)) {
  stop(
    "ONEDRIVE_POSEIDAT is not set.\n",
    "Add it to your .Renviron (usethis::edit_r_environ()):\n",
    "  ONEDRIVE_POSEIDAT = C:/Users/<you>/OneDrive/Poseidat"
  )
}

# ----------------------------------------------------------------------------
# Vessel configuration
#
# source        : "dbmatic" | "efice" | "pefa"
# journal_id    : Vessel identifier in the respective API
# vessel_name   : Used as folder name — keep consistent across runs
# cfr           : Community Fleet Register number
# enabled       : FALSE to skip during sync without deleting the row
#
# device_labels : Named list mapping device UUID -> friendly label.
#   Reference only — does NOT restrict which devices are synced.
#   Devices are always discovered live from the API.
#   Labels should be short uppercase strings matching Poseidat value_types
#   where possible: POSITION, FUEL_1..4, TEMPERATURE, DEPTH, TRAWL_TENSION.
#
#   To discover device UUIDs for a vessel, run in Poseidat_sandbox.R:
#     get_devices("efice", journal_id)
#   To see which value_types each device produces:
#     poseidat_value_types()
#
# device_details : Added further down, after this tibble is built — full
#   device metadata (brand, product_no, serial_no, ...) fetched live from
#   the DBMatic/PEFA APIs. See that section for details.
# ----------------------------------------------------------------------------

vessel_config <- tibble(
  source      = c(
    "dbmatic",                        
    "dbmatic",                        
    
    "efice", 
    "efice", 
    "efice",        
    "efice",  
    "efice",
    
    "pefa",
    "pefa"
  ),
  journal_id  = c(
    "d63c867e-df5c-4e2f-a88d-5ef75454d6cf",   # MDV1
    "05447236-b531-4c62-b22c-59fb7b2490ab",   # KW45
    
    "6a5c162e-e967-41b6-9e8a-3cfddf5019b2",   # Lub Senior
    "a9294c48-ab38-4679-9dd5-f34003ba6b14",   # Polar
    "f4fc3064-fd63-4c9a-9635-0b292d534827",   # Willem Jacob
    "c02a2bc9-baf6-42f2-84ed-d3f7761f6c89",   # Immanuel MDV1
    "28501497-65b0-4125-9889-85f8c7ed8c8a",   # Anna Hendrika KW45
    
    "59e2c628-6b6f-49dd-be24-6e73c9221e25",   # Catherina KW145
    "940acddd-b61c-4d9a-a20f-ff2cd87d9156"    # Aravis SCH99
  ),
  vessel_name = c(
    "Immanuel",
    "Anna Hendrika",
    
    "Lub Senior",
    "Polar",
    "Willem Jacob",
    "Immanuel",
    "Anna Hendrika",
    
    "Catherina",
    "Aravis"
  ),
  cfr = c(
    "MDV1",
    "KW45",
    
    "UK-153", 
    "UK-150", 
    "UK-158",
    "MDV1", 
    "KW45",
    
    "KW145",
    "SCH99"
  ),
  enabled = c(
    TRUE,          
    TRUE,
    
    TRUE, 
    TRUE, 
    TRUE,
    FALSE, 
    FALSE,
    
    TRUE,
    TRUE
  ),
  device_labels = list(
    # DBMatic MDV1
    list(
      "32307764-6734-7a74-0000-000200000001" = "POSITION",
      "32307764-6734-7a74-0000-000200000003" = "DEPTH",
      "32307764-6734-7a74-0000-000200000004" = "FUEL_MAIN"
    ),
    
    # DBMatic KW45
    list(
      "70756278-3132-3931-0000-000200000001" = "POSITION",
      "70756278-3132-3931-0000-000200000003" = "DEPTH",
      "70756278-3132-3931-0000-000200000005" = "TRAWL_TENSION",
      "70756278-3132-3931-0000-000200000006" = "FUEL_MAIN",
      "70756278-3132-3931-0000-000200000007" = "SCALE"
    ),
   
    # Lub Senior (UK-153) — 5 devices confirmed Feb 2026
    list(
      "6d336476-7931-7838-0000-000200000001" = "POSITION",
      "6d336476-7931-7838-0000-000200000005" = "FUEL_1",
      "6d336476-7931-7838-0000-000200000006" = "FUEL_2",
      "6d336476-7931-7838-0000-000200000007" = "FUEL_3",
      "6d336476-7931-7838-0000-000200000008" = "FUEL_4"
    ),

    # Polar (UK-150) — 5 devices confirmed Feb 2026
    list(
      "3864776a-3075-3271-0000-000200000001" = "POSITION",
      "3864776a-3075-3271-0000-000200000005" = "FUEL_1",
      "3864776a-3075-3271-0000-000200000006" = "FUEL_2",
      "3864776a-3075-3271-0000-000200000007" = "FUEL_3",
      "3864776a-3075-3271-0000-000200000008" = "FUEL_4"
    ),

    # Willem Jacob (UK-158) — 4 devices confirmed 12 May 2026
    list(
      "61326139-7379-3463-0000-000200000001" = "POSITION",
      "61326139-7379-3463-0000-000200000004" = "DEPTH",
      "61326139-7379-3463-0000-000200000005" = "TRAWL_TENSION",
      "61326139-7379-3463-0000-000200000006" = "FUEL_2",
      "61326139-7379-3463-0000-000200000007" = "FUEL_3",
      "61326139-7379-3463-0000-000200000008" = "FUEL_1"
    ),

    # MDV1 and KW45 vessels — fill once journal IDs confirmed
    list(), # MDV1 / EFICE
    
    list(), # KW45 / EFICE
    
    list(   # KW145 / PEFA
      "386b3671-796f-7977-0000-000100000001" = "POSITION",      # Furuno gps
      "386b3671-796f-7977-0000-000100000018" = "FUEL_BB",       # BB Hulpmotor
      "386b3671-796f-7977-0000-000100000019" = "FUEL_STB",      # STB Hulpmotor
      "386b3671-796f-7977-0000-00010000001f" = "TRAWL_TENSION", # Trekkrachtmeter
      "386b3671-796f-7977-0000-000100000020" = "DEPTH",         # dieptemeter
      "386b3671-796f-7977-0000-000100000025" = "FUEL_MAIN"      # Hoofdmotor
    ),
    
    list( # SCH99 / PEFA
      "ccfb6ee3-7316-4256-8515-dd8056be1084" = "STW",           # STW long [kn]
      "013d8204-fb80-4f06-8c5d-54a074955de4" = "HEADING",       # Heading [deg]
      "d1a55252-cb35-4462-989f-8a6f9b97ab78" = "FUEL_1",        # FC-Ltr-Eng01-ME [l/h]
      "9aba560b-2efe-4a11-b71f-b1807a68cfd7" = "FUEL_2",        # FC-Ltr-Eng02-AE1 [l/h]
      "bbfe79e7-9452-46dd-bbf4-4b4a1a99de06" = "FUEL_3",        # FC-Ltr-Eng03-AE2 [l/h]
      "8df60832-990b-4fb2-8a1f-ef41eebc0274" = "POSITION",      # GPS
      "c511bf10-1445-4962-8f51-c9deff4f7b67" = "TRAWL_TENSION"  # Trekkrachtmeter
    )  
  )
)

# ----------------------------------------------------------------------------
# Detailed device metadata (DBMatic & PEFA only)
#
# device_labels above is your own short reference name per device. The
# DBMatic and PEFA equipment_inventory endpoints separately return a full
# device object per sensor — brand, product_no, serial_no, equipment_name,
# and whatever else each API exposes. This section fetches that live and
# stores it as a second list-column, device_details, alongside device_labels.
#
# EFICE has no equivalent endpoint (see .efice_get_devices() in
# Poseidat_functions.R) — its device_details rows stay NULL below.
#
# Set fetch_device_details <- FALSE to skip the API calls entirely, e.g.
# when you're just relabelling devices offline and don't want to hit the
# network every time you re-run this script.
# ----------------------------------------------------------------------------

fetch_device_details <- TRUE

# Needed for .get_adapter() / .http_get(), which do the actual API work.
# Adjust the path below if Poseidat_functions.R lives somewhere else.
if (fetch_device_details) source("Poseidat_functions.R")

#' Full device/sensor detail for one vessel (DBMatic or PEFA only)
#'
#' Captures every field the equipment_inventory endpoint returns (brand,
#' product_no, serial_no, ...) instead of the fixed device_id/name/type
#' subset get_devices() uses for day-to-day sync. Pure dplyr/tidyr — no
#' purrr::map(), no apply family.
#'
#' @param source     "dbmatic" or "pefa"
#' @param journal_id Vessel journal UUID
#' @return Tibble, one row per device.
get_device_details <- function(source, journal_id) {

  if (!source %in% c("dbmatic", "pefa"))
    stop("get_device_details() only supports 'dbmatic' and 'pefa' — ",
         "EFICE has no equipment_inventory endpoint.")

  adp <- .get_adapter(source)

  endpoint <- switch(source,
    dbmatic = glue::glue("journals/{journal_id}/equipment_inventory"),
    pefa    = glue::glue("api/v1/Journals/{journal_id}/equipment_inventory")
  )

  data       <- .http_get(adp$base_url, adp$token, endpoint)
  equip_list <- data$equipment

  if (is.null(equip_list) || length(equip_list) == 0) {
    return(tibble(journal_id = journal_id))
  }

  tibble(equipment = equip_list) |>
    tidyr::unnest_wider(equipment) |>
    dplyr::rename(equipment_name = name, equipment_type = type) |>
    tidyr::unnest_longer(devices, values_to = "device", keep_empty = TRUE) |>
    tidyr::unnest_wider(device) |>
    dplyr::rename(device_name = name, device_type = type) |>
    dplyr::filter(!is.na(device_id)) |>
    dplyr::mutate(journal_id = journal_id, .before = 1) |>
    dplyr::relocate(journal_id, equipment_id, equipment_name, equipment_type,
                     device_id, device_name, device_type)
}

vessel_config <- vessel_config |>
  dplyr::mutate(device_details = vector("list", dplyr::n()))

if (fetch_device_details) {
  for (i in seq_len(nrow(vessel_config))) {
    row <- vessel_config[i, ]
    if (!row$enabled || !row$source %in% c("dbmatic", "pefa")) next

    message(sprintf("  Fetching device details: %s / %s ...", row$source, row$vessel_name))
    vessel_config$device_details[[i]] <- tryCatch(
      get_device_details(row$source, row$journal_id),
      error = function(e) {
        message(sprintf("    Failed: %s", conditionMessage(e)))
        NULL
      }
    )
  }
}

# get_device_details("pefa","940acddd-b61c-4d9a-a20f-ff2cd87d9156")

# ----------------------------------------------------------------------------
# Helper: resolve a device UUID to its friendly label for a given vessel
# Usage: get_device_label(vessel_config, "efice", journal_id, device_id)
# ----------------------------------------------------------------------------
get_device_label <- function(vessel_config, source, journal_id, device_id) {
  row <- vessel_config |>
    filter(source == .env$source, journal_id == .env$journal_id)
  if (nrow(row) == 0) return(device_id)
  label <- row$device_labels[[1]][[device_id]]
  if (is.null(label)) device_id else label
}

# ----------------------------------------------------------------------------
# Helper: pull one detail field (brand, serial_no, ...) for a device
# Usage: get_device_detail(vessel_config, "pefa", journal_id, device_id, "brand")
# Returns NA if device_details hasn't been fetched (e.g. EFICE, or
# fetch_device_details was FALSE) or the field/device isn't present.
# ----------------------------------------------------------------------------
get_device_detail <- function(vessel_config, source, journal_id, device_id, field) {
  row <- vessel_config |>
    filter(source == .env$source, journal_id == .env$journal_id)
  if (nrow(row) == 0) return(NA)

  details <- row$device_details[[1]]
  if (is.null(details) || nrow(details) == 0 || !field %in% names(details)) return(NA)

  val <- details |> filter(device_id == .env$device_id) |> pull(.data[[field]])
  if (length(val) == 0) NA else val[[1]]
}

# ----------------------------------------------------------------------------
# Save config to OneDrive
# ----------------------------------------------------------------------------
config_dir <- file.path(onedrive_base, "config")
dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(vessel_config, file.path(config_dir, "poseidat_vessels.rds"))

message("✓ Poseidat config saved")
message(sprintf("  OneDrive base : %s", onedrive_base))
message(sprintf("  Vessels       : %d total (%d enabled)",
                nrow(vessel_config), sum(vessel_config$enabled)))

by_source <- vessel_config |> filter(enabled) |> count(source, name = "n")
for (i in seq_len(nrow(by_source)))
  message(sprintf("    %-10s: %d vessel(s)", by_source$source[i], by_source$n[i]))

message("\nDevice labels (reference — devices discovered live from API):")
for (i in seq_len(nrow(vessel_config))) {
  v      <- vessel_config[i, ]
  if (!v$enabled) next
  labels <- v$device_labels[[1]]
  if (length(labels) == 0) {
    message(sprintf("  %-20s: (no labels defined)", v$vessel_name))
  } else {
    for (uuid in names(labels))
      message(sprintf("  %-20s: ...%s = %s",
                      v$vessel_name,
                      substr(uuid, nchar(uuid) - 7, nchar(uuid)),
                      labels[[uuid]]))
  }
}

message("\nDevice details (DBMatic/PEFA only — full API metadata):")
for (i in seq_len(nrow(vessel_config))) {
  v <- vessel_config[i, ]
  if (!v$enabled) next
  if (!v$source %in% c("dbmatic", "pefa")) {
    message(sprintf("  %-20s: n/a (%s has no detail endpoint)", v$vessel_name, v$source))
    next
  }
  d <- v$device_details[[1]]
  n <- if (is.null(d)) 0L else nrow(d)
  message(sprintf("  %-20s: %d device(s) with full detail%s",
                  v$vessel_name, n,
                  if (n == 0 && !fetch_device_details) " (fetch_device_details was FALSE)" else ""))
}
