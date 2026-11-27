# ============================================================================
# EFICE_config.R
# Configuration for EFICE Poseidat production data access
# ============================================================================

library(dplyr)

# ----------------------------------------------------------------------------
# OneDrive base path — adjust to your local OneDrive sync folder
# ----------------------------------------------------------------------------
onedrive_base <- file.path(
  Sys.getenv("USERPROFILE"),       # Windows: C:/Users/<name>
  "Martin Pastoors",                  # adjust if named differently
  "DIGIvloot - data - data"           # top-level folder inside OneDrive
)

# Fallback for macOS / Linux
if (!nzchar(Sys.getenv("USERPROFILE"))) {
  onedrive_base <- file.path(
    Sys.getenv("HOME"),
    "OneDrive - <OrganisationName>",  # adjust to your OneDrive mount name
    "Poseidat"
  )
}

# ----------------------------------------------------------------------------
# Vessel / journal configuration
# Each row = one vessel your EFICE token has access to.
# journal_id  : UUID from get_vessels_efice()
# vessel_name : Human-readable name used for folder and file naming
# cfr         : Community Fleet Register number (optional, informational)
# enabled     : Set FALSE to temporarily skip a vessel during sync
# ----------------------------------------------------------------------------
vessel_config <- tibble(
  journal_id  = c(
    "REPLACE-WITH-REAL-JOURNAL-UUID-1",
    "REPLACE-WITH-REAL-JOURNAL-UUID-2"
  ),
  vessel_name = c(
    "Vessel_A",
    "Vessel_B"
  ),
  cfr = c(
    "NLD000000001",
    "NLD000000002"
  ),
  enabled = c(TRUE, TRUE)
)

# Filter only enabled vessels
vessel_config <- vessel_config |> filter(enabled)

# ----------------------------------------------------------------------------
# Save config for use by sync scripts
# ----------------------------------------------------------------------------
config_dir <- file.path(onedrive_base, "config")
dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(vessel_config,  file.path(config_dir, "efice_vessels.rds"))
saveRDS(onedrive_base,  file.path(config_dir, "onedrive_base.rds"))

message("✓ EFICE config saved")
message(sprintf("  OneDrive base : %s", onedrive_base))
message(sprintf("  Vessels       : %d", nrow(vessel_config)))
