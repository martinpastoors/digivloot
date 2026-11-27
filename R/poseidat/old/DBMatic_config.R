# config_dbmatic.R

# Device configuratie
device_config <- tibble(
  journal_id = c(
    "21eacd08-fcd3-4665-9dba-9c23c325bfa3",
    "21eacd08-fcd3-4665-9dba-9c23c325bfa3"
  ),
  device_id = c(
    "6a737577-6670-3277-0000-000200000001",
    "another-device-id-here"  # Voeg meer devices toe
  ),
  device_name = c(
    "GPS_Processor",
    "Temperature_Sensor"
  ),
  enabled = c(TRUE, TRUE)
)

# Filter alleen enabled devices
device_config <- device_config |> filter(enabled)

# Bewaar configuratie
saveRDS(device_config, "config/dbmatic_devices.rds")