# sync_dbmatic_daily.R

library(dplyr)
library(purrr)
library(arrow)
library(httr2)
library(glue)
library(lubridate)

# Source alle functies
source(file.path(here(),"R/Poseidat", "dbmatic_sync_functions.R"))

# Setup
setup_dbmatic_sync(
  base_url = "https://api.dbmatic.eu/api/v1",
  api_token = Sys.getenv("POSEIDAT_TOKEN"),  # Sla token op in .Renviron
  data_dir = "data/dbmatic",
  log_dir = "logs"
)

# Laad device configuratie
device_config <- readRDS("config/dbmatic_devices.rds")

# Sync alle devices
results <- sync_all_devices(
  device_config,
  lookback_days = 7  # Voor eerste keer, daarna incrementeel
)

# Optioneel: consolideer oude data (eens per week)
if (wday(Sys.Date()) == 2) {  # Elke maandag
  for (i in seq_len(nrow(device_config))) {
    device <- device_config[i, ]
    consolidate_device_data(
      device$device_id,
      device$device_name,
      keep_daily_for_days = 30
    )
  }
}

cat("\n=== Sync completed ===\n")
