# sync_dbmatic_daily.R

library(dplyr)
library(purrr)
library(arrow)
library(httr2)
library(glue)
library(lubridate)
library(here)

# Source alle functies
source(file.path(here(),"R/Poseidat", "EFICE_sync_functions.R"))

# Setup
setup_efice_sync(
  base_url = "https://poseidat-journal.m-catch.com/docs",
  api_token = Sys.getenv("EFICE_TOKEN"),  # Sla token op in .Renviron
  data_dir = "data/efice",
  log_dir = "logs/efice"
)

# Laad device configuratie
device_config <- readRDS("config/efice_devices.rds")

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
