# ==============================================================================
# Poseidat Data Management System - Setup
# ==============================================================================
# This script sets up the environment, installs required packages, and 
# configures directory structure
# ==============================================================================

# Required packages ----
required_packages <- c(
  "tidyverse",   # Data manipulation
  "arrow",       # Parquet file handling
  "httr2",       # Modern HTTP client
  "glue",        # String interpolation
  "jsonlite",    # JSON handling
  "lubridate",   # Date/time handling
  "fs",          # File system operations,
  "here",        # Get root directory
  "vroom"        # Fast reader for R
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installing required packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(httr2)
  library(glue)
  library(jsonlite)
  library(lubridate)
  library(fs)
  library(here)
})

cat("✓ All required packages loaded\n\n")


# set working directory ----

setwd(file.path(here(),"R/poseidat"))


# Configure OneDrive path ----
# IMPORTANT: Set your OneDrive path here
# Example: "C:/Users/YourName/OneDrive/DIGIvloot - data - data"
configure_onedrive_path <- function() {
  
  # Option 1: Set your OneDrive path directly here (recommended)
  onedrive_path <- "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data"
  
  # Option 2: Or prompt for it interactively
  # onedrive_path <- readline(prompt = "Enter your OneDrive data path: ")
  
  # Check if path exists
  if (!dir.exists(onedrive_path)) {
    cat("⚠ OneDrive path does not exist:", onedrive_path, "\n")
    cat("Please update the path in 00_setup.R or create the directory\n\n")
    
    # Try to create it
    response <- readline(prompt = "Do you want to create this directory? (y/n): ")
    if (tolower(response) == "y") {
      dir_create(onedrive_path)
      cat("✓ Created directory:", onedrive_path, "\n\n")
    } else {
      stop("Please update the onedrive_path in 00_setup.R")
    }
  }
  
  cat("✓ Using OneDrive path:", onedrive_path, "\n\n")
  return(onedrive_path)
}

onedrive_base <- configure_onedrive_path()

# Create directory structure ----
create_directory_structure <- function(base_path) {
  
  # Subdirectories within OneDrive
  dirs <- c(
    file.path(base_path, "raw/trips"),
    file.path(base_path, "raw/catches"),
    file.path(base_path, "raw/sensors"),
    file.path(base_path, "processed"),
    file.path(base_path, "cache"),
    file.path(base_path, "audit/deletions"),
    file.path(base_path, "outputs/reports"),
    file.path(base_path, "outputs/figures")
  )
  
  for (dir in dirs) {
    dir_create(dir)
  }
  
  cat("✓ Directory structure created in OneDrive:\n")
  cat(paste0("  ", dirs, collapse = "\n"), "\n\n")
}

create_directory_structure(onedrive_base)

# Configuration ----
config <- list(
  # OneDrive base path
  onedrive_base = onedrive_base,
  
  # API settings
  base_url = "https://poseidat-journal-tst.m-catch.com/api/v1",
  token_env_var = "POSEIDAT_TOKEN",
  
  # Data paths (relative to OneDrive base)
  raw_data_path = file.path(onedrive_base, "raw"),
  processed_data_path = file.path(onedrive_base, "processed"),
  audit_path = file.path(onedrive_base, "audit"),
  outputs_path = file.path(onedrive_base, "outputs"),
  cache_path = file.path(onedrive_base, "cache"),
  
  # API settings
  max_retries = 3,
  rate_limit_delay = 0.5,  # seconds between requests
  
  # Date format
  date_format = "%Y-%m-%d"
)

# Save configuration ----
# Config is saved locally in your git folder for easy access
# Data goes to OneDrive
config_path <- "config.json"
write_json(config, config_path, pretty = TRUE, auto_unbox = TRUE)
cat("✓ Configuration saved to", config_path, "\n")
cat("  Data will be stored in OneDrive:", onedrive_base, "\n\n")

# Check bearer token setup ----
check_token_setup <- function() {
  token <- Sys.getenv("POSEIDAT_TOKEN")
  
  if (token == "") {
    cat("⚠ WARNING: POSEIDAT_TOKEN not found!\n\n")
    cat("Please set up your bearer token:\n")
    cat("1. Run: usethis::edit_r_environ()\n")
    cat("2. Add line: POSEIDAT_TOKEN=your_bearer_token_here\n")
    cat("3. Save file and restart R\n\n")
    return(FALSE)
  } else {
    cat("✓ Bearer token found (length:", nchar(token), "characters)\n\n")
    return(TRUE)
  }
}

check_token_setup()

# Create .Renviron template
create_renviron_template <- function() {
  template <- "# Poseidat API Configuration
# Copy this to your .Renviron file (use: usethis::edit_r_environ())
# Replace YOUR_TOKEN_HERE with your actual bearer token

POSEIDAT_TOKEN=YOUR_TOKEN_HERE
"
  writeLines(template, "env_template.txt")
  cat("✓ Created env_template.txt\n\n")
}

create_renviron_template()

# Print setup summary
cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
cat("POSEIDAT DATA MANAGEMENT SYSTEM - SETUP COMPLETE\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
cat("Next steps:\n")
cat("1. Configure your bearer token in .Renviron (see env_template.txt)\n")
cat("2. Restart R session\n")
cat("3. Source 01_api_functions.R\n")
cat("4. Source 02_storage_functions.R\n")
cat("5. Source 03_deletion_functions.R\n")
cat("6. See 04_example_workflow.R for usage examples\n\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
