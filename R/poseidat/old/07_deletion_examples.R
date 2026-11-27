# ==============================================================================
# Selective Deletion Examples
# ==============================================================================
# Examples showing how to use the enhanced deletion features
# ==============================================================================

library(tidyverse)
source("03_deletion_functions.R")

# ==============================================================================
# EXAMPLE 1: Delete ALL data for a vessel (default behavior)
# ==============================================================================

# This is the standard deletion - removes everything
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner request - complete data removal",
  requested_by = "John Smith",
  dry_run = TRUE
)

# Result: Deletes ALL trips, catches, AND sensors for NLD123456

# ==============================================================================
# EXAMPLE 2: Delete only SENSOR data (keep trips and catches)
# ==============================================================================

# Maybe sensor data has privacy concerns but trip logs are fine
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Privacy request - sensor data only",
  requested_by = "John Smith",
  dry_run = TRUE,
  data_types = c("sensors")  # Only delete sensors!
)

# Result: Deletes ONLY sensor data, trips and catches remain intact

# ==============================================================================
# EXAMPLE 3: Delete only CATCHES (keep trip records and sensors)
# ==============================================================================

# Useful if catch data was entered incorrectly
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Incorrect catch data",
  requested_by = "Data Manager",
  dry_run = TRUE,
  data_types = c("catches")  # Only delete catches
)

# Result: Trip and sensor records preserved, only catches deleted

# ==============================================================================
# EXAMPLE 4: Delete only OLD data (before a certain date)
# ==============================================================================

# Delete data from before 2024 (data retention policy)
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Data retention policy - delete records before 2024",
  requested_by = "Compliance Officer",
  dry_run = TRUE,
  date_to = "2023-12-31"  # Delete everything UP TO this date
)

# Result: All data from 2024 onwards is kept, only pre-2024 data deleted

# ==============================================================================
# EXAMPLE 5: Delete data from a specific PERIOD
# ==============================================================================

# Delete data from a specific time period (e.g., when equipment was faulty)
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Faulty sensor period - delete suspect data",
  requested_by = "Technical Team",
  dry_run = TRUE,
  date_from = "2024-03-01",
  date_to = "2024-03-31"  # Delete only March 2024
)

# Result: Only data from March 2024 is deleted, everything else remains

# ==============================================================================
# EXAMPLE 6: Combine data_types AND date filtering
# ==============================================================================

# Delete only sensor data from before 2024 (very specific!)
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Old sensor data cleanup",
  requested_by = "Data Manager",
  dry_run = TRUE,
  data_types = c("sensors"),  # Only sensors
  date_to = "2023-12-31"      # Only before 2024
)

# Result: Very targeted deletion - only old sensor data removed

# ==============================================================================
# EXAMPLE 7: Delete multiple data types for a specific period
# ==============================================================================

# Delete catches AND sensors for a faulty period
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Equipment malfunction period",
  requested_by = "Fleet Manager",
  dry_run = TRUE,
  data_types = c("catches", "sensors"),  # Catches and sensors
  date_from = "2024-06-15",
  date_to = "2024-06-20"  # Specific 5-day period
)

# Result: Trips remain, but catches and sensors from that period are deleted

# ==============================================================================
# EXAMPLE 8: Working with subfolders
# ==============================================================================

# Delete test data from test_data subfolder
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Clean up test data",
  requested_by = "Developer",
  dry_run = TRUE,
  subfolder = "test_data"
)

# Delete only 2023 sensor data (if organized by year)
delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Archive old sensor data",
  requested_by = "Data Manager",
  dry_run = TRUE,
  data_types = c("sensors"),
  subfolder = "2023"  # If you organize by year
)

# ==============================================================================
# EXAMPLE 9: View what WOULD be deleted before executing
# ==============================================================================

# ALWAYS do a dry run first!
preview <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner request",
  requested_by = "John Smith",
  dry_run = TRUE,
  data_types = c("catches", "sensors"),
  date_from = "2024-01-01",
  date_to = "2024-06-30"
)

# Review the output - shows:
# - How many rows would be deleted
# - Column headers of affected data
# - Files that would be modified
# - Total impact

# If it looks good, execute:
actual_deletion <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner request",
  requested_by = "John Smith",
  dry_run = FALSE,  # NOW actually delete
  data_types = c("catches", "sensors"),
  date_from = "2024-01-01",
  date_to = "2024-06-30"
)

# ==============================================================================
# EXAMPLE 10: Understanding the column headers output
# ==============================================================================

# When you run a dry run, you'll see column headers like:

# TRIPS: 25 rows found (would be deleted)
#   Columns (11): trip_id, vessel_id, departure_date, arrival_date, 
#                 departure_port, arrival_port, fishing_days, ...

# This tells you:
# - What fields exist in the data
# - What columns would be affected
# - Helps verify you're deleting the right data type

# ==============================================================================
# USE CASES
# ==============================================================================

# USE CASE 1: GDPR "Right to be Forgotten"
# Owner requests ALL their data be deleted
delete_vessel_data("NLD123456", "GDPR Right to be Forgotten", "John Smith", 
                   dry_run = FALSE)

# USE CASE 2: Data Quality Issue
# Sensor was miscalibrated during specific period
delete_vessel_data("NLD123456", "Miscalibrated sensor", "Tech Team",
                   dry_run = FALSE, data_types = c("sensors"),
                   date_from = "2024-03-15", date_to = "2024-03-20")

# USE CASE 3: Retention Policy
# Company policy: delete all data older than 5 years
delete_vessel_data("NLD123456", "5-year retention policy", "Compliance",
                   dry_run = FALSE, date_to = "2020-01-01")

# USE CASE 4: Trial Period Cleanup
# Vessel was testing during trial period, remove trial data
delete_vessel_data("NLD123456", "Trial period cleanup", "Project Manager",
                   dry_run = FALSE, date_from = "2023-01-01", 
                   date_to = "2023-01-31")

# USE CASE 5: Selective Privacy
# Owner wants sensor data deleted but trip logs kept for research
delete_vessel_data("NLD123456", "Privacy request - sensors only", "Owner",
                   dry_run = FALSE, data_types = c("sensors"))

# ==============================================================================
# BEST PRACTICES
# ==============================================================================

# 1. ALWAYS dry run first
#    - See what would be deleted
#    - Verify column headers match expectations
#    - Check row counts make sense

# 2. Use specific date ranges when possible
#    - Safer than deleting everything
#    - Allows for surgical data removal
#    - Keeps recent/valid data intact

# 3. Be selective with data_types
#    - Maybe you only need to delete sensors, not trips
#    - Preserve trip metadata even if removing detailed catch data
#    - Consider what data is truly sensitive

# 4. Document the reason clearly
#    - Audit trail is important
#    - Include reference numbers
#    - Name the requester

# 5. Generate certificates
#    - Proves deletion was completed
#    - Important for GDPR compliance
#    - Include in deletion request records

# ==============================================================================
# VERIFICATION
# ==============================================================================

# After deletion, always verify:
verify_vessel_deletion("NLD123456")

# For selective deletions, check manually:
trips <- load_poseidat_data("trips", vessel_ids = "NLD123456")
catches <- load_poseidat_data("catches", vessel_ids = "NLD123456") 
sensors <- load_poseidat_data("sensors", vessel_ids = "NLD123456")

# Check row counts and date ranges match expectations

# ==============================================================================
# TROUBLESHOOTING
# ==============================================================================

# If no records found:
# - Check vessel_id is correct
# - Check subfolder if data is organized
# - Use show_data_columns() to see what's in the data
# - Use list_data_files() to see what files exist

# If date filtering doesn't work as expected:
# - Check date column names match (departure_date, catch_date, date)
# - Verify dates are in YYYY-MM-DD format
# - Use date_from for "delete older than" (everything before)
# - Use date_to for "delete newer than" (everything after)
# - Use both for a specific period (between dates)

cat("\n✓ Selective deletion examples loaded\n")
cat("Review the examples above to understand all deletion options\n\n")
