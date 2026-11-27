# ==============================================================================
# Poseidat Data Management - Example Workflow
# ==============================================================================
# This script demonstrates how to use the Poseidat data management system
# 
# NOTE: All data is automatically stored in your OneDrive folder
#       You don't need to specify paths - just use the functions!
# ==============================================================================

# Load all required functions
source("01_api_functions.R")
source("02_storage_functions.R")
source("03_deletion_functions.R")

# ==============================================================================
# 1. INITIAL DATA FETCH
# ==============================================================================

# Define your vessels of interest
my_vessels <- c("NLD123456", "NLD789012", "NLD345678")

# Fetch data for a specific date range
trips_data <- get_trips(
  vessel_ids = my_vessels,
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

catches_data <- get_catches(
  vessel_ids = my_vessels,
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

sensors_data <- get_sensors(
  vessel_ids = my_vessels,
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

# Or fetch all data types at once
all_data <- get_all_vessel_data(
  vessel_ids = my_vessels,
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

# ==============================================================================
# 2. SAVE DATA TO PARQUET
# ==============================================================================

# Save individual data types
save_poseidat_data(trips_data, "trips", c("2024-01-01", "2024-12-31"))
save_poseidat_data(catches_data, "catches", c("2024-01-01", "2024-12-31"))
save_poseidat_data(sensors_data, "sensors", c("2024-01-01", "2024-12-31"))

# Or save all at once
save_all_poseidat_data(all_data, c("2024-01-01", "2024-12-31"))

# Save with subfolder organization (e.g., by year)
save_all_poseidat_data(all_data, c("2024-01-01", "2024-12-31"), subfolder = "2024")

# ==============================================================================
# 3. LOAD DATA FROM STORAGE
# ==============================================================================

# Load all trips data
trips <- load_poseidat_data("trips")

# Load with filters
trips_filtered <- load_poseidat_data(
  "trips",
  date_from = "2024-06-01",
  date_to = "2024-12-31",
  vessel_ids = c("NLD123456")
)

# Load all data types
all_loaded <- load_all_poseidat_data(
  date_from = "2024-01-01",
  vessel_ids = my_vessels
)

# ==============================================================================
# 4. INCREMENTAL UPDATES
# ==============================================================================

# Update with new data since last fetch
trips_updated <- update_poseidat_data(
  vessel_ids = my_vessels,
  data_type = "trips"
)

# ==============================================================================
# 5. CREATE PROCESSED/SUMMARY DATA
# ==============================================================================

# Create trip-level summary with catch information
trip_summary <- create_trip_summary(save_output = TRUE)

# View the summary
trip_summary |>
  select(trip_id, vessel_id, departure_date, total_catch_kg, n_species) |>
  head(10)

# Create custom analysis
custom_analysis <- trips |>
  left_join(
    catches |>
      group_by(trip_id) |>
      summarise(
        herring_kg = sum(weight_kg[species == "HER"], na.rm = TRUE),
        cod_kg = sum(weight_kg[species == "COD"], na.rm = TRUE)
      ),
    by = "trip_id"
  ) |>
  filter(herring_kg > 0)

# Save custom analysis
write_parquet(custom_analysis, "data/processed/herring_cod_analysis.parquet")

# ==============================================================================
# 6. DATA EXPLORATION
# ==============================================================================

# View storage statistics
get_storage_stats()

# List all data files
list_data_files()

# List files for specific data type
list_data_files("trips")

# ==============================================================================
# 7. HELPER FUNCTIONS FOR DATE-BASED QUERIES
# ==============================================================================

# Get last 30 days of data
recent_trips <- get_recent_data(my_vessels, days = 30, data_type = "trips")

# Get specific month
jan_2024 <- get_month_data(my_vessels, year = 2024, month = 1, data_type = "all")

# ==============================================================================
# 8. DATA DELETION (GDPR COMPLIANCE)
# ==============================================================================

# ALWAYS START WITH DRY RUN to preview what will be deleted
deletion_preview <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner data deletion request - Ref#2024-0042",
  requested_by = "John Smith, vessel owner",
  dry_run = TRUE  # Preview only
)

# If data is in a subfolder, specify it:
deletion_preview <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Test deletion",
  requested_by = "Test User",
  dry_run = TRUE,
  subfolder = "test_data"  # For test data or organized by year/quarter
)

# After reviewing preview, execute actual deletion
deletion_report <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner data deletion request - Ref#2024-0042",
  requested_by = "John Smith, vessel owner",
  dry_run = FALSE  # Actually delete
)

# Verify deletion was successful
verify_vessel_deletion("NLD123456")

# If deleted from subfolder, verify there too:
verify_vessel_deletion("NLD123456", subfolder = "test_data")

# Generate official deletion certificate
generate_deletion_certificate(deletion_report$deletion_id)

# View deletion history
deletion_history <- view_deletion_history()

# ==============================================================================
# 9. TYPICAL ANALYSIS WORKFLOWS
# ==============================================================================

# Example: Analyze catch composition by vessel
catch_composition <- catches |>
  left_join(trips |> select(trip_id, vessel_id), by = "trip_id") |>
  group_by(vessel_id, species) |>
  summarise(
    total_kg = sum(weight_kg, na.rm = TRUE),
    n_catches = n(),
    .groups = "drop"
  ) |>
  arrange(vessel_id, desc(total_kg))

# Example: Monthly catch trends
monthly_trends <- catches |>
  left_join(trips |> select(trip_id, departure_date), by = "trip_id") |>
  mutate(month = floor_date(departure_date, "month")) |>
  group_by(month, species) |>
  summarise(
    total_kg = sum(weight_kg, na.rm = TRUE),
    .groups = "drop"
  )

# Visualize trends
library(ggplot2)

monthly_trends |>
  filter(species %in% c("HER", "COD", "MAC")) |>
  ggplot(aes(x = month, y = total_kg / 1000, color = species)) +
  geom_line(size = 1) +
  labs(title = "Monthly Catch Trends",
       x = "Month",
       y = "Catch (tonnes)",
       color = "Species") +
  theme_minimal()

# ==============================================================================
# 10. BEST PRACTICES
# ==============================================================================

# 1. Always use dry_run = TRUE first before deleting data
# 2. Keep raw data separate from processed data
# 3. Use date_range parameter when saving for better organization
# 4. Use subfolders to organize by year/quarter
# 5. Create specific processed datasets for different analyses
# 6. Regularly check storage statistics
# 7. Document all data deletions with proper reason and requester
# 8. Verify deletions are complete
# 9. Keep audit logs for compliance

message("\n✓ Example workflow complete!")
message("  Review the code above and adapt to your specific needs")
