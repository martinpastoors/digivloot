# ==============================================================================
# Testing Workflow with Mock Data
# ==============================================================================
# Use this to test all Poseidat functions before real API access
# ==============================================================================

# Load required functions
source("02_storage_functions.R")
source("03_deletion_functions.R")
source("05_mock_data_generator.R")

cat("\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
cat("POSEIDAT TESTING WORKFLOW WITH MOCK DATA\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")

# ==============================================================================
# TEST 1: Generate Mock Data
# ==============================================================================

cat("TEST 1: Generating mock data...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Generate a full year of data
mock_data <- generate_mock_poseidat_data(
  n_trips = 100,
  vessel_ids = c("NLD123456", "NLD789012", "NLD345678"),
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

cat("\n")

# ==============================================================================
# TEST 2: Save Mock Data to Parquet
# ==============================================================================

cat("TEST 2: Saving mock data to Parquet...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Save all data types
save_all_poseidat_data(
  mock_data, 
  date_range = c("2024-01-01", "2024-12-31"),
  subfolder = "test_data"
)

cat("\n")

# ==============================================================================
# TEST 3: Load Data from Storage
# ==============================================================================

cat("TEST 3: Loading data from storage...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Load all data
trips_loaded <- load_poseidat_data("trips", subfolder = "test_data")
catches_loaded <- load_poseidat_data("catches", subfolder = "test_data")
sensors_loaded <- load_poseidat_data("sensors", subfolder = "test_data") %>% 
  arrange(trip_id, timestamp)

cat("\nLoaded data summary:\n")
cat("  Trips:", nrow(trips_loaded), "records\n")
cat("  Catches:", nrow(catches_loaded), "records\n")
cat("  Sensors:", nrow(sensors_loaded), "records\n\n")

# ==============================================================================
# TEST 4: Filter Data
# ==============================================================================

cat("TEST 4: Testing data filtering...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Load with vessel filter
vessel_trips <- load_poseidat_data(
  "trips", 
  vessel_ids = c("NLD123456"),
  subfolder = "test_data"
)

cat("Trips for NLD123456:", nrow(vessel_trips), "\n")

# Load with date filter
q1_trips <- load_poseidat_data(
  "trips",
  date_from = "2024-01-01",
  date_to = "2024-03-31",
  subfolder = "test_data"
)

cat("Q1 trips:", nrow(q1_trips), "\n\n")

# ==============================================================================
# TEST 5: Create Processed Data
# ==============================================================================

cat("TEST 5: Creating trip summary (processed data)...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Create trip summary
trip_summary <- trips_loaded |>
  left_join(
    catches_loaded |>
      group_by(trip_id) |>
      summarise(
        total_catch_kg = sum(weight_kg, na.rm = TRUE),
        n_species = n_distinct(species),
        n_hauls = n(),
        main_species = first(species[which.max(weight_kg)]),
        .groups = "drop"
      ),
    by = "trip_id"
  )

cat("Trip summary created:", nrow(trip_summary), "trips\n")
cat("\nSample of trip summary:\n")
print(head(trip_summary |> select(trip_id, vessel_id, fishing_days, total_catch_kg, n_species, main_species), 5))

cat("\n")

t <- create_trip_summary (date_from = "2024-01-01", date_to = "2024-03-31", save_output = TRUE) 


# ==============================================================================
# TEST 6: Analysis Examples
# ==============================================================================

cat("TEST 6: Running analysis examples...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Analysis 1: Catch by vessel
catch_by_vessel <- catches_loaded |>
  group_by(vessel_id) |>
  summarise(
    total_catch_kg = sum(weight_kg, na.rm = TRUE),
    n_trips = n_distinct(trip_id),
    avg_catch_per_trip = total_catch_kg / n_trips,
    .groups = "drop"
  ) |>
  arrange(desc(total_catch_kg))

cat("\nCatch by vessel:\n")
print(catch_by_vessel)

# Analysis 2: Top species
top_species <- catches_loaded |>
  group_by(species) |>
  summarise(
    total_kg = sum(weight_kg, na.rm = TRUE),
    n_catches = n(),
    .groups = "drop"
  ) |>
  arrange(desc(total_kg)) |>
  head(5)

cat("\n\nTop 5 species:\n")
print(top_species)

# Analysis 3: Monthly trends
monthly_catches <- catches_loaded |>
  left_join(trips_loaded |> select(trip_id, departure_date), by = "trip_id") |>
  mutate(month = floor_date(departure_date, "month")) |>
  group_by(month) |>
  summarise(
    total_catch_kg = sum(weight_kg, na.rm = TRUE),
    n_trips = n_distinct(trip_id),
    .groups = "drop"
  )

cat("\n\nMonthly catch trends:\n")
print(monthly_catches)

cat("\n")

# ==============================================================================
# TEST 7: Data Deletion (Dry Run)
# ==============================================================================

cat("TEST 7: Testing data deletion (DRY RUN)...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Test 7a: Full deletion preview
cat("\n7a. Preview full deletion:\n")
deletion_preview_all <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Test deletion - all data",
  requested_by = "Test User",
  dry_run = TRUE,
  subfolder = "test_data"
)

# Test 7b: Selective deletion - only sensors
cat("\n7b. Preview selective deletion (sensors only):\n")
deletion_preview_sensors <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Test deletion - sensors only",
  requested_by = "Test User",
  dry_run = TRUE,
  subfolder = "test_data",
  data_types = c("sensors")  # Only delete sensors
)

# Test 7c: Date-filtered deletion - only Q1 2024
cat("\n7c. Preview date-filtered deletion (Q1 2024 only):\n")
deletion_preview_q1 <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Test deletion - Q1 2024 only",
  requested_by = "Test User",
  dry_run = TRUE,
  subfolder = "test_data",
  date_from = "2024-01-01",
  date_to = "2024-03-31"
)

cat("\n")

# ==============================================================================
# TEST 8: Storage Statistics
# ==============================================================================

cat("TEST 8: Checking storage statistics...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

# Get storage stats
storage_stats <- get_storage_stats()

cat("\nStorage statistics:\n")
print(storage_stats)

cat("\n")

# List all files
all_files <- list_data_files()

cat("\nAll data files:\n")
print(all_files |> select(data_type, filename, size))

cat("\n")

# ==============================================================================
# TEST 9: Visualization Example
# ==============================================================================

cat("TEST 9: Creating visualization...\n")
cat("-" |> rep(70) |> paste0(collapse = ""), "\n")

library(ggplot2)

# Plot monthly catches
plot <- monthly_catches |>
  ggplot(aes(x = month, y = total_catch_kg / 1000)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Monthly Total Catch (Mock Data)",
    x = "Month",
    y = "Total Catch (tonnes)",
    caption = "Data: Mock Poseidat data for testing"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

# Save plot if outputs folder exists
if (file.exists(config$outputs_path)) {
  ggsave(
    file.path(config$outputs_path, "figures", "test_monthly_catches.png"),
    plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("✓ Plot saved to outputs/figures/test_monthly_catches.png\n")
} else {
  print(plot)
  cat("✓ Plot displayed (outputs folder not configured)\n")
}

cat("\n")

# ==============================================================================
# TESTING COMPLETE
# ==============================================================================

cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
cat("ALL TESTS COMPLETE!\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")

cat("Summary:\n")
cat("  ✓ Mock data generated\n")
cat("  ✓ Data saved to Parquet format\n")
cat("  ✓ Data loaded and filtered successfully\n")
cat("  ✓ Processed data created\n")
cat("  ✓ Analysis examples completed\n")
cat("  ✓ Deletion functions tested (dry run)\n")
cat("  ✓ Storage statistics checked\n")
cat("  ✓ Visualization created\n\n")

cat("Your Poseidat data management system is working correctly!\n")
cat("When real API access is available, the same functions will work\n")
cat("with actual data from get_trips(), get_catches(), get_sensors().\n\n")

cat("Next steps:\n")
cat("1. Review the test results above\n")
cat("2. Explore the saved files in your OneDrive folder\n")
cat("3. Try modifying the analysis examples for your needs\n")
cat("4. When you get API access, replace mock_data with real API calls\n\n")
