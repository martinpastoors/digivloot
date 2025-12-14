# Poseidat Data Management System

A complete R-based system for fetching, storing, managing, and analyzing Poseidat fishing data with full GDPR compliance.

## Features

- ✅ API integration with bearer token authentication
- ✅ Efficient Parquet file storage
- ✅ Tidyverse-compatible data frames
- ✅ Incremental data updates
- ✅ GDPR-compliant data deletion with audit trail
- ✅ Separate storage for raw and processed data
- ✅ Helper functions for common date-based queries

## Quick Start

### 1. Installation

```r
# Run the setup script
source("00_setup.R")
```

**IMPORTANT**: Before running setup, edit `00_setup.R` and set your OneDrive path:
```r
# Line ~18 in 00_setup.R
onedrive_path <- "C:/Users/YourName/OneDrive/DIGIvloot - data - data"
```

This will:
- Install required packages
- Create directory structure in your OneDrive folder
- Generate configuration file (stored in your local git folder)
- Create .Renviron template

### 2. Configure Bearer Token

```r
# Edit your .Renviron file
usethis::edit_r_environ()

# Add this line (replace with your actual token):
POSEIDAT_TOKEN=your_bearer_token_here

# Save and restart R
```

### 3. Load Functions

```r
source("01_api_functions.R")
source("02_storage_functions.R")
source("03_deletion_functions.R")
```

### 4. Fetch and Save Data

```r
# Define your vessels
vessels <- c("NLD123456", "NLD789012")

# Fetch data
all_data <- get_all_vessel_data(
  vessel_ids = vessels,
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

# Save to Parquet files
save_all_poseidat_data(all_data, c("2024-01-01", "2024-12-31"))
```

### 5. Load and Analyze

```r
# Load data
trips <- load_poseidat_data("trips")
catches <- load_poseidat_data("catches")

# Create analysis
trip_summary <- trips |>
  left_join(
    catches |>
      group_by(trip_id) |>
      summarise(total_catch_kg = sum(weight_kg, na.rm = TRUE)),
    by = "trip_id"
  )
```

## File Structure

**Local (Git folder):**
```
your_project/
├── 00_setup.R
├── 01_api_functions.R
├── 02_storage_functions.R
├── 03_deletion_functions.R
├── 04_example_workflow.R
├── 05_mock_data_generator.R       # Generate test data
├── 06_test_workflow.R             # Complete testing workflow
├── 07_deletion_examples.R         # Selective deletion examples
├── 08_gps_line_visualization.R    # NEW: GPS & line visualizations
├── README.md
├── TESTING_GUIDE.txt              
├── config.json                    # Configuration (points to OneDrive)
└── env_template.txt
```

**OneDrive (Data storage):**
```
C:/Users/.../DIGIvloot - data - data/
├── raw/                          # Raw API data (Parquet)
│   ├── trips/
│   ├── catches/
│   └── sensors/
├── processed/                    # Processed/summarized data
├── audit/                        # Deletion audit logs
│   └── deletions/
├── outputs/                      # Your outputs
│   ├── reports/
│   └── figures/
└── cache/                        # Temporary files
```

**Benefits of this setup:**
- ✅ Code in git (version controlled, no large files)
- ✅ Data in OneDrive (backed up, synced, large files OK)
- ✅ No need to type long paths in your code
- ✅ Easy collaboration (just share OneDrive folder)

## Core Functions

### API Functions (01_api_functions.R)

```r
# Fetch specific data types
trips <- get_trips(vessel_ids, date_from, date_to)
catches <- get_catches(vessel_ids, date_from, date_to)
sensors <- get_sensors(vessel_ids, date_from, date_to)

# Fetch all types at once
all_data <- get_all_vessel_data(vessel_ids, date_from, date_to)

# Helper functions
recent <- get_recent_data(vessel_ids, days = 30)
monthly <- get_month_data(vessel_ids, year = 2024, month = 1)
```

### Storage Functions (02_storage_functions.R)

```r
# Save data
save_poseidat_data(data, "trips", c("2024-01-01", "2024-01-31"))
save_all_poseidat_data(all_data, date_range, subfolder = "2024")

# Load data
trips <- load_poseidat_data("trips", date_from, date_to)
all_data <- load_all_poseidat_data(vessel_ids = vessels)

# Update with new data
updated <- update_poseidat_data(vessels, "trips")

# Create processed datasets
trip_summary <- create_trip_summary(save_output = TRUE)

# Utilities
get_storage_stats()
list_data_files("trips")
```

### Deletion Functions (03_deletion_functions.R)

```r
# Full deletion (all data types)
delete_vessel_data(vessel_id, reason, requested_by, dry_run = TRUE)

# NEW: Selective deletion by data type
delete_vessel_data(vessel_id, reason, requested_by, dry_run = TRUE,
                   data_types = c("sensors"))  # Only sensors

# NEW: Date-filtered deletion
delete_vessel_data(vessel_id, reason, requested_by, dry_run = TRUE,
                   date_from = "2024-01-01", date_to = "2024-12-31")

# NEW: Combined filtering
delete_vessel_data(vessel_id, reason, requested_by, dry_run = TRUE,
                   data_types = c("catches", "sensors"),
                   date_from = "2023-01-01", date_to = "2023-12-31")

# Verify and document
verify_vessel_deletion(vessel_id)
generate_deletion_certificate(report$deletion_id)
view_deletion_history()
```

**NEW Features:**
- ✅ Delete specific data types only (trips, catches, or sensors)
- ✅ Delete by date range (e.g., delete only old data)
- ✅ Column headers shown in reports
- ✅ Combine filters for surgical data removal

See `07_deletion_examples.R` for 10+ detailed examples.

## Data Deletion (GDPR Compliance)

The system provides complete data deletion capabilities with full audit trail:

### 1. Preview Deletion (Dry Run)

```r
preview <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner request - Ref#2024-0042",
  requested_by = "John Smith",
  dry_run = TRUE
)
```

### 2. Execute Deletion

```r
report <- delete_vessel_data(
  vessel_id = "NLD123456",
  reason = "Owner request - Ref#2024-0042",
  requested_by = "John Smith",
  dry_run = FALSE
)
```

### 3. Verify and Document

```r
# Verify complete removal
verify_vessel_deletion("NLD123456")

# Generate official certificate
generate_deletion_certificate(report$deletion_id)
```

### Audit Trail

All deletions are logged in:
- `data/audit/deletions/deletion_master_log.csv` - Master log
- `data/audit/deletions/DEL_*.json` - Individual deletion details
- `data/audit/deletions/DEL_*_certificate.txt` - Deletion certificates

## Storage Strategy

### Keep Data Separate

- **Raw data**: Separate Parquet files for trips, catches, sensors
- **Processed data**: Created on-demand for specific analyses
- **Benefits**: Easy updates, efficient storage, flexible analysis

### File Organization

```
data/raw/trips/
├── trips_2024-01-01_2024-01-31.parquet
├── trips_2024-02-01_2024-02-29.parquet
└── ...

data/processed/
├── trip_summary.parquet
├── vessel_annual_summary.parquet
└── ...
```

## Best Practices

1. **Always use dry_run = TRUE** before deleting data
2. **Keep raw data separate** from processed data
3. **Use date_range parameter** when saving for organization
4. **Create specific processed datasets** for different analyses
5. **Regularly check storage** with `get_storage_stats()`
6. **Document all deletions** with proper reason and requester
7. **Verify deletions** are complete
8. **Keep audit logs** for compliance
9. **Use incremental updates** to avoid re-fetching all data
10. **Organize by year/quarter** using subfolders

## Example Workflows

See `04_example_workflow.R` for complete examples of:

- Initial data fetch and storage
- Loading and filtering data
- Incremental updates
- Creating processed datasets
- Data deletion workflow
- Common analysis patterns
- Visualization examples

## Testing Before API Access

Don't have real data yet? No problem! Use the mock data generator:

```r
# Load mock data generator
source("05_mock_data_generator.R")

# Generate realistic test data
mock_data <- generate_mock_poseidat_data(
  n_trips = 100,
  vessel_ids = c("NLD123456", "NLD789012"),
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# Test all your functions
save_all_poseidat_data(mock_data, c("2024-01-01", "2024-12-31"))
trips <- load_poseidat_data("trips")
```

**Or run the complete test workflow:**
```r
source("06_test_workflow.R")
```

This tests all functions with realistic mock data. See `TESTING_GUIDE.txt` for details.

**NEW: Visualize GPS and line length data:**
```r
source("08_gps_line_visualization.R")
```

**Mock Data Includes:**
- **Trips**: Fishing trips with departure/arrival dates and ports
- **Catches**: Catch events with species, weights, locations
- **Sensors** (minute-by-minute):
  - GPS latitude/longitude (vessel position tracking)
  - Fishing line length (0-20m retracted, 50-500m deployed)
  - Temperature, speed, fuel, engine RPM (periodic readings)

When real API access is available, just replace mock data with API calls - everything else stays the same!

## Required Packages

- `tidyverse` - Data manipulation
- `arrow` - Parquet file handling
- `httr2` - API requests
- `glue` - String interpolation
- `jsonlite` - JSON handling
- `lubridate` - Date/time operations
- `fs` - File system operations

## Configuration

### OneDrive Path Setup

**Before running setup**, edit line ~18 in `00_setup.R`:

```r
onedrive_path <- "C:/Users/YourName/OneDrive/DIGIvloot - data - data"
```

Replace with your actual OneDrive path. The system will:
- Store all data files in OneDrive (backed up, synced)
- Keep your code in git (version controlled)
- Let you reference paths simply as "data" or "outputs" in your code

### API Configuration

The `config.json` file (created by setup) contains:

```json
{
  "onedrive_base": "C:/Users/.../DIGIvloot - data - data",
  "base_url": "https://poseidat-journal-tst.m-catch.com/api/v1",
  "raw_data_path": "C:/Users/.../DIGIvloot - data - data/raw",
  "processed_data_path": "C:/Users/.../DIGIvloot - data - data/processed",
  "max_retries": 3,
  "rate_limit_delay": 0.5
}
```

You don't need to edit this manually - it's generated automatically from your OneDrive path.

## Troubleshooting

### API Connection Issues

```r
# Check token is configured
Sys.getenv("POSEIDAT_TOKEN")

# Test API connection
test_data <- get_trips(vessel_ids = "TEST_ID", date_from = "2024-01-01", date_to = "2024-01-31")
```

### Storage Issues

```r
# Check storage statistics
get_storage_stats()

# List all files
list_data_files()

# Verify directory structure
fs::dir_tree("data")
```

### Deletion Issues

```r
# Always use dry_run first
delete_vessel_data(vessel_id, reason, requested_by, dry_run = TRUE)

# Verify before and after
verify_vessel_deletion(vessel_id)
```

## Support

For issues or questions:
1. Check `04_example_workflow.R` for usage examples
2. Review function documentation in source files
3. Verify configuration in `data/config.json`
4. Check API documentation: https://poseidat-journal-tst.m-catch.com/docs

## License

This code is provided as-is for managing Poseidat data. Ensure compliance with all relevant data protection regulations in your jurisdiction.
