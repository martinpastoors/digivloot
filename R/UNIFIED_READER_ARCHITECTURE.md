# Unified Poseidat Reader Architecture
# =====================================

## Recommended File Structure

Your Poseidat reader should have this modular structure:

```
R/
├── poseidat_reader_v2.R          # Core parsing functions (existing)
├── poseidat_reader_unified.R     # Unified interface (NEW)
└── helper_functions.R            # Shared helpers (optional)

test/
└── test_unified_reader.R         # Test script
```

## Architecture Overview

### Layer 1: Core Parsing Functions (poseidat_reader_v2.R)
- Individual parsing functions for each data type
- extract_trip_info()
- summarize_trip_catches()
- parse_gps_data()
- parse_trawl_tension_data()
- etc.

### Layer 2: Unified Interface (poseidat_reader_unified.R)
- read_poseidat() - Read and detect type
- detect_poseidat_type() - Auto-detect data type
- extract_poseidat() - Route to appropriate parser
- read_and_extract_poseidat() - One-step convenience

## Key Design Decisions

### 1. Auto-Detection Logic

```r
detect_poseidat_type <- function(data) {
  # Count entry types
  n_device <- sum(data$entry_type == "device-measurement")
  n_trip <- sum(data$entry_type %in% c("departure", "arrival", "fishing-activity"))
  
  # Calculate proportions
  prop_device <- n_device / nrow(data)
  prop_trip <- n_trip / nrow(data)
  
  # Decision rules:
  # - >80% device-measurement → "sensor"
  # - >80% trip entries → "trip"
  # - Mix of both >30% → "mixed"
}
```

### 2. S3 Class System

Using S3 classes provides clean printing and method dispatch:

```r
# Create S3 object
result <- list(data = ..., type = ..., metadata = ...)
class(result) <- c("poseidat", class(result))

# Custom print method
print.poseidat <- function(x, ...) {
  cat("Poseidat Data Object\n")
  cat("Type:", x$type, "\n")
  # ...
}
```

### 3. Routing Strategy

```r
extract_poseidat <- function(obj, what = "auto") {
  if (what == "auto") {
    # Route based on detected type
    if (obj$type == "sensor") return(parse_all_sensor_data(obj$data))
    if (obj$type == "trip") return(extract_all_trip_data(obj$data))
    if (obj$type == "mixed") return(list(trip = ..., sensor = ...))
  } else {
    # Manual specification
    if (what == "gps") return(parse_gps_data(obj$data))
    # ...
  }
}
```

## Usage Patterns

### Pattern 1: Simplest (One-liner)
```r
data <- read_and_extract_poseidat("file.json")
```

**Use when:** You trust auto-detection and want immediate results

### Pattern 2: Inspect First
```r
obj <- read_poseidat("file.json")
print(obj)  # Check type and contents
data <- extract_poseidat(obj)
```

**Use when:** You want to verify file contents before parsing

### Pattern 3: Manual Control
```r
obj <- read_poseidat("file.json")
gps <- extract_poseidat(obj, what = "gps")
trawl <- extract_poseidat(obj, what = "trawl")
```

**Use when:** You only need specific data types

### Pattern 4: Batch Processing
```r
files <- list.files(pattern = "*.json")
all_data <- lapply(files, read_and_extract_poseidat)

# Combine by type
all_gps <- lapply(all_data, function(x) {
  if (!is.null(x$gps)) x$gps 
  else if (!is.null(x$sensor_data$gps)) x$sensor_data$gps
}) %>% bind_rows()
```

**Use when:** Processing many files at once

## Benefits of This Architecture

### ✅ Separation of Concerns
- Core parsing functions are independent
- Unified interface handles routing
- Easy to test and maintain

### ✅ Flexibility
- Can use low-level functions directly
- Or high-level unified interface
- Supports both automatic and manual modes

### ✅ Extensibility
- Add new sensor types? Just add parse_X_data()
- Add new entry types? Update detect_poseidat_type()
- Interface remains stable

### ✅ User-Friendly
- Auto-detection removes guesswork
- Clear error messages
- Intuitive function names

### ✅ Type Safety
- S3 class ensures proper object structure
- Type detection prevents wrong parser usage
- Metadata preserved throughout

## Example: Adding a New Sensor Type

### Step 1: Add parsing function to poseidat_reader_v2.R
```r
parse_new_sensor_data <- function(data) {
  dm <- data[data$entry_type == "device-measurement", ]
  # ... parsing logic ...
  return(results)
}
```

### Step 2: Update parse_all_sensor_data()
```r
parse_all_sensor_data <- function(data) {
  result <- list(
    gps = parse_gps_data(data),
    trawl = parse_trawl_tension_data(data),
    new_sensor = parse_new_sensor_data(data)  # ADD THIS
  )
  return(result)
}
```

### Step 3: (Optional) Add to extract_poseidat()
```r
extract_poseidat <- function(obj, what = "auto") {
  # ...
  } else if (what == "new_sensor") {
    return(parse_new_sensor_data(obj$data))
  }
  # ...
}
```

That's it! The unified interface automatically includes the new sensor.

## Recommended Next Steps

1. **Keep your existing poseidat_reader_v1_0.R**
   - Rename to poseidat_reader_v2.R
   - This contains all parsing functions

2. **Add the unified interface**
   - Use poseidat_reader_unified.R
   - Source both files in order:
     ```r
     source("R/poseidat_reader_v2.R")
     source("R/poseidat_reader_unified.R")
     ```

3. **Test with both file types**
   - Use test_unified_reader.R as template
   - Verify auto-detection works correctly

4. **Update your workflow**
   - Start using read_and_extract_poseidat()
   - Falls back gracefully if detection fails

## Error Handling Strategy

The unified reader includes multiple levels of error handling:

```r
# Level 1: File reading
tryCatch({
  raw_data <- fromJSON(file_path)
}, error = function(e) {
  stop("Failed to read JSON: ", e$message)
})

# Level 2: Type detection
if (type == "unknown") {
  warning("Could not detect type, returning raw data")
}

# Level 3: Parsing
Each parse function returns empty data.frame() on error
```

## Performance Considerations

- **Auto-detection is fast:** Just counts entry types
- **Lazy evaluation:** Only parses what you request
- **Memory efficient:** Doesn't duplicate data
- **Vectorized:** Uses dplyr where possible

## Common Issues and Solutions

### Issue: Auto-detection is wrong
**Solution:** Use manual mode
```r
obj <- read_poseidat("file.json", detect_type = FALSE)
data <- extract_poseidat(obj, what = "sensor")  # Force sensor parsing
```

### Issue: Mixed file with mostly one type
**Solution:** Detection threshold is 80%, adjust if needed in detect_poseidat_type()

### Issue: New entry type not recognized
**Solution:** Update detect_poseidat_type() with new entry types

### Issue: Need both trip and sensor from mixed file
**Solution:** Auto mode returns both
```r
data <- extract_poseidat(obj, what = "auto")
trips <- data$trip_data
sensors <- data$sensor_data
```
