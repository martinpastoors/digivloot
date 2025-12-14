# =====================================================
# DIAGNOSTIC SCRIPT - Find the Sensor Data Issue
# =====================================================

library(tidyverse)
library(jsonlite)

# Set your directory
mydir <- "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data"

# Read the sensor data file
cat("Reading sensor data file...\n")
raw_data <- fromJSON(file.path(mydir, "poseidat_sensor_data_example.json"), 
                     simplifyVector = TRUE)
data <- raw_data$items

cat("\n=== BASIC INFO ===\n")
cat("Total entries:", nrow(data), "\n")
cat("Entry types:", paste(unique(data$entry_type), collapse = ", "), "\n\n")

# Filter device measurements
dm <- data %>% filter(entry_type == "device-measurement")
cat("Device measurements:", nrow(dm), "\n\n")

# Check the value column structure
cat("=== VALUE COLUMN STRUCTURE ===\n")
cat("Class of value column:", class(dm$value), "\n")
cat("Is it a data.frame?", is.data.frame(dm$value), "\n")
cat("Is it a list?", is.list(dm$value), "\n")

if (is.data.frame(dm$value)) {
  cat("\nValue is a DATA.FRAME\n")
  cat("Columns:", paste(names(dm$value), collapse = ", "), "\n")
  cat("Rows:", nrow(dm$value), "\n")
  
  if ("type" %in% names(dm$value)) {
    cat("\nTypes found:\n")
    print(table(dm$value$type))
  }
  
} else if (is.list(dm$value)) {
  cat("\nValue is a LIST\n")
  cat("Length:", length(dm$value), "\n")
  
  # Check first few elements
  cat("\nFirst 3 elements:\n")
  for (i in 1:min(3, length(dm$value))) {
    cat("\n[[", i, "]]\n", sep = "")
    val <- dm$value[[i]]
    cat("  Class:", class(val), "\n")
    if (is.list(val)) {
      cat("  Names:", paste(names(val), collapse = ", "), "\n")
      if ("type" %in% names(val)) {
        cat("  Type:", val$type, "\n")
      }
    }
  }
}

# Try to access value elements the way the code does
cat("\n=== TESTING ACCESS METHODS ===\n")

cat("\nMethod 1: dm$value[[1]]\n")
tryCatch({
  val1 <- dm$value[[1]]
  cat("  Success! Got:", class(val1), "\n")
  if (is.list(val1) && "type" %in% names(val1)) {
    cat("  Type:", val1$type, "\n")
  }
}, error = function(e) {
  cat("  ERROR:", e$message, "\n")
})

cat("\nMethod 2: Loop through 1:nrow(dm)\n")
for (i in 1:min(3, nrow(dm))) {
  cat("  Row", i, ":\n")
  tryCatch({
    value_obj <- dm$value[[i]]
    cat("    Success! Type:", ifelse(is.list(value_obj) && "type" %in% names(value_obj), 
                                     value_obj$type, "NO TYPE"), "\n")
  }, error = function(e) {
    cat("    ERROR:", e$message, "\n")
  })
}

cat("\n=== RECOMMENDATION ===\n")
cat("Based on the structure above, we need to adjust the parsing code.\n")
cat("Please share the output of this script!\n")
