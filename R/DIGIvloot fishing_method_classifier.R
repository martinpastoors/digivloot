library(tidyverse)
library(sf)
library(lubridate)
library(zoo)

# ============================================================================
# FISHING METHOD CLASSIFIER
# Automatically identifies fishing method from GPS data using pattern analysis
# ============================================================================

# ----------------------------------------------------------------------------
# 1. LOAD AND PREPARE GPS DATA
# ----------------------------------------------------------------------------

# Load GPS data
gps_data <- read_csv('your_gps_file.csv')

# Create sf object with calculated metrics
gps_sf <- gps_data %>%
  mutate(timestamp = dmy_hm(ts)) %>%
  st_as_sf(coords = c("gps_longitude", "gps_latitude"), crs = 4326) %>%
  arrange(timestamp) %>%
  mutate(
    # Extract coordinates
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    
    # Calculate distance and speed
    distance_m = st_distance(geometry, lead(geometry), by_element = TRUE) %>% as.numeric(),
    speed_ms = distance_m / 60,
    speed_knots = speed_ms * 1.94384,
    
    # Smooth speed
    speed_smooth = rollmean(speed_knots, k = 5, fill = NA, align = "center"),
    
    # Calculate bearing
    bearing = atan2(lead(lon) - lon, lead(lat) - lat) * 180 / pi,
    bearing = (bearing + 360) %% 360,
    
    # Bearing change
    bearing_change = abs(bearing - lag(bearing)),
    bearing_change = pmin(bearing_change, 360 - bearing_change)
  )

# ----------------------------------------------------------------------------
# 2. DETECT FISHING OPERATIONS (SPEED-BASED)
# ----------------------------------------------------------------------------

# Find periods of sustained activity (not steaming or stopped)
fishing_periods <- gps_sf %>%
  filter(speed_smooth > 2.0, speed_smooth < 10.0) %>%  # Fishing speeds
  mutate(
    # Detect breaks (>20 min gap = different operation)
    time_gap = as.numeric(difftime(timestamp, lag(timestamp), units = "mins")),
    new_period = is.na(time_gap) | time_gap > 20,
    period_id = cumsum(new_period)
  ) %>%
  group_by(period_id) %>%
  summarise(
    start_time = first(timestamp),
    end_time = last(timestamp),
    duration_min = as.numeric(difftime(last(timestamp), first(timestamp), units = "mins")),
    n_points = n(),
    
    # Speed characteristics
    mean_speed = mean(speed_smooth, na.rm = TRUE),
    speed_sd = sd(speed_smooth, na.rm = TRUE),
    min_speed = min(speed_smooth, na.rm = TRUE),
    max_speed = max(speed_smooth, na.rm = TRUE),
    
    # Spatial characteristics
    start_lat = first(lat),
    start_lon = first(lon),
    end_lat = last(lat),
    end_lon = last(lon),
    start_geom = first(geometry),
    end_geom = last(geometry),
    
    .groups = "drop"
  ) %>%
  # Filter for substantial operations (>10 minutes)
  filter(duration_min > 10) %>%
  mutate(
    # Calculate return distance
    return_dist_m = st_distance(start_geom, end_geom, by_element = TRUE) %>% as.numeric()
  )

cat(sprintf("\nDetected %d potential fishing operations\n", nrow(fishing_periods)))

# ----------------------------------------------------------------------------
# 3. CALCULATE SPATIAL PATTERN FEATURES
# ----------------------------------------------------------------------------

# For each fishing period, calculate detailed spatial metrics
fishing_patterns <- fishing_periods %>%
  rowwise() %>%
  mutate(
    # Get the track for this period
    track_data = list({
      gps_sf %>%
        filter(timestamp >= start_time, timestamp <= end_time)
    }),
    
    # Count sharp turns (>60 degrees)
    n_sharp_turns = sum(track_data$bearing_change > 60, na.rm = TRUE),
    
    # Count very sharp turns (>80 degrees)
    n_very_sharp_turns = sum(track_data$bearing_change > 80, na.rm = TRUE),
    
    # Calculate path shape metrics
    lat_range = max(track_data$lat) - min(track_data$lat),
    lon_range = max(track_data$lon) - min(track_data$lon),
    
    # Aspect ratio (1.0 = square, higher = elongated)
    aspect_ratio = if_else(
      min(lat_range, lon_range) > 0,
      max(lat_range, lon_range) / min(lat_range, lon_range),
      NA_real_
    ),
    
    # Path linearity (how straight is the path?)
    # 1.0 = perfectly straight, <0.5 = very curved
    straight_line_dist = st_distance(start_geom, end_geom, by_element = TRUE) %>% as.numeric(),
    actual_path_dist = sum(track_data$distance_m, na.rm = TRUE),
    linearity = straight_line_dist / actual_path_dist,
    
    # Calculate if it forms a closed loop
    is_closed = return_dist_m < 1000,  # Returns within 1km
    
    # Speed consistency (low = steady, high = variable)
    speed_cv = speed_sd / mean_speed  # Coefficient of variation
    
  ) %>%
  ungroup() %>%
  select(-track_data)  # Remove nested data

# ----------------------------------------------------------------------------
# 4. FISHING METHOD CLASSIFICATION RULES
# ----------------------------------------------------------------------------

classify_fishing_method <- function(data) {
  data %>%
    mutate(
      # Initialize scores for each method
      score_flyshoot = 0,
      score_beam_trawl = 0,
      score_otter_trawl = 0,
      score_purse_seine = 0,
      score_gillnet = 0,
      
      # FLYSHOOT INDICATORS
      score_flyshoot = score_flyshoot + 
        case_when(
          # Duration: 20-50 minutes (strong indicator)
          duration_min >= 20 & duration_min <= 50 ~ 3,
          duration_min >= 15 & duration_min <= 60 ~ 1,
          TRUE ~ -1
        ),
      
      score_flyshoot = score_flyshoot +
        case_when(
          # Speed: 4-8 knots
          mean_speed >= 4 & mean_speed <= 8 ~ 3,
          mean_speed >= 3 & mean_speed <= 9 ~ 1,
          TRUE ~ -1
        ),
      
      score_flyshoot = score_flyshoot +
        case_when(
          # Returns to start (KEY INDICATOR)
          return_dist_m < 800 ~ 5,  # Strong evidence
          return_dist_m < 1500 ~ 3,
          return_dist_m < 3000 ~ 1,
          TRUE ~ -2
        ),
      
      score_flyshoot = score_flyshoot +
        case_when(
          # Multiple sharp turns (square pattern)
          n_very_sharp_turns >= 3 ~ 3,
          n_sharp_turns >= 3 ~ 2,
          n_sharp_turns >= 2 ~ 1,
          TRUE ~ 0
        ),
      
      score_flyshoot = score_flyshoot +
        case_when(
          # Aspect ratio close to square
          aspect_ratio >= 1.0 & aspect_ratio <= 3.0 ~ 2,
          aspect_ratio >= 1.0 & aspect_ratio <= 5.0 ~ 1,
          TRUE ~ 0
        ),
      
      # BEAM TRAWL INDICATORS
      score_beam_trawl = score_beam_trawl +
        case_when(
          # Duration: 60-180 minutes
          duration_min >= 60 & duration_min <= 180 ~ 3,
          duration_min >= 40 & duration_min <= 240 ~ 1,
          TRUE ~ 0
        ),
      
      score_beam_trawl = score_beam_trawl +
        case_when(
          # Speed: 4-6 knots
          mean_speed >= 4 & mean_speed <= 6 ~ 3,
          mean_speed >= 3 & mean_speed <= 7 ~ 1,
          TRUE ~ 0
        ),
      
      score_beam_trawl = score_beam_trawl +
        case_when(
          # High linearity (straight tracks)
          linearity >= 0.7 ~ 3,
          linearity >= 0.5 ~ 1,
          TRUE ~ 0
        ),
      
      score_beam_trawl = score_beam_trawl +
        case_when(
          # Doesn't return to start
          return_dist_m > 3000 ~ 2,
          return_dist_m > 1500 ~ 1,
          TRUE ~ -1
        ),
      
      score_beam_trawl = score_beam_trawl +
        case_when(
          # Few sharp turns
          n_sharp_turns <= 2 ~ 2,
          n_sharp_turns <= 4 ~ 1,
          TRUE ~ 0
        ),
      
      # OTTER TRAWL INDICATORS
      score_otter_trawl = score_otter_trawl +
        case_when(
          # Duration: 60-240 minutes (longer than beam)
          duration_min >= 60 & duration_min <= 240 ~ 3,
          duration_min >= 40 & duration_min <= 300 ~ 1,
          TRUE ~ 0
        ),
      
      score_otter_trawl = score_otter_trawl +
        case_when(
          # Speed: 3-5 knots (slower than beam)
          mean_speed >= 3 & mean_speed <= 5 ~ 3,
          mean_speed >= 2 & mean_speed <= 6 ~ 1,
          TRUE ~ 0
        ),
      
      score_otter_trawl = score_otter_trawl +
        case_when(
          # Moderate linearity (less straight than beam)
          linearity >= 0.4 & linearity <= 0.7 ~ 2,
          linearity >= 0.3 & linearity <= 0.8 ~ 1,
          TRUE ~ 0
        ),
      
      score_otter_trawl = score_otter_trawl +
        case_when(
          # Doesn't return
          return_dist_m > 2000 ~ 2,
          TRUE ~ 0
        ),
      
      # PURSE SEINE INDICATORS
      score_purse_seine = score_purse_seine +
        case_when(
          # Duration: 10-30 minutes (quick!)
          duration_min >= 10 & duration_min <= 30 ~ 3,
          duration_min >= 8 & duration_min <= 40 ~ 1,
          TRUE ~ 0
        ),
      
      score_purse_seine = score_purse_seine +
        case_when(
          # Speed: 6-12 knots (fast!)
          mean_speed >= 6 & mean_speed <= 12 ~ 3,
          mean_speed >= 5 & mean_speed <= 13 ~ 1,
          TRUE ~ 0
        ),
      
      score_purse_seine = score_purse_seine +
        case_when(
          # Returns to start (closes the circle)
          return_dist_m < 500 ~ 4,
          return_dist_m < 1000 ~ 2,
          TRUE ~ 0
        ),
      
      score_purse_seine = score_purse_seine +
        case_when(
          # Low linearity (curved path)
          linearity <= 0.3 ~ 3,
          linearity <= 0.5 ~ 1,
          TRUE ~ 0
        ),
      
      score_purse_seine = score_purse_seine +
        case_when(
          # Few sharp turns (smooth curve)
          n_sharp_turns <= 2 ~ 2,
          TRUE ~ 0
        ),
      
      # GILLNET INDICATORS
      score_gillnet = score_gillnet +
        case_when(
          # Duration: 30-120 minutes
          duration_min >= 30 & duration_min <= 120 ~ 3,
          duration_min >= 20 & duration_min <= 180 ~ 1,
          TRUE ~ 0
        ),
      
      score_gillnet = score_gillnet +
        case_when(
          # Speed: 2-4 knots (slow)
          mean_speed >= 2 & mean_speed <= 4 ~ 3,
          mean_speed >= 1.5 & mean_speed <= 5 ~ 1,
          TRUE ~ 0
        ),
      
      score_gillnet = score_gillnet +
        case_when(
          # High linearity (straight lines)
          linearity >= 0.8 ~ 3,
          linearity >= 0.6 ~ 1,
          TRUE ~ 0
        ),
      
      score_gillnet = score_gillnet +
        case_when(
          # Very few turns
          n_sharp_turns <= 1 ~ 2,
          TRUE ~ 0
        ),
      
      # DETERMINE CLASSIFICATION
      max_score = pmax(score_flyshoot, score_beam_trawl, score_otter_trawl, 
                       score_purse_seine, score_gillnet),
      
      fishing_method = case_when(
        max_score < 5 ~ "Unknown",
        score_flyshoot == max_score ~ "Flyshoot",
        score_beam_trawl == max_score ~ "Beam Trawl",
        score_otter_trawl == max_score ~ "Otter Trawl",
        score_purse_seine == max_score ~ "Purse Seine",
        score_gillnet == max_score ~ "Gillnet",
        TRUE ~ "Uncertain"
      ),
      
      # Confidence based on score margin
      score_margin = max_score - pmax(
        if_else(score_flyshoot != max_score, score_flyshoot, -999),
        if_else(score_beam_trawl != max_score, score_beam_trawl, -999),
        if_else(score_otter_trawl != max_score, score_otter_trawl, -999),
        if_else(score_purse_seine != max_score, score_purse_seine, -999),
        if_else(score_gillnet != max_score, score_gillnet, -999)
      ),
      
      confidence = case_when(
        max_score < 5 ~ "Very Low",
        score_margin >= 5 ~ "High",
        score_margin >= 3 ~ "Medium",
        score_margin >= 1 ~ "Low",
        TRUE ~ "Very Low"
      )
    )
}

# Apply classification
classified_operations <- fishing_patterns %>%
  classify_fishing_method()

# ----------------------------------------------------------------------------
# 5. DISPLAY RESULTS
# ----------------------------------------------------------------------------

cat("\n============================================\n")
cat("FISHING METHOD CLASSIFICATION RESULTS\n")
cat("============================================\n\n")

# Summary by method
summary_by_method <- classified_operations %>%
  count(fishing_method, confidence) %>%
  arrange(fishing_method, confidence)

cat("Summary:\n")
print(summary_by_method)

cat("\n\nDetailed Classifications:\n")
cat("────────────────────────────────────────────────────────────────────\n")

for (i in 1:nrow(classified_operations)) {
  op <- classified_operations[i, ]
  
  cat(sprintf("\nOperation %d: %s (%s confidence)\n", 
              i, op$fishing_method, op$confidence))
  cat(sprintf("  Time:        %s to %s\n", 
              format(op$start_time, "%Y-%m-%d %H:%M"),
              format(op$end_time, "%H:%M")))
  cat(sprintf("  Duration:    %.0f minutes\n", op$duration_min))
  cat(sprintf("  Speed:       %.2f knots (±%.2f)\n", op$mean_speed, op$speed_sd))
  cat(sprintf("  Return dist: %.0f meters\n", op$return_dist_m))
  cat(sprintf("  Sharp turns: %d\n", op$n_sharp_turns))
  cat(sprintf("  Linearity:   %.2f\n", op$linearity))
  
  # Show scores
  cat("  Scores: ")
  cat(sprintf("Flyshoot=%d, Beam=%d, Otter=%d, Purse=%d, Gillnet=%d\n",
              op$score_flyshoot, op$score_beam_trawl, op$score_otter_trawl,
              op$score_purse_seine, op$score_gillnet))
}

# ----------------------------------------------------------------------------
# 6. AGGREGATE VESSEL CLASSIFICATION
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("OVERALL VESSEL CLASSIFICATION\n")
cat("============================================\n\n")

# Most common method (weighted by confidence)
vessel_classification <- classified_operations %>%
  filter(confidence %in% c("High", "Medium")) %>%
  count(fishing_method, wt = duration_min) %>%
  arrange(desc(n)) %>%
  slice(1)

if (nrow(vessel_classification) > 0) {
  cat(sprintf("Primary fishing method: %s\n", vessel_classification$fishing_method))
  cat(sprintf("Total time using this method: %.0f minutes\n", vessel_classification$n))
  
  # Calculate percentage
  total_time <- sum(classified_operations$duration_min)
  pct <- (vessel_classification$n / total_time) * 100
  cat(sprintf("Percentage of fishing time: %.1f%%\n", pct))
} else {
  cat("Unable to determine primary fishing method with confidence\n")
}

# Show distribution
cat("\n\nDistribution of operations by method:\n")
method_dist <- classified_operations %>%
  group_by(fishing_method) %>%
  summarise(
    n_operations = n(),
    total_minutes = sum(duration_min),
    avg_duration = mean(duration_min),
    avg_speed = mean(mean_speed),
    .groups = "drop"
  ) %>%
  arrange(desc(n_operations))

print(method_dist)

# ----------------------------------------------------------------------------
# 7. VISUALIZATION
# ----------------------------------------------------------------------------

library(ggplot2)

# Plot 1: Operations classified by method
p1 <- ggplot(classified_operations, 
             aes(x = mean_speed, y = duration_min, color = fishing_method, 
                 size = confidence)) +
  geom_point(alpha = 0.7) +
  scale_size_manual(values = c("Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 5)) +
  labs(title = "Fishing Operations Classification",
       subtitle = "Based on speed and duration patterns",
       x = "Mean Speed (knots)",
       y = "Duration (minutes)",
       color = "Fishing Method",
       size = "Confidence") +
  theme_minimal() +
  theme(legend.position = "right")

# Add reference regions for different methods
p1 <- p1 +
  annotate("rect", xmin = 4, xmax = 8, ymin = 20, ymax = 50,
           alpha = 0.1, fill = "red") +
  annotate("text", x = 6, y = 55, label = "Flyshoot\nzone", size = 3, color = "red")

ggsave("fishing_method_classification.pdf", p1, width = 12, height = 8)

# Plot 2: Return distance vs linearity
p2 <- ggplot(classified_operations,
             aes(x = return_dist_m, y = linearity, color = fishing_method)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  geom_vline(xintercept = 1000, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  labs(title = "Spatial Pattern Analysis",
       subtitle = "Flyshoot operations typically return to start (left side) with moderate linearity",
       x = "Return Distance to Start (meters, log scale)",
       y = "Path Linearity (1.0 = straight line)",
       color = "Fishing Method") +
  theme_minimal()

ggsave("spatial_pattern_analysis.pdf", p2, width = 12, height = 8)

# ----------------------------------------------------------------------------
# 8. SAVE RESULTS
# ----------------------------------------------------------------------------

write_csv(classified_operations, "fishing_operations_classified.csv")

cat("\n\n============================================\n")
cat("FILES SAVED\n")
cat("============================================\n")
cat("✓ fishing_operations_classified.csv\n")
cat("✓ fishing_method_classification.pdf\n")
cat("✓ spatial_pattern_analysis.pdf\n")

cat("\n\n============================================\n")
cat("CLASSIFICATION COMPLETE\n")
cat("============================================\n\n")

# ----------------------------------------------------------------------------
# 9. CLASSIFICATION REPORT FUNCTION
# ----------------------------------------------------------------------------

# Function to explain why a classification was made
explain_classification <- function(operation_row) {
  cat(sprintf("\n=== Classification Explanation for Operation %d ===\n", 
              operation_row$period_id))
  cat(sprintf("Classified as: %s (%s confidence)\n\n", 
              operation_row$fishing_method, operation_row$confidence))
  
  cat("Key indicators:\n")
  
  if (operation_row$fishing_method == "Flyshoot") {
    if (operation_row$duration_min >= 20 & operation_row$duration_min <= 50) {
      cat("  ✓ Duration (", operation_row$duration_min, " min) matches flyshoot pattern\n")
    }
    if (operation_row$mean_speed >= 4 & operation_row$mean_speed <= 8) {
      cat("  ✓ Speed (", round(operation_row$mean_speed, 1), " kn) typical for flyshoot\n")
    }
    if (operation_row$return_dist_m < 1500) {
      cat("  ✓ Returns to start (", round(operation_row$return_dist_m), " m) - key flyshoot indicator\n")
    }
    if (operation_row$n_sharp_turns >= 2) {
      cat("  ✓ Multiple sharp turns (", operation_row$n_sharp_turns, ") suggests square pattern\n")
    }
  }
  
  cat("\nAll scores:\n")
  cat(sprintf("  Flyshoot:    %2d\n", operation_row$score_flyshoot))
  cat(sprintf("  Beam Trawl:  %2d\n", operation_row$score_beam_trawl))
  cat(sprintf("  Otter Trawl: %2d\n", operation_row$score_otter_trawl))
  cat(sprintf("  Purse Seine: %2d\n", operation_row$score_purse_seine))
  cat(sprintf("  Gillnet:     %2d\n", operation_row$score_gillnet))
}

# Example: Explain first classification
if (nrow(classified_operations) > 0) {
  explain_classification(classified_operations[1, ])
}
