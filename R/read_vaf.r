# ==============================================================================
# Read VAF files
#
# 22/11/2025 first coding
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)
library(patchwork)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/Users/MartinPastoors/Martin Pastoors/DIGIvloot - data - data/tripdata/SCH99/brandstof"
spatialdir <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
gisdir     <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/GIS"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"
  
rect_lr_sf  <- 
  loadRData(file.path(spatialdir, "rect_lr_sf.RData"))

world_mr_sf <- 
  loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()

eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData")) %>% 
  bind_cols(data.frame(valid = sf::st_is_valid(.), reason=TRUE)) %>% 
  filter(valid == TRUE) 
  
mpa         <- 
  loadRData(file.path(spatialdir, "MPANLD4.RData")) %>% 
  filter(SITE_NAME %notin% c("Bruine Bank 265")) %>% 
  mutate(land="NL")

wind        <- 
  loadRData(file.path(spatialdir, "wind_sf.RData")) %>% 
  lowcase() %>% 
  filter(type %in% c("Bestaand","Goedgekeurd","Onder Constructie")) %>% 
  filter(land != "NL") %>% 
  dplyr::select(naam, land, type, geometry) %>% 
  mutate(code = substr(naam, 1, 12))
  
windnl      <- loadRData(file.path(spatialdir, "20241023 nl_wind_nwp_sf.RData")) %>% 
  dplyr::select(code=windgebied, km2, naam=opmerking, plan=windgebie0, geometry) %>% 
  mutate(land = "NL")

windcomb <- bind_rows(wind, windnl)

k <- 
  loadRData(file.path(flyshootdir, "kisten.RData")) %>% 
  filter(vessel == "SCH99")

# k %>% 
#   mutate(month = lubridate::month(datetime)) %>% 
#   group_by(month) %>% 
#   filter(lubridate::year(datetime) >= 2024) %>% 
#   summarise(
#     firsthour = min(lubridate::hour(datetime)),
#     lasthour = max(lubridate::hour(datetime))
#   ) %>% 
#   View()

# k %>% filter(lubridate::hour(datetime) == 0) %>% View()

# Haversine distance function
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000  # Earth radius in meters
  lat1_rad <- lat1 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lon2_rad <- lon2 * pi / 180
  
  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad
  
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  R * c
}

# Calculate boxiness for a segment
calculate_boxiness <- function(bearings) {
  bearings <- bearings[!is.na(bearings)]
  if (length(bearings) < 4) return(0)
  
  # Count points in each cardinal direction (N, E, S, W)
  north <- sum((bearings >= 315) | (bearings < 45))
  east <- sum(bearings >= 45 & bearings < 135)
  south <- sum(bearings >= 135 & bearings < 225)
  west <- sum(bearings >= 225 & bearings < 315)
  
  total <- north + east + south + west
  if (total == 0) return(0)
  
  # Calculate entropy-based boxiness
  props <- c(north, east, south, west) / total
  props <- props[props > 0]
  entropy <- -sum(props * log(props))
  max_entropy <- log(4)
  
  entropy / max_entropy
}


# data
filelist <- list.files(
  path=file.path(datadir),
  pattern="csv",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  gps_data <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {

  myvessel <- stringr::word(basename(filelist[i]), start=1, sep="_")
  
    print(paste("haul", filelist[i]))
    
    gps_data  <-
      bind_rows(
        gps_data,
        read.csv(filelist[i], 
                 # colClasses = "character",
                 stringsAsFactors = FALSE)  %>%
          lowcase() %>%
          mutate(
            filename    = basename(filelist[i]),
            vessel      = myvessel, 
            timestamp   = lubridate::ymd_hms(ts),
            week        = lubridate::isoweek(timestamp),
            hour        = lubridate::hour(timestamp),
            hour2       = ifelse(hour != lag(hour), hour, as.numeric(NA)),
            minutes     = interval(timestamp, lead(timestamp)) %/% minutes(1),
            fuel_rate   = engine1liters + engine2liters+engine3liters,
            fuel_liters = minutes * fuel_rate / 60,
            fuel_rate_smooth = zoo::rollmean(fuel_rate, k = 5, fill = "extend", align = "center")
          ) %>% 
          rename(
            lat = gpslatitude,
            lon = gpslongitude
          ) %>%
          filter(timestamp %notin% gps_data$timestamp)
      ) # end of bind_rows
  } # end of for loop    
} # end of not empty filelist


# results <- detect_flyshoot_hauls(gps_data)

gps_sf <- 
  gps_data %>% 
  sf::st_as_sf(coords = c("lon","lat"), remove = FALSE, crs=4326) %>% 
  group_by(filename) %>% 
  mutate(
    # Calculate distance and speed
    distance_m = st_distance(geometry, lead(geometry), by_element = TRUE) %>% as.numeric(),
    
    # Distance in meters
    # dist_m = haversine_distance(lat, lon, lead(lat), lead(lon)),
    
    speed_ms   = distance_m / (minutes*60),  # m/s (1 minute intervals)
    speed_knots = speed_ms * 1.94384,
    
    # Smooth speed
    speed_smooth = zoo::rollmean(speed_knots, k = 5, fill = "extend", align = "center"),
    
    # Calculate bearing (direction of travel)
    bearing = atan2(lead(lon) - lon, lead(lat) - lat) * 180 / pi,
    bearing = (bearing + 360) %% 360,
    
    # Bearing change (absolute)
    bearing_change = abs(c(NA, diff(bearing))),
    bearing_change = if_else(bearing_change > 180, 360 - bearing_change, bearing_change),
    bearing_change = as.integer(bearing_change),
    
    bearing_change_smooth = zoo::rollmean(bearing_change, k = 5, fill = NA, align = "center"),
    
    # identify active periods
    # Active if moving (speed > 2.5 knots) OR high fuel (> 70 L/min)
    # But not if clearly stopped (speed < 1 AND fuel < 30)
    is_active = ((speed_smooth > 2.5 | fuel_rate > 70) &
                   !(speed_smooth < 1.0 & fuel_rate < 30)),
    
    # Create segment IDs for consecutive active periods
    segment_raw = cumsum(is_active != lag(is_active, default = FALSE)),
    segment = dplyr::if_else(is_active, segment_raw, -1L),

    # Classify activity
    activity = case_when(
      speed_smooth > 4.5 ~ "setting",
      speed_smooth > 2.5 ~ "towing",
      speed_smooth > 0.5 ~ "slow",
      TRUE ~ "stopped"
    ),
    
    # Classify hauling
    activity2 = case_when(
      speed_smooth < 2 & fuel_rate_smooth > 50 ~ "hauling",
      speed_smooth > 5 & fuel_rate_smooth > 50 ~ "setting/steaming",
      TRUE ~ "other"
    )
      
  ) %>% 
  mutate(
    speed_change = abs(c(NA, diff(speed_smooth))),
    activity2 = ifelse(activity2 != lag(activity2) & activity2 != lead(activity2), lag(activity2), activity2)
  )

cat(sprintf("  Active points: %d / %d\n", sum(gps_sf$is_active, na.rm=TRUE), nrow(gps_sf)))
cat(sprintf("  Preliminary segments: %d\n\n", 
            n_distinct(gps_sf$segment[gps_sf$segment >= 0])))

time1 <- "2025-10-28 07:30:00"; time2 <- "2025-10-28 18:30:00"
gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% View()

ggplot() +
  theme_publication() + 
  
  # geom_sf(data=gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2), segment >= 0),
  #         aes(colour=stringr::str_pad(segment, width=3, pad="0")), size=1) +
  
  # geom_path(data=gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)), 
  #           aes(x=lon, y=lat, colour=stringr::str_pad(segment_raw, width=3, pad="0")), alpha=0.5) +
  
  geom_sf_text(data=gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)), 
               aes(label=as.integer(lubridate::minute(timestamp))), alpha=0.5) +
  
  geom_path(data=gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)), 
            aes(x=lon, y=lat, colour=activity2, group=(1)), 
            size=1) +
  
  coord_sf(xlim=c(0.55,0.66), ylim=c(50.53, 50.57))


# speed over time
p1 <- 
  gps_sf %>% 
  filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% 

  ggplot(aes(x = timestamp)) +

  theme_publication() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +

  # Raw speed (semi-transparent)
  # geom_line(aes(y = speed_knots), color = "blue", alpha = 0.3, linewidth = 0.5) +
  
  # Smoothed speed
  # geom_line(aes(y = speed_smooth, colour=stringr::str_pad(segment_raw, width=3, pad="0")), linewidth = 1) +
  geom_line(aes(y = speed_smooth, colour=activity2, group=(1)), size = 1) +
  
  # Threshold lines
  geom_hline(yintercept = 2.0, color = "red", linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 3.0, color = "green", linetype = "dashed", alpha = 0.5) +
  
  # Add haul periods as background shading
  # geom_rect(data = haul_ids, 
  #           aes(xmin = setting_start, xmax = setting_end, 
  #               ymin = -Inf, ymax = Inf),
  #           fill = haul_colors, alpha = 0.1, inherit.aes = FALSE) +
  
  labs(
    title = "Vessel Movement Analysis - Working Backwards from Catch Processing",
    y = "Speed (knots)"
  ) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  )

# Fuel consumption
p2 <- 
  gps_sf %>% 
  filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% 
  mutate(
    fuel_rate_smooth = zoo::rollmean(fuel_rate, k = 10, fill = "extend", align = "center")  
  ) %>% 
  
  ggplot(aes(x = timestamp)) +
  theme_publication() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  
  geom_line(aes(y = fuel_rate), color = "orange", alpha = 0.3, linewidth = 0.5) +
  
  # Smoothed speed
  # geom_line(aes(y = fuel_rate_smooth), color = "orange", linewidth = 1) +
  # geom_line(aes(y = fuel_rate_smooth, colour=stringr::str_pad(segment_raw, width=3, pad="0")), linewidth = 1) +
  geom_line(aes(y = fuel_rate_smooth, colour=activity2, group=(1)), size = 1) +
  
  # Threshold lines
  geom_hline(yintercept = 30.0, color = "red", linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 50.0, color = "green", linetype = "dashed", alpha = 0.5) +
  
  # Add haul periods
  # geom_rect(data = haul_ids, 
  #           aes(xmin = hauling_start, xmax = hauling_end, 
  #               ymin = -Inf, ymax = Inf),
  #           fill = haul_colors, alpha = 0.15, inherit.aes = FALSE) +
  
  labs(y = "Fuel (liters/min)") +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) 

# bearing
p3 <- 
  gps_sf %>% 
  filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% 
  
  ggplot(aes(x = timestamp)) +
  theme_publication() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  # Raw bearing change (semi-transparent)
  # geom_line(aes(y = bearing_change), color = "purple", alpha = 0.3, linewidth = 0.5) +
  # Smoothed bearing change
  geom_line(aes(y = bearing_change_smooth), color = "purple", linewidth = 1) +
  # Corner threshold
  geom_hline(yintercept = 20, color = "red", linetype = "dashed", alpha = 0.5) +
  
  labs(y = "Bearing change (°)") +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) 

# catch processing
p4 <- 
  k %>% 
  rename(timestamp = datetime, weight=gewicht) %>% 
  mutate(timestamp = timestamp - lubridate::hours(1)) %>% 
  filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% 
  mutate(weight_smooth = zoo::rollmean(weight, k = 5, fill = NA, align = "center")) %>% 

  ggplot(aes(x = timestamp)) +
  theme_publication() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  # Raw catch
  geom_point(aes(y = weight), color = "red", alpha = 0.5) +
  # Smoothed catch change
  # geom_line(aes(y = weight_smooth), color = "red", linewidth = 1) +

  labs(y = "Catch processed") +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) 

# speed and catch
p5 <- 
  k %>% 
  rename(timestamp = datetime, weight=gewicht) %>% 
  mutate(timestamp = timestamp - lubridate::hours(1)) %>% 
  filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)) %>% 

  ggplot(aes(x = timestamp)) +
  theme_publication() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  # speed smooth
  geom_line(data = gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)),  
              aes(y = speed_smooth), color = "blue", alpha = 0.5) +
  
  # fuel
  geom_line(data = gps_sf %>% filter(timestamp >= ymd_hms(time1), timestamp <= ymd_hms(time2)),  
            aes(y = fuel/15), color = "purple", alpha = 0.5) +
  
  # catch
  geom_point(aes(y = weight/3), color = "red", alpha = 0.5) +
  scale_y_continuous(
    name = "speed (blue, knots) and fuel (purple, relative)",
    sec.axis = sec_axis(~ . * 3, name = "catch (red, kg)")
  ) +
  labs(y = "Catch processed") +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) 


p1 / p2 +
  plot_layout(heights = c(1, 1))  # Give plot 4 slightly more space for legend

p1 / p2 / p3 / p4 +
  plot_layout(heights = c(1, 1, 1, 1.2))  # Give plot 4 slightly more space for legend

gps_sf %>% filter(!is.na(hour2)) %>% View()
gps_sf %>% filter(speed_knots> 12) %>% View()
gps_sf %>% filter(segment==716) %>% View()
gps_sf %>% filter(timestamp >= ymd_hms("2025-09-17 23:55:01"), timestamp <= ymd_hms("2025-09-18 00:05:01")) %>% View()


ggplot() +
  theme_publication() +

  geom_sf(data=gps_sf %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
          aes(colour=str_pad(segment, width = 3, pad="0")), alpha=0.5, size=1) +
  geom_path(data=gps_sf %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
          aes(x=lon, y=lat, colour=str_pad(segment, width = 3, pad="0")), alpha=0.5) +
  # geom_sf(data=gps_sf %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
  #         aes(colour=str_pad(hour, width = 2, pad="0")), alpha=0.5, size=1) +
  # geom_path(data=gps_sf %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
  #           aes(x=lon, y=lat, colour=str_pad(hour, width = 2, pad="0")), alpha=0.5) +
  coord_sf(xlim=c(0.5,0.7), ylim=c(50.5, 50.6)) +
  guides(colour = guide_legend(nrow = 1, title="hour")) +
  labs(title="SCH99 28/10/2025")




gps_sf %>%
  sf::st_drop_geometry() %>% 
  group_by(filename) %>% 
  mutate(
    minutes    = interval(timestamp, lead(timestamp)) %/% minutes(1),
  ) %>% 
  summarise(
    startdate = min(as.Date(timestamp)),
    enddate   = max(as.Date(timestamp)),
    # fuel          = sum(fuel, na.rm=TRUE),
    # engine1liters = sum(engine1liters, na.rm=TRUE),
    # engine2liters = sum(engine2liters, na.rm=TRUE),
    # engine3liters = sum(engine3liters, na.rm=TRUE),
    fuel_liters         = sum(fuel_liters, na.rm=TRUE),
    distance_nm         = 0.0005399568 * sum(distance_m, na.rm=TRUE)
  ) %>% 
  dplyr::select(-filename) %>% 
  ggplot(aes(x=distance_nm, y=fuel_liters)) +
  theme_publication() +
  geom_point() +
  geom_smooth(se=FALSE, span=1) +
  expand_limits(x=0, y=0)

gps_sf %>%
  sf::st_drop_geometry() %>% 
  group_by(filename) %>% 
  mutate(
    minutes    = interval(timestamp, lead(timestamp)) %/% minutes(1),
  ) %>% 
  summarise(
    startdate = min(as.Date(timestamp)),
    enddate   = max(as.Date(timestamp)),
    fuel_liters         = sum(fuel_liters, na.rm=TRUE),
    distance_nm         = 0.0005399568 * sum(distance_m, na.rm=TRUE)
  ) %>% 
  pander::pandoc.table(.,
                       style = "simple",
                       split.tables=400, 
                       justify = "left",
                       missing=".",
                       round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

setting_mask <- 
  gps_data$datetime >= haul$buoy_start & 
  gps_data$datetime <= haul$buoy_pickup

gps_data$haul_id[setting_mask] <- haul_id
gps_data$phase[setting_mask] <- "setting"

segments <- 
  gps_sf %>%
  filter(segment >= 0) %>%
  group_by(segment) %>%
  summarise(
    start_time = min(timestamp),
    end_time = max(timestamp),
    duration_min = as.numeric(difftime(max(timestamp), min(timestamp), units = "mins")),
    n_points = n(),
    
    # Spatial metrics
    start_lat = first(lat),
    start_lon = first(lon),
    end_lat = last(lat),
    end_lon = last(lon),
    
    # Return distance
    return_dist_m = haversine_distance(start_lat, start_lon, end_lat, end_lon),
    
    # Movement metrics
    mean_speed = mean(speed_smooth, na.rm = TRUE),
    max_speed = max(speed_smooth, na.rm = TRUE),
    mean_fuel = mean(engine1liters, na.rm = TRUE),
    total_distance = sum(distance_m, na.rm = TRUE),
    
    # Path complexity
    mean_bearing_change = mean(bearing_change, na.rm = TRUE),
    n_sharp_turns = sum(bearing_change > 70, na.rm = TRUE),
    
    # Boxiness
    boxiness = calculate_boxiness(bearing),
    
    .groups = "drop"
  ) %>%
  # Filter minimum duration
  filter(duration_min >= 15) %>%
  # Calculate loop closure
  mutate(
    loop_closure = 1 - (return_dist_m / pmax(total_distance, 1))
  )

cat(sprintf("  Analyzed %d segments\n\n", nrow(segments)))





bbox <- sf::st_bbox(gps_sf)
xlim <- c(bbox$xmin, bbox$xmax)
ylim <- c(bbox$ymin, bbox$ymax)

# Function to find setting period around a reference time
find_setting_period <- function(ref_time, gps_data, search_window_min = 30) {
  
  # Search from 30 min before to 30 min after reference time
  window_data <- gps_data %>%
    filter(
      timestamp >= ref_time - minutes(search_window_min),
      timestamp <= ref_time + minutes(search_window_min)
    )
  
  # Find when speed increases above 4.0 knots (setting starts)
  setting_start_candidates <- window_data %>%
    filter(speed_smooth > 4.0) %>%
    arrange(timestamp)
  
  if (nrow(setting_start_candidates) == 0) return(NULL)
  
  # Get the first high-speed point as potential start
  set_start <- setting_start_candidates %>% slice(1) %>% pull(timestamp)
  
  # Find continuous high-speed period
  setting_data <- gps_data %>%
    filter(
      timestamp >= set_start,
      timestamp <= set_start + minutes(60)  # Max 60 min setting
    ) %>%
    mutate(
      is_setting = speed_smooth > 4.0,
      # Allow brief slowdowns
      setting_block = cumsum(is.na(is_setting) | (!is_setting & lag(is_setting, default = TRUE)))
    ) %>%
    filter(setting_block == 1)  # First continuous block
  
  if (nrow(setting_data) < 15) return(NULL)  # At least 15 minutes
  
  set_end <- setting_data %>% slice(n()) %>% pull(timestamp)
  
  tibble(
    set_start = set_start,
    set_end = set_end,
    duration_min = as.numeric(difftime(set_end, set_start, units = "mins")),
    n_points = nrow(setting_data),
    mean_speed = mean(setting_data$speed_smooth, na.rm = TRUE),
    reference_time = ref_time,
    time_diff_from_ref = as.numeric(difftime(set_start, ref_time, units = "mins"))
  )
}

# METHOD 1: Speed-based detection
# Find sustained high-speed periods (setting operations)

setting_periods <- 
  gps_sf %>%
  filter(activity == "setting") %>%
  mutate(
    # Detect breaks in setting periods (>10 min gap)
    time_gap = as.numeric(difftime(timestamp, lag(timestamp), units = "mins")),
    new_period = is.na(time_gap) | time_gap > 10,
    period_id = cumsum(new_period)
  ) %>%
  group_by(period_id) %>%
  summarise(
    set_start = first(timestamp),
    set_end = last(timestamp),
    duration_min = as.numeric(difftime(last(timestamp), first(timestamp), units = "mins")),
    n_points = n(),
    mean_speed = mean(speed_smooth, na.rm = TRUE),
    mean_fuel = mean(fuel_rate, na.rm = TRUE),
    # Calculate return distance
    start_geom = first(geometry),
    end_geom = last(geometry),
    .groups = "drop"
  ) %>%
  mutate(
    return_dist_m = st_distance(start_geom, end_geom, by_element = TRUE) %>% as.numeric()
  ) %>%
  # Filter for typical flyshoot patterns
  filter(
    duration_min > 18,          # Minimum setting duration
    duration_min < 55,          # Maximum setting duration
    return_dist_m < 1500        # Returns close to start (square pattern)
  ) %>%
  mutate(haul_id = row_number()) %>%
  st_drop_geometry()

cat(sprintf("Found %d potential hauls\n\n", nrow(setting_periods)))

# METHOD 2: Validate with fuel consumption
# High fuel consumption should align with setting periods

setting_periods <- setting_periods %>%
  mutate(
    fuel_validation = mean_fuel > quantile(gps_sf$fuel_rate, 0.6, na.rm = TRUE),
    confidence = case_when(
      fuel_validation & return_dist_m < 500 ~ "high",
      fuel_validation | return_dist_m < 800 ~ "medium",
      TRUE ~ "low"
    )
  )

# Find hauling periods for each haul
hauls_identified <- setting_periods %>%
  rowwise() %>%
  mutate(
    haul_start = {
      after_set <- gps_sf %>%
        filter(
          timestamp > set_end,
          timestamp < set_end + minutes(90),
          speed_smooth < 2.0
        )
      
      if (nrow(after_set) > 3) {
        after_set %>% slice(1) %>% pull(timestamp)
      } else {
        NA_POSIXct_
      }
    },
    tow_duration_min = if_else(
      !is.na(haul_start),
      as.numeric(difftime(haul_start, set_end, units = "mins")),
      NA_real_
    )
  ) %>%
  ungroup()

# Display results
cat("Detected Hauls:\n")
cat("─────────────────────────────────────────────────────────────\n")

for (i in 1:nrow(hauls_identified)) {
  haul <- hauls_identified[i, ]
  cat(sprintf("Haul %d (confidence: %s)\n", haul$haul_id, haul$confidence))
  cat(sprintf("  Start:     %s\n", format(haul$set_start, "%Y-%m-%d %H:%M")))
  cat(sprintf("  End:       %s\n", format(haul$set_end, "%Y-%m-%d %H:%M")))
  cat(sprintf("  Duration:  %.0f minutes\n", haul$duration_min))
  cat(sprintf("  Speed:     %.2f knots\n", haul$mean_speed))
  cat(sprintf("  Fuel:      %.1f L/min\n", haul$mean_fuel))
  cat(sprintf("  Return:    %.0f meters\n", haul$return_dist_m))
  
  if (!is.na(haul$haul_start)) {
    cat(sprintf("  Haul at:   %s (tow: %.0f min)\n", 
                format(haul$haul_start, "%H:%M"), haul$tow_duration_min))
  }
  cat("\n")
}



# janitor::compare_df_cols(pefa, e)

# skimr::skim(pefa)

ggplot() +
  theme_publication() +

  geom_sf(data=rect_lr_sf, fill=NA, colour="lightgray") +
  geom_sf(data=world_mr_sf) +
  geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
  
  # fuel
  geom_sf(data=gps_sf, aes(size=fuel, colour=as.character(week)), alpha=0.5) +
  scale_size_continuous(range = c(0.1, 0.5)) +
  coord_sf(xlim=xlim, ylim=ylim)  +
  facet_wrap(~week, ncol=4)


ggplot() +
  theme_publication() +
  
  geom_sf(data=rect_lr_sf, fill=NA, colour="lightgray") +
  geom_sf(data=world_mr_sf) +
  geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
  
  # fuel
  geom_sf(data=gps_sf %>% filter(week==45), aes(size=fuel, colour=as.character(week)), alpha=0.5) +
  scale_size_continuous(range = c(0.1, 0.5)) +
  coord_sf(xlim=c(-1,2), ylim=ylim)

ggplot() +
  theme_publication() +
  theme(legend.position="none") +
  geom_sf(data=rect_lr_sf, fill=NA, colour="lightgray") +
  geom_sf(data=world_mr_sf) +
  geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
  
  # fuel
  geom_sf(data=gps_sf %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
          aes(size=fuel, colour=str_pad(hour, width = 2, pad="0")), alpha=0.5) +
  scale_size_continuous(range = c(0.1, 0.5)) +
  coord_sf(xlim=c(0.5,0.7), ylim=c(50.5, 50.6))

ggplot() +
  theme_publication() +
  geom_point(data=gps_data %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
             aes(x=timestamp, y=fuel, colour=str_pad(hour, width = 2, pad="0"))) +
  geom_line(data=gps_data %>% filter(as.Date(timestamp)==dmy("28/10/2025")), aes(x=timestamp, y=fuel)) 
  

# hist(gps_data$tdiff)
# gps_data %>% filter(tdiff > 1) %>% View()

gps_data %>% 
  group_by(vessel, week) %>% 
  summarise(fuel = sum(fuel, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=week, y=fuel)) +
  theme_publication() +
  geom_line() +
  geom_point() +
  expand_limits(y=0)

gps_sf %>% 
  group_by(vessel, week) %>% 
  summarise(
    fuel = sum(fuel, na.rm=TRUE),
    distance_km = sum(distance_m, na.rm=TRUE)/1000
  ) %>% 
  
  ggplot(aes(x=distance_km, y=fuel)) +
  theme_publication() +
  geom_line() +
  geom_point() +
  expand_limits(y=0)


library(sf)
library(dplyr)
library(lubridate)

# First, calculate turning angles and speed
library(sf)
library(dplyr)
library(lwgeom)

# Calculate bearings and turning angles

gps_analysis <- 
  gps_sf %>%
  arrange(timestamp) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    
    # Distance and speed
    distance_m = st_distance(geometry, lead(geometry), by_element = TRUE) %>% as.numeric(),
    time_diff_sec = as.numeric(difftime(lead(timestamp), timestamp, units = "secs")),
    speed_ms = distance_m / time_diff_sec,
    speed_kmh = speed_ms * 3.6,
    
    # Simple bearing calculation
    bearing = atan2(lead(lon) - lon, lead(lat) - lat) * 180 / pi,
    bearing = (bearing + 360) %% 360,  # normalize to 0-360
    
    # Calculate change in bearing
    bearing_change = abs(bearing - lag(bearing)),
    bearing_change = pmin(bearing_change, 360 - bearing_change)  # handle wraparound
  )

# Identify corners (sharp turns ~90 degrees)
corners <- gps_analysis %>%
  filter(
    bearing_change > 70 & bearing_change < 110,  # ~90 degree turns
    speed_kmh > 2  # vessel is moving
  )

# Find hauls: 4 corners within ~30 minutes
find_hauls <- function(corners_df, time_window = 35) {
  hauls <- list()
  
  for(i in 1:(nrow(corners_df) - 3)) {
    four_corners <- corners_df[i:(i+3), ]
    time_span <- as.numeric(
      difftime(four_corners$timestamp[4], four_corners$timestamp[1], units = "mins")
    )
    
    if(time_span < time_window & time_span > 20) {
      hauls[[length(hauls) + 1]] <- tibble(
        haul_id = length(hauls) + 1,
        set_start = four_corners$timestamp[1],
        set_end = four_corners$timestamp[4],
        duration_mins = time_span
      )
    }
  }
  
  bind_rows(hauls)
}

haul_times <- find_hauls(corners)

gps_points_final <- 
  gps_analysis %>%
  fuzzyjoin::fuzzy_left_join(
    haul_times %>% mutate(haul_id = row_number()),
    by = c("timestamp" = "set_start", "timestamp" = "set_end"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(-set_start, -set_end, -duration_mins) %>% 
  sf::st_as_sf()


ggplot() +
  theme_publication() +
  geom_sf(data=rect_lr_sf, fill=NA, colour="lightgray") +
  geom_sf(data=world_mr_sf) +
  geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
  
  # fuel
  geom_sf(data=gps_points_final %>% filter(as.Date(timestamp)==dmy("28/10/2025")), 
          aes(size=fuel, colour=str_pad(haul_id, width = 2, pad="0")), alpha=0.5) +
  scale_size_continuous(range = c(0.1, 0.5)) +
  coord_sf(xlim=c(0.5,0.7), ylim=c(50.5, 50.6))

ggplot() +
  theme_publication() +
  geom_sf(data=rect_lr_sf, fill=NA, colour="lightgray") +
  geom_sf(data=world_mr_sf) +
  geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
  
  
  geom_sf(data=gps_sf %>% filter(timestamp >= ymd_hms("2025-10-28 07:00:00"),
                                 timestamp <= ymd_hms("2025-10-28 18:00:00")), 
          aes(colour=stringr::str_pad(hour, width=2, pad="0")), size=1) +
  geom_path(data=gps_sf %>% filter(timestamp >= ymd_hms("2025-10-28 07:00:00"),
                                   timestamp <= ymd_hms("2025-10-28 18:00:00")), 
            aes(x=lon, y=lat, colour=stringr::str_pad(hour, width=2, pad="0")), alpha=0.5) +
  geom_sf_text(data=gps_sf %>% filter(timestamp >= ymd_hms("2025-10-28 07:00:00"),
                                 timestamp <= ymd_hms("2025-10-28 18:00:00")), 
          aes(colour=stringr::str_pad(hour, width=2, pad="0"), label=hour2), alpha=0.5) +
  scale_size_continuous(range = c(0.1, 0.5)) +
  coord_sf(xlim=c(0.55,0.68), ylim=c(50.52, 50.58))

gps <- read_csv("Test_2025-10-28_VAF-IVY-Minute_Data.csv") %>%
  mutate(timestamp = dmy_hm(ts)) %>%
  filter(timestamp >= ymd_hms("2025-10-28 07:00:00"),
         timestamp <= ymd_hms("2025-10-28 18:00:00"))