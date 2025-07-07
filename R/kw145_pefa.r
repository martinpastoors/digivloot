# ==============================================================================
# Read KW145 PEFA files
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
# 08/05/2023 Finalized the elog data
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)
library(sf)
library(viridis)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/DATA/DIGIvloot"
spatialdir <- "C:/DATA/RDATA"

mpa         <- loadRData(file.path(spatialdir, "MPANLD.RData")) %>% sf::st_transform(4326) %>% sf::st_as_sf()
rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez.sf.RData"))

# st_crs(mpa)
# mapview::mapview(mpa)

filelist <- list.files(
  path=file.path(datadir),
  pattern="pefa",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  e <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    e  <-
      bind_rows(
        e,
        readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          rename(
            rect = icesrectangle,
            vessel = vesselnumber,
            division = faozone,
            gear = geartype,
            landings = weight
          ) %>% 
          
          {if(any(grepl("latitide",names(.)))) {rename(., lat = latitide)} else{.}} %>% 
          {if(any(grepl("latitude",names(.)))) {rename(., lat = latitude)} else{.}} %>% 
          {if(any(grepl("longitude",names(.)))) {rename(., lon = longitude)} else{.}} %>% 
          {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
          
          mutate(across (any_of(c("boxes", "meshsize", "haul")),
                         as.integer)) %>%
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "landings", "lat", "lon", "conversionfactor")),
                         as.numeric)) %>%
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate")), 
                         ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
          
          mutate(date   = as.Date(catchdate)) %>% 
          mutate(economiczone = ifelse(economiczone == "GBR", economiczone, "XEU")) %>% 
          {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 

          mutate(vessel = gsub("-","", vessel)) %>% 
          mutate(vessel = gsub("\\.","", vessel)) %>% 
          
          mutate(
            year       = lubridate::year(date),
            quarter    = lubridate::quarter(date),
            month      = lubridate::month(date),
            week       = lubridate::week(date),
            yday       = lubridate::yday(date))
          
          # left_join(rect_df, by="rect") %>% 
          # mutate(
          #   lat = lat + 0.25,
          #   lon = lon + 0.5
          # ) %>% 

      )

  } # end of pefa elog for loop
  
} # end of not empty filelist

# janitor::compare_df_cols(pefa, e)

# skimr::skim(e)

t <-
  e %>% 
  filter(gear=="TBB", species=="SOL") %>% 
  filter(year %in% 2021:2024) %>% 
  group_by(year, quarter, rect) %>% 
  summarise(
    landings = sum(landings, na.rm=TRUE),
    effort = n_distinct(vessel, date)
  ) %>% 
  
  mutate(
    cpue = landings/effort
  ) %>% 
  
  ungroup() %>% 
  
  mutate(., effort_interval = cut(effort, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(effort, na.rm=TRUE))), 
                                  dig.lab=10 ) ) %>% 
  mutate(., landings_interval = cut(landings, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(landings, na.rm=TRUE))), 
                                 dig.lab=10 ) ) %>% 
  mutate(., cpue_interval = cut(cpue, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(cpue, na.rm=TRUE))), 
                                dig.lab=10 ) ) %>% 
  drop_na(rect) %>% 
  left_join(rect_lr_sf, by=c("rect"= "ICESNAME")) %>% 
  dplyr::select(-ID, -SOUTH, -NORTH, -WEST, -EAST) %>% 
  ungroup() %>% 
  sf::st_as_sf()


# bb <- sf::st_bbox(t)

t %>% 
  ggplot() +
  theme_publication() +
  theme(plot.margin = margin(1,1,1,1, "mm")) +
  theme(plot.title = element_text(hjust = 0.0)) +
  # theme(legend.position = "none") +
  
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  geom_sf(data=world_mr_sf) +
  
  geom_sf(aes(fill=landings_interval), alpha=0.6) +
  geom_point(aes(size = landings, geometry = geometry), stat = "sf_coordinates", shape=1, show.legend=FALSE) + 
  geom_sf(data=mpa, aes(colour=SITE_NAME), inherit.aes=FALSE, size=1) +
  
  viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +

  coord_sf(xlim=c(0,8), ylim=c(51, 55)) + 
  
  guides(fill=guide_legend(nrow = 1)) + 
  labs(x="",y="", title="landings") +
  facet_grid(year~quarter)

