# ==============================================================================
# Read vangsten M-Catch en PEFA
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
# 08/05/2023 Finalized the elog data
# 15/08/2024 Adapted for use for WQ reporting
# 19/11/2024 Combined PEFA and M-Catch
# ==============================================================================

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek"
dfdir      <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek"
admindir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/administratie"
spatialdir <- "C:/DATA/RDATA"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData"))
fao_sf      <- loadRData(file.path(spatialdir, "fao_sf.RData"))

# MCatch export 2024_10_SELECT_lca_v_name_AS_vessel_name_v_hull_number_FROM_logbook_log_202411120945

filelist <- list.files(
  path=file.path(datadir),
  pattern=glob2rx("*m-catch*csv"),
  # pattern=glob2rx("*"),
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  e <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    e  <-
      bind_rows(
        e,
        # readxl::read_excel(filelist[i], 
        #                    # sheet = "landed catch details table",
        #                    col_names=TRUE, col_types="text",
        #                    .name_repair =  ~make.names(., unique = TRUE))  %>% 
        read.csv(filelist[i], 
                 colClasses = "character",
                 stringsAsFactors = FALSE)  %>%
        data.frame() %>% 
        lowcase() %>% 
        
        rename(
          catchdate=activitydate,
          rect = icesrectangle,
          vessel = hullnumber,
          # weight = weightlive,
          weight = catchweight,
          species = fishspecie,
          economiczone = economicalzone,
          meshsize = meshsize
          # freshness = fishfreshness,
          #presentation = fishpresentation,
          #preservation = fishpreservation
        ) %>% 
        
        # mutate(across (c("fishjuvenile","archived","current"),
        #                as.logical)) %>%
        mutate(across (c("juvenile","discard"),
                       as.logical)) %>%
        filter(!juvenile) %>%
        
        mutate(faozone = paste(faoarea, faosubarea, faodivision, sep=".")) %>% 
        mutate(vessel = gsub("-","", vessel)) %>% 
        mutate(vessel = gsub("\\.","", vessel)) %>% 
        # mutate(vessel = ifelse(vessel=="SL09", "SL9", vessel)) %>%  
        
        mutate(across (c("meshsize"),
                       as.integer)) %>%
        mutate(across (c("weight", "lon","lat"),
                       as.numeric)) %>%
        # mutate(across(c("catchdate"),
        #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
        # mutate(across (c("catchdate"), 
        #                ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
        mutate(across (c("catchdate"), 
                           ~lubridate::as_datetime(catchdate))) %>% 
        mutate(across (c("faozone"),
                       toupper)) %>%
        
        mutate(date   = as.Date(catchdate)) %>% 
        # dplyr::select(-catchdate) %>% 
        
        # mutate(landingdate = as.character(landingdate)) %>% 
        
        mutate(
          year       = lubridate::year(date),
          quarter    = lubridate::quarter(date),
          month      = lubridate::month(date),
          week       = lubridate::week(date),
          yday       = lubridate::yday(date)) %>% 
        
        mutate(division = tolower(faozone)) %>%   
        mutate(file  = basename(filelist[i]))
        
      ) # end of bind_rows
    
  } # end of pefa elog for loop
  
} # end of not empty filelist

# janitor::compare_df_cols(pefa, e)




# PEFA

filelist <- list.files(
  path=file.path(datadir),
  pattern="pefa",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  p <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    p  <-
      bind_rows(
        p,
        readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          rename(
            rect = icesrectangle,
            vessel = vesselnumber,
            division = faozone
          ) %>% 
          
          {if(any(grepl("latitide",names(.)))) {rename(., lat = latitide)} else{.}} %>% 
          {if(any(grepl("latitude",names(.)))) {rename(., lat = latitude)} else{.}} %>% 
          {if(any(grepl("longitude",names(.)))) {rename(., lon = longitude)} else{.}} %>% 
          {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
          
          mutate(across (any_of(c("boxes", "meshsize", "haul")),
                         as.integer)) %>%
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor")),
                         as.numeric)) %>%
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate")),
                         ~excel_timezone_to_utc(., timezone="UTC"))) %>%
          mutate(date   = as.Date(catchdate)) %>% 
          # mutate(economiczone = ifelse(economiczone == "GBR", economiczone, "XEU")) %>% 
          {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 
          
          mutate(vessel = gsub("-","", vessel)) %>% 
          mutate(vessel = gsub("\\.","", vessel)) %>% 
          mutate(vessel = gsub(" ","", vessel)) %>% 
          
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

df <-
  bind_rows(
    e %>% 
      dplyr::select(vessel, date, month, species, division, rect, economiczone, weight) %>% 
      mutate(
        source="M-Catch"),
    p %>% 
      dplyr::select(vessel, date, month, species, division, rect, economiczone, weight) %>% 
      mutate(source="PEFA")
  ) %>% 
  distinct() %>% 
  drop_na(rect) %>% 
  left_join(rect_lr_sf %>% sf::st_drop_geometry() %>% dplyr::select(rect=ICESNAME, lon=WEST, lat=SOUTH),
            by="rect")

save(df, file=file.path(datadir, "df2.RData"))

# janitor::compare_df_cols(e, p)
# skimr::skim(df)
df %>% dplyr::group_by(source) %>% skimr::skim()
df %>% dplyr::group_by(vessel) %>% skimr::skim()

xlim <- range(df$lon, na.rm=TRUE)
ylim <- range(df$lat, na.rm=TRUE) 

df %>% 
  filter(species %in% c("SOL","MAC")) %>% 
  
  ggplot() +
  theme_publication() +
  theme(
    text             = element_text(size=12),
    plot.margin      = unit(c(0,0,0,0), "cm")
  ) +
  
  geom_sf(data=world_mr_sf, fill = "grey75") +
  geom_sf(data=fao_sf, fill = NA, size=0.25, color="black") +
  
  coord_sf(xlim=xlim, ylim=ylim)  +
  
  geom_point(aes(x=lon, y=lat, size=weight, colour=species),
             alpha = 0.5) +
  guides(size = guide_legend(nrow = 1)) + 
  facet_wrap(~month)
