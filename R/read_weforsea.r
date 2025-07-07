# ==============================================================================
# Read WeForSea files
#
# 25/10/2024 first coding
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/data/export"
rdatadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/rdata"
spatialdir <- "C:/DATA/RDATA"
gisdir     <- "C:/DATA/GIS"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData")) %>% 
  bind_cols(data.frame(valid = sf::st_is_valid(.), reason=TRUE)) %>% 
  filter(valid == TRUE)

mpa         <- loadRData(file.path(spatialdir, "MPANLD4.RData")) %>% 
  filter(SITE_NAME %notin% c("Bruine Bank 265")) %>% 
  mutate(land="NL")

sf::st_is_valid(eez_sf, reason=TRUE)
eez_sf[265,]

wind        <- loadRData(file.path(spatialdir, "wind_sf.RData")) %>% 
  lowcase() %>% 
  filter(type %in% c("Bestaand","Goedgekeurd","Onder Constructie")) %>% 
  filter(land != "NL") %>% 
  dplyr::select(naam, land, type, geometry) %>% 
  mutate(code = substr(naam, 1, 12))
  
windnl      <- loadRData(file.path(spatialdir, "20241023 nl_wind_nwp_sf.RData")) %>% 
  dplyr::select(code=windgebied, km2, naam=opmerking, plan=windgebie0, geometry) %>% 
  mutate(land = "NL")

windcomb <- bind_rows(wind, windnl)

# Sabellaria
folder <- "20241120 BruineBank Sabellaria"
layer  <- 'BruineBankArea'
layers <- list.files(path=file.path(gisdir, folder), pattern = "Survey")

surveys <-
  bind_rows(
    sf::read_sf(file.path(gisdir, folder), layer="SurveyAreaA") %>% mutate(area="A"),
    sf::read_sf(file.path(gisdir, folder), layer="SurveyAreaB") %>% mutate(area="B"),
    sf::read_sf(file.path(gisdir, folder), layer="SurveyAreaF") %>% mutate(area="F"),
    sf::read_sf(file.path(gisdir, folder), layer="SurveyAreaG") %>% mutate(area="G"),
  ) %>% 
  sf::st_transform(crs=4326) %>% 
  sf::st_cast(., "POLYGON") %>% 
  lowcase() 

# require(rasterVis)
# require(raster)
# require(ggplot2)

# map <- terra::rast(file.path(gisdir,"20241120 BruineBank Sabellaria", "B_allData_LGS100_rmNadir_30_geotiff.tif")) 
# pts <- terra::as.points(map, na.rm=FALSE) 
# pts <- terra::project(pts, "+proj=longlat +datum=WGS84")
# terra::plot(map)

# raster::crs(map) <- "+proj=longlat +datum=WGS84"
# rasterVis::gplot(map) + geom_tile(aes(fill = value)) +
#   scale_fill_gradient(low = 'white', high = 'black') +
#   coord_equal()
# str(map)
# glimpse(map)
# terra::geom(map)[, c("x", "y")]

# wind and nature areas
all_areas <-
  bind_rows(
    windcomb %>% mutate(class="wind"),
    mpa       %>% mutate(class="mpa")
  )


# data

filelist <- list.files(
  path=file.path(datadir),
  pattern="csv",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  h <- j <- s <- sv <- data.frame(stringsAsFactors = FALSE)
  
  i <- 7
  for (i in 1:length(filelist)) {
  # for (i in 1:10) {
      
    myvessel <- stringr::word(basename(filelist[i]), start=1, sep="_")
    
    if(grepl("haul", filelist[i])) {
      
      print(paste("haul", filelist[i]))
      
      h  <-
        bind_rows(
          h,
        read.csv(filelist[i], 
                 colClasses = "character",
                 stringsAsFactors = FALSE)  %>%
            lowcase() %>% 
            mutate(
              vessel   = myvessel, 
              startdate=lubridate::ymd_hms(startdate),
              enddate = lubridate::ymd_hms(enddate)
            ) %>% 
            rename(
              haul = id, 
              trip = journeyid
            )
        )
      
    } else if (grepl("journey", filelist[i])) {

      print(paste("journey", filelist[i]))
      
      j  <-
        bind_rows(
          j,
          read.csv(filelist[i], 
                   colClasses = "character",
                   stringsAsFactors = FALSE)  %>%
            lowcase() %>% 
            mutate(
              vessel   = myvessel
            ) %>% 
            rename(
              trip = id
            )
        )  
      
    } else if (grepl("scale_value", filelist[i])) {
      
      print(paste("scale_value", filelist[i]))
      
      s  <-
        bind_rows(
          s,
          read.csv(filelist[i], 
                   colClasses = "character",
                   stringsAsFactors = FALSE)  %>%
            lowcase() %>% 
            mutate(location = gsub("[^,.0-9A-Za-z///' ]","" , location )) %>% 
            mutate(location = gsub("typePoint,crstypename,propertiesnameEPSG4326,coordinates","", location)) %>% 
            tidyr::separate(location, into=c("lon","lat"), sep=",") %>% 
            mutate(across(c(kilos, price, revenue, lon, lat), as.numeric)) %>% 
            mutate(vessel   = myvessel) %>% 
            rename(
              haul = haulid,
              trip = journeyid,
              species = specie,
              weight = kilos
            ) %>% 
            left_join(h, by=c("vessel", "trip", "haul")) %>% 
            mutate(
              year = lubridate::year(startdate),
              month = lubridate::month(startdate),
              week  = lubridate::week(startdate),
              yday  = lubridate::yday(startdate)
            ) %>% 
            group_by(vessel, trip, haul, startdate, enddate, year, month, week, yday, species, id) %>% 
            summarise(
              weight = sum(weight, na.rm=TRUE),
              price  = mean(price, na.rm=TRUE),
              revenue = sum(revenue, na.rm=TRUE),
              lat     = mean(lat, na.rm=TRUE),
              lon     = mean(lon, na.rm=TRUE)
            ) %>% 
            drop_na(month)
        )
      
    } else if (grepl("sensor_value", filelist[i])) {
      
      print(paste("sensor_value", filelist[i]))
      
      sv  <-
        bind_rows(
          sv,
          read.csv(filelist[i], 
                   colClasses = "character",
                   stringsAsFactors = FALSE)  %>%
            lowcase() %>% 
            filter(role == "map") %>% 
            mutate(location = gsub("[^,.0-9A-Za-z///' ]","" , location )) %>% 
            mutate(location = gsub("typePoint,crstypename,propertiesnameEPSG4326,coordinates","", location)) %>% 
            tidyr::separate(location, into=c("lon","lat"), sep=",") %>% 
            mutate(across(c("lat","lon"), as.numeric)) %>% 
            mutate(
              date = lubridate::ymd_hms(date),
              vessel   = myvessel
            )
        )
    
    }
    
  } # end of pefa elog for loop
  
} # end of not empty filelist

# janitor::compare_df_cols(pefa, e)

# skimr::skim(pefa)

# sv %>% filter(as.Date(date) == ymd("2024-08-06")) %>% View()

# summarise sv in 10 minute blocks (GPS)
sv <-
  sv %>% 
  
  # remove outlier in lon; need to fix this in a formal way
  filter(lon <= 11) %>% 
  
  mutate(date = lubridate::round_date(date, unit="minute")) %>% 
  mutate(date = update(date, minute = 10*floor(minute(date)/10))) %>% 
  group_by(vessel, sensorid, role, date) %>% 
  summarise(
    lat = mean(lat, na.rm=TRUE),
    lon = mean(lon, na.rm=TRUE)
  ) %>% 
  mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date)
  )

# catch overlap
catch_overlap <-
  sf::st_intersection(
    s %>% sf::st_as_sf(coords = c("lon","lat"), remove = FALSE, crs=4326) ,
    all_areas) 

# eez overlap
catch_eez <-
  sf::st_intersection(
    s %>% sf::st_as_sf(coords = c("lon","lat"), remove = FALSE, crs=4326) ,
    eez_sf) 

catch_summary <-
  s %>% 
  filter(haul %notin% catch_overlap$haul) %>% 
  bind_rows(catch_overlap) %>% 
  mutate(class = ifelse(is.na(class), "open", class)) %>% 
  group_by(vessel, year, class, land, species) %>% 
  summarise(
    weight = sum(weight, na.rm=TRUE)
  ) %>% 
  group_by(vessel, year, species) %>% 
  mutate(prop = weight / sum(weight, na.rm=TRUE))

save(h,j,s,sv,catch_overlap, catch_summary, 
     file = file.path(rdatadir,"weforsea.RData"))






# catch_summary %>% 
#   # filter(species == "SOL") %>% 
#   reshape2::dcast(species+class+land ~ year, value.var = "weight", sum, margins=c("class"))

# catch_summary %>% 
#   filter(species %in% c("PLE","SOL", "TUR", "BLL", "RJH")) %>% 
#   dplyr::relocate(species) %>%
#   reshape2::dcast(species+class+land ~ year, sum, value.var = "weight", margins="class") %>% 
#   group_by(species) %>% 
#   group_modify(~ add_row(.x,.before=0)) %>% 
#   mutate(species=ifelse(is.na(class), NA, species)) %>% 
#   pander::pandoc.table(.,
#                style = "simple",
#                split.tables=400, 
#                missing=".",
#                round=c(0,0,0,0,2,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

# catch_summary %>% 
#   filter(species %in% c("PLE","SOL", "TUR", "BLL", "RJH")) %>% 
#   dplyr::relocate(species) %>%
#   mutate(perc = paste0(round(100*prop, digits=0),"%") ) %>% 
#   dplyr::select(-weight, -prop) %>% 
#   tidyr::pivot_wider(names_from = year, values_from = perc) %>% 
#   arrange(species, class, land) %>% 
#   group_by(species) %>% 
#   group_modify(~ add_row(.x,.before=0)) %>% 
#   mutate(species=ifelse(is.na(class), NA, species)) %>% 
#   pander::pandoc.table(.,
#                        style = "simple",
#                        split.tables=400, 
#                        missing=".",
#                        round=c(0,0,0,0,2,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


# s %>% 
#   # sf::st_as_sf() %>% 
#   filter(species %in% c("PLE","SOL","TUR","RJH")) %>% 
#   
#   ggplot() +
#   theme_publication() +
# 
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill=NA) +
#   
#   geom_point(aes(x=lon, y=lat, size=weight, colour=species), alpha=0.5, shape=21) +  
#   # geom_sf(aes(fill=vessel), alpha=0.6) +
#   # viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
#   
#   coord_sf(xlim=c(0,5), ylim=c(52, 54)) + 
#   
#   guides(fill=guide_legend(nrow = 1)) + 
#   facet_grid(species~month)

# s %>% 
#   # sf::st_as_sf() %>% 
#   filter(species %in% c("SOL")) %>% 
#   
#   ggplot() +
#   theme_publication() +
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill=NA) +
#   geom_sf(data=wind, aes(alpha=Type), colour="green", linewidth=0.1, fill="green") +
#   geom_sf_text(data=wind, aes(label=Naam), colour="darkgreen", size=3) +
#   
#   geom_point(aes(x=lon, y=lat, size=weight, colour=species), alpha=0.5, shape=21) +  
#   # geom_sf(aes(fill=vessel), alpha=0.6) +
#   # viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
#   
#   # coord_sf(xlim=c(0,5), ylim=c(52, 54)) + 
#   coord_sf(xlim=c(2,4.5), ylim=c(52.1, 53.5)) + 
#   scale_size(range=c(0.2,10)) + 
#   guides(fill=guide_legend(nrow = 1)) + 
#   facet_wrap(~month)

# plot hauls by value
# t <-
#   s %>% 
#   # sf::st_as_sf() %>% 
#   filter(!is.na(year)) %>% 
#   group_by(trip, haul, year, month) %>% 
#   summarise(
#     revenue = sum(revenue, na.rm=TRUE),
#     lat     = mean(lat, na.rm=TRUE),
#     lon     = mean(lon, na.rm=TRUE)
#   ) %>% 
#   sf::st_as_sf(coords = c("lon","lat")) %>% 
#   sf::st_set_crs(4326) 

# t %>% 
#   ggplot() +
#   theme_publication() +
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill=NA) +
#   
#   geom_sf(aes(size=revenue), alpha=0.5, shape=21) +  
#   geom_sf(data=sf::st_intersection(t, mpa), aes(size=revenue), alpha=0.5, shape=21, colour="blue") +  
#   # geom_sf(aes(fill=vessel), alpha=0.6) +
#   # viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
#   
#   coord_sf(xlim=c(1,4.5), ylim=c(52, 53.5)) + 
#   
#   guides(fill=guide_legend(nrow = 1)) + 
#   facet_wrap(~month)

# m <- 
#   sf::st_intersection(t, mpa)  %>% 
#   filter(SITE_NAME=="Bruine Bank") %>% 
#   bind_rows(filter(t,
#                    paste(trip, haul, year, month) %notin% paste(.$trip, .$haul, .$year, .$month)))
  
# histograms of hauls revenue
# t %>% 
#   filter(!is.na(year)) %>% 
#   ggplot(aes(x=revenue)) +
#   theme_publication() +
#   geom_histogram(data=m, aes(fill=SITE_NAME))

# t %>% 
#   filter(revenue >= 8000) %>% 
#   View()

# s %>% filter(species == "COD") %>% summarise(weight = sum(weight, na.rm=TRUE))
# s %>% filter(species == "TUR") %>% summarise(weight = sum(weight, na.rm=TRUE))
# s %>% filter(species == "BLL") %>% summarise(weight = sum(weight, na.rm=TRUE))
# s %>% group_by(species) %>% summarise(weight = sum(weight, na.rm=TRUE)) %>% arrange(desc(weight))

# sv %>% 
#   mutate(year = lubridate::year(date)) %>% 
#   filter(year >= 2023) %>% 
#   
#   ggplot() +
#   theme_publication() +
#   theme(legend.position = "none") +
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   
#   # GPS points
#   geom_point(aes(x=lon, y=lat), colour="gray", size=0.1, alpha=0.2) +  
#   
#   # catches of sole
#   geom_point(data = s %>% filter(species=="SOL", year >= 2023), aes(x=lon, y=lat, size=weight, colour=species), alpha=0.8, shape=21) +  
# 
#   geom_sf(data = catch_overlap, aes(size=weight), colour="black", alpha=0.8, shape=21) +  
#   
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill="red", alpha=0.1) +
#   geom_sf_text(data=mpa, aes(label=SITE_NAME), colour="red", linewidth=0.1, size=3) +
#   
#   geom_sf(data=windcomb, aes(fill=land, colour=land), linewidth=0.3, alpha=0.1) +
#   geom_sf_text(data=windcomb, aes(label=code, colour=land), size=3) +
#   
#   geom_sf(data=surveys, fill="green", linewidth=0.3, alpha=0.2) +
#   geom_sf_text(data=surveys, aes(label=area), colour="green", size=3) +
#   
#   coord_sf(xlim=c(2,4.5), ylim=c(52, 53.5)) + 
#   facet_wrap(~year)

# by month
# sv %>% 
#   mutate(month = lubridate::month(date)) %>% 
#   # drop_na(month) %>% 
#   # filter(month <= 2) %>% 
#   
#   ggplot() +
#   theme_publication() +
#   theme(legend.position = "none") +  
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   
#   # GPS points
#   geom_point(aes(x=lon, y=lat), colour="gray", size=0.1, alpha=0.2) +  
#   
#   # catches of sole
#   geom_point(data = s %>% filter(species=="SOL"), aes(x=lon, y=lat, size=weight, colour=species), alpha=0.8, shape=21) +
# 
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill="red", alpha=0.1) +
# 
#   geom_sf(data=windcomb, aes(fill=land, colour=land), linewidth=0.3, alpha=0.1) +
# 
#   coord_sf(xlim=c(2,4.5), ylim=c(52, 53.5)) + 
#   
#   facet_wrap(~month)


# wind internationaal
# wind %>% 
#   filter(Type %in% c("Bestaand","Goedgekeurd","Onder Constructie")) %>% 
#   filter(Land != "NL") %>% 
#   
#   ggplot() +
#   theme_publication() +
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill="red", alpha=0.1) +
#   geom_sf_text(data=mpa, aes(label=SITE_NAME), colour="red", linewidth=0.1, size=3) +
#   
#   geom_sf(aes(alpha=Type, fill=Type), linewidth=0.1) +
#   geom_sf_text(aes(label=Naam), size=3) +
#   
#   geom_sf(data=windnl, aes(alpha=windgebie0, fill=windgebie0), linewidth=0.1) +
#   geom_sf_text(data=windnl, aes(label=windgebied), size=3) +
#   
#   coord_sf(xlim=c(2,4.5), ylim=c(52, 53.5)) + 
#   
#   guides(fill=guide_legend(nrow = 1)) 


# wind NL
# ggplot() +
#   theme_publication() +
#   
#   geom_sf(data=world_mr_sf) +
#   geom_sf(data=eez_sf, linetype="dashed", fill=NA) +
#   geom_sf(data=mpa, colour="red", linewidth=0.1, fill=NA) +
#   
#   geom_sf(data=windnl, aes(alpha=windgebie0, fill=windgebie0), linewidth=0.1) +
#   geom_sf_text(data=windnl, aes(label=windgebied), size=3) +
#   
#   coord_sf(xlim=c(2,4.5), ylim=c(52, 53.5)) + 
#   
#   guides(fill=guide_legend(nrow = 1)) +
#   
#   facet_wrap(~windgebie0)
