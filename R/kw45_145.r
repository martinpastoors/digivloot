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
nzodir     <- "C:/Users/MartinPastoors/Martin Pastoors/NZO - General/WKC/vragen/2024_aanvullende_beschermde_gebieden/documenten"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData"))

mpa         <- loadRData(file.path(spatialdir, "MPANLD3.RData")) %>% sf::st_as_sf()
ncp         <- eez_sf %>% filter(grepl("NLD", ISO_TER1)) %>% sf::st_as_sf()
non_mpa     <- 
  rmapshaper::ms_erase(target=ncp, erase=mpa) %>% 
  mutate(SITE_NAME = "NCP NON-MPA", SITE_TYPE="OPEN") %>% 
  dplyr::select(SITE_NAME, SITE_TYPE)
mpa         <- bind_rows(mpa, non_mpa)

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
filelist <- list.files(
  path=file.path(datadir),
  pattern="mcatch",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  m <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    m  <-
      bind_rows(
        m,
        readxl::read_excel(filelist[i], 
                           sheet = "landed catch details table",
                           col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          rename(
            rect = icesrectangle,
            vessel = vesselhullnumber,
            division = faozone,
            gear = geartype,
            landings = catchweight,
            catchdate = activitydate,
            economiczone = economicalzone,
            species = fishspecie
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

# skimr::skim(m)

myvessel       <- c("KW45", "KW 145") 
myspecies      <- "SOL"
combinevessels <- TRUE

sf_use_s2(FALSE)

t <-
  bind_rows(e, m) %>% 
  filter(gear=="TBB") %>% 
  filter(year %in% 2021:2024) %>% 
  filter(species %in% myspecies) %>% 
  filter(vessel %in% myvessel) %>% 
  
  {if (combinevessels) {
    group_by(., species, year, quarter, rect)    
  } else {
    group_by(., vessel, species, year, quarter, rect)
  }} %>% 
  
  summarise(
    landings = sum(landings, na.rm=TRUE),
    effort = n_distinct(vessel, date)
  ) %>% 
  
  mutate(
    cpue = landings/effort
  ) %>% 
  
  # ungroup() %>% 
  group_by(species) %>%   
  
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
  sf::st_as_sf() %>% 
  sf::st_intersection(mpa) 

t1 <-
  t %>% 
  sf::st_drop_geometry() %>% 
  group_by(year, species) %>% 
  summarise(
    totaleffort = sum(effort, na.rm=TRUE),
    totallandings  = sum(landings, na.rm=TRUE)
  )

t2 <-
  t %>% 
  sf::st_drop_geometry() %>% 
  group_by(year, species, SITE_NAME) %>% 
  summarise(
    effort = sum(effort, na.rm=TRUE),
    landings  = sum(landings, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(t1, by=c("year","species")) %>% 
  mutate(
    prop_effort = effort / totaleffort,
    prop_landings = landings / totallandings
  ) 

t3 <-
  t2 %>% 
  group_by(species, SITE_NAME) %>% 
  summarise(landings = sum(landings, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(landings))

tmp <-
  t2 %>% 
  mutate(SITE_NAME = forcats::fct_reorder(SITE_NAME, landings, .desc=TRUE))

tmp %>% 
  ggplot(aes(x=SITE_NAME, y=prop_landings)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~year)

# bb <- sf::st_bbox(t)

t %>% 
  
  ggplot() +
  theme_publication() +
  theme(plot.margin = margin(1,1,1,1, "mm")) +
  theme(plot.title = element_text(hjust = 0.0)) +
  # theme(legend.position = "none") +
  
  geom_sf(data=eez_sf, aes(fill=GeoName), linetype="dashed") +
  geom_sf(data=world_mr_sf) +
  
  # geom_sf(aes(fill=landings_interval), alpha=0.6) +
  # geom_sf(aes(fill=SITE_NAME), alpha=0.6) +
  geom_point(aes(size = landings, geometry = geometry), stat = "sf_coordinates", shape=1, show.legend=FALSE) + 
  # geom_sf(data=mpa, aes(colour=SITE_NAME), inherit.aes=FALSE, fill=NA, linewidth=0.6) +
  geom_sf_text(data=eez_sf, aes(label=GeoName)) +
  
  viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  
  coord_sf(xlim=c(0,8), ylim=c(51, 58)) + 
  
  guides(fill=guide_legend(nrow = 1)) + 
  labs(x="",y="", title=paste(paste(myvessel, collapse="+"), paste(myspecies, collapse="+"), "landings")) +
  facet_grid(year~quarter)

mregions2::gaz_search("Dutch")
ggplot() +
  theme_publication() +
  theme(plot.margin = margin(1,1,1,1, "mm")) +
  theme(plot.title = element_text(hjust = 0.0)) +
  theme(legend.position = "none") +
  geom_sf(data=eez_sf, aes(fill=GeoName)) +
  geom_sf(data=world_mr_sf) 

p1 <-
  ggplot() +
  theme_void() +
  theme(
    panel.background = element_rect(fill="transparent"),
    plot.background = element_rect(fill="transparent"),
    panel.grid.major = element_blank()
  ) +
  geom_sf(data=eez_sf, fill="transparent", linetype="dashed") +
  geom_sf(data=world_mr_sf, fill="transparent") +
  geom_sf(data=mpa, inherit.aes=FALSE, colour="black", fill="transparent", linewidth=0.6) +
  coord_sf(xlim=c(-5, 10.0), ylim=c(48, 60.0)) 


myfile <- "WECR gemiddelde opbrengst tong 2010-2019"

for (myfile in c("WECR gemiddelde opbrengst tong 2010-2019",
                 "WECR gemiddelde opbrengst tong 2020-2022",
                 "WECR gemiddelde opbrengst TBB 2010-2019",
                 "WECR gemiddelde opbrengst TBB 2020-2022")
     ) {
  
  print(myfile)
  
  img <- 
    magick::image_read(file.path(nzodir, paste0(myfile, ".png"))) 
  
  p2 <- 
    cowplot::ggdraw() +
    cowplot::draw_image(img, valign=0.00) +
    cowplot::draw_plot(p1) 
  
  p3 <-
    cowplot::ggdraw() +
    cowplot::draw_plot(p2, .056, .01, .89, .90) +
    cowplot::draw_label(label=myfile, size = 10, x=0.5, y=0.95, vjust=1, hjust=0.5, fontface="bold")
  
  print(p3)
  
  png(filename=file.path(nzodir, paste0(myfile," met N2000.png")),
      width=7, height=9.8, units="in", res=300)
  print(p3)
  dev.off()  
}


  

