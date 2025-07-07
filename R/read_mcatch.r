# ==============================================================================
# Read M-Catch files
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
# 08/05/2023 Finalized the elog data
# 15/08/2024 Adapted for use for WQ reporting
# ==============================================================================

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek/2024q4_m10"
admindir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/administratie"
spatialdir <- "C:/DATA/RDATA"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData"))

# MCatch export 2024_10_SELECT_lca_v_name_AS_vessel_name_v_hull_number_FROM_logbook_log_202411120945

filelist <- list.files(
  path=file.path(datadir),
  pattern=glob2rx("*m-catch*csv*"),
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

scenario1 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario1.xlsx")) %>% rename(targetcatch=target)
scenario2 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario2.xlsx")) %>% rename(targetcatch=target)

# skimr::skim(mcatch)
# inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

# mcatch %>% ggplot(aes(x=lon, y=lat, colour=as.character(year))) + geom_point()
# mcatch %>% filter(weight > 1000) %>% arrange(desc(weight)) %>% View()


# Sole North Sea
targetspecies <- "SOL"
# targetcatch  <- 1277
targetareas  <- c("27.4.b","27.4.c")
targetmonths <- c(10)

#scenario1
t1 <-
  e %>% 
  left_join(scenario1, by="vessel") %>% 
  filter(species==targetspecies) %>% 
  filter(month %in% targetmonths) %>% 
  filter(division %in% targetareas) %>% 
  filter(date >= startdate) %>% 
  group_by(vessel, species, targetcatch, date, division, economiczone) %>% 
  summarise(catch = sum(weight, na.rm=TRUE))  %>% 
  group_by(vessel, species) %>% 
  
  arrange(vessel, species, catch) %>% 
  mutate(cumcatch = cumsum(catch)) %>% 
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>%
  # mutate(include = ifelse((cumcatch < targetcatch),TRUE, FALSE)) %>% 
  filter(include == TRUE) %>% 
  dplyr::select(vessel, date, species, targetcatch, division, economiczone, catch) %>% 
  
  arrange(vessel, species, -catch)  %>% 
  mutate(cumcatch = cumsum(catch)) %>% 
  mutate(include = ifelse((cumcatch < targetcatch),TRUE, FALSE)) %>% 
  filter(include == TRUE) %>% 
  
  dplyr::select(-cumcatch, -include) %>% 
  
  arrange(vessel, date)  

t1 %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

t1 %>% 
  group_by(vessel, species, targetcatch) %>% 
  summarise(catch = sum(catch)) %>% 
  mutate(remainder=targetcatch - catch) %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

writexl::write_xlsx(dplyr::select(t1, -targetcatch), path=file.path(admindir, "2024q4_m10 wq m-catch sol scenario1.xlsx"))


#scenario2
t2 <-
  e %>% 
  left_join(scenario2, by="vessel") %>% 
  filter(species==targetspecies) %>% 
  filter(month %in% targetmonths) %>% 
  filter(division %in% targetareas) %>% 
  filter(date >= startdate) %>% 
  group_by(vessel, species, targetcatch, date, division, economiczone) %>% 
  summarise(catch = sum(weight, na.rm=TRUE))  %>% 
  group_by(vessel, species) %>% 
  
  arrange(vessel, species, catch) %>% 
  mutate(cumcatch = cumsum(catch)) %>% 
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>% 
  filter(include == TRUE) %>% 
  dplyr::select(vessel, date, species, targetcatch, division, economiczone, catch) %>% 
  
  arrange(vessel, species, -catch)  %>% 
  mutate(cumcatch = cumsum(catch)) %>% 
  mutate(include = ifelse(cumcatch < targetcatch,TRUE, FALSE)) %>% 
  filter(include == TRUE) %>% 
  
  dplyr::select(-cumcatch, -include) %>% 
  
  arrange(vessel, date)  

t2 %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

t2 %>% 
  group_by(vessel, species, targetcatch) %>% 
  summarise(catch = sum(catch)) %>% 
  mutate(remainder=targetcatch - catch) %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

writexl::write_xlsx(dplyr::select(t2, -targetcatch), path=file.path(admindir, "2024q4_m10 wq m-catch sol scenario2.xlsx"))


# e %>% filter(vessel == "KW45", date == dmy("13-06-2024")) %>% View()


# Sole North Sea - all reports
# targetspecies <- "SOL"
# targetareas  <- c("27.4.b","27.4.c")
# targetmonths <- c(5:6)
# 
# tmp <-
#   e %>% 
#   filter(species==targetspecies) %>% 
#   filter(month %in% targetmonths) %>% 
#   filter(division %in% targetareas) %>% 
#   group_by(vessel, species, date, division, economiczone) %>% 
#   summarise(catch = sum(weight, na.rm=TRUE))  %>% 
#   ungroup() %>% 
#   arrange(vessel, date)  
# 
# tmp %>% 
#   pander::pandoc.table(
#     style = "simple",
#     split.tables=400, 
#     justify = "right",
#     missing=".",
#     round=c(0))
# 
# writexl::write_xlsx(bind_rows(tmp),     path=file.path(admindir, "2024q2 wq pefa sol v2 all.xlsx"))

# bb <- sf::st_bbox(t)

# e %>% 
#   drop_na(rect) %>% 
#   left_join(rect_lr_sf, by=c("rect"= "ICESNAME")) %>% 
#   dplyr::select(-ID, -SOUTH, -NORTH, -WEST, -EAST) %>% 
#   ungroup() %>% 
#   sf::st_as_sf() %>% 
#   
#   ggplot() +
#   theme_publication() +
#   theme(plot.margin = margin(1,1,1,1, "mm")) +
#   theme(plot.title = element_text(hjust = 0.0)) +
#   # theme(legend.position = "none") +
#   
#   geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
#   geom_sf(data=world_mr_sf) +
#   
#   geom_sf(aes(fill=vessel), alpha=0.6) +
#   viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
#   
#   coord_sf(xlim=c(0,10), ylim=c(51, 58)) + 
#   
#   guides(fill=guide_legend(nrow = 1)) + 
#   labs(x="",y="", title=paste(paste(myvessel, collapse="+"), paste(myspecies, collapse="+"), "landings")) +
#   facet_grid(year~quarter)
