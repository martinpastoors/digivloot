# ==============================================================================
# Read PEFA files
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
# 08/05/2023 Finalized the elog data
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek/2024q4_m10"
admindir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/administratie"


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

# janitor::compare_df_cols(pefa, e)
# skimr::skim(pefa)

scenario1 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario1.xlsx")) %>% rename(targetcatch=target)
scenario2 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario2.xlsx")) %>% rename(targetcatch=target)


# Mackerel North Sea
# targetspecies <- "MAC"
# targetcatch  <- 3290
# targetareas  <- c("27.4.a","27.4.b","27.4.c")
# targetmonths <- c(7:9)

# t1 <-
#   e %>% 
#   filter(species==targetspecies) %>% 
#   filter(month %in% targetmonths) %>% 
#   filter(division %in% targetareas) %>% 
#   group_by(vessel, species, date, division, economiczone) %>% 
#   summarise(catch = sum(weight, na.rm=TRUE))  %>% 
#   group_by(vessel, species) %>% 
#   
#   arrange(catch) %>% 
#   mutate(cumcatch = cumsum(catch)) %>% 
#   mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>% 
#   filter(include == TRUE) %>% 
#   dplyr::select(vessel, date, species, division, economiczone, catch) %>% 
#   
#   arrange(-catch)  %>% 
#   mutate(cumcatch = cumsum(catch)) %>% 
#   mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>% 
#   filter(include == TRUE) %>% 
#   dplyr::select(-cumcatch, -include) %>% 
#   
#   arrange(date)  

# t1 %>% 
#   pander::pandoc.table(
#     style = "simple",
#     split.tables=400, 
#     justify = "right",
#     missing=".",
#     round=c(0))


# Mackerel Channel
# targetspecies <- "MAC"
# targetcatch  <- 1277
# targetareas  <- c("27.7.d","27.7.e")
# targetmonths <- c(7:9)

# t2 <-
#   e %>% 
#   filter(species==targetspecies) %>% 
#   filter(month %in% targetmonths) %>% 
#   filter(division %in% targetareas) %>% 
#   group_by(vessel, species, date, division, economiczone) %>% 
#   summarise(catch = sum(weight, na.rm=TRUE))  %>% 
#   group_by(vessel, species) %>% 
#   
#   arrange(catch) %>% 
#   mutate(cumcatch = cumsum(catch)) %>% 
#   mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>% 
#   filter(include == TRUE) %>% 
#   dplyr::select(vessel, date, species, division, economiczone, catch) %>% 
#   
#   arrange(-catch)  %>% 
#   mutate(cumcatch = cumsum(catch)) %>% 
#   mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>% 
#   filter(include == TRUE) %>% 
#   dplyr::select(-cumcatch, -include) %>% 
#   
#   arrange(date)  

# t2 %>% 
#   pander::pandoc.table(
#     style = "simple",
#     split.tables=400, 
#     justify = "right",
#     missing=".",
#     round=c(0))

# writexl::write_xlsx(bind_rows(t1, t2), path=file.path(admindir, "2024q3 wq pefa mac v2.xlsx"))


# Sole North Sea 
targetspecies <- "SOL"
# targetcatch  <- 1277
targetareas  <- c("27.4.b","27.4.c")
targetmonths <- c(10)

# scenario1
t3 <-
  e %>% 
  left_join(scenario1, by="vessel") %>% 
  filter(species==targetspecies) %>% 
  filter(month %in% targetmonths) %>% 
  filter(division %in% targetareas) %>% 
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

t3 %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

t3 %>% 
  group_by(vessel, species, targetcatch) %>% 
  summarise(catch = sum(catch)) %>% 
  mutate(remainder=targetcatch - catch) %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

# scenario2
t4 <-
  e %>% 
  left_join(scenario2, by="vessel") %>% 
  filter(species==targetspecies) %>% 
  filter(month %in% targetmonths) %>% 
  filter(division %in% targetareas) %>% 
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

t4 %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

t4 %>% 
  group_by(vessel, species, targetcatch) %>% 
  summarise(catch = sum(catch)) %>% 
  mutate(remainder=targetcatch - catch) %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

writexl::write_xlsx(dplyr::select(t3, -targetcatch),     path=file.path(admindir, "2024q4_m10 wq pefa sol scenario1.xlsx"))
writexl::write_xlsx(dplyr::select(t4, -targetcatch),     path=file.path(admindir, "2024q4_m10 wq pefa sol scenario2.xlsx"))





