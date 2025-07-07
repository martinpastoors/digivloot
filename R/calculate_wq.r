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

datadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek/2024q4_m10"
datadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek"
admindir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/administratie"
spatialdir <- "C:/DATA/RDATA"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData"))


load(file=file.path(datadir, "df.RData"))


# scenario1 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario1.xlsx")) %>% rename(targetcatch=target)
scenario2 <- readxl::read_excel(file.path(admindir, "2024q4_m10 scenario2 - new.xlsx")) %>% rename(targetcatch=target)

# Sole North Sea
targetspecies <- "SOL"
# targetcatch  <- 1277
targetareas  <- c("27.4.b","27.4.c")
targetmonths <- c(10, 11)



#scenario2
t1 <-
  df %>% 
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

writexl::write_xlsx(dplyr::select(t1, -targetcatch), path=file.path(admindir, "2024q4_m10_11 wq sol scenario2 v20250114.xlsx"))




# Mackerel Channel
targetspecies <- "MAC"
targetcatch  <- 2300
targetareas  <- c("27.7.d","27.7.e")
targetmonths <- c(10:11)

t2 <-
  df %>%
  filter(species==targetspecies) %>%
  filter(month %in% targetmonths) %>%
  filter(division %in% targetareas) %>%
  group_by(vessel, species, date, division, economiczone) %>%
  summarise(catch = sum(weight, na.rm=TRUE))  %>%
  group_by(vessel, species) %>%

  arrange(catch) %>%
  mutate(cumcatch = cumsum(catch)) %>%
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>%
  filter(include == TRUE) %>%
  dplyr::select(vessel, date, species, division, economiczone, catch) %>%

  arrange(-catch)  %>%
  mutate(cumcatch = cumsum(catch)) %>%
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>%
  filter(include == TRUE) %>%
  dplyr::select(-cumcatch, -include) %>%

  arrange(date)

t2 %>%
  pander::pandoc.table(
    style = "simple",
    split.tables=400,
    justify = "right",
    missing=".",
    round=c(0))

t2 %>% 
  group_by(vessel, species) %>% 
  summarise(catch = sum(catch)) %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))



# Mackerel North Sea

targetspecies <- "MAC"
targetcatch  <- 8000
targetareas  <- c("27.4.a","27.4.b","27.4.c")
targetmonths <- c(10:11)

t3 <-
  df %>%
  filter(species==targetspecies) %>%
  filter(month %in% targetmonths) %>%
  filter(division %in% targetareas) %>%
  group_by(vessel, species, date, division, economiczone) %>%
  summarise(catch = sum(weight, na.rm=TRUE))  %>%
  group_by(vessel, species) %>%

  arrange(catch) %>%
  mutate(cumcatch = cumsum(catch)) %>%
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>%
  filter(include == TRUE) %>%
  dplyr::select(vessel, date, species, division, economiczone, catch) %>%

  arrange(-catch)  %>%
  mutate(cumcatch = cumsum(catch)) %>%
  mutate(include = ifelse((cumcatch < targetcatch | lag(cumcatch) < targetcatch),TRUE, FALSE)) %>%
  filter(include == TRUE) %>%
  dplyr::select(-cumcatch, -include) %>%

  arrange(date)

t3 %>%
  pander::pandoc.table(
    style = "simple",
    split.tables=400,
    justify = "right",
    missing=".",
    round=c(0))

writexl::write_xlsx(bind_rows(t2, t3), path=file.path(admindir, "2024q4_m10_11 wq mac v20241210.xlsx"))



