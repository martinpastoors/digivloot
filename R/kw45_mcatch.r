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

datadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/logboek/2024q2"
admindir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/administratie"

filelist <- list.files(
  path=file.path(datadir),
  pattern="m-catch",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  e <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    e  <-
      bind_rows(
        e,
        readxl::read_excel(filelist[i], 
                           # sheet = "landed catch details table",
                           col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          
          rename(
            catchdate=operationdatetime,
            rect = icesrectangle,
            vessel = hullnumber,
            weight = weightlive,
            species = fishspecies,
            economiczone = economicalzone,
            meshsize = gearmeshsize,
            faozone = faoarea,
            # freshness = fishfreshness,
            #presentation = fishpresentation,
            #preservation = fishpreservation
          ) %>% 
          
          filter(fishjuvenile == FALSE) %>% 
          
          mutate(vessel = gsub("-","", vessel)) %>% 
          mutate(vessel = gsub("\\.","", vessel)) %>% 
          # mutate(vessel = ifelse(vessel=="SL09", "SL9", vessel)) %>%  
          
          mutate(across (c("catchdate", "meshsize"),
                         as.integer)) %>%
          mutate(across (c("catchdate", "weight", "lon","lat"),
                         as.numeric)) %>%
          # mutate(across(c("catchdate"),
          #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
          mutate(across (c("catchdate"), 
                         ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
          mutate(across (c("faozone"),
                         toupper)) %>%
          
          mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
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
        
      )
    
  } # end of pefa elog for loop
  
} # end of not empty filelist

# janitor::compare_df_cols(pefa, e)


# skimr::skim(mcatch)
# inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

# mcatch %>% ggplot(aes(x=lon, y=lat, colour=as.character(year))) + geom_point()
# mcatch %>% filter(weight > 1000) %>% arrange(desc(weight)) %>% View()


targetspecies <- "SOL"
targetcatch  <- 1277
targetareas  <- c("27.4.b","27.4.c")
targetmonths <- c(5:6)

t1 <-
  e %>% 
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

t1 %>% 
  pander::pandoc.table(
    style = "simple",
    split.tables=400, 
    justify = "right",
    missing=".",
    round=c(0))

writexl::write_xlsx(bind_rows(t1), path=file.path(admindir, "2024q2 wq m-catch sol.xlsx"))

# e %>% filter(vessel == "KW45", date == dmy("13-06-2024")) %>% View()
