# ==============================================================================
# Read VCU export files KW14
#
# 14/01/2025 first coding
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/data/KW14"
rdatadir   <- "C:/Users/MartinPastoors/Martin Pastoors/WQ - General/rdata"
spatialdir <- "C:/DATA/RDATA"
gisdir     <- "C:/DATA/GIS"

# rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
# world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
# eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData")) %>% 
#   bind_cols(data.frame(valid = sf::st_is_valid(.), reason=TRUE)) %>% 
#   filter(valid == TRUE)

# haul
filelist <- list.files(
  path=file.path(datadir,"hauls"),
  pattern="csv",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  h <- data.frame(stringsAsFactors = FALSE)
  
  myvessel <- stringr::word(dirname(filelist[1]), start=-2, sep="/")
  
  i <- 1
  for (i in 1:length(filelist)) {
    # for (i in 1:10) {
    
    test <- try( read.csv(filelist[i]), silent=TRUE)
    
    if(class(test) %in% 'try-error') {next} else {
      
      print(paste(i, filelist[i]))
      
      h  <-
        bind_rows(
          h,
          read.csv(filelist[i], 
                   colClasses = "character",
                   stringsAsFactors = FALSE,
                   sep=";")  %>%
            lowcase() %>% 
            rename(haul=sourceid, startdate = creation, rect=icesrectangle) %>% 
            tidyr::separate(latlon, into=c("lat","lon"), sep=",") %>% 
            mutate(across(c(lat, lon), as.numeric)) %>% 
            mutate(startdate   = lubridate::ymd_hms(startdate)) %>% 
            # left_join(h, by=c("vessel", "trip", "haul")) %>% 
            mutate(
              year = lubridate::year(startdate),
              month = lubridate::month(startdate),
              week  = lubridate::week(startdate),
              yday  = lubridate::yday(startdate),
              vessel = myvessel,
              trip = gsub("\\.csv","", basename(filelist[i]))
            ) 
        )
      
    } # end of try-error 
    
  } # end of for loop
  
} # end of not empty filelist

# scale
filelist <- list.files(
  path=file.path(datadir,"scale"),
  pattern="csv",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  s <- data.frame(stringsAsFactors = FALSE)
  
  myvessel <- stringr::word(dirname(filelist[1]), start=-2, sep="/")
  
  i <- 2
  for (i in 1:length(filelist)) {
  # for (i in 1:10) {
      
    test <- try( read.csv(filelist[i]), silent=TRUE)
   
    if(class(test) %in% 'try-error') {next} else {

      print(paste(i, filelist[i]))

      s  <-
        bind_rows(
          s,
          read.csv(filelist[i], 
                   colClasses = "character",
                   stringsAsFactors = FALSE,
                   sep=";")  %>%
            lowcase() %>% 
            rename(
              date = creation,
              presentation = prescode,
              species = speciescode,
              grade = usergrade
            ) %>% 
            mutate(across(c(weight), as.numeric)) %>% 
            mutate(date   = lubridate::ymd_hms(date)) 
        )
      
    } # end of try-error 

  } # end of for loop
  
  s <-
    s %>% 
    mutate(grade = stringr::str_trim(tolower(grade))) %>% 
    mutate(grade = gsub('^([0-9]{1})-', '\\1:', grade)) %>%
    mutate(grade = gsub("super \\(","super: ", grade)) %>% 
    tidyr::separate(grade, into=c("grade","desc"), sep=":") %>% 
    mutate(desc = trim.spaces(desc)) %>% 
    mutate(desc = gsub("\\( ","\\(", desc)) %>% 
    
    left_join(h, by="haul")
  
  
} # end of not empty filelist

# janitor::compare_df_cols(pefa, e)

# skimr::skim(s)

# sv %>% filter(as.Date(date) == ymd("2024-08-06")) %>% View()

save(h, s, file = file.path(rdatadir,"vcu.RData"))

# s %>% 
#   group_by(year, month, species, presentation, grade) %>% 
#   summarise(weight = sum(weight, na.rm=TRUE)) %>% 
#   reshape2::dcast(year+species ~ grade, value.var = "weight", sum, margins=c("grade", "year")) %>% 
#   pander::pandoc.table(.,
#                        style = "simple",
#                        split.tables=400, 
#                        missing=".",
#                        big.mark = ',',
#                        justify=c(rep("left",2), rep("right",12)),
#                        round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


