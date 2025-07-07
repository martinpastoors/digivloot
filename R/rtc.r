# ==============================================================================
# rtc.r
#
# 15/11/2024 first coding
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)
library(sf)
library(viridis)

rm(list=ls())

source("../FLYSHOOT/r/FLYSHOOT utils.R")

datadir    <- "C:/DATA/DIGIvloot/data"
spatialdir <- "C:/DATA/RDATA"

rect_lr_sf  <- loadRData(file.path(spatialdir, "rect_lr_sf.RData"))
world_mr_sf <- loadRData(file.path(spatialdir, "world_mr_sf.RData")) %>% sf::st_as_sf()
eez_sf      <- loadRData(file.path(spatialdir, "eez_sf.RData"))

rtc <-
  readxl::read_excel(file.path(datadir, "rtc.xlsx"))

# visweken
bind_rows(e, m) %>% 
  group_by(vessel, year, week) %>% 
  summarise(landings = sum(landings))  
  
# overzicht
myvessel       <- c("KW14", "KW45", "KW 145", "SL42") 
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
  drop_na(rect) %>% 
  left_join(rect_lr_sf, by=c("rect"= "ICESNAME")) %>% 
  dplyr::select(-ID, -SOUTH, -NORTH, -WEST, -EAST) %>% 
  ungroup() %>% 
  sf::st_as_sf() %>% 
  sf::st_intersection(mpa %>% 
                        group_by(SITE_NAME) %>% 
                        summarise(do_union=TRUE) %>% 
                        ungroup() %>% 
                        sf::st_as_sf() 
  ) %>% 
  mutate(area = as.numeric(sf::st_area(.))/1000000) %>% 
  group_by(species, year, quarter, rect) %>% 
  mutate(
    prop = area / sum(area),
    landings = landings * prop
  ) %>% 
  group_by(species) %>%   
  
  mutate(., effort_interval = cut(effort, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(effort, na.rm=TRUE))), 
                                  dig.lab=10 ) ) %>% 
  mutate(., landings_interval = cut(landings, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(landings, na.rm=TRUE))), 
                                    dig.lab=10 ) ) %>% 
  mutate(., cpue_interval = cut(cpue, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(cpue, na.rm=TRUE))), 
                                dig.lab=10 ) ) %>% 
  sf::st_as_sf()

# t %>% 
#   filter(rect=="33F3") %>%
#   # View()
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=SITE_NAME), alpha=0.6) +
#   geom_sf_text(aes(label=format(prop, nsmall=1)), inherit.aes = FALSE) +
#   geom_sf(data=mpa, inherit.aes=FALSE, fill=NA, linewidth=0.6)

# t %>% 
#   filter(rect=="33F3") %>%
#   # View()
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=landings_interval), alpha=0.6) +
#   geom_sf(data=mpa, inherit.aes=FALSE, fill=NA, linewidth=0.6)


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
  # geom_sf(aes(fill=SITE_NAME), alpha=0.6) +
  geom_point(aes(size = landings, geometry = geometry), stat = "sf_coordinates", shape=1, show.legend=FALSE) + 
  geom_sf(data=mpa, aes(colour=SITE_NAME), inherit.aes=FALSE, fill=NA, linewidth=0.6) +
  # geom_sf_text(data=eez_sf, aes(label=ISO_TER1)) +
  
  viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  
  coord_sf(xlim=c(0,6), ylim=c(51, 54)) + 
  
  guides(fill=guide_legend(nrow = 1)) + 
  labs(x="",y="", title=paste(paste(myvessel, collapse="+"), paste(myspecies, collapse="+"), "landings")) +
  facet_grid(year~quarter)



# plot by MPA etc
t %>% 
  group_by(species, year, SITE_NAME) %>% 
  summarise(landings = sum(landings)) %>% 
  group_by(species) %>%   
  
  mutate(., landings_interval = cut(landings, scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(landings, na.rm=TRUE))), 
                                    dig.lab=10 ) ) %>% 
  sf::st_as_sf() %>% 

  ggplot() +
  theme_publication() +
  theme(plot.margin = margin(1,1,1,1, "mm")) +
  theme(plot.title = element_text(hjust = 0.0)) +
  # theme(legend.position = "none") +
  
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  geom_sf(data=world_mr_sf) +
  
  geom_sf(aes(fill=landings_interval), alpha=0.6) +
  # geom_sf(data=mpa, aes(colour=SITE_NAME), inherit.aes=FALSE, fill=NA, linewidth=0.6) +
  # geom_sf_text(aes(label=as.integer(landings))) +
  
  viridis::scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  
  coord_sf(xlim=c(0,6), ylim=c(51, 54)) + 
  
  guides(fill=guide_legend(nrow = 1)) + 
  labs(x="",y="", title=paste(paste(myvessel, collapse="+"), paste(myspecies, collapse="+"), "landings")) +
  facet_wrap(~year)


# bar plot of catch by area
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





# Plot with figures from WECR report

# p1 <-
#   ggplot() +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill="transparent"),
#     plot.background = element_rect(fill="transparent"),
#     panel.grid.major = element_blank()
#   ) +
#   geom_sf(data=eez_sf, fill="transparent", linetype="dashed") +
#   geom_sf(data=world_mr_sf, fill="transparent") +
#   geom_sf(data=mpa, inherit.aes=FALSE, colour="black", fill="transparent", linewidth=0.6) +
#   coord_sf(xlim=c(-5, 10.0), ylim=c(48, 60.0)) 
# 
# 
# myfile <- "WECR gemiddelde opbrengst tong 2010-2019"
# 
# for (myfile in c("WECR gemiddelde opbrengst tong 2010-2019",
#                  "WECR gemiddelde opbrengst tong 2020-2022",
#                  "WECR gemiddelde opbrengst TBB 2010-2019",
#                  "WECR gemiddelde opbrengst TBB 2020-2022")
#      ) {
#   
#   print(myfile)
#   
#   img <- 
#     magick::image_read(file.path(nzodir, paste0(myfile, ".png"))) 
#   
#   p2 <- 
#     cowplot::ggdraw() +
#     cowplot::draw_image(img, valign=0.00) +
#     cowplot::draw_plot(p1) 
#   
#   p3 <-
#     cowplot::ggdraw() +
#     cowplot::draw_plot(p2, .056, .01, .89, .90) +
#     cowplot::draw_label(label=myfile, size = 10, x=0.5, y=0.95, vjust=1, hjust=0.5, fontface="bold")
#   
#   print(p3)
#   
#   png(filename=file.path(nzodir, paste0(myfile," met N2000.png")),
#       width=7, height=9.8, units="in", res=300)
#   print(p3)
#   dev.off()  
# }


  

mpa %>% 
  filter(SITE_NAME %notin% c("GBR","FRA","DEU","BEL","DNK","NCP NON-MPA")) %>% 
  mutate(SITE_NAME = ifelse(SITE_NAME=="Friese Front" & SITE_TYPE=="N2000", "Friese Front (N2000)",SITE_NAME)) %>% 
  arrange(SITE_NAME) %>% 
  mutate(N = str_pad(row_number(), width=2, pad="0")) %>% 
  mutate(label = paste(N, SITE_NAME)) %>% 
  sf::st_as_sf() %>% 

  ggplot() +
  theme_minimal() +
  # theme(plot.margin = margin(1,1,1,1, "mm")) +
  # theme(plot.title = element_text(hjust = 0.0)) +
  theme(legend.position = "right") +
  
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  geom_sf(data=world_mr_sf, fill=NA) +
  geom_sf(aes(colour=label, fill=label), alpha=0.4) +
  geom_sf_text(aes(label=N, colour=label), size=4) +
  coord_sf(xlim=c(2,7), ylim=c(51.5, 56)) + 
  guides(colour="none") +  
  labs(x="", y="")

