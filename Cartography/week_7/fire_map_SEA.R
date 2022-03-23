# Packages
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
#library("wesanderson")
library(ggsn)
library(osmdata)
#library(OpenStreetMap)
library(rgdal)
library(sf)
library(dplyr)
#Loading the World
world <- map_data("world") #downloading coordinates for the world,we'll select countries by ourselves
str(world)

Sea <- c("Brunei", "Myanmar", "Cambodia", "Laos",
         "Indonesia", "Malaysia",
         "Philippines", "Singapore", "Thailand", "Vietnam")
sea_map <- world %>% 
  filter(region %in% Sea) %>% 
  dplyr::select(-subregion) 

# Loading fires in SEA

fire_sea <- read.csv("Cartography/week_7/fire_sea.csv")

fire_sea <- filter(fire_sea, CONFIDENCE >= 75)


# Mapping SEA
(sea_map <- ggplot() +
    geom_polygon(data = sea_map, 
                 aes(x=long, y = lat, group = group), fill="#ece2e3", alpha=1, 
                 color="#bd8ca0", size = 0.3) + 
    geom_point(data = fire_sea, aes(x = LONGITUDE, y = LATITUDE), colour = "#973e5d", size = 0.5, alpha = 08) +
  theme_void() + 
  guides(size = FALSE) +
  coord_map())

ggsave(plot = sea_map,filename = "Cartography/week_7/fire_sea_map.png", height = 4, width = 8)
  