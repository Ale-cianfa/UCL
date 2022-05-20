# Packages
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggsn)
library(osmdata)
library(rgdal)
library(sf)
library(maptools)
library(cartogram)

getwd()
#Loading italy shapefile 

regions <- read_sf("Cartography/final_assessment/map_4/Reg01012022/Reg01012022_WGS84.shp")


