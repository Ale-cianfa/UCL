# Map 1 of final Assessment: Fuga di Cervelli - The big brain drain 

#In this i want to show: 
  #- a connection map of where young italians are going 
  #- a stacked plot (or similar) to show the proportion of italians leaving at diff ages 
  #- from which region they are mostly leaving (maybe a basic choropleth map? as a base) 

#LOADING THE LIBARIES----

library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot

#LOADING THE ISTAT DATA: 

