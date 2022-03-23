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

getwd()

#LOADING THE ISTAT DATA 18-39: 
em_18_39 <- read.csv("Cartography/final_assessment/map_1_data/emigrazione_18_39.csv")
str(em_18_39) #here we see everything is a character value so we need to do some data wrangling 

#BASIC DATA WRANGLING ISTAT 18-39:----

#Making the data into longform:
em_18_39 <- gather(em_18_39, Year, Length,  #in this order: data frame, key, value
                          c(X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019))  

em_18_39$Year<-gsub("X"," ", as.character(em_18_39$Year)) #removing the x from the years

#Making sure all the columns are the right data type: 
em_18_39$Year <- as.factor(em_18_39$Year)
em_18_39$Country.of.next.residence <- as.factor(em_18_39$Country.of.next.residence)
em_18_39$Length <- as.numeric(em_18_39$Length)

#Renaming the Columns: 
em_18_39 <- em_18_39 %>% 
  rename(Destinazione = Country.of.next.residence,
         Total = Length)

#DOWNLOADING WORLD DATA:----

world <- map_data("world")

#Adding Centroids: 
  #This is where the lines will connect to 
centroids <- read.csv("Cartography/final_assessment/map_1_data//centroids.csv")
str(centroids)

#Only keeping the important bits
centroids <- centroids %>%
  dplyr::select(name, Longitude, Latitude, iso_a3)

#Joining the datasets 
em_18_39 <- left_join(em_18_39, centroids, by = c("Destinazione" = "name")) 
#NB: there are some countries to fix cause they don't match up! 


#STARTING TO MAP:----

#Basic map with all the centroids:

(world_basic <- ggplot() +
   geom_polygon(data = India, aes(x=long, y = lat, group = group), fill="#EDEDED", alpha=1) +
   geom_point( data = cities_india, aes(x = lng, y = lat, size = 1, colour = city)) +
   theme_minimal() +
   scale_color_viridis_d() +
   guides(size = FALSE) +
   coord_map())
  








