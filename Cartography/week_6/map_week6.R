# Making a graph for Cartography week 6 
# Employment in Europe

# Laoding Packages----

library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)
library(ggrepel)
library(ggtext)
library(fmsb)
library("wesanderson")
library(viridis)
library(RColorBrewer)

# Loading the Data----

## Employment----
employment <- read.csv("Cartography/week_6/employment.csv")

## World data----
world <- map_data("world") #downloading coordinates for the world,we'll select countries by ourselves

head(world) 
str(world)

# Selecting European Countries on the map and the dataset 

EU <- c("Belgium", "Bulgaria", "Czech Republic", "Denmark", 
        "Germany", "Estonia", "Ireland", "Greece", "Spain", 
        "France", "Croatia", "Italy", "Cyprus", "Latvia", 
        "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands",
        "Austria", "Poland", "Portugal", "Romania", "Slovenia",
        "Slovakia", "Finland", "Sweden", "United Kingdom", "Switzerland")

## EU map----
eu_map <- world %>% 
  filter(region %in% EU) %>% #only keeping the countries we have specified in the vector above 
  dplyr::select(-subregion)

## EU employment dataset----
eu_employment <- emplyment %>% 
  filter(Country %in% EU) %>% 
  mutate(Country = str_replace(Country, "Czechia", "Czech Republic"))

eu_employment <- left_join(eu_map, eu_employment, by = c("region" = "Country")) 
head(eu_employment)
str(eu_employment)

eu_employment <- eu_employment %>% 
  rename(HDI = HDI.rank, Employment_ratio = Employment.to.population.ratio, 
         Labour_participation = Labour.force.participation.rate,
         Agriculture = Employment.in.agriculture, 
         Services = Employment.in.services,
         Youth_not_school_or_employment =  Youth.not.in.school.or.employment..13.18., 
         High_to_low = High.skill.to.low.skill.ratio..08.18.,
         Pension = Old.age.pension.recipients..09.18.)

str(eu_employment)
eu_employment$Employment_ratio <- as.numeric(eu_employment$Employment_ratio)
eu_employment$Labour_participation <- as.numeric(eu_employment$Labour_participation)
eu_employment$Agriculture <- as.numeric(eu_employment$Agriculture)
eu_employment$Services <- as.numeric(eu_employment$Services)
eu_employment$Total <- as.numeric(eu_employment$Total)
eu_employment$Youth <- as.numeric(eu_employment$Youth)
eu_employment$Youth_not_school_or_employment <- as.numeric(eu_employment$Youth_not_school_or_employment)
eu_employment$High_to_low <- as.numeric(eu_employment$High_to_low)
eu_employment$Pension <- as.numeric(eu_employment$Pension)

# Starting to map----
(EU_map <- ggplot() +
   geom_polygon(data = eu_employment, aes(x = long, y = lat, group = group, fill = Employment_ratio) 
                , color="black", size = 0.3) + #plot the data points on the map
   theme_minimal() + #choosing what type of background we want to display 
   #scale_fill_viridis(direction = -1) +
   ylim(35,70) + #this allow us to section where we want our focus
   coord_map())  #making the projection nice








