# Making a graph for Cartography week 6 
# Employment in Europe

# Laoding Packages----

library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)
library(ggrepel)
library(ggtext)
library(extrafont)
library(maptools)
library(fmsb)
library(RColorBrewer) #for some more colors
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
        "Slovakia", "Finland", "Sweden", "UK", "Switzerland")

## EU map----
eu_map <- world %>% 
  filter(region %in% EU) %>% #only keeping the countries we have specified in the vector above 
  dplyr::select(-subregion)

## EU employment dataset----
employment <- employment %>%
  mutate(Country = str_replace(Country, "Czechia", "Czech Republic")) %>% 
  mutate(Country = str_replace(Country, "United Kingdom", "UK")) %>% 
  filter(Country %in% EU)

eu_employment <- left_join(eu_map, employment, by = c("region" = "Country")) 
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


# Adding Centroids----
centroids <- read.csv("Cartography/week_6/centroids.csv")
Europe <- c("Belgium", "Bulgaria", "Czech Rep.", "Denmark", 
            "Germany", "Estonia", "Ireland", "Greece", "Spain", 
            "France", "Croatia", "Italy", "Cyprus", "Latvia", 
            "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands",
            "Austria", "Poland", "Portugal", "Romania", "Slovenia",
            "Slovakia", "Finland", "Sweden", "United Kingdom","Switzerland")
centroids <- centroids %>% 
  filter(name %in% Europe) %>% 
  dplyr::select(name, Longitude, Latitude, iso_a3)

# Fixing some centroids lables----
centroids["12", "Longitude"] <- 2 #here we are individually changing the values in a specific box

centroids["12", "Latitude"] <- 46 

centroids["5", "Longitude"] <- 33 #here we are individually changing the values in a specific box

centroids["5", "Latitude"] <- 35

# Making the EU employment map----
(EU_map <- ggplot() +
   geom_polygon(data = eu_employment, aes(x = long, y = lat,
                    group = group, fill = Employment_ratio),
                    color="black", size = 0.1) + #plot the data points on the map
   theme_void() + #choosing what type of background I want to display 
   scale_fill_fermenter(direction = 1, palette = "PuBuGn") + 
   geom_text(data = centroids, aes(Longitude, Latitude, label = iso_a3), size = 2.5) + 
   ylim(35,70) + #this allow us to section where we want our focus
   theme(plot.title = element_text(family = "Georgia",size = 16, hjust = 0, face = "bold"),
         legend.position = "bottom",
         legend.title = element_text(family = "Georgia", size = 10, face ="bold"),
         legend.text = element_text(family = "Georgia",size = 8))  +
   labs(fill = "Employment Ratio (%)",
        title = "Employment in Europe: Where are we?") +
  coord_map("gilbert"))

#ggsave(plot = EU_map, filename = "Cartography/week_6/img/Ratio_employment.png", height = 9, width = 7)

data(wrld_simpl)
plot(eu)

library(ggmap)
library(gridExtra)

map <- get_stamenmap(bbox = c(left = -13, 
                             bottom = 23, right = 37, top = 71), 
                    zoom = 4, maptype = "watercolor")

ggmap(map) + 
  theme_void() + 
  theme(plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2))
