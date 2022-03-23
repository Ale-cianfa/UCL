# Making a graph for Cartography week 6 
# Employment in Europe

# Laoding Packages----

library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)
library(ggrepel)
library(ggtext)
library(ggridges)
library(extrafont)
library(fmsb)
library(RColorBrewer) #for some more colors
library("wesanderson")
library(viridis)
library(RColorBrewer)

# Loading the Data----

employment <- read.csv("Cartography/week_6/Employment_carto.csv")


EU <- c("Belgium", "Bulgaria", "Czech Republic", "Denmark",
        "Germany", "Estonia", "Ireland", "Greece", "Spain", "Iceland", 
        "France", "Croatia", "Italy", "Cyprus", "Latvia", 
        "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands",
        "Austria", "Poland", "Portugal", "Romania", "Slovenia",
        "Slovak Republic", "Finland", "Sweden", "United Kingdom", "Switzerland")

str(employment)

employment <- employment %>%
  filter(Country.Name %in% EU) %>% 
  rename(Country = Country.Name, Code = Country.Code, 
         Indicator = Indicator.Name) %>% 
  dplyr::select(-Indicator, -Indicator.Code) 

colnames(employment) <- gsub("^X", "",  colnames(employment))

write_csv(employment, "Cartography/week_6/europe_emp.csv")

employment <- gather(employment, Year, Value, c(3:32))

employment$Year <- as.factor(employment$Year)

str(employment)

(employment_scat <- ggplot(employment, aes (x = Value, y = Country, fill = Country)) +
    geom_density_ridges() +
    theme_ridges() +
    ylim(0,100) +
    theme(legend.position = "right"))












