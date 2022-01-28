# Making a graph for Cartography week 3 
# open space in london wards 
# income 
library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)
library(ggrepel)
library(ggtext)
library(fmsb)
library("wesanderson")
library(viridis)

# Dowloading the data----
wards <- read.csv("Cartography/week_3/Wards_info.csv")

# Data clean-up----
boroughs <- wards %>% 
  slice(626:658) 

boroughs <- boroughs %>% 
  rename(Borough = Ward.name, Old_code = Old.code, New_code = New.code,
         Population_2015 = Population...2015, 
         House_income =  Median.Household.income.estimate..2012.13., 
         open_space = X..area.that.is.open.space...2014) 


wards <- wards %>% 
  slice(0:625)

wards <- wards %>% 
  rename(Ward = Ward.name, Old_code = Old.code, New_code = New.code,
       Population_2015 = Population...2015,
       House_income =  Median.Household.income.estimate..2012.13.,
       open_space = X..area.that.is.open.space...2014) 

str(wards)

wards$House_income <- as.numeric(wards$House_income)
wards$open_space <- as.numeric(wards$open_space)

boroughs$House_income <- as.numeric(gsub(",", ".", boroughs$House_income))
boroughs$open_space <- as.numeric(gsub(",", ".",boroughs$open_space))

# Making a Borough OS dataset----
boroughs_os <- boroughs %>% 
  select(c(Borough, open_space))

b <- boroughs_os$Borough
os <- boroughs_os$open_space

boroughs_os_sp <- data.frame(City = c(100, 0, 18.6), Barking = c(100, 0, 33.7), Barnet = c(100, 0, 37.7), 
                          Bexley = c(100, 0, 38.9), Brent = c(100, 0, 22.3), Bromley = c(100, 0, 57.4), 
                          Camden = c(100, 0, 24.6), Croydon = c(100, 0, 32.2), Ealing = c(100, 0, 31.5), 
                          Enfield = c(100, 0, 48.5), Greenwich = c(100, 0, 39.3),  Hackney = c(100, 0, 28.3), 
                          Hammersmith = c(100, 0, 21.1), Haringey = c(100, 0, 27.8), Harrow = c(100, 0, 32.1), 
                          Havering = c(100, 0, 59.0), Hillingdon = c(100, 0, 45.0), Hounslow = c(100, 0, 37.5), 
                          Islington = c(100, 0, 13.7), Kensington = c(100, 0, 19.8), Kingston = c(100, 0, 36.9), 
                          Lambeth = c(100, 0, 20.7), Lewisham = c(100, 0, 22.1), Merton = c(100, 0, 35.9), 
                          Newham = c(100, 0, 29.9), Redbridge = c(100, 0, 40.3), Richmond = c(100, 0, 57.0), 
                          Southwark = c(100, 0, 22.6), Sutton = c(100, 0, 33.9), Tower_Hamlets = c(100, 0, 26.9), 
                          Waltham_Forest = c(100, 0, 34.8), Wandsworth = c(100, 0, 31.6), Westminster = c(100, 0, 28.6))
                          
## Spider Plot OS----                          
wes_palette("GrandBudapest2")

(open_space <- radarchart(boroughs_os_sp, axistype = 1, 
                          title = "Percentage of Open Space in the 33 London Boroughs",
                          pcol= rgb(0.2,0.5,0.5,0.5), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=1,
                          cglcol="grey", cglty=1, axislabcol="grey",
                          seg = 4, caxislabels = seq(0,100,25), cglwd=0.8,vlcex=0.8))
## Circular barplot OS----
str(boroughs_os)

(plt_os<- ggplot(boroughs_os, aes(x=Borough, y=open_space, fill = Borough)) + 
  geom_bar(stat='identity') + 
  ylim(0,75) +
  scale_fill_viridis_d() + #color by threshold (maybe a color every 10?)
  theme_minimal() +
  coord_polar())

# Making df for income by Borough----
boroughs_i <- boroughs %>% 
  select(c(Borough, House_income))

## Circular barplot for income
(plt_i <- ggplot(boroughs_i, aes(x = Borough, y = House_income, fill = Borough)) + 
    geom_bar(stat='identity') + 
    scale_fill_viridis_d(option = "plasma") + 
    #color by threshold (maybe a color every 10?)
    theme_minimal() +
    theme(legend.position = "none",
          axis.ticks.x=element_blank(),
          axis.title = element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size = 11),
          plot.title = element_text(size = 16, face ="bold", hjust = 0.5)) +
    labs(title = "Income per household in the 33 London Boroughs\n") + 
    coord_polar())

