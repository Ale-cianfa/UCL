# Making a graph for Cartography week 3 
# open space in london wards 
# income 
library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)
library(ggrepel)
library(ggtext)
library(fmsb)
library("wesanderson")
library(viridis)
library(RColorBrewer)


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
                          
boroughs_i_sp <- data.frame(City = c(150000, 0, 63620), Barking = c(150000, 0, 34080), Barnet = c(150000, 0, 54530), 
                             Bexley = c(150000, 0, 44430), Brent = c(150000, 0, 39630), Bromley = c(150000, 0, 55140), 
                             Camden = c(150000, 0, 67990), Croydon = c(150000, 0, 45120), Ealing = c(150000, 0, 45690), 
                             Enfield = c(150000, 0, 41250), Greenwich = c(150000, 0, 44370),  Hackney = c(150000, 0, 42690), 
                             Hammersmith = c(150000, 0, 62910), Haringey = c(150000, 0, 45860), Harrow = c(150000, 0, 49060), 
                             Havering = c(150000, 0, 44430), Hillingdon = c(150000, 0, 44950), Hounslow = c(150000, 0, 44490), 
                             Islington = c(150000, 0, 54950), Kensington = c(150000, 0, 116350), Kingston = c(150000, 0, 56920), 
                             Lambeth = c(150000, 0, 48610), Lewisham = c(150000, 0, 43360), Merton = c(150000, 0, 57160), 
                             Newham = c(150000, 0, 34260), Redbridge = c(150000, 0, 45380), Richmond = c(150000, 0, 76610), 
                             Southwark = c(150000, 0, 48000), Sutton = c(150000, 0, 49170), Tower_Hamlets = c(150000, 0, 45720), 
                             Waltham_Forest = c(150000, 0, 39460), Wandsworth = c(150000, 0, 66220), Westminster = c(150000, 0, 80760))

## Spider Plot OS----                          
wes_palette("GrandBudapest2")

(open_space <- radarchart(boroughs_os_sp, axistype = 1, 
                          title = "Percentage of Open Space in the 33 London Boroughs",
                          pcol= rgb(0.2,0.5,0.5,0.5), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=1,
                          cglcol="grey", cglty=1, axislabcol="grey",
                          seg = 4, caxislabels = seq(0,100,25), cglwd=0.8,vlcex=0.8))

(income <- radarchart(boroughs_i_sp, axistype = 1, 
                          title = "Average household income in the 33 London Boroughs",
                          pcol= rgb(0.2,0.5,0.5,0.5), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=1,
                          cglcol="grey", cglty=1, axislabcol="grey",
                          seg = 4, cglwd=0.8,vlcex=0.8))

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

##Making the lables---- 
  #NB: this is all from the R graph gallery 

boroughs_comp <- data.frame(boroughs_i, id= seq(1,33), open_space = boroughs_os$open_space)
label_data <- boroughs_comp
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

## Circular barplot for income----

boroughs_comp <- data.frame(boroughs_comp, condition_i= seq(1,33))

boroughs_comp$condition_i <- c("4","1", "3", "2", "1", "3", "4", "2",
                          "2", "2", "2", "2", "4","2","2","2",
                          "2","2","3","9", "3", "2", "2","3", 
                          "1","2", "5", "2","2","2","1", "4", "6")

colors_i <- c("#D7F3FE", "#8eb5f0", "#7364d2", 
            "#613dc1", "#5829a7", "#4e148c", "#3d0e61")
(plt_i <- ggplot(boroughs_comp, aes(x = Borough, y = House_income, fill = condition_i)) + 
    geom_bar(stat='identity') + 
  #  ylim(0,130000) +
    theme_minimal() + 
    scale_fill_manual(labels=c("£30-40k","£40-50k","£50-60k",
                               "£60-70k", "£70-80k", "£80-90k", "£110-120k"), 
                      values = colors_i) +
    theme(legend.position = "right",
          legend.title = element_text(size = 10, face ="bold"),
          axis.text.x =element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_blank(),
          axis.text.y =element_blank(),
          panel.grid = element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size = 11),
          plot.title = element_text(size = 16, face ="bold", hjust = 0.5)) +
  labs(fill = "Yearly \nHousehold \nIncome") + 
  geom_text(data=label_data, aes(x=id, y = 120000, label=Borough, hjust=hjust), 
            color="black", fontface="bold",alpha=1, size=2, 
             angle= label_data$angle, inherit.aes = FALSE) +
  coord_polar())

#ggsave("Cartography/week_3/img/income_plot.png", plot = plt_i, height = 5, width = 7)

## Circualr barplot open space-----

boroughs_comp <- data.frame(boroughs_comp, condition_os= seq(1,33))

boroughs_comp$condition_os <- c("1","3", "3", "3", "2", "5", "2", "3",
                          "3", "4", "3", "2", "2","2","3","5",
                          "4","3","1","1", "3", "2", "2","3", 
                          "2","4", "5", "2","3","2","3", "3", "2")

colors_os <- c("#BFEDD0", "#84C2B0", "#279084", 
               "#0C4644", "#011414")

(plt_os <- ggplot(boroughs_comp, aes(x = Borough, y = open_space, fill = condition_os)) + 
    geom_bar(stat='identity') + 
    scale_fill_manual(labels=c("10-20%","20-30%","30-40%",
                               "40-50%", "50-60%", "60-70%", "70-80%"), 
                      values = colors_os) +
    theme_minimal() + 
    theme(legend.position = "right",
          legend.title = element_text(size = 10, face ="bold"),
          axis.text.x =element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_blank(),
          axis.text.y =element_blank(),
          panel.grid = element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size = 11),
          plot.title = element_text(size = 16, face ="bold", hjust = 0.5)) +
    labs(fill = "Percentage \nGreen \nSpace") + 
    geom_text(data=label_data, aes(x=id, y = 60, label=Borough, hjust=hjust), 
              color="black", fontface="bold",alpha=1, size=2, 
              angle= label_data$angle, inherit.aes = FALSE) +
    coord_polar())

#ggsave("Cartography/week_3/img/vegetation_plot.png", plot = plt_os, height = 5, width = 7)

  