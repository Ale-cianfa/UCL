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


em_18_39$X2009 <- as.numeric(em_18_39$X2009)
em_18_39$X2010 <- as.numeric(em_18_39$X2010)
em_18_39$X2011 <- as.numeric(em_18_39$X2011)
em_18_39$X2012 <- as.numeric(em_18_39$X2012)
em_18_39$X2013 <- as.numeric(em_18_39$X2013)
em_18_39$X2014 <- as.numeric(em_18_39$X2014)
em_18_39$X2015 <- as.numeric(em_18_39$X2015)
em_18_39$X2016 <- as.numeric(em_18_39$X2016)
em_18_39$X2017 <- as.numeric(em_18_39$X2017)
em_18_39$X2018 <- as.numeric(em_18_39$X2018)
em_18_39$X2019 <- as.numeric(em_18_39$X2019)

#BASIC DATA WRANGLING ISTAT 18-39:----
str(em_18_39)

colnms <- c("X2009", "X2010", "X2011", 
            "X2012", "X2013", 
            "X2014", "X2015", "X2016",
            "X2017", "X2018", "X2019")

em_18_39$Total <- rowSums(em_18_39[, colnms], na)

write.csv(em_18_39,'Cartography/final_assessment/map_1_data/em_18_39.csv')



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

#Removing parenthesis from names cause they make R go funny
em_18_39$Destinazione <- gsub("\\s*\\([^\\)]+\\)"," ", as.factor(em_18_39$Destinazione)) #removing the x from the years


#DOWNLOADING WORLD DATA:----

world <- map_data("world")

#Adding Centroids: 
  #This is where the lines will connect to 
centroids <- read.csv("Cartography/final_assessment/map_1_data//centroids.csv")
str(centroids)

#Only keeping the important bits
centroids <- centroids %>%
  dplyr::select(name, Longitude, Latitude, iso_a3)

#Changing the names to they match up between the two datasets for when we merge them
centroids <- centroids %>% 
  mutate(name = str_replace(name, "Antigua and Barb.", "Antigua and Barbuda")) %>% 
  mutate(name = str_replace(name, "Bosnia and Herz.", "Bosnia and Herzegovina")) %>% 
  mutate(name = str_replace(name, "Czech Rep.", "Czech Republic")) %>% 
  mutate(name = str_replace(name, "Central African Rep.", "Central African Republic")) %>% 
  mutate(name = str_replace(name, "Dem. Rep. Congo", "DRC")) %>% 
  mutate(name = str_replace(name, "CÃ´te d'Ivoire", "Côte D'Ivoire")) %>% 
  mutate(name = str_replace(name, "Eq. Guinea", "Equatorial Guinea")) %>%
  mutate(name = str_replace(name, "SÃ£o TomÃ© and Principe", "Sao Tome and Principe")) %>%
  mutate(name = str_replace(name, "S. Sudan", "South Sudan")) %>%
  mutate(name = str_replace(name, "Vatican", "Holy See")) %>%
  mutate(name = str_replace(name, "Dem. Rep. Korea", "North Korea")) %>%
  mutate(name = str_replace(name, "Dominican Rep.", "Dominican Republic")) %>%
  mutate(name = str_replace(name, "St. Kitts and Nevis", "Saint Kitts and Nevis")) %>%
  mutate(name = str_replace(name, "St. Vin. and Gren.", "Saint Vincent and The Grenadines"))

em_18_39 <- em_18_39 %>% 
  mutate(Destinazione = str_replace(Destinazione, "Macedonia, The Former Yugoslav Republic of", "Macedonia")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Moldova, Republic of", "Moldova")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Russian Federation", "Russia")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Congo, The Democratic Republic of The", "DRC")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Libyan Arab Jamahiriya", "Lybia")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Tanzania, United Republic of", "Tanzania")) %>% 
  mutate(Destinazione = str_replace(Destinazione, "Brunei Darussalam", "Brunei")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Korea, Republic of", "Korea")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Korea, Democratic People's Republic of", "North Korea")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Iran, Islamic Republic of", "Iran")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Lao People's Democratic Republic", "Lao PDR")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Syrian Arab Republic", "Syria")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Taiwan, Province of China", "Taiwan")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Palestinian Territory, Occupied", "Palestine")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Viet Nam", "Vietnam")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Bolivia, Plurinational State of", "Bolivia")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Venezuela, Bolivarian Republic of", "Venezuela")) %>%
  mutate(Destinazione = str_replace(Destinazione, "Micronesia, Federated States of", "Micronesia"))
  
#NB there is no "Tuvalu" in centroids and Vatican and south sudan doesn't show up 

#Joining the datasets 
em_18_39 <- left_join(em_18_39, centroids, by = c("Destinazione" = "name")) 
#NB: there are some countries to fix cause they don't match up! 

Longitude_IT <- 12.4964
Latitude_IT <- 41.902

Italy <- data.frame(Longitude_IT, Latitude_IT)

em_18_39 <- cbind(em_18_39, Italy)

#STARTING TO MAP:----

#Basic map with all the centroids and geom segments! 
(world_basic <- ggplot() +
   geom_polygon(data = world, aes(x=long, y = lat, group = group), col="#f2f2f2", bg="black", lwd=0.005) +
   ylim(-80,80) +
   geom_point(data = em_18_39, aes(x = Longitude, y = Latitude, colour = Destinazione)) +
   geom_segment(data = em_18_39, aes(y=Latitude_IT, x=Longitude_IT, xend=Longitude, yend=Latitude, colour = Destinazione), alpha = 0.2)+
   theme_minimal() +
   theme(legend.position = "none") +
   coord_map())

#DATA STACKED PLOT:----
em_tot <- read.csv("Cartography/final_assessment/map_1_data/emigrazione_tot_world.csv")

em_tot$X2009 <- as.numeric(em_tot$X2009)
em_tot$X2010 <- as.numeric(em_tot$X2010)
em_tot$X2011 <- as.numeric(em_tot$X2011)
em_tot$X2012 <- as.numeric(em_tot$X2012)
em_tot$X2013 <- as.numeric(em_tot$X2013)
em_tot$X2014 <- as.numeric(em_tot$X2014)
em_tot$X2015 <- as.numeric(em_tot$X2015)
em_tot$X2016 <- as.numeric(em_tot$X2016)
em_tot$X2017 <- as.numeric(em_tot$X2017)
em_tot$X2018 <- as.numeric(em_tot$X2018)
em_tot$X2019 <- as.numeric(em_tot$X2019)

str(em_tot)

#The value for 40-64 in 2014 was coming out of as an NA
em_tot[2, 7] = 37383

#Adding a column for % difference between 2009 and 2019 
em_tot <- em_tot %>% 
  mutate(perc_diff = (((em_tot$X2019 - em_tot$X2009)/em_tot$X2009) *100))

em_tot <- gather(em_tot, Year, Emigrations,  #in this order: data frame, key, value
                   c(X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019))  

str(em_tot)

em_tot$Year <- as.factor(em_tot$Year)
em_tot$X <- as.factor(em_tot$X)
str(em_tot)


#STACKED PLOT:----

(stacked <- ggplot(em_tot, aes(x = Year , y = Emigrations, fill = X))) + 
   geom_area(position = "stack"))
 