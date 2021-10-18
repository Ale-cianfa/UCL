# COURSEWORK 1 - ANALYTICAL AND NUMERICAL METHODS 
# Author: Alessandra Cianfanelli

# Libraries----
library(tidyverse)  # for data wrangling
library(ggthemes)   # for a mapping theme
library(plotrix) # for the standard error 
library(xlsx) # to save the dataset as an xlsx file so i can check it in excel 

# checking the wd 
getwd()

# Importing the data----
my_data <- read.csv("Analytical and numerical methods/coursework 1/my_data_aanm_c1.csv")
str(my_data)

# Should I split it into two datasets (one with time and northings and one with T and eastings)

# splitting datasets for northings and eastings and renaming the columns so is more legible

northings <- my_data %>% 
  dplyr::select(- E..m.) %>% 
  rename(Time_s = Time..s., N_m =N..m.)

eastings <- my_data %>% 
  dplyr::select(- N..m.) %>% 
  rename(Time_s = Time..s., E_m =E..m.)

## Finding initial mean and standard error----
  # NB: as we remove outliers the mean and standard error will- 
  #-change but this is the starting point

mean(eastings$E_m) # value: 502123.1
sd(eastings$E_m) #standard deviation: 9.950692

mean(northings$N_m) #value: 186342.3
sd(northings$N_m) #value: 10.96424 

# Histogram of initial distribution----

(dist_east <- ggplot(eastings, aes(x = E_m)) + 
    geom_histogram(binwidth = 5, colour = "#EEAD0E", fill = "gold") + 
    xlab("Eastings") + 
    ylab("Count") +
    theme_bw()) 

(dist_north <- ggplot(northings, aes(x = N_m)) +
    geom_histogram(binwidth = 5, colour = "#008B8B", 
                   fill = "#7AC5CD") + 
    xlab("Northings") + 
    ylab("Count") +
    theme_bw()) 
# They both have normal distribution but we already knew that 

#we can also plot the dsitribution of the data like
  #because it might be useful to visually identify the outliers: 

plot(eastings)
plot(northings)

## Calculating the z-score of all values----

# I think what i need to do is use mutate to add a column and add the formula there 

# EAST: 
eastings_comp <- eastings %>% 
  mutate(zscore = (E_m - mean(E_m))/sd(E_m)) # do it need to group them? 
str(eastings_comp)

mean(eastings_comp$zscore) #does the mean of zscore has to be 0? why is that not shown here but it is in the summary? 
sd(eastings_comp$zscore) #the sd for z scores is 1 
summary(eastings_comp$zscore) #with this the mean that is returned is 0.0000 so it shoudl be good! 

# NORTH: 
north_comp <- northings %>% 
  mutate(zscore = N_m - mean(N_m))

str(north_comp)

mean(north_comp$zscore) #
sd(north_comp$zscore)  
summary(north_comp$zscore)




