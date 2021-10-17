# COURSEWORK 1 - ANALYTICAL AND NUMERICAL METHODS 

# Libraries----
library(tidyverse)  # for data wrangling
library(ggthemes)   # for a mapping theme
library(plotrix)    # for the standard error 

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

# Finding outliers----

#we are plotting the distribution of 
  #the data because it might be useful to visually identify the outliers

plot(eastings)
plot(northings)

## Finding initial mean and standard error----
  # NB: as we remove outliers the mean and standard error will- 
  #-change but this is the starting point

# useful link: https://statsandr.com/blog/outliers-detection-in-r/#introduction

summary(northings$N_m) #this gives us a summary of the data 
std.error(northings$N_m) #sd error: 0.5004469

# Results: 
#  Min.  1st Qu.  Median  Mean   3rd Qu.  Max. 
# 186274  186338  186343  186342  186347  186388 
# The mean is what we care about for the excercise = 186342

summary(eastings$E_m)
std.error(eastings$E_m) #sd error: 0.4541849

# Results: 
#  Min.    1st Qu. Median  Mean    3rd Qu. Max. 
#  502063  502120  502123  502123  502128  502165 
# The mean is what we care about for the excercise = 502123

# Histogram of distribution 

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

#They both have a normal distribution 

## Calculating the z-score of all values----

# I think what i need to do is use mutate to add a column and add the formula there 

mean(eastings$E_m) # value: 502123.1
sd(eastings$E_m) #standard deviation: 9.950692

eastings_comp <- eastings %>% 
  mutate(zscore = (E_m - mean(E_m))/sd(E_m))

mean(eastings_comp$zscore) #does the mean of zscore has to be 0? I read it somewhere but couldn't find it confirmed
sd(eastings_comp$zscore) #the sd for z scores is 1 


