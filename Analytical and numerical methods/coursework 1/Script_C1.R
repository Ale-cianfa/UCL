# COURSEWORK 1 - ANALYTICAL AND NUMERICAL METHODS 

# Libraries----
library(tidyverse)  # for data wrangling
library(ggthemes)   # for a mapping theme

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











