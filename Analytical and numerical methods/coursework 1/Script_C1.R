# COURSEWORK 1 - ANALYTICAL AND NUMERICAL METHODS - WORKSHEET
# Author: Alessandra Cianfanelli

## Libraries----
library(tidyverse)  # for data wrangling and plotting

# checking the wd 
getwd()

## Importing the data----
my_data <- read.csv("Analytical and numerical methods/coursework 1/my_data_aanm_c1.csv")
str(my_data) #Checking for what type of variables are in the dataset 

## Splitting the datasets----

# Here i am splitting the dataset into northings and eastings - 
  # - as these are treated as independent from eachother for the purpose of this excercise 

# Northings: 
northings <- my_data %>% 
  dplyr::select(- E..m.) %>% 
  rename(Time_s = Time..s., N_m =N..m.)

# Eastings: 
eastings <- my_data %>% 
  dplyr::select(- N..m.) %>% 
  rename(Time_s = Time..s., E_m =E..m.)

## Finding initial mean and standard error----
  # NB: as we remove outliers the mean and standard error will- 
  #-change but this is the starting point

options(digits = 10)

#EAST: 
mean(eastings$E_m) # Initial mean: 502123.0578
sd(eastings$E_m) #Initial standard deviation: 9.9507
  #NB: the sd() function is computed by calculating the square root of -
    # - the variance (which is calculated using with the var function and  n-1 as the denominator) 

mean(northings$N_m) #Initial mean: 186342.2976
sd(northings$N_m) #Initial standard deviation: 10.9642

# Histogram of initial distribution----

(dist_east <- ggplot(eastings, aes(x = E_m)) + 
    geom_histogram(binwidth = 5, colour = "#A22EDC", fill = "#D783FF") + 
   theme_classic() +
    xlab("Eastings") + 
    ylab("Count"))

#ggsave(dist_east, file = "Analytical and numerical methods/coursework 1/initial_E_dist.png", height = 4, width = 6)

(dist_north <- ggplot(northings, aes(x = N_m)) +
    geom_histogram(binwidth = 5, colour = "#7A81FF", 
                   fill = "#A4B4FD") + 
    theme_classic() +
    xlab("Northings") + 
    ylab("Count"))

#ggsave(dist_north, file = "Analytical and numerical methods/coursework 1/initial_N_dist.png", height = 4, width = 6)


# They both have normal distribution, which was already known, 
  # but it is also possible to see lots of values towards the tails of the function 

## Calculating the z-score of all values and removing outliers----

# NORTH: 
north_comp <- northings %>% 
  mutate(zscore = (N_m - mean(N_m))/sd(N_m)) %>% 
  filter(zscore <= 2.5758) %>% 
  filter(zscore >= -2.5758) 

#NB: 2.5758 is the critical value for removal fora 99% confidence two-tailed test, 
  # this was found using excel (refer to spreadsheet)

# Observation count northings afrer outlier removal: 463 

(dist_north2 <- ggplot(north_comp, aes(x = N_m)) +
    geom_histogram(binwidth = 5, colour = "#7A81FF", 
                   fill = "#A4B4FD") + 
    theme_classic() +
    xlab("Northings") + 
    ylab("Count"))

ggsave(dist_north2, file = "Analytical and numerical methods/coursework 1/N_distribution_after.png", height = 4, width = 6)

# From the histogram of distribution we can see that a lot of the values -
  #- that were at the end of the tail previously have now been removed 

# EAST: 

east_comp <- eastings %>% 
  mutate(zscore = (E_m - mean(E_m))/sd(E_m)) %>% 
  filter(zscore <= 2.5758) %>% 
  filter(zscore >= -2.5758)

# Observation count eastings afrer outlier removal: 462

(dist_east2 <- ggplot(east_comp, aes(x = E_m)) +
    geom_histogram(binwidth = 5, colour = "#A22EDC", fill = "#D783FF") + 
    theme_classic() +
    xlab("Eastings") + 
    ylab("Count")) 

ggsave(dist_east2, file = "Analytical and numerical methods/coursework 1/E_distribution_after.png", height = 4, width = 6)

# Similar to above, the curve is now much more centered without
  # a lot of the noise towards the end of the tails 

## Final Summary----

options(digits = 10) #making sure I have enough decimal places (4 to reflect the original dataset)

#NORTH: 
mean(north_comp$N_m) # value: 186343.0604
sd(north_comp$N_m) #standard deviation: 6.9732 

#EAST: 
mean(east_comp$E_m) # value: 502123.7236 
sd(east_comp$E_m) #standard deviation: 5.5803





