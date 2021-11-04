#COURSEWORK 1 - ANALYTICAL AND NUMERICAL METHODS - Figures
# Author: Alessandra Cianfanelli

## Libraries----
library(tidyverse)  # for data wrangling and plotting (dplyr and ggplot packages)

## Loading the data from my own working directory----
  #NB: these data sets were pasted from excel into a csv file in order to compute the graphs 

complete <- read.csv("Analytical and numerical methods/coursework 1/my_data_aanm_c1.csv")
# ^^this is the complete dataset with all of the coordinates before any outlier removal 
eastings <- read.csv("Analytical and numerical methods/coursework 1/Eastings.csv")
# ^^ this is the final easting dataset after outlier removal
northings <- read.csv("Analytical and numerical methods/coursework 1/northings.csv")
# ^^ this is the final northings dataset after outlier removal

## Brief data wrangling----
complete <- complete %>% 
  rename(Time_s = Time..s., N_m =N..m., E_m = E..m.) #renaming the columns for clarity

eastings <- eastings %>% 
  rename(Time_s = Time..s., E_m = E..m.)

northings <- northings %>% 
  rename(Time_s = Time..s., N_m =N..m.)

## Plotting Initial Distribution----

# Initial Distribution Eastings

(dist_east <- ggplot(complete, aes(x = E_m)) + 
   geom_histogram(binwidth = 4, colour = "#A22EDC", fill = "#D783FF") + 
   theme_classic() +
   xlab("Eastings") + 
   ylab("Frequency"))

#ggsave(dist_east, file = "Analytical and numerical methods/coursework 1/img/initial_E_dist.png", height = 4, width = 6)

# Initial Distribution Northings

(dist_north <- ggplot(complete, aes(x = N_m)) +
    geom_histogram(binwidth = 4, colour = "#7A81FF", 
                   fill = "#A4B4FD") + 
    theme_classic() +
    xlab("Northings") + 
    ylab("Frequency"))

#ggsave(dist_north, file = "Analytical and numerical methods/coursework 1/img/initial_N_dist.png", height = 4, width = 6)

## Plotting Distribution after Outlier removal----

# New Eastings Distribution

(final_east <- ggplot(eastings, aes(x = E_m)) +
   geom_histogram(binwidth = 4, colour = "#A22EDC", fill = "#D783FF") + 
   theme_classic() +
   xlab("Eastings") + 
   ylab("Frequency")) 

#ggsave(final_east, file = "Analytical and numerical methods/coursework 1/img/E_distribution_after.png", height = 4, width = 6)

# New Northings Distribution 

(final_north <- ggplot(northings, aes(x = N_m)) +
   geom_histogram(binwidth = 4, colour = "#7A81FF", 
                  fill = "#A4B4FD") + 
   theme_classic() +
   theme(plot.margin = margin(10, 25, 10, 10)) + #this allows me to print out the entire xaxis
   xlab("Northings") + 
   ylab("Frequency"))

#ggsave(final_north, file = "Analytical and numerical methods/coursework 1/img/N_distribution_after.png", height = 4, width = 6)





