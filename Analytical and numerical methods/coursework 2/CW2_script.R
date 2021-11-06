# DAN Coursework 2 script for graphs
# Candidate number: rvzk6
# dzta: 8th November 2021

## Loading Packages--- 
library(tidyverse) #this contains the ggplot package for graphing 
library(ggstatsplot)
library(palmerpenguins)

## Loading the Datasets and basic data wrangling----
primary <- read.csv("Analytical and numerical methods/coursework 2/Dan cw2 final primary.csv")
  #^primary dataset after outlier removal
secondary <- read.csv("Analytical and numerical methods/coursework 2/dan cw2 final secondary.csv") 
  #^secondary dataset after outlier removal

primary$df <- "Primary" #adding a column that describes what dataframe (df) the data belongs to 

secondary$df <- "Secondary" #doing the same thing for the secondary dataset

total <- rbind(primary, secondary) #creating a final dataset including both primary and seocndary data

## Plottig----
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#this is the source necessary for the violin box plots (found trough the our coding club website tutorial, here:https://ourcodingclub.github.io/tutorials/dataviz-beautification-synthesis/)

(violin_sp <- ggplot(total, aes(df, MSL, group = df, fill = df)) + 
    geom_flat_violin(position = position_nudge(x = 0.15, y = 0), alpha = 0.4) + 
      #^The half violins and their positions and shading
    geom_boxplot(width = 0.2, alpha = 1) + #The boxplots
    labs(y = "Mean Sea Level (m)", x = "\nCoordinate set", fill = "Coordinate set") +
    scale_fill_manual(values = c("#D690FF", "#9BC2E6")) + #choosing the color for the two datasets
    coord_flip() + #flipping the x and y for easier readability 
    theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 13, face = "plain")))
#ggsave(violin_sp, file = "Analytical and numerical methods/coursework 2/img/P_S_boxplot.png", height = 4, width = 6)

#Density distribution plot: 
  (Deansity_p <- ggplot(primary, aes(x=MSL)) + #plotting the mean sea level on the x axis and frequency on the y 
  geom_density(fill="#D690FF", color="#8A2BE2", alpha=0.8) +
  labs(y = "Frequency", x = "Mean Sea Level (m)") +
  theme_classic() + #removing the grid background
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")))

#ggsave(Deansity_p, file = "Analytical and numerical methods/coursework 2/img/distribution_p.png", height = 4, width = 6)


(Deansity_s <- ggplot(secondary, aes(x=MSL)) +
    geom_density(fill="#9BC2E6", color="#00688B", alpha=0.8) +
    labs(y = "Frequency", x = "Mean Sea Level (m)") +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

#ggsave(Deansity_s, file = "Analytical and numerical methods/coursework 2/img/distribution_s.png", height = 4, width = 6)

#Plotting of initial distribution before outlier removal----

primary_r <- read.csv("Analytical and numerical methods/coursework 2/DAN cw2 raw primary.csv")

secondary_r <- read.csv("Analytical and numerical methods/coursework 2/DAN cw2 raw secondary.csv")

#Density distribution plot: 
(raw_Deansity_p <- ggplot(primary_r, aes(x=MSL)) + #plotting the mean sea level on the x axis and frequency on the y 
    geom_density(fill="#D690FF", color="#8A2BE2", alpha=0.8) +
    labs(y = "Frequency", x = "Mean Sea Level (m)") +
    theme_classic() + #removing the grid background
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

#ggsave(raw_Deansity_p, file = "Analytical and numerical methods/coursework 2/img/raw_distribution_p.png", height = 4, width = 6)


(raw_Deansity_s <- ggplot(secondary_r, aes(x=MSL)) +
    geom_density(fill="#9BC2E6", color="#00688B", alpha=0.8) +
    labs(y = "Frequency", x = "Mean Sea Level (m)") +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))






