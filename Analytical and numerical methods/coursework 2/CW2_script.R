# DAN Coursework 2 script for graphs
# Candidate number: rvzk6
# dzta: 8th November 2021

## Loading Packages--- 
library(tidyverse) #this contains the ggplot package for graphing 
library(ggstatsplot)
library(palmerpenguins)

## Loading the Datasets and basic data wrangling----
primary <- read.csv("Analytical and numerical methods/coursework 2/Dan cw2 primary.csv")
  #^primary dataset after outlier removal
secondary <- read.csv("Analytical and numerical methods/coursework 2/Dan cw2 secondary.csv") 
  #^secondary dataset after outlier removal

primary$df <- "Primary" #adding a column that describes what dataframe (df) the data belongs to 

secondary$df <- "Secondary" #doing the same thing for the secondary dataset

total <- rbind(primary, secondary) #creating a final dataset including both primary and seocndary data

## Plottig----
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#this is the source necessary for the violin box plots (found trough the our coding club website tutorial, here:https://ourcodingclub.github.io/tutorials/dataviz-beautification-synthesis/)

(violin_sp <- ggplot(total, aes(df, MSL, group = df, fill = df)) + 
    geom_flat_violin(position = position_nudge(x = 0.15, y = 0), alpha = 0.4) + #The half violins
    geom_boxplot(width = 0.2, alpha = 1) + #The boxplots
    labs(y = "Mean Sea Level (m)", x = "\nCoordinate set", fill = "Coordinate set") +
    scale_fill_manual(values = c("#D690FF", "#9BC2E6")) +
    scale_colour_manual(values = c("darkorchid3", "#104E8B")) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 23)) +
    guides(color = "none") +
    theme_bw() + #without thegridline is theme_classic
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 13, face = "plain")))
ggsave(violin_sp, file = "Analytical and numerical methods/coursework 2/img/P_S_boxplot.png", height = 4, width = 6)

#Density plot: 
  (Deansity_p <- ggplot(primary, aes(x=MSL)) +
  geom_density(fill="#D690FF", color="#8A2BE2", alpha=0.8) +
  labs(y = "Frequency", x = "Mean Sea Level (m)") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")))

#ggsave(Deansity_p, file = "Analytical and numerical methods/coursework 2/img/distribution_p.png", height = 4, width = 6)


(Deansity_s <- ggplot(secondary, aes(x=MSL)) +
    geom_density(fill="#9BC2E6", color="#00688B", alpha=0.8) +
    labs(y = "Frequency", x = "Mean Sea Level (m)") +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))
#ggsave(Deansity_s, file = "Analytical and numerical methods/coursework 2/img/distribution_p.png", height = 4, width = 6)
