# DAN Coursework 2 script for graphs
# data: 8th November 2021

## Loading Packages--- 
library(tidyverse) #this contains the ggplot package for graphing 

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
    labs(y = "Mean Sea Level (m)", x = "\nDataset", fill = "Dataset") +
    scale_fill_manual(values = c("#D690FF", "#9BC2E6")) + #choosing the color for the two datasets
    coord_flip() + #flipping the x and y for easier readability 
    theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 13, face = "plain")))
#ggsave(violin_sp, file = "Analytical and numerical methods/coursework 2/img/P_S_boxplot.png", height = 4, width = 6)





