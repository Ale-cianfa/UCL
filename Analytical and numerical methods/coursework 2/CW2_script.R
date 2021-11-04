# Coursework 2 script 

#Packages 
library(tidyverse)

# Loading the Datasets 
primary <- read.csv("Analytical and numerical methods/coursework 2/Dan cw2 primary.csv") #csv file with all the traits
secondary <- read.csv("Analytical and numerical methods/coursework 2/Dan cw2 secondary.csv") #csv file with all the traits

primary$df <- "Primary"

secondary$df <- "Secondary"

total <- rbind(primary, secondary)

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(violin <- ggplot(total, aes(df, MSL, group = df, fill = df)) + 
    geom_flat_violin(position = position_nudge(x = 0, y = 0), alpha = 0.4) + #The half violins
    geom_boxplot(width = 0.4, alpha = 1) + #The boxplots
    labs(y = "Mean Sea Level (m)", x = "\nCoordinate set", fill = "Coordinate set") +
    scale_fill_manual(values = c("#D690FF", "#9BC2E6")) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 23)) +
    theme_bw() + #without thegridline is theme_classic
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

#Density plot: 
  (Deansity_p <- ggplot(primary, aes(x=MSL)) +
  geom_density(fill="#D690FF", color="#8A2BE2", alpha=0.8) +
  labs(y = "Frequency", x = "Mean Sea Level (m)") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain")))


(Deansity_s <- ggplot(secondary, aes(x=MSL)) +
    geom_density(fill="#9BC2E6", color="#00688B", alpha=0.8) +
    labs(y = "Frequency", x = "Mean Sea Level (m)") +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))
