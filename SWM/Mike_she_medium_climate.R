#MIKE SHE CLIMATE SIMULATION HIGH RESULTS 


#Loading the packages----
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(lubridate) 
library(naniar)

getwd()

#Loading the csvs----
obs_wl <- read.csv("SWM/Mike_she/observed_WL.csv")
obs_discharge <- read.csv("SWM/Mike_she/observed_discharge.csv")
medium_wl <- read.csv("SWM/Mike_she/medium_climate_wl.csv")
medium_discharge <- read.csv("SWM/Mike_she/medium_discharge_mike_she.csv")

# Wrangling the data----
medium_discharge$Time <- as.Date(medium_discharge$Time, format = "%d/%m/%Y" )
obs_discharge$Time <- as.Date(obs_discharge$Time, format = "%d/%m/%Y")
obs_wl$Time <- as.Date(obs_wl$Time, format = "%d/%m/%Y")
medium_wl$Time <- as.Date(medium_wl$Time, format = "%d/%m/%Y")

obs_wl <- obs_wl %>% 
  replace_with_na(replace = list(Obs_5 = -999.000, 
                                 Obs_35 = -999.000, 
                                 Obs_37 = -999.000, 
                                 Obs_65 = -999.000))

#Climate Simulation low graph for discharge at Hagebro----
(medium_climate_discharge_hag <- ggplot() +
   geom_point(data = obs_discharge, 
              aes(x = Time, y = observed_karup_at_hagebro), size = 0.2, color = "#d2b7e5") +
   geom_line(data = medium_discharge, 
             aes(x = Time, y= Karup_at_Hagebro, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
    ylim(3,7.5) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
   ggtitle("Medium Climate Simulation vs. Observed measurements of water discharge at Hagebro"))

ggsave("SWM/IMG/discharge_mid_hag.png", plot = medium_climate_discharge_hag, height = 3, width = 8)

#Calibration graph for discharge at Karup----
(medium_climate_discharge_kar <- ggplot() +
   geom_point(data = obs_discharge, 
              aes(x = Time, y = observed_karup_at_karup), size = 0.1, color = "#d2b7e5") +
   geom_line(data = medium_discharge, 
             aes(x = Time, y= Karup_at_Karup, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
    ylim(1.5,3.25) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
   ggtitle("Medium Climate Simulation vs. Observed measurements of water discharge at Karup"))

ggsave("SWM/IMG/discharge_mid_kar.png", plot = medium_climate_discharge_kar, height = 3, width = 8)

#Climate simulation low graph for WL Obsv 5----
(obsv_5_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_5), size = 1, color = "#61a5c2") +
   geom_line(data = medium_wl, 
             aes(x = Time, y= mid_Obs_5), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Medium Climate Simulation vs. Modelled Water Level at Borehole n.5"))

ggsave("SWM/IMG/obs5_mid.png", plot = obsv_5_wl, height = 3, width = 8)

#Climate simulation low graph for WL Obsv 35----
(obsv_35_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_35), size = 1, color = "#61a5c2") +
   geom_line(data = medium_wl, 
             aes(x = Time, y= mid_Obs_35), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Medium Climate Simulation vs. Modelled Water Level at Observation Borehole n.35"))

ggsave("SWM/IMG/obs35_mid.png", plot = obsv_35_wl, height = 3, width = 8)

#Climate simulation low graph for WL Obsv 37----
(obsv_37_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_37), size = 1, color = "#61a5c2") +
   geom_line(data = medium_wl, 
             aes(x = Time, y= mid_Obs_37), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Medium Climate Simulationvs. Modelled Water Level at Observation Borehole n.37"))

ggsave("SWM/IMG/obs37_mid.png", plot = obsv_37_wl, height = 3, width = 8)

#Climate simulation low graph for WL Obsv 65----
(obsv_65_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_65), size = 1, color = "#61a5c2") +
   geom_line(data = medium_wl, 
             aes(x = Time, y= mid_Obs_65), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Medium Climate Simulation vs. Modelled Water Level at Observation Borehole n.65"))

ggsave("SWM/IMG/obs65_mid.png", plot = obsv_65_wl, height = 3, width = 8)



