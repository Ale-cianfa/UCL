#MIKE SHE VALIDATION RESULTS

#Loading the packages----
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(lubridate) 
library(naniar)

getwd()

#Loading the csvs----
vali_wl <- read.csv("SWM/Mike_she/validation_mike_she.csv")
obs_wl <- read.csv("SWM/Mike_she/observed_WL.csv")
vali_discharge <- read.csv("SWM/Mike_she/discharge_validation_ms.csv")
obs_discharge <- read.csv("SWM/Mike_she/observed_discharge.csv")

# Wrangling the data----
vali_discharge$Time <- as.Date(vali_discharge$Time, format = "%d/%m/%Y" )
obs_discharge$Time <- as.Date(obs_discharge$Time, format = "%d/%m/%Y")
obs_wl$Time <- as.Date(obs_wl$Time, format = "%d/%m/%Y")
vali_wl$Time <- as.Date(vali_wl$Time, format = "%d/%m/%Y")

vali_discharge <- filter(vali_discharge, Time >= "1976-01-01")
vali_wl <- filter(vali_wl, Time >= "1976-01-01")
obs_wl <- obs_wl %>% 
  replace_with_na(replace = list(Obs_5 = -999.000, 
                                 Obs_35 = -999.000, 
                                 Obs_37 = -999.000, 
                                 Obs_65 = -999.000))

obs_wl <- filter(obs_wl, Time >= "1976-01-01")
obs_discharge_vali <- filter(obs_discharge, Time >= "1976-01-01")

#Calibration graph for discharge at Hagebro----
(discharge_hag <- ggplot() +
   geom_point(data = obs_discharge_vali, 
              aes(x = Time, y = observed_karup_at_hagebro), size = 0.2, color = "#d2b7e5") +
   geom_line(data = vali_discharge, 
             aes(x = Time, y= karup_at_hagebro, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
    
   ggtitle("Validation vs. Observed measurements of water discharge at Hagebro"))

ggsave("SWM/IMG/discharge_vali_hag.png", plot = discharge_hag, height = 5, width = 8)

#Calibration graph for discharge at Karup----
(discharge_kar <- ggplot() +
   geom_point(data = obs_discharge_vali, 
              aes(x = Time, y = observed_karup_at_karup), size = 0.1, color = "#d2b7e5") +
   geom_line(data = vali_discharge, 
             aes(x = Time, y= karup_at_karup, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
   ggtitle("Validation vs. Observed measurements of water discharge at Karup"))

ggsave("SWM/IMG/discharge_vali_kar.png", plot = discharge_kar, height = 5, width = 8)

#Calibration graph for WL Obsv 5----
(obsv_5_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_5), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_5), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Validated vs. Modelled Water Level at Borehole n.5"))

ggsave("SWM/IMG/obs5_vli.png", plot = obsv_5_wl, height = 5, width = 8)

#Calibration graph for WL Obsv 35----
(obsv_35_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_35), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_35), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.35"))

ggsave("SWM/IMG/obs35_vli.png", plot = obsv_35_wl, height = 5, width = 8)

#Calibration graph for WL Obsv 37----
(obsv_37_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_37), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_37), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.37"))

ggsave("SWM/IMG/obs37_vli.png", plot = obsv_37_wl, height = 5, width = 8)

#Calibration graph for WL Obsv 65----
(obsv_65_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_65), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs.65), 
             color="#013a63") +
   theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.65"))

ggsave("SWM/IMG/obs65_vli.png", plot = obsv_65_wl, height = 5, width = 8)


