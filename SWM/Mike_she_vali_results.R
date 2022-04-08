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
   ggtitle("Validation vs. Observed measurements of water discharge at Hagebro"))

#Calibration graph for discharge at Karup----
(discharge_kar <- ggplot() +
   geom_point(data = obs_discharge_vali, 
              aes(x = Time, y = observed_karup_at_karup), size = 0.1, color = "#d2b7e5") +
   geom_line(data = vali_discharge, 
             aes(x = Time, y= karup_at_karup, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
   ggtitle("Validation vs. Observed measurements of water discharge at Karup"))

#Calibration graph for WL Obsv 5----
(obsv_5_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_5), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_5), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Validated vs. Modelled Water Level at Borehole n.5"))

#Calibration graph for WL Obsv 35----
(obsv_35_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_35), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_35), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.35"))

#Calibration graph for WL Obsv 37----
(obsv_37_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_37), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_37), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.37"))

#Calibration graph for WL Obsv 65----
(obsv_65_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_65), size = 1, color = "#61a5c2") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs.65), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Validated vs. Modelled Water Level at Observation Borehole n.65"))



