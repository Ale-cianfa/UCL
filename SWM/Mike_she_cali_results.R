#MIKE SHE CALIBRATION RESULTS

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
cali_wl <- read.csv("SWM/Mike_she/calibration_WL.csv")
cali_discharge <- read.csv("SWM/Mike_she/Calibration_discharge_mike_she.csv")

# Wrangling the data----
cali_discharge$Time <- as.Date(cali_discharge$Time, format = "%d/%m/%Y" )
obs_discharge$Time <- as.Date(obs_discharge$Time, format = "%d/%m/%Y")
obs_wl$Time <- as.Date(obs_wl$Time, format = "%d/%m/%Y")
cali_wl$Time <- as.Date(cali_wl$Time, format = "%d/%m/%Y")
str(cali_discharge)
str(obs_discharge)
str(obs_wl)
str(cali_wl)

obs_discharge_cali <- filter(obs_discharge, Time <= "1975-12-31")

obs_wl <- obs_wl %>% 
  replace_with_na(replace = list(Obs_5 = -999.000, 
                                 Obs_35 = -999.000, 
                                 Obs_37 = -999.000, 
                                 Obs_65 = -999.000))

obs_wl <- filter(obs_wl, Time <= "1975-12-31")

#Calibration graph for discharge at Hagebro----
(discharge_hag <- ggplot() +
  geom_point(data = obs_discharge_cali, 
             aes(x = Time, y = observed_karup_at_hagebro), size = 0.2, color = "#d2b7e5") +
  geom_line(data = cali_discharge, 
              aes(x = Time, y= Calibrated_Karup_at_Hagebro, group = 1), 
              color="#7b2cbf") +
  theme_minimal() +
  ggtitle("Calibration vs. Observed measurements of water discharge at Hagebro"))

#Calibration graph for discharge at Karup----
(discharge_kar <- ggplot() +
   geom_point(data = obs_discharge_cali, 
              aes(x = Time, y = observed_karup_at_karup), size = 0.1, color = "#d2b7e5") +
   geom_line(data = cali_discharge, 
             aes(x = Time, y= Calibrated_Karup_at_Karup, group = 1), 
             color="#7b2cbf") +
   theme_minimal() +
   ggtitle("Calibration vs. Observed measurements of water discharge at Karup"))

#Calibration graph for WL Obsv 5----
(obsv_5_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_5), size = 1, color = "#61a5c2") +
   geom_line(data = cali_wl, 
             aes(x = Time, y= cali_obs_5), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Calibrated vs. Modelled Water Level at Borehole n.5"))

#Calibration graph for WL Obsv 35----
(obsv_35_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_35), size = 1, color = "#61a5c2") +
   geom_line(data = cali_wl, 
             aes(x = Time, y= cali_obs_35), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Calibrated vs. Modelled Water Level at Observation Borehole n.35"))

#Calibration graph for WL Obsv 37----
(obsv_37_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_37), size = 1, color = "#61a5c2") +
   geom_line(data = cali_wl, 
             aes(x = Time, y= cali_obs_37), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Calibrated vs. Modelled Water Level at Observation Borehole n.37"))

#Calibration graph for WL Obsv 65----
(obsv_65_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_65), size = 1, color = "#61a5c2") +
   geom_line(data = cali_wl, 
             aes(x = Time, y= cali_obs_65), 
             color="#013a63") +
   theme_minimal() +
   ggtitle("Calibrated vs. Modelled Water Level at Observation Borehole n.65"))



