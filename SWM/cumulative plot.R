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
low_wl <- read.csv("SWM/Mike_she/climate_low_wl.csv")
low_discharge <- read.csv("SWM/Mike_she/climate_low_discharge.csv")
medium_wl <- read.csv("SWM/Mike_she/medium_climate_wl.csv")
medium_discharge <- read.csv("SWM/Mike_she/medium_discharge_mike_she.csv")
vali_wl <- read.csv("SWM/Mike_she/validation_mike_she.csv")
vali_discharge <- read.csv("SWM/Mike_she/discharge_validation_ms.csv")
cali_wl <- read.csv("SWM/Mike_she/calibration_WL.csv")
cali_discharge <- read.csv("SWM/Mike_she/Calibration_discharge_mike_she.csv")
high_wl <- read.csv("SWM/Mike_she/climate_high_wl.csv")
high_discharge <- read.csv("SWM/Mike_she/climate_high_discharge.csv")


# Wrangling the data----
low_discharge$Time <- as.Date(low_discharge$Time, format = "%d/%m/%Y" )
obs_discharge$Time <- as.Date(obs_discharge$Time, format = "%d/%m/%Y")
obs_wl$Time <- as.Date(obs_wl$Time, format = "%d/%m/%Y")
low_wl$Time <- as.Date(low_wl$Time, format = "%d/%m/%Y")
medium_discharge$Time <- as.Date(medium_discharge$Time, format = "%d/%m/%Y" )
medium_wl$Time <- as.Date(medium_wl$Time, format = "%d/%m/%Y")
vali_discharge$Time <- as.Date(vali_discharge$Time, format = "%d/%m/%Y" )
vali_wl$Time <- as.Date(vali_wl$Time, format = "%d/%m/%Y")
cali_discharge$Time <- as.Date(cali_discharge$Time, format = "%d/%m/%Y" )
cali_wl$Time <- as.Date(cali_wl$Time, format = "%d/%m/%Y")
high_discharge$Time <- as.Date(high_discharge$Time, format = "%d/%m/%Y" )
high_wl$Time <- as.Date(high_wl$Time, format = "%d/%m/%Y")



obs_discharge_cali <- filter(obs_discharge, Time <= "1975-12-31")
vali_discharge <- filter(vali_discharge, Time >= "1976-01-01")
vali_wl <- filter(vali_wl, Time >= "1976-01-01")

obs_wl <- obs_wl %>% 
  replace_with_na(replace = list(Obs_5 = -999.000, 
                                 Obs_35 = -999.000, 
                                 Obs_37 = -999.000, 
                                 Obs_65 = -999.000))

obs_discharge <- obs_discharge %>%
  mutate(newcol = 1)


#DISCHARGE----
##Discharge at Hagebro----
(cali_vali_kar <- ggplot() +
   geom_point(data = obs_discharge, 
              aes(x = Time, y = observed_karup_at_hagebro), size = 0.2, color = "#d2b7e5") +
   geom_line(data = vali_discharge, 
             aes(x = Time, y= karup_at_hagebro, group = 1),
             color = "#7b2cbf") +
   geom_line(data = cali_discharge, 
             aes(x = Time, y= Calibrated_Karup_at_Hagebro, group = 1), 
             color = "#240046") +
   ylim(3,7.5) +
   theme_minimal() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
   labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
   ggtitle("Calibration and Validation Discharge at Hagebro"))


#ggsave("SWM/IMG/cali_vali_hag.png", plot = cali_vali_hag, height = 3, width = 9)

##Discharge at Karup----
(cali_vali_kar <- ggplot() +
    geom_point(data = obs_discharge, 
               aes(x = Time, y = observed_karup_at_karup), size = 0.2, color = "#d2b7e5") +
    geom_line(data = vali_discharge, 
              aes(x = Time, y= karup_at_karup, group = 1),
              color = "#7b2cbf") +
    geom_line(data = cali_discharge, 
              aes(x = Time, y= Calibrated_Karup_at_Karup, group = 1), 
              color = "#240046") +
    ylim(1.5,3.25) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = expression(paste("Discharge ", m^{3},sec^{-1}))) +
    ggtitle("Calibration and Validation Discharge at Karup"))

ggsave("SWM/IMG/cali_vali_kar.png", plot = cali_vali_kar, height = 3, width = 9)


# OBSERVATIONS----
##Obs 5----
(obsv_5_wl <- ggplot() +
   geom_point(data = obs_wl, 
              aes(x = Time, y = Obs_5), size = 1, color = "#a9d6e5") +
   geom_line(data = vali_wl, 
             aes(x = Time, y= vali_Obs_5), 
             color="#468faf") +
   geom_line(data = cali_wl, 
             aes(x = Time, y= cali_obs_5), 
             color="#065a60") +
   theme_minimal() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
   ylab("Ground water elevation (m)") +
   ggtitle("Calibration and Validation Water Level at Borehole n.5"))

ggsave("SWM/IMG/obs5_vli.png", plot = obsv_5_wl, height = 3, width = 8)

##Obs 35----

(obsv_35_wl <- ggplot() +
    geom_point(data = obs_wl, 
               aes(x = Time, y = Obs_35), size = 1, color = "#a9d6e5") +
    geom_line(data = vali_wl, 
              aes(x = Time, y= vali_Obs_35), 
              color="#468faf") +
    geom_line(data = cali_wl, 
              aes(x = Time, y= cali_obs_35), 
              color="#065a60") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
    ggtitle("Calibration and Validation Water Level at Borehole n.35"))

ggsave("SWM/IMG/obs35.png", plot = obsv_35_wl, height = 3, width = 8)

##Obs 37----

(obsv_37_wl <- ggplot() +
    geom_point(data = obs_wl, 
               aes(x = Time, y = Obs_37), size = 1, color = "#a9d6e5") +
    geom_line(data = vali_wl, 
              aes(x = Time, y= vali_Obs_37), 
              color="#468faf") +
    geom_line(data = cali_wl, 
              aes(x = Time, y= cali_obs_37), 
              color="#065a60") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
    ggtitle("Calibration and Validation Water Level at Borehole n.37"))

ggsave("SWM/IMG/obs37.png", plot = obsv_37_wl, height = 3, width = 8)

##Obs 65----

(obsv_65_wl <- ggplot() +
    geom_point(data = obs_wl, 
               aes(x = Time, y = Obs_65), size = 1, color = "#a9d6e5") +
    geom_line(data = vali_wl, 
              aes(x = Time, y= vali_Obs.65), 
              color="#468faf") +
    geom_line(data = cali_wl, 
              aes(x = Time, y= cali_obs_65), 
              color="#065a60") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Ground water elevation (m)") +
    ggtitle("Calibration and Validation Water Level at Borehole n.65"))

ggsave("SWM/IMG/obs65.png", plot = obsv_65_wl, height = 3, width = 8)

