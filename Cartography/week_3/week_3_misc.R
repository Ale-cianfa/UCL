boroughs_i <- boroughs_i %>% 
  mutate(Borough = str_replace(Borough, "City of London", "City")) %>%  
  mutate(Borough = str_replace(Borough, "Tower Hamlets", "Tower H.")) %>% 
  mutate(Borough = str_replace(Borough, "Hammersmith and Fulham", "Hammersmith")) %>% 
  mutate(Borough = str_replace(Borough, "Barking and Dagenham", "Barking")) %>% 
  mutate(Borough = str_replace(Borough, "Kensington and Chelsea", "Kensington")) %>% 
  mutate(Borough = str_replace(Borough, "Kingston upon Thames", "Kingston")) %>% 
  mutate(Borough = str_replace(Borough, "Richmond upon Thames", "Richmond")) %>% 
  mutate(Borough = str_replace(Borough, "Waltham Forest", "Waltham F.")) 
