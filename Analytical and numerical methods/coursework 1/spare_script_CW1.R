# Spare Script for all CW1

# useful link: https://statsandr.com/blog/outliers-detection-in-r/#introduction

summary(northings$N_m) #this gives us a summary of the data 
std.error(northings$N_m) #sd error: 0.5004469

# Results: 
#  Min.  1st Qu.  Median  Mean   3rd Qu.  Max. 
# 186274  186338  186343  186342  186347  186388 
# The mean is what we care about for the excercise = 186342

summary(eastings$E_m)
std.error(eastings$E_m) #sd error: 0.4541849

# Results: 
#  Min.    1st Qu. Median  Mean    3rd Qu. Max. 
#  502063  502120  502123  502123  502128  502165 
# The mean is what we care about for the excercise = 502123

## Trying a different way to scale it in R (function scale())----
eastings_comp2 <- eastings %>% 
  mutate(z_score = scale(E_m))

northings_comp <- northings %>% 
  mutate(z_score = scale(N_m))

