
# .rs.restartR()
rm(list = ls(all.names = T))

##############################
### Packages and functions ###
##############################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)

################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/bouted_behaviours_SPPA2024_1.csv")

########################
### Data exploration ###
########################

behaviours %>% 
  split(.$Session) %>% 
  map_dfr(~data.frame(
    Session = .x$Session[1],
    Prop_na = .x %>% 
      pull(F_in) %>% 
      is.na() %>% 
      mean() %>% 
      round(3)
  ))



for (i in 1:dim(test)[1]){
  
}