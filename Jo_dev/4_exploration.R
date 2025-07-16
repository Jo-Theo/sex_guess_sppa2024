
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
    Nb_na = .x %>% 
      pull(F_in) %>% 
      is.na() %>% 
      sum(),
    Prop_na = .x %>% 
      pull(F_in) %>% 
      is.na() %>% 
      mean() %>% 
      round(3)
  ))

test <- behaviours %>%
  filter(Session ==  unique(Session)[4])


# test <- behaviours %>% 
#   filter(Session ==  unique(Session)[4]) %>% 
#   pull(F_in) %>% 
#   as.character() %>% 
#   replace_na("NA")
# 
# two_next_diff_value <- function(vec,pos){
#   pos1 <- pos+1
#   while (vec[pos] == vec[pos1]) {
#     if(pos1 > length(vec)){
#       return(c(NA,NA))
#     }
#     pos1 <- pos1+1
#   }
#   pos2 <- pos1 + 1
#   while (vec[pos1] == vec[pos2]) {
#     if(pos2 > length(vec)){
#       return(c(pos1,NA))
#     }
#     pos2 <- pos2+1
#   }
#   return(c(pos1,pos2))
# }
# 
# two_next_diff_value(test,2)

