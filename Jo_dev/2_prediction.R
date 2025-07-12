
# .rs.restartR()
# rm(list = ls(all.names = T))

##############################
### Packages and functions ###
##############################

source("jo_dev/dev/preprossesing_functions.R")

################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/bouted_behaviours.csv")

########################
### Data exploration ###
########################


behaviours %>% 
  split(.$Session) %>% 
  map_dfr(~data.frame(nb_U_remaning = sum(.x$U_in),
                      length_of_session = dim(.x)[1],
                      row.names = unique(.x$Session)))

# Not evenly divided between session


behaviours %>% 
  split(.$Session) %>% 
  map_dfr(~data.frame(nb_U_remaning = sum(.x$U_in),
                      length_of_session = dim(.x)[1],
                      row.names = unique(.x$Session)))
