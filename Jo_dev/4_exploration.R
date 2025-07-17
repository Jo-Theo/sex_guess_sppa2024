
# .rs.restartR()
rm(list = ls(all.names = T))

##############################
### Packages and functions ###
##############################

source("jo_dev/dev/bout_manipulation.R")

################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/bouted_behaviours_SPPA2024_1.csv")

########################
### Data exploration ###
########################

# defining sub_session
sub_session_behaviours <- sub_session_by_session(behaviours, length_min = 1)

### So we have all sub session but we want to get rid of first and last states of each session (for male in/out and female in/out). 
# We do this because we are not sure that the stating state actually started at the beginning of the sub_session 
# So the bout length information isn't underestimated we will erode all sub_session from their first and last stages

# Looking for number of long sessions 

sub_session_behaviours$Sub_session %>% 
  as.factor() %>% 
  summary() %>% 
  sort()


i <- 1

get_bout_ind <- function(behaviours, na_last_first = FALSE){
  behaviours %>% 
    mutate(male_bout =  .indiv_bout_nb(behaviours$M_in, na_last_first),
           female_bout = .indiv_bout_nb(behaviours$F_in, na_last_first))
} 


behaviours <- sub_session_behaviours %>% 
  filter(Sub_session == unique(Sub_session)[i])

get_bout_ind(behaviours,T)







