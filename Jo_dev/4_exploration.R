
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

sex_male <- T
i <- 1
splited_bout <- sub_session_behaviours %>% 
  filter(Sub_session == unique(Sub_session)[i]) %>% 
  pull(if(sex_male){"M_in"}else{"F_in"})


.indiv_bout_nb <- function(splited_bout){
  c(T,splited_bout[-1] != splited_bout[-length(splited_bout)]) %>% 
    as.numeric() %>% 
    cumsum()
}

data.frame(splited_bout,ind_bout = .indiv_bout_nb(splited_bout))

c(T,splited_bout[-1] != splited_bout[-length(splited_bout)]) %>% 
  as.numeric() %>% 
  cumsum()

