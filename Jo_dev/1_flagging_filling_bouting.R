# .rs.restartR()
# rm(list = ls(all.names = T))


##############################
### Packages and functions ###
##############################

source("jo_dev/dev/preprossesing_functions.R")

################### 
### Import data ###
###################

behaviours <- read.csv("01_cleaning/input/Building.csv",
                       colClasses = "character", na.strings = "", 
                       stringsAsFactors = FALSE)




################################
### Cleaning and exploration ###
################################


behav_session <- cleaning_table(behaviours = behaviours,
                                other_activity = T,
                                date_format = "%m/%d/%Y")

### Look at the number of cases

c("Sex","Activity","Activity_code") %>% 
  map(~behav_session[.x] %>% summary())

##############################################
### Estimation of bird number and flagging ###
##############################################


behav_session_bird_pred <- guess_nb_of_bird_by_session(behaviours = behav_session, 
                                                       include_flag = T)

behav_session_bird_pred %>% 
  filter(!is.na(Flag)) %>% 
  pull(Flag)


behav_session_bird_pred %>% 
  write.csv2("Jo_dev/Data/flagged_behaviours.csv", row.names = F)

#############################################
### Filling missing events and rechecking ###
#############################################

## correction by session based on nb of bird pred before
corrected_behaviours <- add_missing_event_by_session(behaviours = behav_session,
                                                     nb_bird_prediction = behav_session_bird_pred$Nb_bird) 

## check id any flags
corrected_behaviours_flag <-  guess_nb_of_bird_by_session(corrected_behaviours,
                                                          include_flag = T)

corrected_behaviours_flag %>% 
  filter(!is.na(Flag)) %>% 
  pull(Flag)

# data is complete ! 

##################################
### Filtering other Activities ###
##################################

complete_behaviours <- corrected_behaviours_flag %>% 
  select(-Flag) %>%
  filter(Activity %in% c("EX","EN")) 

complete_behaviours %>% 
  write.csv2("Jo_dev/Data/complete_behaviours.csv", row.names = F)

###################################################################
### Bouting dataset and nest status definition (M_in,F_in,U_in) ###
###################################################################

# complete_behaviours <- read.csv2("Jo_dev/Data/complete_behaviours.csv")

bouted_behav <- bouting_by_session(complete_behaviours)

any(bouted_behav$Bout_length<0)

bouted_behav %>% 
  write.csv2("Jo_dev/Data/bouted_behaviours.csv", row.names = F)

