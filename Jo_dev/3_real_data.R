
# .rs.restartR()
rm(list = ls(all.names = T))

##############################
### Packages and functions ###
##############################

source("jo_dev/dev/preprossesing_functions.R")

################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/behaviours_SPPA2024_1.csv",
                       colClasses = "character", na.strings = "", 
                       stringsAsFactors = FALSE)


################################
### Cleaning and exploration ###
################################


behav_session <- cleaning_table(behaviours = behaviours,
                                other_activity = T,
                                date_format = "%d/%m/%Y")
### Look at the number of cases

c("Sex","Activity","Activity_code") %>% 
  map(~behav_session[.x] %>% summary())



##############################################
### Estimation of bird number and flagging ###
##############################################


behav_session_bird_pred <- guess_nb_of_bird_by_session(behaviours = behav_session, 
                                                       include_flag = T)

warnings()
## empty session or almost empty

behav_session_bird_pred %>% 
  split(.$Session) %>% 
  map_chr(~ifelse(max(.x$Nb_bird) == 2, "not empty",
              ifelse(max(.x$Nb_bird) == 1, "Only one bird at a time",
                     ifelse(max(.x$Nb_bird) == 0,"No bird",'problem')))) %>% 
  as.factor() %>% 
  summary()

## there a lot of empty sessions :
# We erase them for rest of analysis since there is nothing to learn from it 
# We erase them for rest of analysis since there is nothing to learn from it 
list_empty <- list_empty_session(behav_session_bird_pred)

behav_session_not_empty <- remove_sessions(behav_session,list_empty)
behav_session_bird_pred_not_empty <- remove_sessions(behav_session_bird_pred,list_empty)


behav_session_bird_pred %>% 
  filter(!is.na(Flag)) %>% 
  pull(Flag)


behav_session_bird_pred %>% 
  write.csv2("Jo_dev/Data/flagged_behaviours_SPPA2024_1.csv", row.names = F)

#############################################
### Filling missing events and rechecking ###
#############################################


## correction by session based on nb of bird pred before
corrected_behaviours <- add_missing_event_by_session(behaviours = behav_session_not_empty,
                                                     nb_bird_prediction = behav_session_bird_pred_not_empty$Nb_bird)

## check id any flags
corrected_behaviours_flag <-  guess_nb_of_bird_by_session(corrected_behaviours,
                                                          include_flag = T)

corrected_behaviours_flag %>% 
  filter(!is.na(Flag)) %>% 
  pull(Flag)

### here still some flags !!!!!!!! To check or recucive correction
corrected_behaviours_flag <-  add_missing_event_by_session(corrected_behaviours,
                                                      corrected_behaviours_flag$Nb_bird) %>% 
  guess_nb_of_bird_by_session(include_flag = T)


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
  write.csv2("Jo_dev/Data/complete_behaviours_SPPA2024_1.csv", row.names = F)

###################################################################
### Bouting dataset and nest status definition (M_in,F_in,U_in) ###
###################################################################

# complete_behaviours <- read.csv2("Jo_dev/Data/complete_behaviours.csv")


bouted_behav <- bouting_by_session(complete_behaviours)

any(bouted_behav$Bout_length<0)

bouted_behav %>% 
  write.csv2("Jo_dev/Data/bouted_behaviours_SPPA2024_1.csv", row.names = F)



################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/bouted_behaviours.csv")

########################
### Data exploration ###
########################
