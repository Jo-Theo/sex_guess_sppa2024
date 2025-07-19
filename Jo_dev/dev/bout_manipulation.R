

##############################
### Packages and functions ###
##############################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)
library(ggplot2)
library(tidymodels)

##############################
### Sub_sessions functions ###
##############################

# Divide a session into subsession of size min length_min a sub_session is a continuous serie of Nas
.sub_session <- function(behaviours, length_min,
                          bird_male = T ){
  if(length_min<0){
    stop('length_min is a positive or null integer')
  }
  #Femelle/puis male
  bird_activity <- if(bird_male){behaviours$M_in}else{behaviours$F_in} # exctract bird behaviour
  na_pos <- c(0,which(is.na(bird_activity)), length(bird_activity)+1) # Record Na position and add fakes na before 1 and after last bout
  sub_session <- map2(na_pos[-1],
                       na_pos[-length(na_pos)],
                       function(x,y){
                         between <- (y+1):(x-1) # Takes all event between Na flags 
                         if(length(between)<length_min | (length(between)>1 && between[1] > between[2])){
                           return(NULL) # If list too short or if list is reversed (y = x) do not return it
                         }
                         return(between) # Else send those positions as one sub_session
                       }) %>% # return in list shape 
    compact() %>% # erase all NULL elemebt from a list
    map2_dfr(1:length(.),
             ~data.frame(id_sub_session = .y,row = .x)) # create Id for each session and return position
  behaviours %>% 
    mutate(Sub_session = 1:dim(.)[1] %>% 
             map_int(~if(.x %in% sub_session$row){# Return subsession when exists
               return(sub_session$id_sub_session[which(sub_session$row == .x)])
             }else{
               return(NA)
             }))
}

# wrapper by session  
sub_session_by_session <- function(behaviours, length_min = 1){
  session_order <- unique(behaviours$Session)
  behaviours %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map_dfr(~.sub_session(.x,length_min)) %>% # Bout each of them
    mutate(Sub_session = ifelse(is.na(Sub_session),NA, # cross info Session and sub_session
                                 paste0(Session,'_',Sub_session)))%>%
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session) %>% 
    relocate(Sub_session, .after = Session )
}

##########################
### Individual bouting ###
##########################

### So we have all sub session but we want to get rid of first and last states of each session (for male in/out and female in/out). 
# We do this because we are not sure that the stating state actually started at the beginning of the sub_session 
# So the bout length information isn't underestimated we will erode all sub_session from their first and last stages

# Takes a bird activity (in a sub_session) column and givses at each lines a
# personnal bout number everytime  (no more pair behaviour but split female and male ) 
.indiv_bout_nb <- function(splited_bout, na_last_first = FALSE){
  bout_nb <- c(T,splited_bout[-1] != splited_bout[-length(splited_bout)]) %>% # List of changes 
    as.numeric() %>% 
    cumsum() # cumulative Summing every changes across the vector, This is the list of bout id
  if(na_last_first){
      bout_nb[which(bout_nb == 1 | bout_nb == max(bout_nb))] <- NA # erase the first and last steps because we are unsure of ther begining and end
  }
  return(bout_nb)
}

# create columns giving the bout number for female and male
.get_bout_ind <- function(behaviours, na_last_first = FALSE){
  behaviours %>% 
    mutate(male_bout =  .indiv_bout_nb(behaviours$M_in, na_last_first),
           female_bout = .indiv_bout_nb(behaviours$F_in, na_last_first))
} 

# Based on the bout number contract the Pair bouts information (here we can add other descritors)
.contract_bout <- function(behaviours,male_bird = T){
  if(male_bird){
    split_by <- behaviours$male_bout
    to_erase <- c("F_in","U_in","male_bout", "female_bout")
    rename_this <- "M_in"
    sex <- "M"
  }else{
    split_by <- behaviours$female_bout
    to_erase <- c("M_in","U_in","male_bout", "female_bout")
    rename_this <- "F_in"
    sex <- "F"
  }
  behaviours %>% 
    split(split_by) %>% # list of dataframe by individual bout
    map_dfr(function(bout){
      res_bout <- bout[1,] # Onle line df keeping all infos
      res_bout[to_erase] <- NULL # erase sex related columns (Sex will be a new variable )
      res_bout$Sex <- sex # Giving sex
      res_bout$Bout_length <- sum(bout$Bout_length) # sum nest bout 
      res_bout$Nb_bird <- sum(bout$Nb_bird*bout$Bout_length)/res_bout$Bout_length # mean of bird number weighted by time 
      res_bout %>% 
        rename_at(rename_this,~'Bird_in')
    })
  
}

# Final wrapper takes get known individual bouts by continuous sub_session  
extract_individual_bout <- function(behaviours){
  sub_session_order <- unique(behaviours$Sub_session) # subsession order in table 
  behaviours %>% 
    split(.$Sub_session) %>% 
    map_dfr(function(sub_session){ # for each subsession
      sub_session <- .get_bout_ind(sub_session, na_last_first = T) # create bout ids
      bind_rows(
        .contract_bout(sub_session,male_bird = F), # female bout
        .contract_bout(sub_session,male_bird = T) # male bout
      )
    }) %>% # Bout each of them
    mutate(Sub_session = factor(Sub_session,sub_session_order)) %>% # reorder
    arrange(Sub_session) %>% 
    mutate(Id_bird = paste0(Pair,"_",Sex)) # create personnal ID for bird from pair and sex 
}
