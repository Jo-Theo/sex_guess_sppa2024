

##############################
### Packages and functions ###
##############################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)

##############################
### Minisessions functions ###
##############################

# Divide a session into minisession of size min length_min 
.mini_session <- function(behaviours, length_min,
                          bird_male = T ){
  if(length_min<0){
    stop('length_min is a positive or null integer')
  }
  #Femelle/puis male
  bird_activity <- if(bird_male){behaviours$M_in}else{behaviours$F_in}
  na_pos <- c(0,which(is.na(bird_activity)), length(bird_activity)+1)
  mini_session <- map2(na_pos[-1],
                       na_pos[-length(na_pos)],
                       function(x,y){
                         between <- (y+1):(x-1)
                         if(length(between)<length_min | (length(between)>1 && between[1] > between[2])){
                           return(NULL)
                         }
                         return(between)
                       }) %>% 
    compact() %>% 
    map2_dfr(1:length(.),
             ~data.frame(id_minisession = .y,row = .x))
  behaviours %>% 
    mutate(Mini_session = 1:dim(.)[1] %>% 
             map_int(~if(.x %in% mini_session$row){
               return(mini_session$id_minisession[which(mini_session$row == .x)])
             }else{
               return(NA)
             }))
}

# wrapper by session  
mini_session_by_session <- function(behaviours, length_min = 1){
  session_order <- unique(behaviours$Session)
  behaviours %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map_dfr(~.mini_session(.x,length_min)) %>% # Bout each of them
    mutate(Mini_session = ifelse(is.na(Mini_session),NA,
                                 paste0(Session,'_',Mini_session)))%>%
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session)
}