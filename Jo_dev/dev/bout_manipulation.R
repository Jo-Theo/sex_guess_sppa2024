

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
.sub_session <- function(behaviours, length_min,
                          bird_male = T ){
  if(length_min<0){
    stop('length_min is a positive or null integer')
  }
  #Femelle/puis male
  bird_activity <- if(bird_male){behaviours$M_in}else{behaviours$F_in}
  na_pos <- c(0,which(is.na(bird_activity)), length(bird_activity)+1)
  sub_session <- map2(na_pos[-1],
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
             ~data.frame(id_sub_session = .y,row = .x))
  behaviours %>% 
    mutate(Sub_session = 1:dim(.)[1] %>% 
             map_int(~if(.x %in% sub_session$row){
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
    mutate(Sub_session = ifelse(is.na(Sub_session),NA,
                                 paste0(Session,'_',Sub_session)))%>%
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session) %>% 
    relocate(Sub_session, .after = Session )
}
