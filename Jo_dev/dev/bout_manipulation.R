

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

##########################
### Individual bouting ###
##########################

.indiv_bout_nb <- function(splited_bout, na_last_first = FALSE){
  bout_nb <- c(T,splited_bout[-1] != splited_bout[-length(splited_bout)]) %>% 
    as.numeric() %>% 
    cumsum()
  if(na_last_first){
      bout_nb[which(bout_nb == 1 | bout_nb == max(bout_nb))] <- NA
  }
  return(bout_nb)
}


.get_bout_ind <- function(behaviours, na_last_first = FALSE){
  behaviours %>% 
    mutate(male_bout =  .indiv_bout_nb(behaviours$M_in, na_last_first),
           female_bout = .indiv_bout_nb(behaviours$F_in, na_last_first))
} 

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
    split(split_by) %>% 
    map_dfr(function(bout){
      res_bout <- bout[1,]
      res_bout[to_erase] <- NULL
      res_bout$Sex <- sex
      res_bout$Bout_length <- sum(bout$Bout_length)
      res_bout$Nb_bird <- sum(bout$Nb_bird*bout$Bout_length)/res_bout$Bout_length
      res_bout %>% 
        rename_at(rename_this,~'Bird_in')
    })
  
}

extract_individual_bout <- function(behaviours){
  sub_session_order <- unique(behaviours$Sub_session)
  behaviours %>% 
    split(.$Sub_session) %>% 
    map_dfr(function(sub_session){
      sub_session <- .get_bout_ind(sub_session, na_last_first = T)
      bind_rows(
        .contract_bout(sub_session,male_bird = F),
        .contract_bout(sub_session,male_bird = T)
      )
    }) %>% # Bout each of them
    mutate(Sub_session = factor(Sub_session,sub_session_order)) %>% # reorder
    arrange(Sub_session) %>% 
    mutate(Id_bird = paste0(Pair,"_",Sex))
}
