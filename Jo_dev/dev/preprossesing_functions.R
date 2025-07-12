
################
### Packages ###
################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)

##########################
### Cleaning functions ###
##########################

## Correct 0024 -> 2024 error type
.year_corrector <- function(date){
  if(any(year(date)<2000)){
    warning("years before 2000 where corrected this way 0024 -> 2024")
    year(date) <- ifelse(year(date)<2000,year(date)+2000,year(date))
  }
  return(date)
}


## Detect and correct error in date format, ,
# try a set of different formats if there is no na in one of them it keep this 
# format and give warning 
.dumb_date_corrector <- function(date,format,all_formats = c("%d/%m/%Y","%m/%d/%Y","%d/%m/%y","%m/%d/%y")){
  if(date %>% unique() %>% as.Date(format = format) %>% is.na() %>% any()){ # if the indicated format gives NA
    format_nb <- 1
    # try a set of different formats
    while(date %>% unique() %>% as.Date(format = all_formats[format_nb]) %>% is.na() %>% any()){ 
      format_nb <- format_nb + 1
      if(format_nb == 5){ # if no format works return given one and give a warning
        warning("SessionStartRealDate has different date format used indicated one, NA induced")
        return(.year_corrector(as.Date(date, format = format)))
      }
    }
    # if there is no na in one of them it keep this format and give warning for change
    format <- all_formats[format_nb]
    warning(paste("detected wrong format switch to",format,"no NA induced"))
  }
  return(.year_corrector(as.Date(date, format = format)))
}


#### Extraction of nest name and pair name

.get_nest_pair <- function(nest_id){
  str_locate_all(nest_id,"\\.") %>% 
    map2_dfr(nest_id,function(point_pos,ID){
      if(dim(point_pos)[1] >= 3){
        data.frame(Nest = substr(ID,point_pos[1,1]+1,point_pos[2,1]-1),
                   Pair = substr(ID,point_pos[2,1]+1,point_pos[3,1]-1))
      }else{
        data.frame(Nest = substr(ID,point_pos[1,1]+1,str_length(ID)),
                   Pair = NA)
      }
      })
}


### Clean behaviour table
# other_activity = F => we erase all non exits or enters

cleaning_table <- function(behaviours, other_activity = F, date_format = "%d/%m/%Y"){ 
  behaviours <- behaviours %>% 
    mutate(
      # Creation of session id
      Date = .dumb_date_corrector(SessionStartRealDate,format = date_format),
      Session = str_c(BreedingAttempt, Date, sep = "_"), 
      # standardization of sex
      Sex = toupper(Sex),
      Sex = case_when(
        Sex == "FEMALE" ~ "F",
        Sex == "FEMALE " ~ "F",
        Sex == "MALE" ~ "M",
        Sex == "MALE "~ "M",
        Sex == "DNR"~"U",
        Sex == "NA"~"U",
        TRUE ~ Sex) %>% 
        as.factor(),
      # standardization of Activities
      Activity = tolower(Activity),
      Activity = case_when(
        str_detect(Activity,"enter") ~ "EN",
        str_detect(Activity,"exit") ~ "EX",
        # is.na(Activity) ~ "NA", if we want to keep those lines
        other_activity ~ Activity, # if we want to keep other activity than entrance and exit
        !other_activity ~ 'to_erase'),  # other case
      # explicitation of time variables
      TimeAction = ifelse(AudRealTimeAction %in% c("NA","DNR"),NA,AudRealTimeAction) %>% as_hms(),
      Year = year(Date) %>% as.factor()
    ) %>% 
    filter(Activity != "to_erase") %>% # erase activity that we don't want
    mutate(Activity = as.factor(Activity),
           Activity_code = paste(Sex,Activity,sep = "_") %>% # merge sex and activity
             as.factor()) %>% 
    bind_cols(.get_nest_pair(.$BreedingAttempt)) # extraction nest and pair names
  
  if(any(is.na(behaviours$TimeAction))){
    session_Na_time <- behaviours %>%
      filter(is.na(TimeAction)) %>%
      pull(Session) %>%
      unique()
    all_na <- session_Na_time %>%
      map_lgl(~behaviours %>%
                filter(Session == .x) %>%
                pull(TimeAction) %>%
                is.na() %>%
                all())
    warning("TimeAction column has Na values thoes values are gonna be deleted")
    if(sum(all_na) > 0 ){
      warning("Sessions :",paste(session_Na_time[all_na], collapse = ", "), " are erased because only Na TimeAction")
    }
    behaviours <- behaviours %>%
      filter(!is.na(TimeAction))
  }
  
  session_order <- unique(behaviours$Session)
  behaviours %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map_dfr(~ arrange(.x,TimeAction)) %>% #to reorder time (cause i found some problems when bouting)
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session) %>% 
    mutate(EntryID = 1:dim(.)[1])
}



##################################
### Nb of bird count functions ###
##################################


## Take benefit of ex ex and en en situations to predict the number od bird in nest.
# flag the ex ex ex and en en en 
.double_guess <- function(activity){
  nb_bird <- rep(NA,length(activity))
  flag <- rep(NA,length(activity))
  # pour corriger les cas ou d'autres activités sont entre les deux
  nest_change <- which(activity %in% c('EN','EX'))
  if(length(nest_change) == 0){
    warning("Some sessions didn't have any enters or exits : Nb bird max = 0")
    return(data.frame(nb_bird = rep(0,length(activity)), flag = flag, activity = activity))
  }else if(length(nest_change) == 1){
    warning("Some sessions have only one enters or exits : Nb bird max = 1")
    return(data.frame(nb_bird = c(rep(as.numeric(activity[nest_change] == "EX"),nest_change-1),
                                  rep(as.numeric(activity[nest_change] == "EN"),length(activity) - nest_change + 1)),
                      flag = flag, activity = activity))
  }else if(!any(activity[nest_change[-1]] == activity[nest_change[-length(nest_change)]])){
    warning("Some sessions have no En En or Ex Ex : Nb bird max = 1")
    return(data.frame(nb_bird = c(rep(as.numeric(activity[nest_change[1]] == "EX"),nest_change[1]-1),
                                  rep(as.numeric(activity[nest_change[1]] == "EN"),length(activity) - nest_change[1] + 1)),
                      flag = flag, activity = activity))
  }
  for (i in 1:(length(nest_change)-1)){
    pos1 <- nest_change[i]
    pos2 <- nest_change[i+1]
    if(i>1){
      pos0 <- nest_change[i-1]
    }
    if (activity[pos1] == "EN" & activity[pos2] == "EN"){
      if(i>1 &&  activity[pos0] == "EN" & activity[pos1] == "EN"){
        flag[pos2] <- paste("missing exit between line",pos0,"and",pos2)
      }
      nb_bird[pos1] <- 1
      nb_bird[pos2] <- 2
    }else if(activity[pos1] == "EX" & activity[pos2] == "EX"){
      if(i>1 &&  activity[pos0] == "EX" & activity[pos1] == "EX"){
        flag[pos2] <- paste("missing enter between line",pos0,"and",pos2)
      }
      nb_bird[pos1] <- 1
      nb_bird[pos2] <- 0
    }
  }
  return(data.frame(nb_bird = nb_bird, flag = flag, activity = activity))
}

## Predict backward the number of bird in nest given an initial known point and 
# var = -1 if exit 1 if enter
.backward_guess <- function(from,to,init_status,var){
  map_int(from:to, ~ init_status - sum(var[(.x+1):(to + 1)]))
}

## Predict forward the number of bird in nest given an initial known point and 
# var = -1 if exit 1 if enter
.forward_guess <- function(from,to,init_status,var){
  map_int(from:to, ~ init_status + sum(var[from:.x]))
}

## Predict the missing values in number of bird starting from closest known point using ex and en
# It predict by interval between known status
# flag when 2 intervals don't join correctly
.closest_neighbour_pred <- function(double_guess_pred){
  nb_bird <- double_guess_pred$nb_bird
  flag <- double_guess_pred$flag
  activity <- double_guess_pred$activity
  
  known_position <- which(!is.na(nb_bird)) 
  var <- ifelse(activity != "EN",ifelse(activity == "EX",-1,0),1) # bird leaving -1  bird coming +1
  nb_inter <- length(known_position)+1
  
  for(i in 1:nb_inter){ # for each interval
    if(i == 1){ # first interval we go backward from first sure state to t == 1
      from <- 1
      to <- known_position[1]-1
      if(to >= from){
        nb_bird[from:to] <- .backward_guess(from, to, init_status = nb_bird[to+1], var)
      }
    }else if(i == length(known_position)+1){ # last interval we go forward from last sure state to t == end
      from <- known_position[length(known_position)]+1
      to <- length(nb_bird)
      if(to >= from){
        nb_bird[from:to] <- .forward_guess(from, to, init_status = nb_bird[from-1],var)
      }
    }else{ # all intervals in between we go forward from begin to end
      from <-known_position[i-1]+1
      to <- known_position[i]
      if(to > from){
        pred <- .forward_guess(from,to, init_status = nb_bird[from-1],var)
        last_pred_error <- pred[length(pred)] - nb_bird[to]
        if(last_pred_error != 0){ # we flag if next sure state is not fitting with the forward guess
          what <- ifelse(last_pred_error>0,"exits","enters")
          flag[to] <- paste("missing",abs(last_pred_error),what,"between line",from-1,"and",to + 1)
        }
        nb_bird[from:(to-1)] <- pred[-length(pred)]
      }
    }
  }
  flag[nb_bird>2] <- "missing exit this flag could be mixt another one"
  flag[nb_bird<0] <- "missing enter this flag could be mixt another one"
  return(data.frame(Nb_bird = nb_bird, Flag = flag))
}

## predict the missing values in number of bird starting from closest known point using ex and en
# Wrap the functions doing prediction and for the table by Session
guess_nb_of_bird_by_session <- function(behaviours, include_flag = T){
  behaviours <- behaviours %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map_dfr(~ bind_cols(.x, .x$Activity %>% 
                          .double_guess() %>% 
                          .closest_neighbour_pred())) %>% 
    relocate(EntryID,Session,Sex,Activity,Nb_bird,Flag,Activity_code,TimeAction,BreedingStage,Pair,Nest,Date,Year) %>% 
    arrange(as.integer(EntryID))
  if(include_flag){
    behaviours
  }else{
    behaviours %>% 
      select(-Flag)
  }
}

## list_empty_session with no bird in nest
list_empty_session <- function(behaviours){
  behaviours %>% 
    split(.$Session) %>% 
    map_int(~if(max(.x$Nb_bird) == 0){1}else{0}) %>% 
    .[.==1] %>% 
    names()
}

remove_sessions <- function(behaviours,to_remove){
  session_order <- unique(behaviours$Session)
  behaviours %>% 
    filter(!Session %in% list_empty) %>% 
    mutate(Session = as.character(Session) %>% as.factor()) 
}
#############################################
### Filling missing events and rechecking ###
#############################################

### fill up detected missing event for 1 session
.add_missing_event <- function(behaviours,nb_bird_prediction){
  behaviours <- behaviours %>% 
    mutate(EntryID = 1:dim(.)[1])
  
  activity <- behaviours$Activity
  nest_change <- which(activity %in% c('EN','EX'))
  if(length(nest_change) <2){
    return(behaviours)
  }
  var <- ifelse(activity != "EN",ifelse(activity == "EX",-1,0),1)
  
  for (change in 1:(length(nest_change)-1)){
    pos1 <- nest_change[change]# to just look the changes 
    pos2 <- nest_change[change + 1]
    ecart <- nb_bird_prediction[pos1] + var[pos2] - nb_bird_prediction[pos2]
    if(ecart != 0){# if not [fitting
      random_line <- sample(pos1:pos2,1)  # we take a random line to copy
      temp_random_line_position <- which(behaviours$EntryID == random_line)[1] # real place of that line in modified table
      line_value <- behaviours[temp_random_line_position,]
      line_value$Activity <- ifelse(ecart > 0, "EX","EN")
      line_value$Comments <- "line added automaticaly to complete missing event"
      line_value$Age <- "U"
      line_value$Sex <- "U"
      line_value$Activity_code <- paste0("U_",line_value$Activity)
      behaviours <- behaviours[c(1:temp_random_line_position,temp_random_line_position:dim(behaviours)[1]),]   # line dupplication
      behaviours[sum(temp_random_line_position,random_line != pos2),] <- line_value   # overwritting
    }
  }
  behaviours
}


### fill up detected missing event for all sessions in once
add_missing_event_by_session <- function(behaviours,nb_bird_prediction){
  session_order <- unique(behaviours$Session) # to keep session order later
  split_nb_bird_prediction <- split(nb_bird_prediction,behaviours$Session) # Make a list of vector by one session
  behaviours %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map2_dfr(split_nb_bird_prediction,~ .add_missing_event(.x,.y)) %>% # apply completion by session
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session) %>% 
    mutate(EntryID = 1:dim(.)[1])
}


###################################################################
### Bouting dataset and nest status definition (M_in,F_in,U_in) ###
###################################################################

# guess status of male female or unknown based on number of bird and last activity
.guess_in_out <- function(behaviours){
  behaviours %>% 
    mutate(
      M_in = ifelse(Nb_bird == 2 | (Nb_bird == 1 & Activity_code %in% c("M_EN","F_EX")),1,0),
      F_in = ifelse(Nb_bird == 2 | (Nb_bird == 1 & Activity_code %in% c("F_EN","M_EX")),1,0),
      U_in = Nb_bird - M_in - F_in
    ) %>% 
    mutate_at(c("F_in","M_in"),~ifelse(U_in == 1,NA,.x))
}

# Bouting for one session
.bouting <- function(session){
  if(dim(session)[1] <= 1){
    warning("Session ", unique(session$Session)," doesn't have enough events to build bouts : Session is erased")
    return(NULL)
  }
  session %>% 
    .[1:(dim(.)[1]-1),] %>% 
    mutate(Bout_length = session$TimeAction[-1] - session$TimeAction[-dim(session)[1]])
}

# Bouting for all session in once
bouting_by_session <- function(behaviours){
  session_order <- unique(behaviours$Session) # to keep session order later
  behav_in_out <- .guess_in_out(behaviours) %>% # guess status 
    mutate(
      TimeAction = as_hms(TimeAction),
      Date = as_date(Date),
      Year = Year %>% as.factor()
    ) %>% 
    select(EntryID,Session,Nb_bird,M_in,F_in,U_in,TimeAction,BreedingStage,Pair,Nest,Date,Year) # only boutable vars
  
  behav_in_out %>% 
    split(.$Session) %>% # Make a list of data.frame each df is one session
    map_dfr(.bouting) %>% # Bout each of them
    mutate(Session = factor(Session,session_order)) %>% # reorder
    arrange(Session)
}





