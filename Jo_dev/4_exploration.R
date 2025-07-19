
# .rs.restartR()
rm(list = ls(all.names = T))

##############################
### Packages and functions ###
##############################

source("jo_dev/dev/bout_manipulation.R")

################### 
### Import data ###
###################

behaviours <- read.csv2("Jo_dev/Data/bouted_behaviours_SPPA2024_1.csv")%>% 
  mutate(TimeAction = as_hms(TimeAction),
         BreedingStage = as.factor(BreedingStage),
         Pair = as.factor(Pair),
         Nest = as.factor(Nest),
         Date = as.Date(Date),
         Year = as.factor(Year))
sub_session_behaviours <- read.csv2("Jo_dev/Data/sub_session_behaviours_SPPA2024_1.csv") %>% 
  mutate(TimeAction = as_hms(TimeAction),
         BreedingStage = as.factor(BreedingStage),
         Pair = as.factor(Pair),
         Nest = as.factor(Nest),
         Date = as.Date(Date),
         Year = as.factor(Year))
individual_bout <- read.csv2("Jo_dev/Data/individual_bouts_SPPA2024_1.csv") %>% 
  mutate(TimeAction = as_hms(TimeAction),
         BreedingStage = as.factor(BreedingStage),
         Pair = as.factor(Pair),
         Nest = as.factor(Nest),
         Date = as.Date(Date),
         Year = as.factor(Year),
         Sex = as.factor(Sex),
         Id_bird = as.factor(Id_bird))



########################
### Data exploration ###
########################


# Looking for number of long sessions 

sub_session_behaviours$Sub_session %>% 
  as.factor() %>% 
  summary() %>% 
  sort()


# Only one rep for this Breeding stage
individual_bout %>% 
  filter(BreedingStage == "Digging, Building")

# Lets look into the homogeneity of sex unknown in across time for every Bird_ID / Breeding Stage (kinda true individual behaviour steps) 

full_join(
  behaviours %>% 
    group_by(BreedingStage,Pair) %>% 
    reframe(Nb_na = sum(is.na(F_in))) %>% 
    mutate(Pair = as.factor(Pair)),
  individual_bout %>% 
    group_by(Sub_session,BreedingStage,Pair) %>% 
    reframe(Female_session_length = sum(Sex == "F"),
            Male_session_length = sum(Sex == "M")) %>% 
    ungroup() %>% 
    group_by(BreedingStage,Pair) %>% 
    reframe(Female_session_length_max = max(Female_session_length),
            Male_session_length_max = max(Male_session_length),
            Number_of_session = n())
)

# Where the na are usually session are almost empty for at least one bird maybe 
# this while more data is included but I think if some bird could be treated 
# individually we will have to treat together for most of the exercise 



#####################
### Visualization ###
#####################

individual_bout %>% 
  ggplot(aes(x = Bout_length, fill = BreedingStage)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Density Plot by Pair",
       x = "Bout length",
       y = "Density") +
  theme_minimal()


individual_bout %>% 
ggplot(aes(x = Bout_length, fill = Sex)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Density Plot by sex",
       x = "Bout length",
       y = "Density") +
  theme_minimal()

individual_bout %>% 
  mutate(Pair = as.factor(Pair)) %>% 
  ggplot(aes(x = Bout_length, fill = Pair)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Density Plot by Pair",
       x = "Bout length",
       y = "Density") +
  theme_minimal()

individual_bout %>% 
  ggplot(aes(x = Bout_length, fill = Id_bird)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(title = "Density Plot by Pair",
       x = "Bout length",
       y = "Density") +
  theme_minimal()



##################################
### I - GLMnet poisson models  ###
##################################

library(glmnet)


# Split data into train and test
set.seed(421)
split <- individual_bout[,c("Bout_length","Bird_in","Nb_bird", "TimeAction", "BreedingStage","Pair","Nest","Date","Sex","Id_bird")] %>% 
  initial_split( prop = 0.8, strata = "Id_bird")



train <- split %>% 
  training()
test <- split %>% 
  testing()

x_matrices <- makeX(train = train[,-1],test = test[,-1])


cvfit <- cv.glmnet(x_matrices$x, train$Bout_length, alpha = 0.2, family = "poisson")



tidy(cvfit)

plot(cvfit)


mirror <- x_matrices$xtest 
mirror[,c('SexM','SexF')] <- mirror[,c('SexF','SexM')]



pred_means <- predict(cvfit,
                      newx = x_matrices$xtest,
                      type = "response")
pred_means_mirror <- predict(cvfit,
                      newx = mirror,
                      type = "response")

probabilities <- data.frame(True = ppois(test$Bout_length, pred_means, lower.tail = FALSE),
                            False = ppois(test$Bout_length, pred_means_mirror, lower.tail = FALSE))

###########################
### II - Random forest  ###
###########################

# library(randomForest)
# ind <- sample(2, nrow(individual_bout), replace = TRUE, prob = c(0.7, 0.3))
# train <- individual_bout[ind==1,]
# test <- individual_bout[ind==2,]
# 
# rf <- randomForest(Bout_length ~ Bird_in + TimeAction + BreedingStage + Pair + Nest + Date + Sex + Id_bird, data=train, proximity=TRUE) 
# print(rf)
# 
# p1 <- predict(rf, train)
# confusionMatrix(p1, train$ Species)

#################################
### III -  Markov type models ###
#################################