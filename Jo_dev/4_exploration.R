
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
sub_session_behaviours <- sub_session_by_session(behaviours, length_min = 1) %>% 
  mutate(Bout_length = ifelse(Bout_length == 0,1 ,Bout_length))

### So we have all sub session but we want to get rid of first and last states of each session (for male in/out and female in/out). 
# We do this because we are not sure that the stating state actually started at the beginning of the sub_session 
# So the bout length information isn't underestimated we will erode all sub_session from their first and last stages

# Looking for number of long sessions 

sub_session_behaviours$Sub_session %>% 
  as.factor() %>% 
  summary() %>% 
  sort()


individual_bout <- extract_individual_bout(sub_session_behaviours)

individual_bout %>% 
  filter(BreedingStage == "Digging, Building")

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

individual_bout <- individual_bout %>% 
  mutate(TimeAction = as_hms(TimeAction),
         BreedingStage = as.factor(BreedingStage),
         Pair = as.factor(Pair),
         Nest = as.factor(Nest),
         Date = as.Date(Date),
         Year = as.factor(Year),
         Sex = as.factor(Sex),
         Id_bird = as.factor(Id_bird))


# library(randomForest)

library(glmnet)
# Split data into train and test
set.seed(421)
split <- individual_bout[,c("Bout_length","Bird_in","Nb_bird", "TimeAction", "BreedingStage","Pair","Nest","Date","Sex","Id_bird")] %>% 
  initial_split( prop = 0.8, strata = "Id_bird")
train <- split %>% 
  training()
test <- split %>% 
  testing()


model <- glmnet(train[,-1], train$Bout_length, alpha = 0.2,family =  "poisson")



tidy(model)

plot(model)
# 
# pred_proba <- predict(model,
#                       newx = test[,-1],
#                       type = "response")
# pred_class <- predict(model,
#                       new_data = test,
#                       type = "link")
results <- test %>%
  select(Bird_in) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = Bird_in, estimate = .pred_class)




# ind <- sample(2, nrow(individual_bout), replace = TRUE, prob = c(0.7, 0.3))
# train <- individual_bout[ind==1,]
# test <- individual_bout[ind==2,]
# 
# rf <- randomForest(Bout_length ~ Bird_in + TimeAction + BreedingStage + Pair + Nest + Date + Sex + Id_bird, data=train, proximity=TRUE) 
# print(rf)
# 
# p1 <- predict(rf, train)
# confusionMatrix(p1, train$ Species)
