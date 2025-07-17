
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
  mutate(Bird_in = as.factor(Bird_in),
         TimeAction = as_hms(TimeAction),
         BreedingStage = as.factor(BreedingStage),
         Pair = as.factor(Pair),
         Nest = as.factor(Nest),
         Date = as.Date(Date),
         Year = as.factor(Year),
         Sex = as.factor(Sex),
         Id_bird = as.factor(Id_bird))
individual_bout %>% 
  map()

glm(data = individual_bout, formula = Bird_in ~ I(Bout_length**2) + Bout_length + Nb_bird + TimeAction + BreedingStage + Pair + Nest + Date + Sex + Id_bird, family = )


library(randomForest)
library(tidymodels)

# Split data into train and test
set.seed(421)
split <- initial_split(individual_bout, prop = 0.8, strata = Bird_in)
train <- split %>% 
  training()
test <- split %>% 
  testing()

model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Bird_in ~ I(Bout_length**2) + Bout_length + Nb_bird + TimeAction + BreedingStage + Pair + Nest + Date + Sex + Id_bird, data = train)

tidy(model)



pred_proba <- predict(model,
                      new_data = test,
                      type = "prob")
pred_class <- predict(model,
                      new_data = test,
                      type = "class")
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
