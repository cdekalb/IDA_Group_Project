library(tidyverse)
library(caret)

# Introduction to the problem

# The problem we want to examine is the prediction of the winner of the 2019-2020 NBA champion.

# Description of the data

# Our data is the total season stats for every NBA team since the implementation of the 3-point line (the 1979-1980 season). We
# chose this because it gives us a sufficient amount of data, while also minimizing missingness, namely the 3-point stats for
# each team before the 1979-1980 being NA's. Our intial data only contained in-game statistics and did not give us any data for
# the team from an overal season perspective; thus through additional research, we added columns pertaining to whether a given
# team for a given season won the championship. We also added a column noting whether a given team made the NBA finals for that
# season.

# Exploratory data analysis

# Preprocessing

# read in teams data
teams <- read_csv("teams.csv")

# read in players data
players <- read_csv("players.csv") 

head(teams)

# change variable type for the factor variables
teams$Champion <- as.logical(teams$Champion)
teams$Finals <- as.logical(teams$Finals)
teams$Champion <- ifelse(teams$Champion == 1, "Yes", "No")
teams$Finals <- ifelse(teams$Finals == 1, "Yes", "No")

# add features that might be relevant by finding shot percentages and per game stats
teams$`FG%` <- teams$FG / teams$FGA
teams$`2P%` <- teams$`2P` / teams$`2PA`
teams$`3P%` <- teams$`3P` / teams$`3PA`
teams$`FT%` <- teams$FT / teams$FTA
teams$TRBperG <- teams$TRB / teams$G
teams$ASTperG <- teams$AST / teams$G
teams$STLperG <- teams$STL / teams$G
teams$BLKperG <- teams$BLK / teams$G
teams$PTSperG <- teams$PTS / teams$G
teams$PFperG <- teams$PF / teams$G

head(teams)

# Filter out observations for 'Total' Stats
players <- players %>% filter(Tm != "TOT")

# Check missingness
players %>% mutate_all(is.na) %>% summarise_all(mean) %>% glimpse

# Impute 0 for percentage fields that had 0 attempts
players<-players %>% replace_na(list(`FG%` = 0, `2P%` = 0, `3P%` = 0, `eFG%` = 0, `FT%` = 0, `TS%` = 0))

# Aggregate player data per team and season
players_aggr <- players %>%
  group_by(Tm, Season) %>%
  summarise(
    Age_mean = mean(Age),
    WS_mean = mean(WS),
    BPM_mean = mean(BPM),
    VORP_mean = mean(VORP),
    priorYearAllNBA_sum = sum(priorYearAllNBA)
  )

teams <- teams %>% inner_join(players_aggr, by=c("Tm","Season"))

# select 70% of the teams data to form our training data and the remaining 30% to use as test data
noTrainRows <- floor(nrow(teams) * .7)
set.seed(4)
sampleRows <- sample(1:nrow(teams), noTrainRows, replace = F)
trainData <- teams[sampleRows,]
testData <- teams[-sampleRows,]

# Plot exploration

# ggplot(data = teams, aes(x = teams$Finals, y = teams$`FG%`)) + geom_boxplot() + labs(x = "Finals", y = "FG%")
# # FG% appears to be a decent predictor of Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$`2P%`)) + geom_boxplot() + labs(x = "Finals", y = "2P%")
# # 2P% appears to be a decent predictor of Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$`3P%`)) + geom_boxplot() + labs(x = "Finals", y = "3P%")
# # 3P% appears to have little to no correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$`FT%`)) + geom_boxplot() + labs(x = "Finals", y = "FT%")
# # FT% appears to have little to no correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$TRBperG)) + geom_boxplot() + labs(x = "Finals", y = "TRBperG")
# # TRBperG appears to be a decent predictor of Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$ASTperG)) + geom_boxplot() + labs(x = "Finals", y = "ASLperG")
# # ASTperG appears to be a decent predictor of Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$STLperG)) + geom_boxplot() + labs(x = "Finals", y = "STLperG")
# # STLperG appears to have minimal correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$BLKperG)) + geom_boxplot() + labs(x = "Finals", y = "BLKperG")
# # BLKperG appears to have minimal correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$PTSperG)) + geom_boxplot() + labs(x = "Finals", y = "PTSperG")
# # PTSperG appears to have minimal correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$PFperG)) + geom_boxplot() + labs(x = "Finals", y = "PFperG")
# # PFperG appears to have minimal correlation with Finals
# 
# ggplot(data = teams, aes(x = teams$Finals, y = teams$`W/L%`)) + geom_boxplot() + labs(x = "Finals", y = "W/L%")
# # W/L% appears to be a very good predictor of Finals

# Description of modeling approach

# We used a generalized linear model on our training data to implement logistic regression, with logLoss as our accuracy 
# measurement. Our intial choices for model features were the features that we created, in addition to W/L%. We chose these
# because they scale each team data so that team data can be compared across years. From our exploratory data analysis, we
# expected 3P%, FT%, STLperG, BLKperG, and PTSperG to not be significant predictors in our generalized linear model. However,
# after running our model we found that the set of features 2P%, 3P%, TRBperG, ASTperG, PTSperG, and W/L% provides the best
# performing model, as found by taking our original model and removing insignificant features until our model AIC was minimized.

# In the future, we plan on implementing other logisitc regression techniques to see how mour model can be improved.

control = trainControl(method = "repeatedcv", 
                       number = 10, 
                       classProbs = TRUE, 
                       summaryFunction=mnLogLoss, 
                       verboseIter = TRUE)


################################################################################
# glm Model
################################################################################
glm_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG + Age_mean + WS_mean +
                  BPM_mean + VORP_mean + priorYearAllNBA_sum
                ,data= trainData, 
                method = "glm", 
                family = binomial(), 
                metric = "logLoss", 
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(glm_fit)

# glm_predictions <-predict(object=glm_fit, newdata = testData, type="prob")[,2]
# 
# # probabilities
# glm_probs<-tibble(Team=testData$Tm, Season=testData$Season, predProbOfFinalsBerth=glm_predictions, predFinalsBerth=glm_predictions>0.5)


################################################################################
# MARS Model
################################################################################

mars_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG +
                  BPM_mean + priorYearAllNBA_sum
                ,data= trainData,
                method = "earth",
                # family = binomial(),
                metric = "logLoss",
                glm=list(family=binomial),
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(mars_fit)

# mars_predictions <-predict(object=mars_fit, newdata = testData, type="prob")[,2]
# 
# # probabilities
# mars_probs<-tibble(Team=testData$Tm, Season=testData$Season, predProbOfFinalsBerth=mars_predictions, predFinalsBerth=mars_predictions>0.5)
# 

################################################################################
# ctree Model
################################################################################

dtree_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG +
                  BPM_mean + priorYearAllNBA_sum
                ,data= trainData,
                method = "ctree",
                # family = binomial(),
                metric = "logLoss",
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(dtree_fit)

################################################################################
# rf Model
################################################################################

rf_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG +
                  BPM_mean + priorYearAllNBA_sum
                ,data= trainData,
                method = "rf",
                # family = binomial(),
                metric = "logLoss",
                # glm=list(family=binomial),
                # tuneLength=5,
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(rf_fit)

################################################################################
# svm Model
################################################################################

svm_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG +
                 BPM_mean + priorYearAllNBA_sum
               ,data= trainData,
               method = "svmRadial",
               # family = binomial(),
               metric = "logLoss",
               # glm=list(family=binomial),
               # tuneLength=5,
               trControl = control,
               preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(svm_fit)

################################################################################
# xgb Model
################################################################################

xgb_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG +
                  BPM_mean + priorYearAllNBA_sum
                ,data= trainData,
                method = "xgbDART",
                # family = binomial(),
                metric = "logLoss",
                # glm=list(family=binomial),
                tuneLength=5,
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(xgb_fit)



 #  #predict xgb and select the 'yes' predicitions
 # xgb_predictions <- predict(object = xgb_fit, newdata = testData, type="prob")[,2]
 #  
  # # probabilities
  # xgb_probs<-tibble(Team=testData$Tm, Season=testData$Season, predProbOfFinalsBerth=xgb_predictions, predFinalsBerth=xgb_predictions>0.5)


################################################################################
# Model predictions for test
################################################################################

# Set which model to use for predictions and calculation of confusion matrix
### NOTE: Change the model for the object = '' variable, to choose the model to compare 
model_predictions = predict(object = glm_fit, newdata = testData, type="prob")[,2]

# compare predictions to test
actual <- ifelse(testData$Finals == "Yes", 1, 0)
  
pred <- as.numeric(model_predictions>0.5)

m <- confusionMatrix(as.factor(pred), as.factor(actual), positive="1")

m






