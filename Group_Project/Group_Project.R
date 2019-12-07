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
player_team <- players %>% left_join(teams, by=c("Tm","Season"))

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

# select 70% of the teams data to form our training data and the remaining 30% to use as test data
noTrainRows <- floor(nrow(teams) * .7)
set.seed(4)
sampleRows <- sample(1:nrow(teams), noTrainRows, replace = F)
trainData <- teams[sampleRows,]
testData <- teams[-sampleRows,]

# Plot exploration

ggplot(data = teams, aes(x = teams$Finals, y = teams$`FG%`)) + geom_boxplot() + labs(x = "Finals", y = "FG%")
# FG% appears to be a decent predictor of Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$`2P%`)) + geom_boxplot() + labs(x = "Finals", y = "2P%")
# 2P% appears to be a decent predictor of Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$`3P%`)) + geom_boxplot() + labs(x = "Finals", y = "3P%")
# 3P% appears to have little to no correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$`FT%`)) + geom_boxplot() + labs(x = "Finals", y = "FT%")
# FT% appears to have little to no correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$TRBperG)) + geom_boxplot() + labs(x = "Finals", y = "TRBperG")
# TRBperG appears to be a decent predictor of Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$ASTperG)) + geom_boxplot() + labs(x = "Finals", y = "ASLperG")
# ASTperG appears to be a decent predictor of Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$STLperG)) + geom_boxplot() + labs(x = "Finals", y = "STLperG")
# STLperG appears to have minimal correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$BLKperG)) + geom_boxplot() + labs(x = "Finals", y = "BLKperG")
# BLKperG appears to have minimal correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$PTSperG)) + geom_boxplot() + labs(x = "Finals", y = "PTSperG")
# PTSperG appears to have minimal correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$PFperG)) + geom_boxplot() + labs(x = "Finals", y = "PFperG")
# PFperG appears to have minimal correlation with Finals

ggplot(data = teams, aes(x = teams$Finals, y = teams$`W/L%`)) + geom_boxplot() + labs(x = "Finals", y = "W/L%")
# W/L% appears to be a very good predictor of Finals

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

glm_fit = train(Finals ~ `2P%` + `3P%` + `W/L%` + PFperG, 
                data= trainData, 
                method = "glm", 
                family = binomial(), 
                metric = "logLoss", 
                trControl = control,
                preProcess = c("center", "scale")) #important to set metric to logLoss to tune for logloss minimization

summary(glm_fit)

glm_predictions <-predict(object=glm_fit, newdata = testData, type="prob")[,2]

glmSubmission<-tibble(Team=testData$Tm, Season=testData$Season, predProbOfFinalsBerth=glm_predictions, predFinalsBerth=glm_predictions>0.5)


pred <- as.numeric(glm_fit$finalModel$fitted.values>0.5)

teams$Finals <- ifelse(teams$Finals == "Yes", 1, 0)
trainData <- teams[sampleRows,]
testData <- teams[-sampleRows,]

actual <- as.numeric(trainData$Finals)


m <- confusionMatrix(as.factor(pred), as.factor(actual), positive="1")

m

# Initial results




