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

# read in teams data
teams <- read_csv("teams.csv")

head(teams)

# change variable type for the factor variables
teams$Champion <- as.factor(teams$Champion)
teams$Finals <- as.factor(teams$Finals)

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

head(teams)

# select 70% of the teams data to form our training data and the remaining 30% to use as test data
noTrainRows <- floor(nrow(teams) * .7)
set.seed(4)
sampleRows <- sample(1:nrow(teams), noTrainRows, replace = F)
trainData <- teams[sampleRows,]
testData <- teams[-sampleRows,]




