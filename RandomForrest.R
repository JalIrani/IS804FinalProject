## Bagging and Random Forests
## Change this to fix your working cirection
setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

set.seed(1)
train <- sample(1:nrow(playerStats), nrow(playerStats) / 2)

###
library(randomForest)
set.seed(1)
bag.playerStats <- randomForest(Age ~ ., data = playerStats,
                           subset = train, mtry = 12, importance = TRUE)
bag.playerStats
###
yhat.bag <- predict(bag.playerStats, newdata = playerStats[-train, ])
playerStats.test <- playerStats[-train, "Age"]
plot(yhat.bag, playerStats.test)
abline(0, 1)
mean((yhat.bag - playerStats.test)^2)
###
bag.playerStats <- randomForest(Age ~ ., data = playerStats,
                           subset = train, mtry = 12, ntree = 25)
yhat.bag <- predict(bag.playerStats, newdata = playerStats[-train, ])
mean((yhat.bag - playerStats.test)^2)
###
set.seed(1)
rf.playerStats <- randomForest(Age ~ ., data = playerStats,
                          subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.playerStats, newdata = playerStats[-train, ])
mean((yhat.rf - playerStats.test)^2)
###
importance(rf.playerStats)
###
varImpPlot(rf.playerStats)