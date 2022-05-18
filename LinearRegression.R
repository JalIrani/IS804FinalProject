## Change this to fix your working cirection
setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)


# Simple Linear Regression
summary(playerStats)
lm.fit=lm(Age~FGA)
lm.fit=lm(Age~FGA,data=playerStats)
attach(playerStats)
lm.fit=lm(Age~FGA)
lm.fit
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
plot(Age,FGA)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(FGA,Age,col="red")
plot(FGA,Age,pch=20)
plot(FGA,Age,pch="+")
plot(1:20,1:20,pch=1:20)
plot(predict(lm.fit), residuals(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(Age~FGA+FTA,data=playerStats)
summary(lm.fit)
lm.fit=lm(Age~.,data=playerStats)
summary(lm.fit)
lm.fit1=lm(Age~.-FTA,data=playerStats)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-FTA)
