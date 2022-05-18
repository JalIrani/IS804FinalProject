playerStats <- read.csv(file = './NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)


# Simple Linear Regression
summary(playerStats)
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=playerStats)
attach(playerStats)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
plot(predict(lm.fit), residuals(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=playerStats)
summary(lm.fit)
lm.fit=lm(medv~.,data=playerStats)
summary(lm.fit)
lm.fit1=lm(medv~.-age,data=playerStats)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)
