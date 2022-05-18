playerStats <- read.csv(file = './NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

View(playerStats)
dim(playerStats)
playerStats[1:4, ]
###
playerStats <- na.omit(playerStats)
dim(playerStats)
###
names(playerStats)


plot(playerStats$Age, playerStats$FG)
attach(playerStats)
plot(Age, FG)
###
Age <- as.factor(Age)
###
plot(Age, FG)
plot(Age, FG, col = "red")
plot(Age, FG, col = "red", varwidth = T,
     horizontal = T)
plot(Age, FG, col = "red", varwidth = T,
     xlab = "cylinders", ylab = "MPG")
###
hist(FG)
hist(FG, col = 2)
###
pairs(playerStats)
###pairs(
 ### ~ mpg + displacement + horsepower + weight + acceleration,
  ###data = Auto
###)
###
plot(horsepower, FG)
###
summary(playerStats)
###
summary(FG)



###
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









