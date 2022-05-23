## Change this to fix your working cirection
setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

set.seed(1)
# 50%-%0% split for training and testing
train=sample(296,148)
lm.fit=lm(Age~PTS,data=playerStats,subset=train)
attach(playerStats)
#compute MSE on testing data
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#a new random 50%-50% split of training and testing data
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(Age~G+GS+MP+FG+FGA+FGP+3P+3PA+3PP+2P+2PA+2PP+eFGP+FG+FTA+FTP,data=Auto)
coef(glm.fit)
lm.fit=lm(Age~G+GS+MP+FG+FGA+FGP+3P+3PA+3PP+2P+2PA+2PP+eFGP+FG+FTA+FTP,data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(Age~G+GS+MP+FG+FGA+FGP+3P+3PA+3PP+2P+2PA+2PP+eFGP+FG+FTA+FTP,data=Auto)
cv.err=cv.glm(playerStats,glm.fit)
?cv.glm
cv.err$delta

#use cv to evaluate Polynomial models with different degrees
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
degree=1:5
#type="b": plot for both points and lines
plot(degree,cv.error, type="b")

# 10-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(Age~poly(G+GS+MP+FG+FGA+FGP+3P+3PA+3PP+2P+2PA+2PP+eFGP+FG+FTA+FTP,i),data=Auto)
  cv.error.10[i]=cv.glm(playerStats,glm.fit,K=10)$delta[1]
}
cv.error.10
degree=1:10

#type="b": plot for both points and lines
plot(degree,cv.error.10, type="b")
