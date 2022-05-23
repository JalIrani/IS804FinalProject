## Change this to fix your working cirection
setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

playerStats=na.omit(playerStats)
x=model.matrix(Salary~.-1,data=playerStats)
y=playerStats$FGA
train=sample(1:nrow(x), nrow(x)/2)

# Ridge Regression

library(glmnet)

#alpha=0 for Ridge regression and alpha=1 for Lasso
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)
cv.ridge=cv.glmnet(x,y, alpha=0)
# dotted lines show range of minimum and one standard error from minimum MSE

plot(cv.ridge)


# The Lasso, default alpha=1

fit.lasso=glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)
lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr, x[-train,])
pred
rmse=sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda), rmse, type="b", xlab="log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s=lam.best)