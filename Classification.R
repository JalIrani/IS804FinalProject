## Change this to fix your working cirection
setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

#Classification

# Logistic Regression
?glm
glm.fit=glm(MVP~Tm+G+GS+MP+FG+FGA,data=playerStats,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.pred=predict(glm.fit)
summary(glm.pred)
glm.probs=predict(glm.fit,type="response")
summary(glm.probs)
glm.probs[1:10]
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
glm.pred


# Now splitting the data into training and test sets
train=(Age<30)
train
playerStats=playerStats[!train,]
playerStats.30
dim(playerStats.30)
playerStats.30.30=playerStats.30[!train]
#Direction.2005
glm.fit=glm(MVP~G+GS+MP+FG+FGA+FGP+3P+3PA+3PP+2P+2PA+2PP+eFGP+FG+FTA+FTP,data=playerStats,family=binomial,subset=train)
glm.probs=predict(glm.fit,playerStats.30,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
#build a new dataset with two testing instances
glm.probs=predict(glm.fit,newdata=data.frame(Lag1=c(1.2, 1.5),Lag2=c(1.1, -0.8)),type="response")
glm.probs

