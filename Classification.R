#Classification

# Logistic Regression
?glm
glm.fit=glm(Age~Tm+G+GS+MP+FG+FGA,data=playerStats,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
#predictions are of log-odds scale
glm.pred=predict(glm.fit)
summary(glm.pred)
#predictions are predicted probabilities
glm.probs=predict(glm.fit,type="response")
summary(glm.probs)
glm.probs[1:10]
#contrasts(Direction)
# initialize the vector to all "Down"
glm.pred=rep("Down",1250)
# update the vector based on glm.probs
glm.pred[glm.probs>.5]="Up"
glm.pred
#build a contingency table
#table(glm.pred,Direction)
# two ways to compute accuracy
(507+145)/1250
#mean(glm.pred==Direction)
#split the data into train and test sets

#train=(Year<2005)
#train
#Smarket.2005=Smarket[!train,]
#Smarket.2005
#dim(Smarket.2005)
#Direction.2005=Direction[!train]
#Direction.2005
#glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
#glm.probs=predict(glm.fit,Smarket.2005,type="response")
#glm.pred=rep("Down",252)
#glm.pred[glm.probs>.5]="Up"
#table(glm.pred,Direction.2005)
#mean(glm.pred==Direction.2005)
#mean(glm.pred!=Direction.2005)
#glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#compute precision when predict "up"
106/(106+76)
#build a new dataset with two testing instances
glm.probs=predict(glm.fit,newdata=data.frame(Lag1=c(1.2, 1.5),Lag2=c(1.1, -0.8)),type="response")
glm.probs






