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


