
# Lab: Non-linear Modeling

setwd('/Users/jal/Desktop/IS804FinalProject')

playerStats <- read.csv(file = 'NBAPlayerStats.csv', na.strings = "?", stringsAsFactors = T)
head(playerStats)

###
library(ISLR)

# Get min/max values of age using the range() function

agelims = range(playerStats$Age)

# Generate a sequence of age values spanning the range
playerStats$Age.grid = seq(from = min(agelims), to = max(agelims))
playerStats$Age.grid

## Splines

###
library(splines)
fit <- lm(FGA ~ bs(playerStats$Age, knots = c(25, 40, 60)), data = FGA)
pred <- predict(fit, newdata = list(playerStats$Age = playerStats$Age.grid), se = T)
plot(playerStats$Age, FGA, col = "gray")
lines(playerStats$Age.grid, pred$fit, lwd = 2)
lines(playerStats$Age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(playerStats$Age.grid, pred$fit - 2 * pred$se, lty = "dashed")

### natural cubic splines
fit2 <- lm(FGA ~ ns(playerStats$Age, df = 4), data = playerStats)
pred2 <- predict(fit2, newdata = list(playerStats$Age = playerStats$Age.grid),
                 se = T)
lines(playerStats$Age.grid, pred2$fit, col = "red", lwd = 2)
### Smoothing spline
plot(playerStats$Age, FGA, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(playerStats$Age, FGA, df = 16)
fit2 <- smooth.spline(playerStats$Age, FGA, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
### Local regression
plot(playerStats$Age, FGA, xlim = FGA, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(FGA ~ Age, span = .2, data = Age)
fit2 <- loess(FGA ~ Age, span = .5, data = Age)
lines(FGA.grid, predict(fit, data.frame(FGA = FGA.grid)),
      col = "red", lwd = 2)
lines(FGA.grid, predict(fit2, data.frame(FGA = FGA.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

## GAMs
library(gam)
###
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education,
           data = Wage)

### s: a smoothing spline fit in a GAM

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
              data = Wage)
###
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")
###
plot.Gam(gam1, se = TRUE, col = "red")
###
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education,
              data = Wage)
anova(gam.m1, gam.m2, test = "F")

###
preds <- predict(gam.m2, newdata = Wage)
### lo: a local polynomial regression fit (loess) in a GAM model
gam.lo <- gam(
  wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
  data = Wage
)
plot(gam.lo, se = TRUE, col = "green")

### GAM for classification with logistic regresson
gam.lr <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")
###
table(education, I(wage > 250))
###
gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage,
  subset = (education != "1. < HS Grad")
)
plot(gam.lr.s, se = T, col = "green")
###

