
# Lab: Non-linear Modeling

###
library(ISLR)
attach(Wage)

# Get min/max values of age using the range() function

agelims = range (age)

# Generate a sequence of age values spanning the range
age.grid = seq(from = min(agelims), to = max(agelims))
age.grid

## Splines

###
library(splines)
### bs: fit a polynomial spline, default degree is 3
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

### ns: natural cubic splines
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
                 se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)
### Smoothing spline
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
### Local regression
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
# loess: local regression, span controls the degree of smoothing
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
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

