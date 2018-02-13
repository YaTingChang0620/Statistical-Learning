library(MASS)
library(ISLR)
library(car)
attach(Boston)
# predict median house value using 13 predictors ######
lm.fit = lm(medv~lstat)
summary(lm.fit)

# 1. attributes of linear function object
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# 2. produce confidence interval and prediction interval for the prediction 
# of medv for a given value of lstat
# 2-1. 
# *confidence interval: quantify the uncertainity surrounding the average 
#  median house value over a large number of neighborhoods/
# *prediction interval: quantify the uncertainity surrounding the median house value 
#  for a particular negihborhood. 
predict(lm.fit,data.frame(lstat = c(5,10,15)),interval = "confidence")
predict(lm.fit,data.frame(lstat = c(5,10,15)),interval = "prediction")

# 3. simple basic plot
plot(lstat,medv)
abline(lm.fit)
plot(lstat,medv,pch = "+")

# 4. diagnostic plots
# 4-1.
# residual plots are a useful graphic tool for identifying non-linearity.
par(mfrow = c(2,2))
plot(lm.fit)
# 4-2.
# To determine which points are outliers, we could use studentized residuals;
# Observations whose studentized residuals are greater than 3 in absolute value are
# possible outliers. 
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

# 4-3. 
# observations with high leverage have an unusual value for x.
# high leverage observations tend to have a sizable impact
# on the estimated regression line. 
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# 5. Multiple Linear Regression
lm.fit = lm(medv~.,data = Boston)
lm.fit = update(lm.fit, ~.-age)
# 5-1. Variance inflation factor(VIF): 
# determine the multi-collinearity; a VIF value exceeds 5 or 10 indicates 
# a problematic amount of collinearity.
vif(lm.fit)

# 6. Non-linear Transformation of the Predictors
# 6-1. ANOVA performs a hypothesis test comparing the two models.
# The null hypothesis is that the two models fit the data equally;
# the alternative hypothesis is that the full model is superior. 
lm.fit2 = lm(medv~lstat + I(lstat^2))
lm.fit = lm(medv~lstat)
anova(lm.fit,lm.fit2)
lm.fit5 = lm(medv~poly(lstat,5))


# 7. Interaction Terms
# lstat:age -> include an interaction term between lstat and age.
# lstat * age = lstat + age + lstat:age

