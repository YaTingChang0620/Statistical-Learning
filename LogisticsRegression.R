library(ISLR)
# Fit a logistic regression model to predict Direction using Lag1 through Lag5 and Volume###

# The only substantial correlation is between Year and Volume
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,
              family=binomial)
summary(glm.fit)$coef

### Fits the model ###
# 1. glm() function fits generalized linear models, a class of models that 
# includes logistic regression. 
# 2. The type = "response" tells R to output probabilities of the form 
# P(Y=1|X)
# 3. Training was performed using only the dates before 2005
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Direction[!train]
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,
               family=binomial,subset=train)
# 4. Compute the prediction for 2005
glm.probs = predict(glm.fits,Smarket.2005,type="response")
# 5. Compare the prediction to actual movements
dim(Smarket.2005)
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) # test error 

### Update the model ###
# From the summary, we know that Lag1 and Lag2 have the smallest p-value
glm.fits = glm(Direction~Lag1+Lag2,data=Smarket,
               family=binomial,subset = train)
glm.probs = predict(glm.fits,Smarket.2005,type="response")
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)

# Though the result tends to be a little better(56%), 
# the confusion matrix shows that on days when logistic regression predicts
# an increase in the market, it has a 58% of accuracy rate.
106/(106+76)
