library(MASS)
library(ISLR)
attach(Smarket)

train = (Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

##### Linear Discriminant Analysis (LDA) #####
lda.fit = lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
# predict() function returns three elements:
# 1. class: LDA's prediction
# 2. posterior: Pr(Y=K|X=x), the probability that the observation belongs to the kth class
# 3. x: linear discriminant.If the value is large,then the LDA classifier will predict a market increase.
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

# determine the error rate
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

# Apply different threshold: 0.5
# Example: we wish to predict a market decrease only if we are very certain
# that the market will indeed decrease(threshold changes to 0.9)
# we need to knowif there's any observation meeting the threshold first.
sum(lda.pred$posterior[,1]>0.9)
max(lda.pred$posterior[,1])

##### Quadratic Discriminant Analysis (QDA) #####
qda.fit = qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.pred=predict(qda.fit,Smarket.2005)
qda.class=qda.pred$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
# The QDA prediction are accurate almost 60% of the time
# This suggests that quadratic form assumed by QDA may capture the true relationship
# more accurately than the linear foms assumed by LDA and logistic regression. 