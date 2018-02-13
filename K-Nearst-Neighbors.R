# knn() function requires four inputs:
# 1. a matrix containing the predictors associated with the training data 
# 2. a matrix containing the predictors associated with the test data
# 3. a vector containing the class label for the training observations
# 4. k: the number of nearest neighbors to be used by the classfier

library(class)
library(ISLR)
attach(Smarket)

train = (Year<2005)
train.x = cbind(Lag1,Lag2)[train,]
test.x = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]
Direction.2005=Direction[!train]

#k=1
set.seed(1)
knn.pred = knn(train.x,test.x,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252 #0.5

#k=3
knn.pred = knn(train.x,test.x,train.Direction,k=3)
table(knn.pred,Direction.2005)
(48+87)/252 #0.5

##### An Application to Caravan Insurance Data #####
# objective: 85 predictors and the response variables is Purchase which 
# indicates whether or not a given indvidual purchases a caravan insurance.
attach(Caravan)
summary(Purchase)
# Two issues about KNN:
# 1. Any variables that are on a large scale will have a much 
# larger effect on the KNN than variables that are on a small scale.
# ex: age & salary
# 2. measurement unit 
# Solution: standardize 
standardized.X=scale(Caravan[,-86])

test=1:1000     
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)

# KNN error rate 
# only 6% of customers purchased insurance
# we could get the error rate down to 6% by always 
# predicting 'no'
mean(test.Y != knn.pred) #0.118
mean(test.Y!='No') # 0.059

# The overall error rate is not of interest. Instead, the fraction of 
# individuals that are correctly predicted to buy insurance is of interest. 
table(knn.pred,test.Y)
9/(68+9) # 0.11, double the rate that one would obtain from random guessing

# using k=3
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/(21+5) #0.19

# using k=5
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/(11+4) #0.267

##### Using logistic regression #####
# use 0.5 as the predicted probability cut-off for the classifier
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep('no',1000)
glm.pred[glm.probs>0.5]='yes'
table(glm.pred,test.Y) # only seven are predicted to purchase the insurance 

# use 0.25 as the threshold
glm.pred=rep('no',1000)
glm.pred[glm.probs>0.25]='yes'
table(glm.pred,test.Y)
11/(11+22) #correct for about 0.33
