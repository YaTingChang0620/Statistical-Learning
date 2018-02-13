library(ISLR)
library(glmnet)
Hitters = na.omit(Hitters)

# glmnet(): 
# 1). fit ridge, lasso (and more) models.
# 2). x-> must be a matrix; y-> must be a vector
# 3). only take numerical, quantative inputs
# 4). alpha=0: ridge regression; alpha=1: lasso regression
# model.matrix():
# 1). produce a matrix corresponding to the predictors
# 2). transform any qualitative variable into dummy variables 

x=model.matrix(Salary~.,Hitters)[,-1] 
y=Hitters$Salary

## Ridge Regression ##
# specify the lambda: from null model (contain only intercept) to the least square fit
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) #s=lambda; mewx=test data set 
# test MSE
mean((y.test-ridge.pred)^2)
# check whether there is any benefit performing ridge regression with lambda=4 instead of 
# performing least square fit 
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Use cross-validation to choose the tuning parameter(lambda):
# built-in cross-validation function: cv.glmnet()
# by default, the function preforms ten-fold cross validation.(can be changed using "nfolds")
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
# the value of lambda that results in the smallest cross-validation
# error is 212.
bestlam=cv.out$lambda.min
# test MSE associated with this value of lambda 
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((y.test-ridge.pred)^2)
# refit our ridge regression model on the full data set 
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


## Lasso Regression ##
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
# each curve represents to a variable; 
# The x-axis above indicates the number of nonzero coefficients at current lambda
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# Perform lasso regression on full data set 
# The lasso has an advantage over ridge: resulting coefficient estimates are sparse
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
