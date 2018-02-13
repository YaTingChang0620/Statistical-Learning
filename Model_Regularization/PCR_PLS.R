library(pls)
library(ISLR)
set.seed(2)
Hitters=na.omit(Hitters)

##  Principal Components Regression ##
# pcr():
# 1). scale=true: standardize predictors
# 2). validation="CV": compute ten-fold cross validation error
# 3). need to square the CV score in order to get usual MSE
# 4). % variance explained: the amount of information about the predictors
# or the response that is captured using M pricipal components. 

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# perform PCR on the training data and evaluate its test set performance:
# The lowest cross-validation error occurs when components = 7
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

# compute the test MSE: the test set MSE is competitive with the result
# obtained using lasso and ridge regression
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

# fit the model on the full data set using M=7
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

##  Partial Least Square ##
# fit the model using train data set
# the lowest cross validation error occurs when M=2
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pls.fit,val.type="MSEP")

# evaluate the test set MSE
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((y.test-pls.pred)^2)

# perform PLS on using the full data set and M=2
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
