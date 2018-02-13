library(ISLR)
library(leaps)
library(tidyverse)

# Salary is missing for 59 players:
# remove all of the rows that have missing values in any variable
sum(is.na(Hitters$Salary))
Hitters_noNa=na.omit(Hitters)
dim(Hitters)

##### Best Subset Selection Methods #####
# fit up to a 19-variable model
regfit.full=regsubsets(Salary~.,Hitters_noNa,nvmax=19)
reg.summary=summary(regfit.full) 

##### Forward and Backward Stepwise Selection #####
regfit.fwd=regsubsets(Salary~.,data=Hitters_noNa,nvmax=19,method="forward")
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")


##### Choose optimal model(use best subset method as an example) #####
# Two ways:
# 1). (indirect) make an adjustment to the training error to account for 
# the bias due to over fitting.- RSS, AdjustedR,Cp,BIC
# 2). (direct) use validation set or cross validation approach

# plotting the RSS, adjusted R,Cp,and BIC helps us determine which model to select:
# 1-1. RSS:
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab='Number of Variables',ylab='RSS',type='l')
# plot a red dot to indicate the model with the smallest RSS
which.min(reg.summary$rss)
points(19,reg.summary$rss[19],col="red",cex=2,pch=20)

# 1-2. Adjusted R:
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type='l') 
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)

# 1-3. Cp:
plot(reg.summary$cp,xlab='Number of Variables',ylab='Cp',type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

# 1-4. BIC:
plot(reg.summary$bic,xlab='Number of Variables',ylab='Cp',type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# display the selected variables
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="bic")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")

# display the coefficient estimates asscociated with this model
coef(regfit.full,11)

# 2-1. Validation Set
set.seed(1)
# MUST use only the training observations to perform all
# aspects of model-fitting including variable selection. 
train=sample(c(TRUE,FALSE),nrow(Hitters_noNa),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters_noNa[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters_noNa[test,])
val.errors=rep(NA,19)

# compute test MSE
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters_noNa$Salary[test]-pred)^2)
}
# find the minimum of test MSE and extract the model's coefficient
which.min(val.errors)
coef(regfit.best,10)

# Finally, perform best subset selection on the full data set
# and select the best ten-variable model.
regfit.best=regsubsets(Salary~.,data=Hitters_noNa,nvmax = 19)
coef(regfit.best,10)

# write our own method(becausse regsubsets() doesn't have its own predict() function):
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars] %*% coefi
}

#2-2. Cross-Validation #
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters_noNa),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))

# perform cross-validation k times;
# each time compute test MSE for a given model size
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters_noNa[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict.regsubsets(best.fit,Hitters_noNa[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters_noNa$Salary[folds==j]-pred)^2)
  }
}

# average test MSE for each model size
mean.cv.errors=apply(cv.errors,2,mean)
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
# perform best subset selection on the full data set to obtain the 11-variable model
reg.best=regsubsets(Salary~.,data=Hitters_noNa,nvmax=19)
coef(reg.best,11)
