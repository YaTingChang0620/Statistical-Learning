library(ISLR)
library(boot)
set.seed(1)
train = sample(392,196)

##### The Validation Set Approach #####
attach(Auto)
# use train data to fit the model
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
# calculate the MSE of the 196 observations in the validation set
# mean((mpg - predict(lm.fit,Auto[-train]))^2)
mean((mpg - predict(lm.fit,Auto))[-train]^2)

# continue estimating the test error rate for the quadratic and cubic regressions
lm.fit2 = lm(mpg~poly(horsepower,2),subset = train)
mean((mpg - predict(lm.fit2,Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3),subset = train)
mean((mpg - predict(lm.fit3,Auto))[-train]^2)

# choose different training set
# The results are consistent: a model that predicts mpg using a quadratic function 
# of horsepower performs better. 
set.seed(2)
train = sample(392,196)
lm.fit = lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

##### Leave-One-Out Cross-Validation #####
cv.error = rep(0,5)
for( i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}

##### k-Fold Cross-Validation #####
# k = 10
set.seed(6)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}


##### Bootstrap #####
# Step1: create a function that computes the statistics of interest
# Step2: use boot() to perform bootstrap by sampling observations from dataset with replacement 
# Estimating the Accuracy of a Statistics of Interest:
alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}
boot(Portfolio,alpha.fn,R=1000)# (R: the number of bootstrap replicates)

# Estimating the Accuracy of a Linear Regression Model:
boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower,data=Auto,subset=index)))
}
boot(Auto,boot.fn,1000)

# Compared to using standard formula to compute the standard error
# Difference: standard formula relies on some assumptions; whereas the bootstrap
# analysis doesn't. 
summary(lm(mpg~horsepower,data=Auto))$coef

