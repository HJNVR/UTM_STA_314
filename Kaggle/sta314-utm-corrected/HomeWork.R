rm(list=ls())
library(leaps) # this contians the function regsubsets
library(MASS)
library(splines)
library(gam)
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')

d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

fit = lm(y ~ ., data = d.train)
fit
d.test[37,]

# q 2) 
id37_pred = predict(fit, newdata = d.test[37,])
id37_pred


# q 3) 
regfit.forward = regsubsets(y~.,data = d.train,method='forward',nvmax=112)
sum = summary(regfit.forward)
sum
length(sum[[1]][10,])
sum[[1]][50,]*rep(1,112)==1
which(sum[[1]][50,]*rep(1,112)==1)
coef(regfit.forward,2)
# the answer would be X10 and X13 


# q 4） 
#X15 = d.train$X15
#X15
#X10 = d.train$X10
#X10
#X102 = d.train$X102
#X102
gam.m1=gam(y~X15+ns(X10,knots=c(-1,0,1))+poly(X102,4),data=d.train)
summary(gam.m1)
gam.m1$coefficients
coef(gam.m1)
coef(gam.m1)[7]
