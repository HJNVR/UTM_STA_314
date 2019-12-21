rm(list = ls())
library(DataExplorer)
#Table things
library(data.table)
library(dplyr)
#Regression
library(car)
#Partitioning the data
library(psych)
#Cross Validation
library(caret)
#Decision Tree
library(rpart)
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')
ytest = model.matrix(id ~. ,data = d.test)[,-1]
lm.fit = lm(y ~ ., data = d.train)
summary(lm.fit)
step(lm.fit)

step = step(lm.fit)
step
str(d.train)

lm_fit_AIC = lm(formula = y ~ X4 + X5 + X6 + X8 + X9 + X10 + X11 + X12 + X13 + 
     X14 + X15 + X16 + X17 + X18 + X19 + X23 + X31 + X33 + X37 + 
     X39 + X40 + X50 + X51 + X54 + X57 + X62 + X63 + X70 + X73 + 
     X78 + X80 + X90 + X93 + X97 + X98 + X109, data = d.train)

d.train_AIC= d.train[,c(1,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,14,32,34,38,40,41,51,52,55,58,63,64,71,74,79,81,91
                  ,94,98,99,110)]
x_AIC = model.matrix(y ~. ,data = d.train_AIC)[,-1]
grid = 10^seq(10,-2,length = 100)
cv.la = cv.glmnet(x_AIC,y,alpha =1,lambda = grid) 
best.la = cv.la$lambda.min
best.la
y=d.train$y
summary(lm_fit_AIC)
pred = predict(lm_fit_AIC, newdata = d.test)
pred

length(pred)

da.sample = data.frame(cbind(1:30000,  pred))
da.sample
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)

