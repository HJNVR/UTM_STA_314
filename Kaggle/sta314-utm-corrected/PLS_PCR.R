rm(list = ls())
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set Hitters
library(pls)    # contains functions for fitting PLS and PCR

setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')

d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

x = model.matrix(y ~ . ,data= d.train )[,-1]
ncol(x) # 111, since there are more columns
ytest = model.matrix(id ~. ,data = d.test)[,-1]
y = d.train$y

set.seed(1)

# cross-validation and PCR
pcr.fit = pcr(y~., data=d.train,scale =TRUE ,validation ="CV")
pcr.fit

summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

MSEP(pcr.fit)$val[2,1,]

which.min(MSEP(pcr.fit)$val[2,1,])-1

selectNcomp(pcr.fit, method = "onesigma", plot = TRUE)

pred.pcr = predict(pcr.fit ,ytest, ncomp= 111)

length(pred.pcr)
example_pred = data.frame(cbind(1:30000, pred.pcr))
names(example_pred) = c('id', 'y')
names(example_pred)

write.csv(example_pred,file='linmod.csv',row.names=FALSE)





