rm(list = ls())
library(splines)
library(gam)

setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')


glm1 = glm(y~.,family = binomial,data=d.train)
summary(glm1)
pred <- predict(glm1, newdata=d.test)
pred
gam.m1=gam(y~s(.),data = d.train)
summary(gam.m1)
coef(gam.m1)
