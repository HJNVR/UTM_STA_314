rm(list = ls())
#install.packages('leaps')
library(leaps) # this contians the function regsubsets
library(MASS)
library(caret)
library(ISLR) # this contains Credit data set

setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')

d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

#x = model.matrix(y ~ . , data=d.train)
x = model.matrix(y~.,data = d.train)[,-1]
x
ytest = model.matrix(id ~. ,data = d.test)[,-1]
y = d.train$y
y

regfit.best = regsubsets(y~., data = d.train, nvmax=111,really.big=T)
regfit.forward = regsubsets(y~.,data = d.train,method='forward',nvmax=111)
sum = summary(regfit.forward)
sum
coef(regfit.forward,119)

which(sum[[1]][50,]*rep(1,112)==1)
x_fit_f_50 = x[,c(5,6,7,10,12,13,14,15,16,17,18,19,20,21,22,26,27,34,36,40,42,43,53,54,56,57,60
                  ,61,63,65,66,72,73,74,76,81,83,84,92,93,95,96,97,99,100,101,103,104,109,112)]

lm_50 = lm(y ~ X4+X5+X6+X8NW+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X23+
             X24+X31+X33+X37+X39+X40+X50+X51+X53+X54+X57+X58+X60+X62+X63 
              + X69+X70+X71+X73+X78+X80+X81+X89+ X90+X92+X93+X94+X96+X97+X98+X100+X101+X106+X109,data = d.train)     


#coef(regfit.forward,4)
example_pred = data.frame(cbind(1:3000,  regfit.forward))
names(example_pred) = c('id', 'y')
names(example_pred)

write.csv(example_pred,file='linmod.csv',row.names=FALSE)


