#install.packages('glmnet')
#install.packages('plotmo')
rm(list = ls())
library(caret)
library('glmnet')
library('plotmo')
library('ISLR')
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

x = model.matrix(y ~ . , data=d.train)[,-1]
ytest = model.matrix(id ~. ,data = d.test)[,-1]
#ytest
x
y = d.train$y
y
get_dummies(d.train)
get_dunmies(d.train)

#grid = 10^seq(10,-2,length = 100)
grid = 10^seq(10,-2,length = 1000)
#grid = 10^seq(2, -2, by = -.1)

# cross validation
set.seed(1)
fit = knnreg(x,y,k=25)
fit
#cv.la = cv.glmnet(x[train,],y[train],alpha =1) 
cv.la = cv.glmnet(x,y,alpha =1,lambda = grid) # cross validation for lasso
plot(cv.la)
# first extract the lambda values obtained by cross-validation
best.la = cv.la$lambda.min
best.la
#0.05336699 better 
#0.05011872
#0.05555776 grid = 1000
lambda_1se = cv.la$lambda.1se
lambda_1se
#x[train,]
#fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)
fit.la = glmnet(x,y,alpha =1, lambda = best.la) #0.05666776
#fit.la = glmnet(x,y,alpha =1, lambda = lambda_1se)
length(fit.la)
#coef(fit.la)
length(ytest)
pred.la = predict(fit.la, s = best.la, newx = ytest )
#pred.la = predict(fit.la, s = lambda_1se, newx = ytest )
length(pred.la)
set.seed(1)
#mean((pred.la-ytest)^2)  
length(pred.la)
pred.la

da.sample = data.frame(cbind(1:30000,  pred.la))
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)

