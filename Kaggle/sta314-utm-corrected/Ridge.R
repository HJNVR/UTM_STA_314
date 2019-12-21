rm(list = ls())

library('glmnet')
library('plotmo')
library('ISLR')
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

x = model.matrix(y ~ . , data=d.train)[,-1]
ytest = model.matrix(id ~. ,data = d.test)[,-1]
typeof(ytest)
y = d.train$y
grid = 10^seq(10,-2,length = 100) 
grid

set.seed(1)

cv.ri = cv.glmnet(x,y,alpha =0)
plot(cv.ri)

best.ri = cv.ri$lambda.min
best.ri
#0.3764936

fit.ri = glmnet(x,y,alpha =0, lambda = best.ri)
length(fit.ri)

pred.ri = predict(fit.ri, s = best.ri, newx = ytest)
pred.ri
length(pred.ri)

da.sample = data.frame(cbind(1:30000,  pred.ri))
da.sample
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)


