rm(list=ls())
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')


d.train = read.csv('trainingdata2.csv')
d.train
d.test = read.csv('test_predictors2.csv')
d.test
attach(d.train)
y
X1
#plot(y ~. , data = d.train)
#library(caret) 
#fit = knnreg(as.matrix(X1),y,k=5) 
#fit
#pr = predict(fit, newdata = data.frame(TV = as.matrix(1:3000)))
#plot(y ~ X1)
#lines(1:3000,pr, col='red', lwd = 2) 
#plot(y ~ X1+X2)
#f = lm(y ~ ., data = d.train)
#f
#pr = predict(fit, newdata = data.frame(X1 = as.matrix(1:30000)))

# use lm to predict
fit = lm(y ~ ., data = d.train)
fit

summary(fit)
pred = predict(fit)
pred

# predict the 37 row from test data set
d.test[37,]
id37_pred = predict(fit, newdata = d.test[37,])
id37_pred


example_pred = data.frame(cbind(1:30000,pred))
example_pred
names(example_pred)

names(example_pred) = c('id','y')
names(example_pred)

write.csv(example_pred,file='linmod.csv',row.names=FALSE)
