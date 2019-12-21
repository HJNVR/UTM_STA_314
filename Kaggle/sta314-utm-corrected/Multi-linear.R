rm(list = ls())

#load libraries
library(caret) #for k-nn regression
library(class) #for k-nn regression
library('glmnet')
library('plotmo')

setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

x = model.matrix(y ~ . , data=d.train)
ytest = model.matrix(id ~. ,data = d.test)
y=d.train$y
class(d.train$X8)
levels(d.train$X8)
contrasts(d.train$X8)

lm.fit = lm(y ~ ., data = d.train)
summary(lm.fit)
# the R^2 is 0.3081, not good

# remove the predictors that are not significant

lm.fit_adj_1 = lm(y ~ X4 + X5 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X23 
                + X31 + X37 + X39 + X62 + X63 + X70 + X93, data = d.train)
summary(lm.fit_adj_1)

predla = predict(lm.fit_adj_1, newdata = d.test)
predla
lm.fit_adj_2 = lm(y ~  X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X23 
                + X31 + X39 + X63 + X70 + X93, data = d.train)
summary(lm.fit_adj_2)

# now lm.fit_adj_3 is the model with the bset training data 
lm.fit_adj_3 = lm(y ~  X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X23 
                  + X31 + X63 + X70 + X93, data = d.train)
summary(lm.fit_adj_3)

# try forward
regfit.forward = regsubsets(y~.,data = d.train,method='forward',nvmax=112)
sum = summary(regfit.forward)
sum
length(sum[[1]][10,])
sum[[1]][50,]*rep(1,112)==1
which(sum[[1]][50,]*rep(1,112)==1)
which(sum[[1]][100,]*rep(1,112)==1)
which(sum[[1]][90,]*rep(1,112)==1)
which(sum[[1]][110,]*rep(1,112)==1)
which(sum[[1]][111,]*rep(1,112)==1)
which(sum[[1]][20,]*rep(1,112)==1)

# try lasso again
x_fit_1 = x[,c(1,12,13,14,15,16,17,18,19,20,21,22,26,34,43,66,73,96)] # multi-linear
x_fit_f_50 = x[,c(5,6,7,10,12,13,14,15,16,17,18,19,20,21,22,26,27,34,36,40,42,43,53,54,56,57,60
                  ,61,63,65,66,72,73,74,76,81,83,84,92,93,95,96,97,99,100,101,103,104,109,112)]
x_fit_f_100 = x[,c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
                   34,35,36,37,38,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                  61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,81,82,83,84,86,88,89,90
                  ,91,92,93,94,95,96,97,99,100,101,102,103,104,106,109,110,111,112)]
x_fit_f_90 = x[,c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
                  34,36,37,38,40,41,42,43,44,45,48,49,50,52,53,54,56,57,59,60,
                  61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,81,82,83,84,88,89,90
                  ,91,92,93,94,95,96,97,99,100,101,102,103,104,106,109,110,111,112)]
x_fit_f_110 = x[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
                   32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,
                   60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,81,82,83,84,
                   85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,
                   107,108,109,110,111,112)]
x_fit_f_20 = x[,c(5,6,7,10,12,13,14,15,16,17,18,19,20,22,26,34,42,66,73,83)]

grid = 10^seq(10,-2,length = 1000) 
cv.la = cv.glmnet(x_fit_f_50,y,alpha =1,lambda = grid) 
cv.la = cv.glmnet(x_fit_1,y,alpha =1,lambda = grid) 
cv.la = cv.glmnet(x_fit_f_100,y,alpha =1,lambda = grid) 
cv.la = cv.glmnet(x_fit_f_90,y,alpha =1,lambda = grid) 
cv.la = cv.glmnet(x_fit_f_110,y,alpha =1,lambda = grid) # same, 0.0555776
cv.la = cv.glmnet(x_fit_f_20,y,alpha =1,lambda = grid)
cv.la
plot(cv.la)
# first extract the lambda values obtained by cross-validation
best.la = cv.la$lambda.min
best.la
#x[train,]
fit.la = glmnet(x,y,alpha =1, lambda = best.la)
length(fit.la)
#coef(fit.la)
length(ytest)
pred.la = predict(fit.la, s = best.la, newx = ytest )
length(pred.la)
set.seed(1)
#mean((pred.la-ytest)^2)  
length(pred.la)
pred.la

da.sample = data.frame(cbind(1:30000,  pred.la))
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)
