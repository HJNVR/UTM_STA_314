rm(list = ls())

library(tree)
library(gbm)
library(MASS)
library(randomForest)
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')
ncol(d.train)
x = model.matrix(y ~ . , data=d.train)[,-1]
X8NE=x[,8]
X8NW=x[,9]
X8SE=x[,10]
ytest = model.matrix(id ~. ,data = d.test)[,-1]
model1 <- randomForest(y ~ ., data = d.train,,ntree = 500, importance = TRUE) # not work
model1
plot(model1) # we choose number of trees to be 100
model5 = randomForest(y~., data=d.train,ntree = 300, importance = TRUE)
model5
importance(model5)
yhat.bag = predict(model5,newdata = d.test)

model2 = randomForest(y ~ ., data = d.train, mtry = ncol(d.train) -1, importance = TRUE)
yhat.bag = predict(model2,newdata=d.test)
model3 = randomForest(y~., data = d.train, ntree = 300, mtry = 10, importance = TRUE)
yhat.bag = predict(model3,newdata=d.test)
model4 = randomForest(y~., data = d.train, mtry = 37, importance = TRUE)
yhat.bag = predict(model4,newdata=d.test)
yhat.bag
length(yhat.bag)



n=ncol(d.train)
for (i in 1:(n-1)){
       model <- randomForest(y~., data = d.train, mtry = i)
       yhat.bag = predict(model,newdata=d.test)
       mse = mean((yhat.bag - d.test)^2)
       print(i)
       print(mse)
}

tree.train = tree(y~.,data=d.train)
tree.train
plot(tree.train)
cv.train= cv.tree(tree.train)
best.prune = cv.train$size[which.min(cv.train$dev)]
best.prune
prune.train = prune.tree(tree.train,best=best.prune)
yhat.tree = predict(prune.train,newdata=d.test)
length(yhat.tree)
yhat.tree
da.sample = data.frame(cbind(1:30000,  yhat.bag))
names(da.sample) = c('id', 'y')
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)

