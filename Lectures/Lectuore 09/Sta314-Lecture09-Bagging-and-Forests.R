rm(list = ls())

library(tree)
library(MASS)

attach(Boston)
Boston
#-----------------------------------------------------------------
# recall: bagging special case of radnom forest
# thus 
# one implementation: R package randomForest
# note: other impleentations also exist
#----------------------------------------------------

library(randomForest)

# begin by slitting sample
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)                    
y.test = Boston[-train,"medv"]

# the function to fit random forsts
# is called randomForest
# arguments:

# first argument: model equation, similar to linear models
# data: optional data frame that contains
# as columns variables used in formula 
# subset: use only subset of data to train model
# all of the above similar to linear models

# new arguments:
# mtry: the number of trees used at each split
# setting mtry = number of predictors corresponds to bagging
# ntree: number of trees to grow
# in lectures we called this B

# the next line does bagging with 500 replications 
bag.boston = randomForest(medv~., data = Boston, subset=train, mtry = ncol(Boston)-1, ntree = 500)
  
# there also is a predict function
# for the output of function randomForest
# newdata should contain the new predictors  
yhat.bag = predict(bag.boston,newdata=Boston[-train,])

# look at test MSE
mean((yhat.bag - y.test)^2)


# how does this compare to a pruned tree?
# this is code from previous lecture

tree.boston = tree(medv~.,data=Boston,subset=train)
cv.boston = cv.tree(tree.boston)
best.prune = cv.boston$size[which.min(cv.boston$dev)]
prune.boston = prune.tree(tree.boston,best=best.prune)
yhat.tree = predict(prune.boston,newdata=Boston[-train,])

mean((yhat.tree - y.test)^2)
mean((yhat.bag - y.test)^2)

# bagging has improved error substantially!
# more detailed comparison with other methods: later

# next consider variable importance measures
# as discussed in lectures
# run bagging on full data set
# if ntree not specified uses defualt ntree = 500
# note: need importance = TRUE
# to get %IncMSE
bag.boston.full = randomForest(medv~.,data = Boston, mtry = 13, importance = TRUE)

# output numerical values  
importance(bag.boston.full)

# make a plot 
varImpPlot(bag.boston.full)

#---------------------------------------------
# next take a brief look at random forests
# compared to above, we only need ot change argument mtree

rf.boston_1 = randomForest(medv~., data = Boston, subset=train, mtry = 1)
rf.boston_2 = randomForest(medv~., data = Boston, subset=train, mtry = 2)
rf.boston_6 = randomForest(medv~., data = Boston, subset=train, mtry = 6)

yhat.rf1 = predict(rf.boston_1,newdata=Boston[-train,])
yhat.rf2 = predict(rf.boston_2,newdata=Boston[-train,])
yhat.rf6 = predict(rf.boston_6,newdata=Boston[-train,])

mean((yhat.tree - y.test)^2)
mean((yhat.bag - y.test)^2)
mean((yhat.rf1 - y.test)^2)
mean((yhat.rf2 - y.test)^2)
mean((yhat.rf6 - y.test)^2)

# m = 6 does a bit better compared to bagging
# m = 1 not that good
# m = 2 already substantial improvement over m = 1



