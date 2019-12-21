#install.packages('tree')
rm(list = ls())
library(tree)
# contains functions for fitting regresion trees


library(MASS)
# contains Boston housing prices data set


# create training set
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# grow a tree to predict medv (median value of house)
# using all other variables as predictors

# automatically uses stopping rule when too few data are in terminal node
# or when further splitting does not lead to a lot of improvement
# this can be controlled by the argument 'control'
# see R help file for more details

tree.boston = tree(medv~.,data=Boston,subset=train)
summary(tree.boston)

# for this tree
# only lstat (social status), 
# dis (distance to employment) 
# rm (number of rooms)
# is used
# this is done automatically

# next, plot the tree with annotations
plot(tree.boston)
text(tree.boston,pretty=0)

# now use cross-validation to do pruning
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# note: size is in reverted order
cv.boston$size
cv.boston$dev
# output tree size 
cv.boston$size[which.min(cv.boston$dev)]

# for this particular spit of the data 
# cross validation suggests that we should not prune

# now make predictions based on the full tree
yhat = predict(tree.boston,newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]

# test error
mean((yhat-boston.test)^2)

#-----------------------------------------------------
# pruning by force
#-----------------------------------------------------

# assume we are stubborn 
# (or have prior information) 
# and prune the tree anyway
# best = 5: look at the best possible subtree 
# with 5+1 terminal nodes
prune.boston = prune.tree(tree.boston,best=6)

# plot the trees side by side to compare
par(mfrow = c(1,2))

plot(tree.boston)
text(tree.boston,pretty=0)

plot(prune.boston)
text(prune.boston,pretty=0)

# how well does the samller tree do in terms of prediction?
yhat.pruned = predict(prune.boston,newdata=Boston[-train,])
mean((yhat.pruned-boston.test)^2)
mean((yhat-boston.test)^2)

# for this split of data prediction error for small tree is larger


#-----------------------------------------------------
# comparison with some other methods
#-----------------------------------------------------

# now let's compare performance to some methods
# that we encountered in lectures so far

library(glmnet)
library(gam)
library(splines)

R = 25

# errors of different methods for different splits will go here
err.tree = numeric(R)
err.lm = numeric(R)
err.ridge = numeric(R)
err.lasso = numeric(R)
err.plm1 = numeric(R)
err.plm2 = numeric(R)
err.plm3 = numeric(R)

attach(Boston)

set.seed(1)

# this will take a while
for(r in 1:R){

  # split the data
  set.seed(r)
  train = sample(1:nrow(Boston), nrow(Boston)/2)
  boston.test = Boston[-train,"medv"]
  
  # use a tree, no pruning
  tree.boston = tree(medv~.,Boston,subset=train)
  yhat.tree   = predict(tree.boston,newdata=Boston[-train,])
  err.tree[r] = mean((yhat.tree-boston.test)^2)
  
  # use a linear model 
  lim.boston = lm(medv~.,data = Boston,subset=train)
  yhat.lim = predict(lim.boston, newdata=Boston[-train,])
  err.lm[r] = mean((yhat.lim-boston.test)^2)
  
  # ridge regularization
  X = model.matrix(medv ~ . ,data = Boston)
  ridge_reg = cv.glmnet(x=X[train,], y=medv[train], alpha = 0)
  best_lambda_ridge = ridge_reg$lambda.min
  yhat.ridge = predict(ridge_reg, s = best_lambda_ridge, newx = X[-train,])
  err.ridge[r] = mean((yhat.ridge-boston.test)^2)

  # lasso regularization
  X = model.matrix(medv ~ . ,data = Boston)
  la_reg = cv.glmnet(x=X[train,], y=medv[train], alpha = 1)
  best_lambda_la = la_reg$lambda.min
  yhat.la = predict(la_reg, s = best_lambda_la, newx = X[-train,])
  err.lasso[r] = mean((yhat.la-boston.test)^2)
    
  # additive model, one component non-linear
  lim.boston3 = lm(medv~ns(lstat,df=6) + . - lstat ,data = Boston,subset=train)
  yhat.lim3 = predict(lim.boston3, newdata=Boston[-train,])
  err.plm1[r] = mean((yhat.lim3-boston.test)^2)

  # additive model, two components non-linear, natural spline
  lim.boston4 = lm(medv ~ ns(lstat,df=6) + ns(nox,df=6) + . - lstat - nox,data = Boston,subset=train)
  yhat.lim4 = predict(lim.boston4, newdata=Boston[-train,])
  err.plm2[r] = mean((yhat.lim4-boston.test)^2)

  # additive model, two components non-linear, smooting spline
  lim.boston5 = gam(medv ~ s(lstat,df=6) + s(nox,df=6) + . - lstat - nox,data = Boston,subset=train)
  yhat.lim5 = predict(lim.boston5, newdata=Boston[-train,])
  err.plm3[r] = mean((yhat.lim5-boston.test)^2)
    
  # print the progress
  cat(r)
  cat('\n')
  
}

# make boxplots to compare methods 

boxplot(cbind(err.tree,err.lm,err.ridge,err.lasso,err.plm1,err.plm2,err.plm3))

