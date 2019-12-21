library(gbm)
library(MASS)
attach(Boston)

set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)

y.test = Boston[-train,'medv']

# basic boosting
# data: as before
# distribution = 'gaussian' is least squares
# n.trees: B in lectures
# interaction.depth: d in lectures
# shrinkage: lambda in lectures
# cv.folds = 5: also do cross-validation

boost.boston = gbm(medv~.,
 data=Boston[train,],
 distribution='gaussian',
 n.trees = 5000,
 interaction.depth = 6,
 shrinkage = 0.01,
 cv.folds = 5
 )

# the next line gives relative variable importance
summary(boost.boston)

# next line gives results from cross-validation
bi = gbm.perf(boost.boston,method="cv")

bi

# predictions 
# note: once we have a model with N trees
# can specify any number of trees for prediction
# as long as it does not exceed N
pr.boo = predict(boost.boston,newdata=Boston[-train,],n.trees=bi)
mean((pr.boo-y.test)^2)

# random forest with m = 6
# on same split gives similar performance

# plot influence of rm on prediction
# this is avergaed over all other predictors
# see details on lecture slides
plot(boost.boston,i='rm',n.trees = bi)

plot(boost.boston,i='lstat',n.trees = bi)

# heat plot of joint effect of rm and lstat
plot(boost.boston,i=c('rm','lstat'),n.trees = 5000)













