rm(list = ls())
library(gbm)
set.seed(1)

setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')
boo = gbm(y~.,
          distribution='gaussian',
          data= d.train,
          n.trees = 10000,
          bag.fraction = 1,
          interaction.depth = 1,
          shrinkage = 0.1,
          cv.folds = 5)

ntree_opt_cv = gbm.perf(boo, method = 'cv')
print(ntree_opt_cv)

pred_boo = predict(boo, newdata = d.test, n.trees = ntree_opt_cv)
pred_boo = predict(boo, newdata = d.test, n.trees = 500)
length(pred_boo)
pred_boo
xgrid = (1:1000)/1000
ntr = c(1)

