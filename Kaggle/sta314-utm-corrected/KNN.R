library(caret) 

rm(list = ls())
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

x = model.matrix(y ~ . , data=d.train)
ytest = model.matrix(id ~. ,data = d.test)
y = d.train$y

# knnreg takes the rows of matrix as predictos with response y
# (1) start with 5
fit = knnreg(x,y, k = 5) # this function does a pre-prossesing step
fit

pr_5 = predict(fit, newdata = ytest)
pr_5

length(pr_5)

da.sample = data.frame(cbind(1:30000,  pr_5))
names(da.sample) = c('id', 'y')
da.sample
write.csv(da.sample, file = "linmod.csv", row.names = FALSE)
