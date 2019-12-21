# here you will need to set your working directory to
# the place where you downloaded the data to
# in this directory you should have the following two files:
# trainingdata.csv
# test_predictors.csv
# those you will download from the kagge website

# setwd('......')
rm(list=ls())
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
# read in the
d.train = read.csv('trainingdata2.csv')
#d.train
names(d.train)
d.test = read.csv('test_predictors2.csv')
d.test

#d.test
d.train$y
d.train$X1
d.test$y
names(d.train)
data.frame(d.train$y)

d.train$y
# in the following we will generate predictions using the sample mean
# the following code repeats the sample mean of y 30000 times
# you will need to replace the following by better ways of generating predictions later
library(caret) 
fit = knnreg(as.matrix(),sales,k=5)
mean_pred = rep(mean(d.train$y),30000)


# now bring in the right format for submission
# the first column wiht name idshould be the number of the observation
# the second line is your prediction
example_pred = data.frame(cbind(1:30000,mean_pred))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='linmod2.csv',row.names=FALSE)

