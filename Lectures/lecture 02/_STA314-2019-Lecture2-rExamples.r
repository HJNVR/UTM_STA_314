#-----------------------------------------------------------------------
# simulate average bias, variance and mse of k-nn as a function of k
#-----------------------------------------------------------------------

rm(list=ls())
library(caret)

set.seed(1)

n = 100 # 100 observations
x = (1:n)/n # values for predictors
f = .5*sin(-2+12*x)

#-------------------------------------------------------
# The validation set approach
#-------------------------------------------------------

# the following is a function
# it expects 3 arguments as input
# X: predictors
# Y: response
# krange: range ok values for K to be considered

# output: array with validation set error for every value of k
# floor(5/2) floor: this returns the largest integer number that is not greater than the given number
valset_knn = function(X,Y,krange){
  n = length(Y)
  l.tr = floor(n/2)
  l.val = n - l.tr
  train_index = sample(1:n,l.tr) # randomly sample l.tr points from 1,...,n
  # this will correspond to the training set

  # create array where results of validation error will be stored
  valset_error = array(0,length(krange))

  # loop over values of K, foer ach store error on validation set
  for(k in 1:length(krange)){
     # only use data with index in train_index to fit the model
     K = krange[k]
     fit = knnreg(as.matrix(x[train_index]),y[train_index],k=K)

     # now use fitted model to predict data which are not in train_index
     # recall from lecture 0: a[-b] gives entris of a which are not indexed by b
     pr = predict(fit, newdata = data.frame(x = x[-train_index]))

     # compute and store the validation error
     valset_error[k] = mean((y[-train_index] - pr)^2)
  }

  return(valset_error) # this will be the output of the function
}

# make reproducible
set.seed(2)

# create data
n = 100 # 100 observations
x = (1:n)/n # values for predictors
f = .5*sin(-2+12*x)
y = f + rnorm(n,mean=0,sd = .5)

# call function 
valset_knn(X=x,Y=y,krange = 1:50)

# call again, save outout in array 
err_vs = valset_knn(X=x,Y=y,krange = 1:50)

# plot ourput
plot(1:50, err_vs,xlab='k',ylab='error',main='validation set aproach')
selected_k = which.min(err_vs) # which.min returns location where input is minimized
selected_k
# add a line to the plot
lines(c(selected_k,selected_k), c(0,100), col= 'red',lwd=3)

set.seed(2)

# now repeat the above procedure r times
# to see how much variation there is across repititions
r = 100 #each tiem we roll 100 time to see the difference 
k_vs = array(0,r) # for storing results

for(j in 1:r){
  y = f + rnorm(n,mean=0,sd = .5)
  k_vs[j] = which.min(valset_knn(x,y,1:50))
  print(j)
}

# make a histogram of the results
hist(k_vs,main = 'Validation set approach', xlab = 'k', breaks = (0:14)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2, lwd=3)


#-------------------------------------------------------
# 10-fold cross-validation
#-------------------------------------------------------

# write a function that does 10-fold cross-validation

# input:
# X: predictors
# Y: response
# krange: range ok values for K to be considered

# output: array with 10-fold cross-validation error for every value of k in krange

do_10cv_knn = function(X,Y,krange){
  n = length(Y) # smaple size

  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx,]
  Y = Y[permidx]

  # size of fold
  foldsize = floor(n/10)

  # for saving errors
  valset_error = array(0,length(krange))

  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds
    for(j in 1:10){
     # for fold 1-9 do same as last time
     if(j < 10){
       testidx = foldsize*(j-1) + (1:foldsize)
     # take care with last fold, it might be larger
     # reason: n/10 might not be an integer
     }else{
       testidx = (foldsize*(j-1)):n
     }

     fit = knnreg(as.matrix(X)[-testidx,,drop=FALSE],Y[-testidx],k=K)
     pr = predict(fit, newdata = as.matrix(X)[testidx,,drop=FALSE])
     valset_error[k] = valset_error[k] + mean((Y[testidx] - pr)^2)
    } # end loop over folds
  } # end loop over k

  # the next line will output the result
  return(valset_error)
}

# in the following compare performance of validation set approach 
# and 10-fold cross-validation
# to do so run both procedures 100 times (each time on same data set)

set.seed(2)

r = 100

# the following arrays are for storing errors
vs = array(0,r)
cv10 = array(0,r)
err_vs = array(0,r)
err_cv10 = array(0,r)
err_opt = array(0,r)

for(j in 1:r){
  # create data
  y = f + rnorm(n,mean=0,sd = .5)
  
  # run validation set and 10-fold cv
  vs[j] = which.min(valset_knn(x,y,1:50))
  cv10[j] = which.min(do_10cv_knn(x,y,1:50))

  x0 = (1:100)/101
  
  # compute fit and error with k given by validation set
  fit_vs = knnreg(as.matrix(x),as.matrix(y),k=vs[j])
  pr_vs = predict(fit_vs,data.frame(x=as.matrix(x0)))
  err_vs[j] = sum((pr_vs - f)^2)

  # compute fit and error with k given by 10-cv
  fit_cv10 = knnreg(as.matrix(x),as.matrix(y),k=cv10[j])
  pr_cv10 = predict(fit_cv10,data.frame(x=as.matrix(x0)))
  err_cv10[j] = sum((pr_cv10 - f)^2)

  # compute fit and error with optimal k (determined earlier)
  fit_opt = knnreg(as.matrix(x),as.matrix(y),k=15)
  pr_opt = predict(fit_opt,data.frame(x=as.matrix(x0)))
  err_opt[j] = sum((pr_opt - f)^2)
  
  # print progress
  print(j)
}

# the following are estimated mean squared errors
# note: those are not exactly numbers in lectures
# those numbers were generated with more repetitions
# in order to minimize effect of randomness 
mean(err_cv10)
mean(err_vs)
mean(err_opt)

# plot results
par(mfrow = c(1,2))
hist(vs,main = 'Validation set approach', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)

hist(cv10,main = '10-fold cross-validation', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)


#-------------------------------------------------------
# 10-fold cross-validation with 1-standard error rule
#-------------------------------------------------------


do_10cv_1se_knn = function(X,Y,krange){
  n = length(Y) # smaple size

  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx,]
  Y = Y[permidx]

  # size of fold
  foldsize = floor(n/10)

  # for saving errors
  cv_error = array(0,length(krange))
  # for saving standard errors
  cv_se = array(0,length(krange))

  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds

    fold_err = array(0,10) # will contain error from each fold
    # used for 1 se rule

    for(j in 1:10){
     # for fold 1-9 do same as last time
     if(j < 10){
       testidx = foldsize*(j-1) + (1:foldsize)
     # take care with last fold, it might be larger
     # reason: n/10 might not be an integer
     }else{
       testidx = (foldsize*(j-1)):n
     }

     fit = knnreg(as.matrix(X)[-testidx,,drop=FALSE],Y[-testidx],k=K)
     pr = predict(fit, newdata = as.matrix(X)[testidx,,drop=FALSE])
     fold_err[j] = mean((Y[testidx] - pr)^2)

    } # end loop over folds
  cv_error[k] = mean(fold_err)
  cv_se[k] = sd(fold_err)
  } # end loop over k

  # create a list with two elements
  res_list = list(cv_error,cv_se)

  # give names to the elements in list
  names(res_list) = c('cv_error','cv_se')

  # the next line will output the result
  return(res_list)
}


# create data
n = 100 # 100 observations
x = (1:n)/n # values for predictors
f = .5*sin(-2+12*x)
y = f + rnorm(n,mean=0,sd = .5)

re = do_10cv_1se_knn(x,y,1:50)

mi.err = min(re$cv_error)
mi.sd = re$cv_se[which.min(re$cv_error)]
max((1:50)[which(re$cv_error < mi.err + mi.sd*10^{-1/2})])


#  ADD MORE HERE


set.seed(2)

r = 100

# the following arrays are for storing errors
vs = array(0,r)
cv10 = array(0,r)
cv10_1se = array(0,r)
err_vs = array(0,r)
err_cv10 = array(0,r)
err_cv10_1se = array(0,r)
err_opt = array(0,r)

for(j in 1:r){
  # create data
  y = f + rnorm(n,mean=0,sd = .5)
  
  # validation set approach
  vs[j] = which.min(valset_knn(x,y,1:50))
  # 10-fold cv
  cv10[j] = which.min(do_10cv_knn(x,y,1:50))
  # 10-fold cv with 1 se rule
  re = do_10cv_1se_knn(x,y,1:50)
  mi.err = min(re$cv_error)
  mi.sd = re$cv_se[which.min(re$cv_error)]
  cv10_1se[j] = max((1:50)[which(re$cv_error < mi.err + mi.sd*10^{-1/2})])


  x0 = (1:100)/101
  
  # compute fit and error with k given by validation set
  fit_vs = knnreg(as.matrix(x),as.matrix(y),k=vs[j])
  pr_vs = predict(fit_vs,data.frame(x=as.matrix(x0)))
  err_vs[j] = sum((pr_vs - f)^2)

  # compute fit and error with k given by 10-cv
  fit_cv10 = knnreg(as.matrix(x),as.matrix(y),k=cv10[j])
  pr_cv10 = predict(fit_cv10,data.frame(x=as.matrix(x0)))
  err_cv10[j] = sum((pr_cv10 - f)^2)

    # compute fit and error with k given by 10-cv
  fit_cv10_1se = knnreg(as.matrix(x),as.matrix(y),k=cv10_1se[j])
  pr_cv10_1se = predict(fit_cv10_1se,data.frame(x=as.matrix(x0)))
  err_cv10_1se[j] = sum((pr_cv10_1se - f)^2)
  
  # compute fit and error with optimal k (determined earlier)
  fit_opt = knnreg(as.matrix(x),as.matrix(y),k=15)
  pr_opt = predict(fit_opt,data.frame(x=as.matrix(x0)))
  err_opt[j] = sum((pr_opt - f)^2)
  
  # print progress
  print(j)
}

# the following are estimated mean squared errors
# note: those are not exactly numbers in lectures
# those numbers were generated with more repetitions
# in order to minimize effect of randomness 
mean(err_cv10_1se)
mean(err_cv10)
mean(err_vs)
mean(err_opt)

# plot results
par(mfrow = c(1,3))
hist(vs,main = 'Validation set approach', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)

hist(cv10,main = '10-fold cross-validation', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)

hist(cv10_1se,main = '10-fold cross-validation 1 se', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)

#-------------------------------------------------------------------
#----------------------------------------------------------
# Example: k-nn with advertising data
#----------------------------------------------------------
#-------------------------------------------------------------------

# read in data
# setwd('C:/Users/stas/Dropbox/________________STA314-2019/Lecture02/RCode') # change to a different directory
#--
# CHOOSE YOUR WORKING DIRECTORY HERE
#--
res = read.csv(file = 'Advertising.csv') # read a file from the working directory
attach(res)


# make results reproducible

set.seed(1)

# sample a test set
test_index = sample(1:200,40)

# run k-nn on the training set
# this includes selecting k via cross-validation

# first use TV as predictor
# select K
K_10CV_TV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
# fit model with selected K
fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_10CV_TV)
# predict data in training set
pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
# compute test error
TV_error = mean((sales[test_index] - pr_TV)^2)
TV_error

# next do the same with radio as predictor
K_10CV_radio = which.min(do_10cv_knn(radio[-test_index],sales[-test_index],krange=1:100))
fit_radio = knnreg(as.matrix(radio[-test_index]),sales[-test_index],k = K_10CV_radio)
pr_radio = predict(fit_radio, newdata = data.frame(radio  = radio[test_index]))
radio_error = mean((sales[test_index] - pr_radio)^2)
radio_error

# next do the same with News as predictor
K_10CV_News = which.min(do_10cv_knn(newspaper[-test_index],sales[-test_index],krange=1:100))
fit_News = knnreg(as.matrix(newspaper[-test_index]),sales[-test_index],k = K_10CV_News)
pr_News = predict(fit_News, newdata = data.frame(newspaper  = newspaper[test_index]))
News_error = mean((sales[test_index] - pr_News)^2)
News_error

# compare all of this with just using the mean
pr_mean = mean(sales[-test_index])
mean_error = mean((sales[test_index] - pr_mean)^2)
mean_error

#----------------------------------------------------------
# now repeat above procedure several times
#----------------------------------------------------------

set.seed(1)

repi = 25
TV_error = array(0,repi)
radio_error = array(0,repi)
News_error = array(0,repi)


for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selradio = which.min(do_10cv_knn(radio[-test_index],sales[-test_index],krange=1:100))
  K_selNews = which.min(do_10cv_knn(newspaper[-test_index],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)

  fit_radio = knnreg(as.matrix(radio[-test_index]),sales[-test_index],k = K_selradio)
  pr_radio = predict(fit_radio, newdata = data.frame(radio  = radio[test_index]))
  radio_error[r] = mean((sales[test_index] - pr_radio)^2)

  fit_News = knnreg(as.matrix(newspaper[-test_index]),sales[-test_index],k = K_selNews)
  pr_News = predict(fit_News, newdata = data.frame(newspaper  = newspaper[test_index]))
  News_error[r] = mean((sales[test_index] - pr_News)^2)

  print(r)

}

par(mfrow = c(1,1))
boxplot(TV_error,radio_error,News_error,main = 'Test Error',names = c('TV','radio','newspaper'),ylim = c(0,max(News_error)))




#----------------------------------------------------------
# now do the same but with models including several predictors
#----------------------------------------------------------

set.seed(1)
# scaling for K-nn
TvRa = scale(data.frame(radio,TV))
TvRaNe = scale(data.frame(radio,TV,newspaper))

repi = 25
TV_error = array(0,repi)
TvRa_error = array(0,repi)
TvRaNe_error = array(0,repi)

for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selTvRa = which.min(do_10cv_knn(TvRa[-test_index,],sales[-test_index],krange=1:100))
  K_selTvRaNe = which.min(do_10cv_knn(TvRaNe[-test_index,],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)

  fit_TvRa = knnreg(TvRa[-test_index,],sales[-test_index],k = K_selTvRa)
  pr_TvRa = predict(fit_TvRa, newdata = TvRa[test_index,])
  TvRa_error[r] = mean((sales[test_index] - pr_TvRa)^2)

  fit_TvRaNe = knnreg(TvRaNe[-test_index,],sales[-test_index],k = K_selTvRaNe)
  pr_TvRaNe = predict(fit_TvRaNe, newdata = TvRaNe[test_index,])
  TvRaNe_error[r] = mean((sales[test_index] - pr_TvRaNe)^2)

  print(r)

}

boxplot(TV_error,TvRa_error,TvRaNe_error,main = 'Test Error',names = c('TV','TV+RA','TV+RA+NE'))

boxplot(TvRa_error,TvRaNe_error,names=c('Ta+RA','TV+Ra+News'))




#----------------------------------------------------------
# now do the same but with models including several predictors
# and without scalig predictors
#----------------------------------------------------------

set.seed(1)
TvRa = data.frame(radio,TV)
TvRaNe = data.frame(radio,TV,newspaper)

repi = 25
TV_error_nosc = array(0,repi)
TvRa_error_nosc = array(0,repi)
TvRaNe_error_nosc = array(0,repi)

for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selTvRa = which.min(do_10cv_knn(TvRa[-test_index,],sales[-test_index],krange=1:100))
  K_selTvRaNe = which.min(do_10cv_knn(TvRaNe[-test_index,],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error_nosc[r] = mean((sales[test_index] - pr_TV)^2)

  fit_TvRa = knnreg(TvRa[-test_index,],sales[-test_index],k = K_selTvRa)
  pr_TvRa = predict(fit_TvRa, newdata = TvRa[test_index,])
  TvRa_error_nosc[r] = mean((sales[test_index] - pr_TvRa)^2)

  fit_TvRaNe = knnreg(TvRaNe[-test_index,],sales[-test_index],k = K_selTvRaNe)
  pr_TvRaNe = predict(fit_TvRaNe, newdata = TvRaNe[test_index,])
  TvRaNe_error_nosc[r] = mean((sales[test_index] - pr_TvRaNe)^2)

  print(r)

}

boxplot(TvRa_error,TvRa_error_nosc,TvRaNe_error,TvRaNe_error_nosc,names=c('TV and Radio','TV and Radio not stadardised','all','all not standardised'))

