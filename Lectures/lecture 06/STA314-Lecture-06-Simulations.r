# for lasso and ridge
library(glmnet)

# the following function is for doing simulatiosn with different settings

# input:
# N is sample size
# Nte is size of test set
# coe if true coefficients of model
# sig is varinace of noise
# R is number of repetitions
# grid is grid of lambda values for penalty
# rho is measuring strength of correlation among predictors

# output: errors of ridge and lasso with penalty selected by
# cross-validation and by cross-validation with one standard error rule

DoSims = function(N,Nte,coe,sig,R,grid,rho){

  # all of the following is for saving errors
  err.ri = numeric(R)
  err.la = numeric(R)
  err.ri1se = numeric(R)
  err.la1se = numeric(R)
  err.lm = numeric(R)

  err.nu = numeric(R)

  di = length(coe)

  for(r in 1:R){

  # generate training data
  x0 = rnorm(N)
  x = (array(rnorm(di*N),c(N,di)) + rho*array(x0,c(N,di)))/(1+abs(rho))
  y = x %*% coe + sig*rnorm(N)

  # cross validation for selecting lambda
  cv.out.ri = cv.glmnet(x,y,alpha =0,lambda=grid)
  cv.out.la = cv.glmnet(x,y,alpha =1,lambda=grid)

  bestlam.ri = cv.out.ri$lambda.min
  bestlam.la = cv.out.la$lambda.min
  bestlam.ri1se = cv.out.ri$lambda.1se
  bestlam.la1se = cv.out.la$lambda.1se

  # compute fitted models for ridge and lasso
  fit.ri = glmnet(x,y,alpha = 0)
  fit.la = glmnet(x,y,alpha = 1)

  # generate test data
  x0te = rnorm(Nte)
  xte = (array(rnorm(di*Nte),c(Nte,di)) + rho*array(x0te,c(Nte,di)))/(1+abs(rho))
  yte = xte %*% coe + sig*rnorm(Nte)

  # predict using results from lasso, ridge regression
  # and linear model corresponding to s = 0 in ridge regression
  pr.ri = predict(fit.ri,s=bestlam.ri,newx = xte, alpha = 0)
  pr.la = predict(fit.la,s=bestlam.la,newx = xte, alpha = 1)
  pr.ri1se = predict(fit.ri,s=bestlam.ri1se,newx = xte, alpha = 0)
  pr.la1se = predict(fit.la,s=bestlam.la1se,newx = xte, alpha = 1)

  pr.lm = predict(fit.ri,s=0,newx = xte, alpha = 0)

  # compute test errors
  err.lm[r] = sum((yte - pr.lm)^2)/Nte
  err.ri[r] = sum((yte - pr.ri)^2)/Nte
  err.la[r] = sum((yte - pr.la)^2)/Nte
  err.ri1se[r] = sum((yte - pr.ri1se)^2)/Nte
  err.la1se[r] = sum((yte - pr.la1se)^2)/Nte

#  err.lm[r] = sum((yte - pr.lm)^2)/Nte

  # output progress to screen
  print(r)

}  # end loop over repetitions

# make plot of results
nam = c('ri','ri1se','la','la1se','lm')
boxplot(err.ri,err.ri1se,err.la,err.la1se,err.lm,names=nam)

# return list of resuting error vectors
return(list(err.ri,err.ri1se,err.la,err.la1se,err.lm))

} # end function



# preparing for simulation
# grid for lasso and ridge regression
grid = 10^seq(15,-5,length = 500)

#-----------------------------------------------------------
# basic settings

N = 100  # size of training set
R = 50 # number of simulations

Nte = 1000 # size of test set

rho = 0.5 # for implementing dependence between predictors
# rho = 0 is no dependence

sig = 1   # variance of errors

#-----------------------------------------------------------
# example 1: a few large non-zero coefficients
# many zero coefficients
# note: dimension = n, so classical linear model not applicable
di = 50
coe = rep(0,di)
coe[1:5] = 2
res_1_50 = DoSims(N,Nte,coe,sig,R=20,grid,rho)

# lasso peforms much better

# example 2: more small non-zero coefficients

di = 50
coe = rep(0,di)
coe[1:25] = .2
res_2_50 = DoSims(N,Nte,coe,sig,R=20,grid,rho)

# ridge performs much better


# example 3: all coefficient are non-zero but small

di = 50
coe = rep(0,di)
coe[1:50] = .3
res_3_50 = DoSims(N,Nte,coe,sig,R=20,grid,rho)

# homework: play around with this to get better idea of overall performance
# of ridge vs lasso in different settings
#-------------------------------------------------------------

