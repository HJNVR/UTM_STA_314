#----------------------------------------------------------
# Take a look at Advertising data
# first try validation set approach
#----------------------------------------------------------

rm(list = ls())

library(caret)

# read in data
# setwd('C:/Users/stas/Desktop') # change to a different directory
# C:\Users\stas\Dropbox\________________STA314-2019\Lecture02\RCode
#setwd('C:/Users/stas/Dropbox/________________STA314-2019/Lecture03/RCode')
res = read.csv(file = '/Users/Starkjing/STA314/Advertising.csv') # read a file from the working directory
attach(res)

#----------------------------------------------------------
# load function for cross-validation
#----------------------------------------------------------

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

#----------------------------------------------------------
# consider advertising data
# make a plot of simple linear regression
#----------------------------------------------------------

plot(TV,sales)

# the next line runs a simple lnear regression
# TV is predictor
# intercept is included by default
lm1 = lm(sales~TV)
# look at what the output of lm is
names(lm1)
# plot result
abline(lm1$coefficients[1],lm1$coefficients[2],col='red')

# now run multiple linear regression
# use TV and Radio as predictors
lm2 = lm(sales~TV+radio)
lm2$coefficients

# some more useful syntax
# run a linear regression using everything in data
# except for sales as predictors
# useful when there are many predictors
# this needs the additional argument data
lm3 = lm(sales ~ ., data = res)
lm3$coefficients

# X is an artifact of reading in the data through read.csv()
# exclude X from set of predictors 
lm4 = lm(sales ~ . - X, data = res)
lm4$coefficients

# note: we could (and should) also have removed X in the very beginning
# one way to do that
names(res)
dim(res)
res = res[,-1]
names(res)

#----------------------------------------------------------
# consider advertising data
# compare performance of k-nn and linear regression
#----------------------------------------------------------

# make results reproducible

set.seed(1)

# scaling for K-nn
TvRa = scale(data.frame(radio,TV))

# number of repeated splits
repi = 25

# for saving results
TV_error = array(0,repi)
TvRa_error = array(0,repi)
lm1_error = array(0,repi)
lm2_error = array(0,repi)


# parts of this we have seen before
# new part is linear models

for(r in 1:repi){

  test_index = sample(1:200,40)
  
  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selTvRa = which.min(do_10cv_knn(TvRa[-test_index,],sales[-test_index],krange=1:100))
  
  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)
  
  fit_TvRa = knnreg(TvRa[-test_index,],sales[-test_index],k = K_selTvRa)
  pr_TvRa = predict(fit_TvRa, newdata = TvRa[test_index,])
  TvRa_error[r] = mean((sales[test_index] - pr_TvRa)^2)
  
  # here comes the new part
  # the additional line subset = ((1:200)[-test_index])
  # indicates that R will only use the data with index in subset
  # to fit the model
  
  fit_lm1 = lm(sales ~ TV, subset = ((1:200)[-test_index]))
  fit_lm2 = lm(sales ~ TV + radio, subset = ((1:200)[-test_index]))
  
  # add newdata = ... because now we are predicting out-of sample
  # recall: res is a data frame that contains all our data
  pr_lm1 = predict(fit_lm1, newdata = res[test_index,])
  pr_lm2 = predict(fit_lm2, newdata = res[test_index,])
  
  
  # compute errors and save in corresponding vectors
  lm1_error[r] = mean((pr_lm1 - sales[test_index])^2)
  lm2_error[r] = mean((pr_lm2 - sales[test_index])^2)
  
  # print the progress
  print(r)

}

# some comparisons
# first k-nn and linear model using just TV as predictor

boxplot(TV_error, lm1_error, main = 'test error', names = c('k-nn, TV only','simple linear regression'))

# next k-nn with TV and radio as predictors and multiple linear regression with same predictors
nam = c('k-nn TV and radio', 'Linear model TV and radio')
boxplot(TvRa_error,lm2_error,names =  nam, main = 'Test Error')



#------------------------------------------------------
# Now look at examples with increasing amount of noise
#------------------------------------------------------

set.seed(1)

# this vector contains ndifferent numbers of
# 'noise predictors' which are added to model
dvec = c(1,5,10,15,20,100)
l.d = length(dvec)

# initialize
error_knn = array(0,c(repi,l.d))
error_lm = array(0,c(repi,l.d))


for(r in 1:repi){

  test_index = sample(1:200,40)

  # this 'inner loop' is over different amount of noise variables
  for(d in 1:l.d){
    # this matrix will contain additional noise variables
    NoiseMat = 200*matrix(runif(200*dvec[d]),ncol = dvec[d], nrow = 200)

    # this is a matrix with the new predictors
    # cbind creates a new matrix by binding columns together
    Pr_new = cbind(TV,radio,NoiseMat)

    # use scaled version for k-nn
    Pr_new_knn = scale(cbind(TV,radio,NoiseMat))

    # create a data frame that contains the values for sales, Tv, radio
    # and the new noise predictors
    daf = data.frame(sales, TV, radio, NoiseMat)

    # cross-validation for k-nn
    K_sel = which.min(do_10cv_knn(Pr_new_knn[-test_index,],sales[-test_index],krange=1:100))

    # computing error for k-nn
    fit = knnreg(Pr_new_knn[-test_index,],sales[-test_index],k = K_sel)
    pr = predict(fit, newdata = data.frame(Pr_new_knn  = Pr_new_knn[test_index,]))
    # save error for linear model
    error_knn[r,d] = mean((sales[test_index] - pr)^2)

    # now we consider linear models
    # the shortcut ~. means that everything in dataframe
    # specified by data = daf which is not sales will be used as predictor
    # note the use of subset
    fit_lm1 = lm(sales ~ ., data = daf, subset = ((1:200)[-test_index]))

    # predict new values for data in test set
    pr_lm1 = predict(fit_lm1, newdata = daf[test_index,])

    # save error for linear model
    error_lm[r,d] = mean((pr_lm1 - sales[test_index])^2)


  }

print(r)

}

# look at boxplot for linear models
nam = dvec
boxplot(error_lm,names = nam,main = 'X-axis: number of noise variables')

# look at boxplot for k-nn
nam = dvec
boxplot(error_knn,names = nam,main = 'X-axis: number of noise variables')

# to look at boxlots next to each other
# create a matrix that has
# first error of knn, first error of lm, second error of knn, etc
err_all = array(0,c(repi,2*length(dvec)))
err_all[,2*(1:length(dvec))] = error_lm
err_all[,2*(1:length(dvec))-1] = error_knn

# this is for x labels
# initialize
nam = rep('x',2*length(dvec))

# fill in nam with what we really want
for(k in 1:length(dvec)){
  nam[2*k] = paste('lm, ',dvec[k],sep = '')
  nam[2*k-1] = paste('knn, ',dvec[k],sep = '')
}

# now plot
boxplot(err_all,names = nam,main = 'X-axis: number of noise variables')




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# now look at plots of residuals
lm1 = lm(sales ~ TV)
lm2 = lm(sales ~ TV + radio)

# the function call predict(lm1) gives predictions for
# the data that were used to fit the model
# predictions for new data will be discussed later in this lecture

pr1 = predict(lm1)
pr2 = predict(lm2)

par(mfrow = c(1,2))

plot(pr1, sales - pr1,ylim = c(-10,10), xlab = 'predicted values', ylab = 'residuals', main = 'TV')
plot(pr2, sales - pr2,ylim = c(-10,10), xlab = 'predicted values', ylab = 'residuals', main = 'TV and Radio')

# note: an equivalent way to obtian residuals in the above example is
resi = lm1$residuals

# verify this is really the same:

sum(abs(resi - (sales - pr1)))
# almost the same...

resi == (sales - pr1)
# this could be expeted to work, but doesn't
# computer (lack of) precision...


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# now add interaction to the model
# R syntax for adding interactions: TV:radio

lm3 = lm(sales ~ TV + radio + TV:radio)

# note: TV*radio is shortcut for TV + radio + TV:radio
# thus this here gives the same model

lm3 = lm(sales ~ TV*radio)

# first, compare residual plots from 3 models discussed so far
pr3 = predict(lm3)

par(mfrow = c(1,3))

plot(pr1, sales - pr1,ylim = c(-10,10))
plot(pr2, sales - pr2,ylim = c(-10,10))
plot(pr3, sales - pr3,ylim = c(-10,10))


# also look at mean of squared predicted residuals
mean(lm1$residuals^2)
mean(lm2$residuals^2)
mean(lm3$residuals^2)

# now plot residuals from model with interaction against predicted values
# and against each of the two predictors
par(mfrow = c(1,3))

plot(pr3, sales - pr3,ylim = c(-10,10),xlab = 'predicted value', ylab = 'residuals')
plot(TV, sales - pr3,ylim = c(-10,10), xlab = 'TV', ylab = 'residuals')
plot(radio, sales - pr3,ylim = c(-10,10), xlab = 'Radio', ylab = 'residuals')

#-------------------------------------------------------------------
# Linear model for Advertising data set with interaction and non-linear impact of TV
#-------------------------------------------------------------------

# add TV^2, TV^3, TV^4 as predictors
# note: I(TV^2) etc is needed
# just writing TV^2 would mean something else
# this has special meaning in the formula syntax
# type ?formula and read 'Details' for some explanations

lm4 = lm(sales ~ TV + I(TV^2) + I(TV^3) + I(TV^4) + radio + TV:radio)
pr4 = predict(lm4)

par(mfrow = c(1,3))

plot(pr4, sales - pr4,ylim = c(-5,5),xlab = 'predicted value', ylab = 'residuals')
plot(TV, sales - pr4,ylim = c(-5,5), xlab = 'TV', ylab = 'residuals')
plot(radio, sales - pr4,ylim = c(-5,5), xlab = 'Radio', ylab = 'residuals')

mean((sales-pr4)^2)

#----------------------------------------------------------
# consider advertising data
# compare performance of k-nn and linear regression
#----------------------------------------------------------

# make results reproducible

set.seed(1)

# scaling for K-nn
TvRa = scale(data.frame(radio,TV))

# number of repeated splits
repi = 25

# for saving results
TV_error = array(0,repi)
TvRa_error = array(0,repi)
lm1_error = array(0,repi)
lm2_error = array(0,repi)
lm3_error = array(0,repi)
lm4_error = array(0,repi)

# parts of this we have seen before
# new part is linear models

for(r in 1:repi){

  test_index = sample(1:200,40)

  K_selTV = which.min(do_10cv_knn(TV[-test_index],sales[-test_index],krange=1:100))
  K_selTvRa = which.min(do_10cv_knn(TvRa[-test_index,],sales[-test_index],krange=1:100))

  fit_TV = knnreg(as.matrix(TV[-test_index]),sales[-test_index],k = K_selTV)
  pr_TV = predict(fit_TV, newdata = data.frame(TV  = TV[test_index]))
  TV_error[r] = mean((sales[test_index] - pr_TV)^2)

  fit_TvRa = knnreg(TvRa[-test_index,],sales[-test_index],k = K_selTvRa)
  pr_TvRa = predict(fit_TvRa, newdata = TvRa[test_index,])
  TvRa_error[r] = mean((sales[test_index] - pr_TvRa)^2)

  # here comes the new part
  # the additional line subset = ((1:200)[-test_index])
  # indicates that R will only use the data with index in subset
  # to fit the model

  fit_lm1 = lm(sales ~ TV, subset = ((1:200)[-test_index]))
  fit_lm2 = lm(sales ~ TV + radio, subset = ((1:200)[-test_index]))
  fit_lm3 = lm(sales ~ TV + radio + TV:radio, subset = ((1:200)[-test_index]))
  fit_lm4 = lm(sales ~ TV + I(TV^2) + I(TV^3) + I(TV^4) + radio + TV:radio, subset = ((1:200)[-test_index]))
  
  # add newdata = ... because now we are predicting out-of sample
  # recall: res is a data frame that contains all our data
  pr_lm1 = predict(fit_lm1, newdata = res[test_index,])
  pr_lm2 = predict(fit_lm2, newdata = res[test_index,])
  pr_lm3 = predict(fit_lm3, newdata = res[test_index,])
  pr_lm4 = predict(fit_lm4, newdata = res[test_index,])

  # compute errors and save in corresponding vectors
  lm1_error[r] = mean((pr_lm1 - sales[test_index])^2)
  lm2_error[r] = mean((pr_lm2 - sales[test_index])^2)
  lm3_error[r] = mean((pr_lm3 - sales[test_index])^2)
  lm4_error[r] = mean((pr_lm4 - sales[test_index])^2)
  
  # print the progress
  print(r)

}

# some comparisons
# first k-nn and linear model using just TV as predictor

boxplot(TV_error, lm1_error, main = 'test error', names = c('k-nn, TV only','simple linear regression'))

# next k-nn with TV and radio as predictors and multiple linear regression with same predictors
nam = c('k-nn TV and radio', 'Linear model TV and radio')
boxplot(TvRa_error,lm2_error,names =  nam, main = 'Test Error')

# next k-nn with two predictors and 3 different linear models using two predictors
nam = c('k-nn TV and Radio', 'lm2','lm3','lm4')
boxplot(TvRa_error,lm2_error,lm3_error,lm4_error,names =  nam, main = 'Test Error')

# take closer look at 3 best models
nam = c('k-nn TV and Radio', 'lm3','lm4')
boxplot(TvRa_error,lm3_error,lm4_error,names =  nam, main = 'Test Error',ylim = c(0,2))

