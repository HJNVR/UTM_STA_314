#install.packages('glmnet')
#install.packages('ISLR')
#install.packages('plotmo')

library(plotmo) # nicer plotting of glmnet output
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set 'Hitters'

attach(Hitters) # works since we loaded the package ISLR

#------------------------------------------------------------

fix(Hitters)

# note: some values of salary are NA, ie unknown
Hitters = na.omit(Hitters) # remove missing values

x = model.matrix(Salary ~ . ,Hitters )[,-1]  
# bring data in format that glmnet can deal with
# [,-1] is to remove intercept

y = Hitters$Salary

grid = 10^seq(10,-2,length = 100) 
# produce a grid of lambda values
# those lambda values will be used for  lasso and ridge

# glmnet with alpha = 1 is lasso
# note: by default, glmnet standardizes predictors
# we don't need to worry about doing that

lasso.mod = glmnet(x,y,alpha = 1, lambda = grid)
plot(lasso.mod, label = TRUE)
# note: plot is against L1 norm (sum of absolute values)
# of estimated b^L(lambda)

# nicer plot with external package
# the function glm_plot is from the library plotmo
plot_glmnet(lasso.mod)

# next look at some of the coefficients
coef(lasso.mod)[,100] # this is the version with small amount of penalization
coef(lasso.mod)[,80] # more penalization, some coefficients are set to zero
coef(lasso.mod)[,70] # even more penalization, more coefficients set to zero
coef(lasso.mod)[,1]

