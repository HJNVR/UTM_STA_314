rm(list = ls())
library(ISLR)
attach(Wage)
#fix(Wage)
#names(Wage)
#-----------------------------------------------------------------
# first look at fitting polynomial models
#-----------------------------------------------------------------
# a shortcut to fitting polynomial models is given by the poly function
# note: poly does NOT produce x, x^2, x^3, it is based on linear transfromations
# this will not impact predicted values

# plot is the most common tech in r, with x-axis and y-axis
#plot(age, wage) 

fit1 = lm(wage ~ age ,data = Wage)
#summary(fit1)
#plot(wage ~ age ,data = Wage) 

fit2 = lm(wage ~ poly(age,2) ,data = Wage)
#plot(wage ~ poly(age,2),data = Wage)

fit6 = lm(wage ~ poly(age,6) ,data = Wage)
fit15 = lm(wage ~ poly(age,15),data = Wage)

# grid on age values where want to predict
age.grid = seq(min(age),max(age),length.out = 500)

# predict 
# for each poly func we do a prediction
preds1 = predict(fit1, newdata = list(age = age.grid))
preds2 = predict(fit2, newdata = list(age = age.grid))
preds6 = predict(fit6, newdata = list(age = age.grid))
preds15 = predict(fit15, newdata = list(age = age.grid))

# plot corresponding polynomials of different degrees
plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid, preds1,lwd = 3)
lines(age.grid, preds2,col = 'red',lwd = 3)
lines(age.grid, preds6,col = "blue",lwd = 3)
lines(age.grid, preds15,col = "green",lwd = 3)


#-----------------------------------------------------------------
# next look at step functions
#-----------------------------------------------------------------

# cut can be used to cut predictors into different areas
# the function cut produces variables with different 'levels'
# this is like having dummy variables

# two possible specifications
# examples
set.seed(1)
x = runif(10)
# cut x into two intervals
# breaks is number of intervals
# intervals are taken to be of equal length
# roughly cutting range of x into equal pieces
cut(x,breaks = 2)

# cut x by specifying breakpoints
cut(x,breaks = c(0,.1,.2,1))

# now consider 'wage' data
fit.pc = lm(wage ~ cut(age,breaks = 2))
pred.pc = predict(fit.pc, newdata = list(age = age.grid))
plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid, pred.pc,lwd = 3)

# what happens with more intervals?

fit.pc10 = lm(wage ~ cut(age,,breaks = 10))
pred.pc10 = predict(fit.pc10, newdata = list(age = age.grid))
lines(age.grid, pred.pc10, lwd = 3,col= 'red')


#-----------------------------------------------------------------
# next look at splines
#-----------------------------------------------------------------

library(splines) # contains functions for fitting splines

# bs for creating spline functions
# can be used in combination with lm function 
# to fit models

# attributes to bs()
# knots: specify the knots
# df: specify the degrees of freedom
# degree: degree of polynomial 

# degree defaults to 3 if not sepcified explicitly
# specifying degre and knots completely specifies spline
# alternative: specify degree and df
# knots are computed automatically
# placed at quantiles of predictor values

# by default: no intercept is added
# this is taken into account in df calculation

#------------------------------------------------------
# Example 1: specify knots
fit.bs = lm(wage ~ bs(age, knots = c(30,60)))
# predict function from lm works with bs
pred.bs = predict(fit.bs, newdata = list(age = age.grid))
plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid, pred.bs,lwd = 3)

# just from graph difficult to say where knots are
# human eyes don't see discontinuities in second derivatives...

# display vertical lines at breakpoints
abline(v = 30)
abline(v = 60)

#------------------------------------------------------
# Example 2: cubic splines, 6 degrees of freedom
# intercept not counted
# this corresponds to 3 knots
# or 4 intervals

fit.bs_df6 = lm(wage ~ bs(age, df = 6))
# predict function from lm works with bs
pred.bs_df6 = predict(fit.bs_df6, newdata = list(age = age.grid))
plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid, pred.bs_df6,lwd = 3)

#------------------------------------------------------
# Example 3: splines of degree 1
# same df parameter
# can allow for more breakpoints
fit_ls6 = lm(wage ~ bs(age, df = 6, degree = 1))
# predict function from lm works with bs
pred_ls6 = predict(fit_ls6, newdata = list(age = age.grid))
lines(age.grid, pred_ls6,lwd = 3,col='red')
#fit.bs = lm(wage ~ bs(age, knots =c(25,50), degree = 1))



# compare with polynomial of degree 6 from previous fit 
lines(age.grid, preds6, col = "blue",lwd = 3)



#------------------------------------------------------
# Next look at natural splines
#------------------------------------------------------

# ns() creates natural splines
# degree is automatically set to 3
# can specify knots or df
  
fit.ns = lm(wage ~ ns(age, df = 6))
pred.ns = predict(fit.ns, newdata = list(age = age.grid))
lines(age.grid, pred.ns,col = "green",lwd = 3)


# Exercise 1: fit k-nn on the same data as above, 
# select k by cross-validation

# Exercise 2: write cross-validation function to select number of 
# knots when knots are placed automaticaly



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# End of examples for basis expansions
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

