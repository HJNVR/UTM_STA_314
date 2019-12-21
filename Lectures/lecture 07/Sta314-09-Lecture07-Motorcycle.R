#install.packages('adlift')
library(adlift)
library(ISLR)

res = data(motorcycledata)

X = motorcycledata[,1]
Y = motorcycledata[,2]

plot(X,Y)

# polynomial regression

fit1 = lm(Y ~ X)
fit2 = lm(Y ~ poly(X,2))
fit6 = lm(Y ~ poly(X,6))
fit15 = lm(Y ~ poly(X,15))

# grid on age values where want to predict
X.grid = seq(min(X),max(X),length.out = 500)

# predict
preds1 = predict(fit1, newdata = list(X = X.grid))
preds2 = predict(fit2, newdata = list(X = X.grid))
preds6 = predict(fit6, newdata = list(X = X.grid))
preds15 = predict(fit15, newdata = list(X = X.grid))

# plot corresponding polynomials of different degrees
plot(X,Y)
lines(X.grid, preds1,lwd = 3)
lines(X.grid, preds2,col = 'red',lwd = 3)
lines(X.grid, preds6,col = "blue",lwd = 3)
lines(X.grid, preds15,col = "green",lwd = 3)

#----------------------------------------------------------
#----------------------------------------------------------


#-----------------------------------------------------------------
# next look at step functions
#-----------------------------------------------------------------

fit.pc = lm(Y ~ cut(X,breaks = 2))
pred.pc = predict(fit.pc, newdata = list(X = X.grid))
plot(X,Y)
lines(X.grid, pred.pc,lwd = 3)

# what happens with more intervals?

fit.pc10 = lm(Y ~ cut(X,breaks = 10))
pred.pc10 = predict(fit.pc10, newdata = list(X = X.grid))
lines(X.grid, pred.pc10, lwd = 3,col= 'red')


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
fit.bs = lm(Y ~ bs(X, knots = c(15,20,30,40)))
# predict function from lm works with bs
pred.bs = predict(fit.bs, newdata = list(X = X.grid))
plot(X,Y)
lines(X.grid, pred.bs,lwd = 3)

#------------------------------------------------------
# Example 2: cubic splines, 8 degrees of freedom
# intercept not counted
# this corresponds to 3 knots

fit.bs_df6 = lm(Y ~ bs(X, df = 8))
# predict function from lm works with bs
pred.bs_df6 = predict(fit.bs_df6, newdata = list(X = X.grid))
plot(X,Y)
lines(X.grid, pred.bs_df6,lwd = 3)

#------------------------------------------------------
# Example 3: splines of degree 1
# same df parameter
# can allow for more breakpoints
fit_ls6 = lm(Y ~ bs(X, df = 8, degree = 1))
# predict function from lm works with bs
pred_ls6 = predict(fit_ls6, newdata = list(X = X.grid))
lines(X.grid, pred_ls6,lwd = 3,col='red')
#fit.bs = lm(Y ~ bs(X, knots =c(25,50), degree = 1))



# compare with polynomial of degree 6 from previous fit
lines(X.grid, preds6, col = "blue",lwd = 3)

#------------------------------------------------------
# Next look at natural splines
#------------------------------------------------------

# ns() creates natural splines
# degree is automatically set to 3
# can specify knots or df

fit.ns = lm(Y ~ ns(X, df = 8))
pred.ns = predict(fit.ns, newdata = list(X = X.grid))
lines(X.grid, pred.ns,col = "green",lwd = 3)


#------------------------------------------------------
# Smoothing splines
#------------------------------------------------------

# does not use linear model, needs apecial function
# function smooth.spline is in packX splines

# specify 'degrees of freedom'
# degrees of freedom for smoothing splines
# correspond to a rough measure of model compexity
# no direct interpretation as number of parameters
fit = smooth.spline(X ,Y ,df = 16)

# we can also choose degrees of freedom by CV
# note: the build-in version is LOOCV
fit2 = smooth.spline (X ,Y , cv=TRUE)

# make a plot
plot(X,Y)
lines(fit ,col ="red ",lwd =2)
lines(fit2 ,col =" blue",lwd =2)

#------------------------------------------------------
# local regression
#------------------------------------------------------

# the loess function is part of the splines package
# span was defined in lectures
# degree can be specified, compare lectures
# default degree is 2
# can take values 0,1,2
# implicitly uses specific kernel K
# K(x) = (1-x^3)^3

# span = .2, local linear
fit.lr0 = loess(Y~X ,span =.2, degree = 0)
fit.lr1 = loess(Y~X ,span =.2, degree = 1)
fit.lr2 = loess(Y~X ,span =.2, degree = 2)

plot(X,Y)
lines(X.grid ,predict(fit.lr0 ,data.frame(X=X.grid)), col ="green ",lwd =2)
lines(X.grid ,predict(fit.lr1 ,data.frame(X=X.grid)), col ="blue",lwd =2)
lines(X.grid ,predict(fit.lr2 ,data.frame(X=X.grid)), col ="red",lwd =2)

# higher degree with same span leads to more wiggling
# degree 0 is quite bad at boundary points
# this is calld boundary effect

# next fix degree and compare different span
fit.lr2_10 = loess(Y~X ,span =.1)
fit.lr2_20 = loess(Y~X ,span =.2)
fit.lr2_80 = loess(Y~X ,span =.8)

plot(X,Y)
lines(X.grid ,predict(fit.lr2_10 ,data.frame(X=X.grid)), col ="green ",lwd =2)
lines(X.grid ,predict(fit.lr2_20 ,data.frame(X=X.grid)), col =" blue",lwd =2)
lines(X.grid ,predict(fit.lr2_80 ,data.frame(X=X.grid)), col ="red",lwd =2)

#---------------------------------------------------


