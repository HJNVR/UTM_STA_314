install.packages('adlift')
install.packages('EbayesThresh')
library(adlift)
library(ISLR)
library(splines)

# load data
res = data(motorcycledata)
X = motorcycledata[,1]
Y = motorcycledata[,2]

plot(X,Y)


# grid on age values where want to predict
X.grid = seq(min(X),max(X),length.out = 500)

#------------------------------------------------------
# Smoothing splines
#------------------------------------------------------

# does not use linear model, needs special function
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
lines(fit ,col ="red",lwd =2)
lines(fit2 ,col ="blue",lwd =2)

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


