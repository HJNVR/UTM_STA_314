library(ISLR)
attach(Wage)
library(splines) # contains functions for fitting splines

#------------------------------------------------------
# Smoothing splines
#------------------------------------------------------

# does not use linear model, needs apecial function
# function smooth.spline is in package splines

# specify 'degrees of freedom'
# degrees of freedom for smoothing splines
# correspond to a rough measure of model compexity
# no direct interpretation as number of parameters
fit = smooth.spline(age ,wage ,df = 16)

# we can also choose degrees of freedom by CV
# note: the build-in version is LOOCV
fit2 = smooth.spline (age ,wage , cv=TRUE)

# make a plot
plot(age,wage,cex = .5, col= 'darkgrey')
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
fit.lr0 = loess(wage~age ,span =.2, data=Wage, degree = 0)
fit.lr1 = loess(wage~age ,span =.2, data=Wage, degree = 1)
fit.lr2 = loess(wage~age ,span =.2, data=Wage, degree = 2)

plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid ,predict(fit.lr0 ,data.frame(age=age.grid)), col ="green ",lwd =2)
lines(age.grid ,predict(fit.lr1 ,data.frame(age=age.grid)), col ="blue",lwd =2)
lines(age.grid ,predict(fit.lr2 ,data.frame(age=age.grid)), col ="red",lwd =2)

# higher degree with same span leads to more wiggling
# degree 0 is quite bad at boundary points
# this is calld boundary effect

# next fix degree and compare different span
fit.lr2_10 = loess(wage~age ,span =.1, data=Wage)
fit.lr2_20 = loess(wage~age ,span =.2, data=Wage)
fit.lr2_80 = loess(wage~age ,span =.8, data=Wage)

plot(age,wage,cex = .5, col= 'darkgrey')
lines(age.grid ,predict(fit.lr2_10 ,data.frame(age=age.grid)), col ="green ",lwd =2)
lines(age.grid ,predict(fit.lr2_20 ,data.frame(age=age.grid)), col =" blue",lwd =2)
lines(age.grid ,predict(fit.lr2_80 ,data.frame(age=age.grid)), col ="red",lwd =2)

#---------------------------------------------------



