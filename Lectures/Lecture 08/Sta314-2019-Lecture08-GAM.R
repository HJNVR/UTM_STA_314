#install.packages('gam')
# install.packages('ISLR')

library(ISLR)
attach(Wage)
library(splines)
library(gam)

# example: GAM with natural splines for year and age, both degree 4
# this can be done by simply using the lm() function
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# prepare plotting region
# plot.gam will produce three plots for the three predictors
# note: we need plot.Gam, not just plot
# the gam package should be loaded to use this
par(mfrow=c(1,3))
plot.Gam(gam1, se=TRUE, col="red")

# now use gam package
# example: GAM with smoothing splines for year and age
# use dummy for education
# note: can not use linear model
# now plot works

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
summary(gam.m3)
coef(gam.m3)
# outcome not too different from previous model

# example: GAM with local regression for age and smoothing sline for year
# note: can not use linear model

gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.lo, se=TRUE, col="green")

# look at the output using 'summary'
# compare lectures for interpretation
summary(gam.lo)

# next consider 3 different models and use anova to compare
# see lectures for explanation of output
gam.m1=gam(wage ~ s(age,df=5)+education,data=Wage)
gam.m2=gam(wage ~ year +s(age,df=5)+education,data=Wage)
gam.m3=gam(wage ~ s(year,df=4)+s(age,df=5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3)

#-----------------------------------------------------------------------
# Extended ANOVA models
# load Advertising data

#setwd('C:/users/stas/Dropbox/________________STA314-2019') # change to a different directory
Adv = read.csv(file = '/Users/Starkjing/STA314/Advertising.csv') # read a file from the working directory
attach(Adv)


# load a package called akima
# will be used for plotting
install.packages('akima')
library(akima)

gam_adv = gam(sales ~ radio + TV:radio + s(TV,df=5) + newspaper,data = Adv)
summary(gam_adv)

# sumary indicates that Newspaper can be dropped

# now consider a model with 'non-parametric interaction'
# between TV and Radio

gam.np=gam(sales~lo(TV,radio,span=0.7)+newspaper,data=Adv)
par(mfrow=c(1,2))
plot(gam.np)

