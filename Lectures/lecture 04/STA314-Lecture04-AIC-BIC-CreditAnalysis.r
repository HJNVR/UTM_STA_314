library(leaps) # this contains the function regsubsets
library(MASS)
library(ISLR)

# for additional details, see chapter 6.5 of the textbook


lm(Balance~.,data=Credit)
summary(lm(Balance~.,data=Credit))
# note: R has automatically turned qualitative predictors into dummies
# this inly works if originally predictors are coded as strings or factors


#-----------------------------------------------------------------------
# now look at different ways of selecting models
# first: best subset selection
regfit.best = regsubsets(Balance~.,data = Credit)
# performs exhaustive search
summary(regfit.best)

# note: by default only search up to 8 predictors

regfit.best = regsubsets(Balance~.,data = Credit,nvmax = 11)
summary(regfit.best)

# to extract coefficients from one particular model, 
# for example model with 4 predictors
coef(regfit.best,4)

#-----------------------------------------------------------------------
# look at forward stepwise
regfit.forward = regsubsets(Balance~.,data = Credit,method='forward')
# performs forward selection
summary(regfit.forward)

# note: for example for k = 4 we get a different model compared
# to the result from best subset selection.

coef(regfit.forward,4)

#-----------------------------------------------------------------------
# look at backward stepwise
regfit.backward = regsubsets(Balance~.,data = Credit,method='backward')
# performs backward selection
summary(regfit.backward)

# note: we again get different models compared
# to forward stepwise and best subset

coef(regfit.backward,4)



#-----------------------------------------------------------------------
# now look at methods using adjusted R^2, C_p, BIC 

# plot adjusted R^2, Cp, BIC against number of predictors
# use nvmax argument to set maximal number of variables considered
regfit.best = regsubsets(Balance~.,data = Credit,nvmax = 11)

# extract values for Cp, BIC, adjusted R^2 from the object regfit.best
adjr2 = summary(regfit.best)$adjr2
cp = summary(regfit.best)$cp
bic = summary(regfit.best)$bic

which.max(adjr2)
which.min(cp)
which.min(bic)

# plot the results
# red points will indictae the number of predictors selected by each
# Cp, BIC, Adjusted R^2
par(mfrow = c(1,3))
plot(adjr2,type='l',xlab='number of predictors',ylab = 'Adjusted R^2')
points(adjr2)
points(which.max(adjr2),adjr2[which.max(adjr2)],col = 'red')
plot(cp,type='l',xlab='number of predictors',ylab = 'Cp')
points(cp)
points(which.min(cp),cp[which.min(cp)],col = 'red')
plot(bic,type='l',xlab='number of predictors',ylab = 'BIC')
points(bic)
points(which.min(bic),bic[which.min(bic)],col = 'red')

#---------------------------------------------------------------------------
# plot the results with smaller range of number predictors
# red points will indictae the number of predictors selected by each
# Cp, BIC, Adjusted R^2
par(mfrow = c(1,3))
plot(3:12,adjr2[3:12],type='l',xlab='number of predictors',ylab = 'Adjusted R^2')
points(3:12,adjr2[3:12])
points(which.max(adjr2),adjr2[which.max(adjr2)],col = 'red')
plot(3:12,cp[3:12],type='l',xlab='number of predictors',ylab = 'Cp')
points(3:12,cp[3:12])
points(which.min(cp),cp[which.min(cp)],col = 'red')
plot(3:12,bic[3:12],type='l',xlab='number of predictors',ylab = 'BIC')
points(3:12,bic[3:12])
points(which.min(bic),bic[which.min(bic)],col = 'red')

