# clear workspace
rm(list = ls())

 #--------------------------------------------------------------
 # Data Analysis: loading data sets into R and making plots
 #--------------------------------------------------------------

 getwd() # show current working directory. this is where on the computer
 # files are saved by default and where R looks ofr files
 setwd('/Users/Starkjing/STA314') # change to a different directory
 # pick the right directory on your computer
 
 res = read.csv(file = 'Advertising.csv') # read a file from the working directory

 fix(res) # take a look at data in table form
 names(res) # names of entries in res
 res$TV # values of column TV
 TV # does not work, not in memory
 attach(res) # make elements of res available in workspace
 TV # now works

 plot(TV,sales) # plot Sales vs TV
 # general way plot function works: first argument gives values for x-axis
 # second argument values for y-axis
 # the plot function has many different options
 # we will see some in lectures
 # for the others: just play around and explore!

 plot(radio,sales) # plot Sales vs Radio
 # note: new plot repalces old plot
 # if we want several plots next to each other, we can use the following function

 par(mfrow = c(1,3)) # tells R to divide plot region into three parts
 # general: par(mfrow = c(A,B)) divides plotting window into A rows and B columns

 # now we can make plots next to each other

 plot(TV,sales)
 plot(radio,sales)
 plot(newspaper,sales)

#  [1] "X"         "TV"        "radio"     "newspaper" "sales" 
 # this is the first plot from lectures
 names(res)
 # try some experiments
 # 1. 
 fit = lm(TV~ sales, data = res)
 summary(fit)
 
# 2. 
 fit = lm(TV ~ . - sales, res)
 summary(fit)
 
# 3.
 fit = lm(TV ~ I(X^2) + radio:X , res)
 summary(fit)
 
# 4。
  fit = lm(TV ~ I(sin(X))+ I(radio^3), res )
  summary(fit)
 #-------------------------------------------------------
 # Plots of some simulated models
 #-------------------------------------------------------

 par(mfrow = c(1,3))

 set.seed(1)

 n = 100 # 100 observations
 x = (1:n)/n # values for predictors

 y1 = 1 + x + rnorm(n,mean=0,sd = .1)
 # f(x) = 1+x
 # epsilon follows a normal distribution with ean 0 and standard deviation 0.1


 plot(x,y1,ylab='y',main = 'f(x) = 1+x')
 lines(x,1+x,lwd=2) # add regression function to the plot
 # lines adds to existing plot

 y2 = 1 + x^2 + rnorm(n,mean=0,sd = .1)
 plot(x,y2,ylab='y',main = 'f(x) = 1+x^2')
 lines(x,1 + x^2,lwd=2)

 y3 = 1 + sin(-2+12*x) + rnorm(n,mean=0,sd = .1)
 plot(x,y3,ylab='y',main = 'f(x) = sin(-2+4*x)')
 lines(x,1 + sin(-2+12*x),lwd=2)


 #--------------------------------------------------------------
 # Data Analysis: loading external packages and using knn-regression
 #--------------------------------------------------------------

 # not all functions in R that we want to use are available in the base version
 # however, there are many packages that contain almost everything one cna imagine
 # we will now load a package that allows ot perform k-nn regression

 # before we can run this line, we need to install this package from the internet
 # to do so, run this here once:
 # install.packages('caret')

 library(caret)

 # the package caret contains the function knnreg
 # which can perform knn regression

 # knnreg(x,y,k=K) uses the rows of the matrix or data frame x as predictors for
 # performing k-nn regression with response y. K is the number of neighbours used
 # note: first entry must be matrix or data frame
 # knnreg(TV,Sales,k=5) does not work

 fit = knnreg(as.matrix(TV),sales,k=5) # this function does a pre-prossesing step
 # it returns an object of class knnreg
 # this object can be further processed by the function predict

 pr = predict(fit, newdata = data.frame(TV = as.matrix(1:300)))

 # the input of the function predict:
 # first argument: an object from a function in R, here knnreg
 # second argument: the values of regressors for which we would like to compute
 # the prediction
 # the output are values of the k-nn regression function at values of newdata

 # in the example above, we compute the value of the knn regression function at the values
 # 1:300
 # so the output will be a vector of length 300
 length(pr)
 # now plot the results
 plot(TV, sales) # first plot original data

 lines(1:300,pr, col='red', lwd = 2) # now add the values of the prediction function
 # lines(x,y) connects the dots (x_1,y_1),...,(x_n,y_n) by lines
 # the argument col='red' means that the line will be red
 # the argment lwd = 2 makes the line thiker

 # next let us reproduce the second plot from lectures
 # you should be able to understand the following code based on what we saw so far

 
 fit5 = knnreg(as.matrix(TV),as.matrix(sales),k=5)
 pr5 = predict(fit5,data.frame(sales=as.matrix(1:300)))

 fit25 = knnreg(as.matrix(TV),as.matrix(sales),k=25)
 pr25 = predict(fit25,data.frame(sales=as.matrix(1:300)))

 fit50 = knnreg(as.matrix(TV),as.matrix(sales),k=50)
 pr50 = predict(fit50,data.frame(sales=as.matrix(1:300)))

 fit100 = knnreg(as.matrix(TV),as.matrix(sales),k=100)
 pr100 = predict(fit100,data.frame(sales=as.matrix(1:300)))

 par(mfrow = c(2,2))

 plot(TV,sales, main = 'K = 5')
 lines(1:300,pr5, lwd=2, col = 2)

 plot(TV,sales, main = 'K = 25')
 lines(1:300,pr25, lwd=2, col = 2)

 plot(TV,sales, main = 'K = 50')
 lines(1:300,pr50, lwd=2, col = 2)

 plot(TV,sales, main = 'K = 100')
 lines(1:300,pr100, lwd=2, col = 2)


#----------------------------------------------------------------------
# Examples with simulated data
#----------------------------------------------------------------------


 par(mfrow = c(2,2))

 set.seed(1)

 n = 100 # 100 observations
 x = (1:n)/n # values for predictors
 f = .5*sin(-2+12*x)

 y = f + rnorm(n,mean=0,sd = .5)
 # f(x) = sin(-2+12*x)
 # epsilon follows a normal distribution with ean 0 and standard deviation 0.2

 plot(x,y,ylab='y',main = 'f(x) = sin(-2+12*x), K = 5')
 lines(x, f ,lwd=2) # add regression function to the plot
 # lines adds to existing plot
 fit5 = knnreg(as.matrix(x),as.matrix(y),k=5)
 pr5 = predict(fit5,data.frame(x=as.matrix((1:300)/300)))
 lines((1:300)/300,pr5,col='red',lwd=2)

 plot(x,y,ylab='y',main = 'f(x) = sin(-2+12*x), K = 25')
 lines(x, f ,lwd=2) # add regression function to the plot
 # lines adds to existing plot
 fit25 = knnreg(as.matrix(x),as.matrix(y),k=25)
 pr25 = predict(fit25,data.frame(x=as.matrix((1:300)/300)))
 lines((1:300)/300,pr25,col='red',lwd=2)


 plot(c(0,1),c(-1.25,1.25),ylab='y',xlab='x',main = 'K=5',type ='n')
 for(i in 1:5){
  y = f + rnorm(n,mean=0,sd = .5)
  fit5 = knnreg(as.matrix(x),as.matrix(y),k=5)
  pr5 = predict(fit5,data.frame(x=as.matrix((1:300)/300)))
  lines((1:300)/300,pr5,col=i+1,lwd=2)
 }
 lines(x, f ,lwd=2)

 plot(c(0,1),c(-1.25,1.25),ylab='y',xlab='x',main = 'K=25',type ='n')
 for(i in 1:5){
  y = f + rnorm(n,mean=0,sd = .5)
  fit25 = knnreg(as.matrix(x),as.matrix(y),k=25)
  pr25 = predict(fit25,data.frame(x=as.matrix((1:300)/300)))
  lines((1:300)/300,pr25,col=i+1,lwd=2)
 }
 lines(x, f ,lwd=2)

 set.seed(1)


#-------------------------------------------------------
# Simulations for bias, variance, MSE of k-nn
#-------------------------------------------------------

n = 100 # 100 observations
x = (1:n)/n # values for predictors
f = .5*sin(-2+12*x)


x0 = (1:100)/101 # new predictors where points are generated 
kmax = 50 # largerst number of k that will be considered

pr = array(0,c(100,kmax,length(x0))) # array that will contian predictions

for(i in 1:100){ # simulate 100 times
    y = f + rnorm(n,mean=0,sd = .5) # create sample
    for(k in 1:kmax){ # loop over k
      fit = knnreg(as.matrix(x),as.matrix(y),k=k)
      pr[i,k,] = predict(fit,data.frame(x=as.matrix(x0)))
    }
}

# will contain bias and variance at each element of x0
bias = array(0,c(kmax,length(x0)))
va = array(0,c(kmax,length(x0)))
for(k in 1:kmax){ # loop over k
  for(i.x in 1:length(x0)){ # loop over x0
    bias[k,i.x] = mean(pr[,k,i.x]) - .5*sin(-2+12*x0[i.x]) # bias
    va[k,i.x] = var(pr[,k,i.x]) # variance
  }
}

# total bias and variance by summing over different points

total.bias.sq = array(0,kmax)
total.va = array(0,kmax)
for(k in 1:kmax){
    total.bias.sq[k] = sum((bias[k,])^2) # sum squared bias over locations
    total.va[k] = sum(va[k,]) # sum variance over locations
}

total.mse = total.bias.sq + total.va

par(mfrow=c(1,1))

# make plots
plot(total.mse,ylim=c(0,max(total.mse)),lwd=2,xlab='K',ylab='',lty=2)
points(1:kmax,total.va,col='red',pch=2,lwd=2)
points(1:kmax,total.bias.sq,col='blue', pch= 3, lwd=2)
points(total.mse,lwd=3)



#-------------------------------------------------------
# Plot training error of K-nn for Advertisement data
#-------------------------------------------------------

 lTV = length(TV)
 te = array(0,lTV)

 for(K in 1:lTV){
   fit = knnreg(as.matrix(TV),as.matrix(sales),k=K)
   te[K] = mean((predict(fit,newdata=as.matrix(TV)) - sales)^2)
 }

 plot(1:lTV,te,xlab = 'K', ylab = 'Training Error')

 
 
 #---------
 x=(1:3)
x
#1.
x = 1 + (1:3)
x
# this will get the third element
x[3] 

# this will remove element which value is 3
z=x[-3]
z

# 
x=(-1:-3)
x
z=x[-3]
z