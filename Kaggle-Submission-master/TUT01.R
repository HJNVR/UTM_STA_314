
#vectors we create v vector
v=c(1:3)
v
v=c("a","b","c")
v

#A list can hold items of different types and the same use
ls = list("a",2,2)
ls

#matrix 
x=matrix(data=c(1,2,3,4),2,2)
x

#be careful to what data type is used for different data types

# array 
arr = array(0.0,c(2,3)) # 2x3 matrix
arr
print(arr) 
arr = array(0.0, c(2,5,4)) # 2x5x4 matrix 3D  
arr
arr[1,2,1]=314
arr
# print is the same as without print

# 
people = c("Alex", "Barb", "carcl") # calls
ages = c(19,20,30)
df = data.frame(people,ages)

# csv ========
install.packages('caret')
library(caret)

par(mfrow=c(2,2))
runif(1)


set.seed(1) 
# between these two commands, same results will be recorded
set.seed(1)

# x unif(0,1 ) have samples

# 
n=100 # 100 observation
x=(1:n)/n # values for predication
x

f = .5*sin(-2+12*x)
f

y = f + rnorm(n,mean=0,sd=0.5)
y

# plot
plot(x,y,ylab='y', main='f(x) = sin(-2+12*x), k=5')
lines(x,f,lwd=2) # lwd: connect all the points to a line

#
fit5 = knnreg(as.matrix(x),as.matrix(y),k=5)

pr5 = predict(fit5, data, frame(x=as.matrix(1:100)))
lines((1:100)/100, pr5, col='red', lwd=2)

plot(x,y,ylab='y', main='f(x) = sin(-2+12*x, k = 25')
lines(x,f, lwd =2) # add regression function to the plot


