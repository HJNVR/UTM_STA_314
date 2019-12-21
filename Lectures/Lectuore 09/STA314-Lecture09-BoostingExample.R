rm(list = ls())

library(gbm)

set.seed(1)

x = runif(500)
x
y = sin(6*x) + rnorm(sd = .2,n=500)

boo = gbm(y~x,
distribution='gaussian',
n.trees = 10000,
bag.fraction = 1,
interaction.depth = 1,
shrinkage = 0.1)

for(ntr in c(1:10,5*(5:10),10*(5:10),50*(3:10),500*(1:10))){
  print(ntr)
}
pred_boo = predict(boo, newdata = data.frame(x=xgrid), n.trees = 5000)
pred_boo

xgrid = (1:1000)/1000
par(ask=TRUE)
for(ntr in c(1:10,5*(5:10),10*(5:10),50*(3:10),500*(1:10))){
  plot(x,y,col='grey',main = ntr)
  pr = predict(boo,newdata = data.frame(x=xgrid),n.trees=ntr)
  lines(xgrid,pr,col='black',lwd=2)
}

50*(3:10)


