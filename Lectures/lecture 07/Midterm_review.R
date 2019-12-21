# 1. 

x=array(0,3)
x
min(x)
length(x)
y=(1:3)
y
min(y)
for(k in 1:length(x)){
    x[k] = -k
}

x
which.min(x)
min(x)


#2
x = array(0,3)
for(k in 1:3){
    x[k] = k^2
}
sum(x)

#3
x=(1:3)
x
sum(x^2)
mean(x)
z =c(2,3,4)
z


#----
x<-matrix(1:6,ncol=3)
x
apply(x,1,sum)
