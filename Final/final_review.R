x=array(0,3)
x

y = array(1,5)
y

z = (1:4)
z

x[1]
x[2]
x[3]
x[4]

for(k in 1:length(x)){
    x[k] = k - 3
}

x
which.min(x)
min(x)

x = array(0,2)
for(k in 1:length(x)){
  x[k] = 5 + k
}

min(x)
