# remember to use set.seed(1) before doing any homework 
set.seed(1)
x=rnorm(10)
set.seed(1)
y=rnorm(10)
x==y

# graphic  plot 
x=rnorm(10)
y=rnorm(10)a
plot(x,y)
plot(x,y,xlab="this is x axis", ylab="this is y axis", main="plot of x vs y")

#use pdf 
#pdf("Figure.pdf")
plot(x,y,col="red")
pdf("Figure.pdf")
dev.off()

# sequence
x=1:10
x
x=seq(1,10)
x
x=seq(-pi ,pi ,length =50)
x

# indexing the data 
# matrix(values, #rows, #columns) 
A=matrix(1:16, 4,4)
A
#A=matrix(1:12,4,3)
#A
A[2,3] # this will be the second row and third columns

A[c(1,3),c(2,4)] # A[1,2] A[1,4] A[3,2] A[3,4]

A[1:3,2:4]  # A[1,2] ....A[3,4] 3x3

A[1:2,] # with all columns and 1th 2nd row 2 x 4 

A[,1:2] # 4 x 2

dim(A)

# loading data
# .data is table and ther is one .data editor: Xquart
# fix(Auto) ->  this line of command use the Xquart to show the table
Auto=read.table("/Users/Starkjing/ST314/1.data")
fix(Auto)  # this command is to show the .data file you created
Auto=read.table("/Users/Starkjing/ST314/1.data", header=T, na.strings="?")
# head = T means the first line is the variable names
fix(Auto) 

Auto=read.csv("/Users/Starkjing/ST314/Advertising.csv")
fix(Auto) 
dim(Auto) # this means the table is just like matrix has dimension
# whatch out that the
Auto[1:4,]

Auto=na.omit(Auto) # this command is to simply remove the rows with missing datas
dim(Auto)

names(Auto)  #this command is to show all the variable names in the table


