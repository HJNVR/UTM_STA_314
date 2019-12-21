rm(list=ls())

setwd('/Users/Starkjing/STA314/Kaggle')

d.train = read.csv('titanic.csv')
d.train
d.train$PassengerId
d.train$Survived
names(d.train)
attach(d.train)
PassengerId
Survived
names(d.train)
plot(PassengerId, Survived)
lm = (Survived ~ PassengerId, data = d.train)
d.train $PassengerId
mean_pred = rep(mean(d.train$PassengerId),418)
mean_pred

#data.frame(1:100)
example_pred = data.frame(cbind(1:418,mean_pred))
#data.frame has number in the front
example_pred

# this line change the names of example_pred to 'id' and 'y'
names(example_pred) = c('id', 'y')
names(example_pred)

write.csv(example_pred,file='linmod.csv',row.names=FALSE)
