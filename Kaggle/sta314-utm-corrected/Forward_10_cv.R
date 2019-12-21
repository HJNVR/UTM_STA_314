rm(list = ls())

library(leaps) # this contians the function regsubsets
library(MASS)
library (randomForest)
library(glmnet)
library(gam)
library(caret)
library(gbm)
B=Boston
setwd('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected')
d.train = read.csv('trainingdata2.csv')
d.test = read.csv('test_predictors2.csv')

# partition the training data into four groups
d.train[,9]
d.train[1,]
list_NW = c()
list_cv = c()
list_SE = c()
list_NE = c() 
c(1,2,3,4,6)
d.train[c(2:4)]
nrow(d.train)
for (i in c(1:nrow(d.train))){
  if (d.train[i,9] == 'NW'){
    list_NW = append(list_NW,i)
  }else if(d.train[i,9] == 'cv'){
    list_cv = append(list_cv,i)
  }else if(d.train[i,9] == 'SE'){
    list_SE = append(list_SE,i)
  }else{
    list_NE = append(list_NE,i)
  }
}
list_NW
list_cv
list_NE
list_SE
length(list_NW) #645
length(list_cv) #432
length(list_NE) #244
length(list_SE) #679

# training data with NW
d.train_NW = d.train[list_NW,]

# training data with cv
d.train_cv = d.train[list_cv,]

# training data with NE
d.train_NE = d.train[list_NE,]

# training data with SE
d.train_SE =  d.train[list_SE,]
y_SE = d.train_SE[,1]
y_SE
attach(d.train_SE)
y
# fit SE
set.seed(1)
lm(y~. - X8 + I(X8),data = d.train_SE)
typeof(d.train)
typeof(d.train_SE)
#model_SE = randomForest(y~., data = d.train_SE, mtry = 10, importance = TRUE)
set.seed(1)
fitControl =trainControl(method = "cv", number = 10, savePredictions = TRUE)
model_SE = train(y~., data = d.train_SE, method="glm", family=binomial(link = 'logit'),
                 trControl = fitControl)

set.seed(1)


# fit NW 
set.seed(2)
#model_NW = randomForest(y~., data = d.train_NW, mtry = 10, importance = TRUE)
model_SE = lm(y~.,data=d.train_SE)
set.seed(2) 

# fit cv with random forest
set.seed(3)
model_cv = randomForest(y~., data = d.train_cv, mtry = 10, importance = TRUE)
set.seed(3)

# fit NE with random forest
set.seed(4)
model_NE = randomForest(y~., data = d.train_NE, mtry = 10, importance = TRUE)
p=predict(model_NE, newdata = d.test_cp[3,])
p
set.seed(4)

# do prediction
d.test_cp=d.test
dim(d.test_cp)
dim(d.test_cp[4,])
final_pred = d.test[,c(1,2)]
final_pred[1,2]
p=predict(model_SE,newdata = d.test_cp[1,])
p
p=predict(model_NW, newdata = d.test_cp[1,])
p
data.frame(d.test_cp[4,])
d.test_cp[1,9] == 'SE'
p=predict(model_SE, newdata = d.test_cp[1,])
p
if (d.test_cp[1,9]=='SE'){
  pred=predict(model_SE, newdata = d.test_cp[1,])
  pred
  final_pred[i,2] = pred
  final_pred
}
for (i in c(1:nrow(d.test))){
  if (d.test_cp[i,9]=='SE'){
    pred=predict(model_SE, newdata = d.test_cp[i,])
    final_pred[i,2] = pred
  }else if (d.test_cp[i,9] == 'NW'){
    pred=predict(model_NW, newdata = d.test_cp[i,])
    final_pred[i,2] = pred
  }else if (d.test_cp[i,9] == 'cv'){
    pred=predict(model_cv, newdata = d.test_cp[i,])
    final_pred[i,2] = pred
  }else if (d.test_cp[i,9] == 'NE'){
    pred=predict(model_NE, newdata = d.test_cp[i,])
    final_pred[i,2] = pred
  }
}
names(final_pred) = c('id', 'y')
final_pred
write.csv(final_pred, file = "linmod.csv", row.names = FALSE)
