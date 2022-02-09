library(ggplot2)
library(MASS)
library(tidyverse)
library(ISLR)
library(pROC)

#Sample Splitting first
data_X = read.table("/Users/wayne/Desktop/breast_cancer_X.csv")
data_Y = read.table("/Users/wayne/Desktop/breast_cancer_y.csv")
data = cbind(y=data_Y$V1, data_X) ##combined data
test_ix = sample(nrow(data_X), size = as.integer(0.25*nrow(data_X)), replace = FALSE) ##test index
train_ix = setdiff(x = (seq(1, nrow(data_X))), y = test_ix) ##train index
x_test = data_X[c(test_ix), ] ##test data in x
x_train = data_X[c(train_ix), ] ## train data in x
y_test = data_Y[c(test_ix),] ##test data in y
y_train = data_Y[c(train_ix), ] ## train data in y
##Fit logistic Regression
glm.fit <- glm(y_train ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10, data = x_train, family = binomial)
beta = matrix(glm.fit$coefficients[2:11], 10, 1) ##beta with all the coeffs
P = exp(as.matrix(x_test) %*% beta)/(1+exp(as.matrix(x_test) %*% beta))
t = 0.000025 ##set arbitray t
cla = P > t
cla = as.numeric(cla)


n = sample(nrow(data))
myData = data[n,]
#Create 5 equally size folds
folds <- cut(seq(1,nrow(myX)),breaks=5,labels=FALSE)

#Perform 5 fold cross validation
elda = list()
eqda = list()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testDataX <- myData[testIndexes, ]
  trainDataX <- myData[-testIndexes, ]
  L <- lda(trainDataX$y~., trainDataX[, -1])
  Q <- qda(trainDataX$y~., trainDataX[, -1])
  
  
  
}
L