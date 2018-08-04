#This file will produce training error using different linear kernel and cost values from user

setwd('E:/Dropbox/ELE 581 Machine Learning/project 1/Midterm project/')
library(e1071)
#read data file
vert.df <- read.csv('vertebralData.csv')

#run the program
#building different models based on kernel (k) and cost values (c) given from function inputs
run.prog <- function(costInput){
  newmodel <- vert_modelbuild(costInput)
  training.error(newmodel)
  crossvaliderror(newmodel)
}

#model building using 10 fold cross validation
vert_modelbuild <- function(c){
  svm(Decision~.,
      data = vert.df,
      type = "C-classification",
      kernel = "linear",
      cost = c,
      cross = 10)
}

#Training error calculation
training.error <- function(get.svm.model){
  predict <- fitted(get.svm.model)
  #confusion Matrix
  cm <- table(vert.df$Decision, predict)
  error <- (cm[1,2] + cm[2,1])/length(predict) * 100
  error
}

#cross validation error
crossvaliderror <- function(getmodel){
  summary(getmodel)
}
