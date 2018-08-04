#This file will produce training error using different polynomial kernel, degrees and cost values from user

setwd('E:/Dropbox/ELE 581 Machine Learning/project 1/Midterm project/')
library(e1071)
#read data file
vert.df <- read.csv('vertebralData.csv')

#run the program
run.prog <- function(cost, degree){
  new.model <- vert_modelbuild(cost, degree)
  training.error(new.model)
  cross.valid.error(new.model)
}

#building different models based on kernel (k) and cost values (c) given from function inputs
vert_modelbuild <- function(c, d){
               svm(Decision~.,
                   data = vert.df,
                   type = "C-classification",
                   kernel = "polynomial",
                   degree = d,
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
cross.valid.error <- function(get.model){
  summary(get.model)
}
