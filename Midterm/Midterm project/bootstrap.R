#Bootstrap sample with replacement using hold-out method

setwd('E:/Dropbox/ELE 581 Machine Learning/project 1/Midterm project/')
library(e1071)
#read data file
vert.df <- read.csv('vertebralData.csv')

#creating 200 bootstrap sample data frame using sample()
loopsize = 1000
#bootstrap.df <- list()
err <- array(dim = loopsize)


#model building using with the best model parameters
train_modelbuild <- function(getdata){
  svm(Decision~.,
      data = getdata,
      type = "C-classification",
      kernel = "linear",
      cost = 100)
}


#Training error calculation
trainingerror <- function(get.testdata, prediction){
  #confusion Matrix
  cm <- table(get.testdata$Decision, prediction)
  error <- (cm[1,2] + cm[2,1])/length(prediction)
  error
}


for (i in 1:loopsize) {
  traindata <- 0
  testdata <- 0
  
  #building the bootstrap with replacement dataframe
  bootstrap.df <- vert.df[sample(x = nrow(vert.df), size = nrow(vert.df), replace = TRUE), ]
  #using hold-out method
  # Now Selecting 70% of data as sample from total 'n' rows of the data  
  datasample <- sample.int(n = nrow(bootstrap.df), size = floor(.70*nrow(bootstrap.df)), replace = FALSE)
  traindata <- bootstrap.df[datasample, ]
  testdata  <- bootstrap.df[-datasample, ]
  
  #svm model building with training data
  trainModel <- train_modelbuild(traindata)
  #prediction data
  pred <- predict(trainModel, testdata)
  #calculate error
  err[i] <- trainingerror(testdata, pred)
  
}


#ascending the error
ascending.error <- sort(err)
#get the boundaries: lb: lower bound, ub: upper bound
lb <- ascending.error[25]
ub <- ascending.error[975]


