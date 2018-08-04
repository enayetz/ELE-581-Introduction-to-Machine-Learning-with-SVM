#This file will produce training error using different linear kernel and cost values from user

setwd('E:/Dropbox/ELE 581 Machine Learning/Final project/Final/')
library(e1071)
# #read data file
airNoise.df <- read.csv('airfoil_self_noise.csv')

#creating 1000 bootstrap sample data frame using sample()
loopsize = 1000
#bootstrap.df <- list()
err <- array(dim = loopsize)


#model building using with the best model parameters
train_modelbuild <- function(getdata){
  svm(sound_level~.,
      data = getdata,
      type = "eps-regression",
      kernel = "radial",
      cost = 10,
      epsilon = 0.1)
}


#Training error calculation
trainingerror <- function(get.testdata, predictedVal){
  difference <- (get.testdata$sound_level - predictedVal)
  # rmseValue <- rmse(error)
  sqrt(mean(difference^2))
}



for (i in 1:loopsize) {
  traindata <- 0
  testdata <- 0
  
  #building the bootstrap with replacement dataframe
  bootstrap.df <- airNoise.df[sample(x = nrow(airNoise.df), size = nrow(airNoise.df), replace = TRUE), ]
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


