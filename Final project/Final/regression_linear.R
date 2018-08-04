#This file will produce training error using different linear kernel and cost values from user

setwd('E:/Dropbox/ELE 581 Machine Learning/Final project/Final/')
library(e1071)
# #read data file
airNoise.df <- read.csv('airfoil_self_noise.csv')

#run the program
#building different models based on kernel (k) and cost values (c) given from function inputs
run.prog <- function(costInput, epInput){
              newModel <- airNoise.model(costInput, epInput)
              result.error <- rmse.error(newModel)
              result.error
              }

airNoise.model <- function(c, e){
                    svm(airNoise.df$sound_level~.,
                        data = airNoise.df,
                        type = "eps-regression",
                        kernel = "linear",
                        cost = c,
                        epsilon = e)
  }


rmse.error <- function(get.model){

  #get the predicted value
  predicted.sound_level <- predict(get.model, airNoise.df)
  #difference between original and predicted values
  difference <- (airNoise.df$sound_level - predicted.sound_level)
  #get the RMSE values
  regressionRMSE <- sqrt(mean(difference^2))
  regressionRMSE

}
