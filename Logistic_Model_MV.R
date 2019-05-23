
#################     SM3 GROUP ASSIGNMENT    ##################

################################################################


# Set working directory
setwd("E:/Documents/GitHub/SM3_Group_Project")

# Import libraries
library(caret)



#################     FUNCTION DEFINITIONS    ##################

# Function to load the data in file (e.g. "data.csv"), with the 
# response variable in the last column
# Returns validation data set of size N, train data set with the rest of the data
loadData = function(file = "train.csv", N = 1000) {

  head(file)
  
  dat = read.csv(file)
  validInd = sample(1:nrow(dat), N) 
  train = dat[-validInd, ]
  valid = dat[validInd, ]
  trainX = train[ ,1:(ncol(dat)-1)]
  trainY = train[ ,ncol(dat)]
  validX = valid[ ,1:(ncol(dat)-1)]
  validY = valid[ ,ncol(dat)]
  
  return(list(trainX, trainY, validX, validY))
}

# Test loadData function and store output
output <- loadData()



# Function to train our logistic regression model
train_logit_model = function(data) {

  # Need to understand logit model hyperparamters better
  
  # Then need to determine the best way in R of generating 
  # and evaluating different models with different hyperparameters
  
  # The output of this function will be the logistic regression 
  # coefficients 
  
  return(0)
  
}

logit_prediction = function(predictors) {
  
  # Use model that we trained to make predictions 
  predictions <- predict(logit_model,predictors,type="response")
  return(0)
  
}


   
#################            DRIVER             ##################

# Read in headers
headers <- read.csv(file = "train.csv", skip = 1, header = F, nrows = 1, as.is = T)

# Read in complete dataset
dat <- read.csv(file = "train.csv", skip = 2, header = F)

# Add the headers to dataset
colnames(dat) <- headers

trainY = dat[ ,ncol(dat)]

# Generate a logistic regression model using all predictors
logit_model <- glm(trainY~., data = dat, family = binomial(logit))
summary(logit_model)


















