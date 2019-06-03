
####################     SM3 GROUP ASSIGNMENT    #####################

######################################################################

# SET UP

  # Set working directory
  setwd("E:/Documents/GitHub/SM3_Group_Project")
  
  # Import libraries
  library(caret)
  library(tidyverse)

  # Load custom scripts 
  source('Scripts/crossValidate.R')

# 1 - READ IN DATASET AND OBTAIN TRAINING AND VALIDATION SETS

  # Read in headers
  headers <- read.csv(file = "train.csv", skip=1, header=F, nrows=1, as.is=T)
  
  # Read in complete dataset
  dat <- read.csv(file = "train.csv", skip = 2, header = F)
  
  # Add the headers to dataset
  colnames(dat) <- headers
  
  # Remove the ID field
  dat <- dat[ ,-1]
  
  # get number of predictors and records in the dataset
  n_predictors = ncol(dat)-1
  n_row = nrow(dat)
  
  # Split the predictors and response
  train.x = data.frame(dat[ ,1:(ncol(dat)-1)])
  train.y = data.frame(dat[ ,ncol(dat)])
  
  # Rename response for brevity, and set as factor
  names(dat)[ncol(dat)]<-"y"
  y = as.factor(y)
  
  # Set up containers for accuracy and model size
  mdl.accuracy = matrix(rep(0,2^4), ncol = 2)
  mdl.size = rep(0,2^4)
  
# 2 - LOOP OVER ALL POSSIBLE MODELS, STORE ERROR ESTIMATES FOR EACH
  
  # Firstly get cross validated results for the null model
  k = 1
  tr.object = train(y~SEX+EDUCATION, data=dat, method="glm", family=binomial(logit), trControl=trainControl(method="cv"))
  tr.object$results$Accuracy
  
  # Loop over the number of predictors to generate all other possible models
  # Each loop, i is the number of included predictors
  for(i in 1:4) {
    
    # Get the number of possible combinations of i predictors
    combs=combn(1:4,i)
    
    # Loop over all possible models with i predictors
    # ncol(combs) is the number of possible models
    # j identifies the specific predictors for each model
    for(j in 1:ncol(combs)) {
      
      # Extract the training dataset with just the included predictors
      train.z = train.x[,combs[,j]]
      
      # Perform cross validation
      tr.object = train(y~train.z, data=dat, method="glm", family=binomial(logit), trControl=trainControl(method="cv"))
      
      # Increment k to store results to correct position in error vectors
      k = k+1      
      
      # Set the model size to the current number of predictors 
      model.size[k] = i
      mdl.accuracy[k,1] = tr.object$results$Accuracy
      mdl.accuracy[k,1] = tr.object$results$AccuracySD
    }
  }

  # Concat results vectors into single dataframe
  results = data.frame(index=c(1:16), mdl.size, mdl.accuracy)


  




