
####################     SM3 GROUP ASSIGNMENT    #####################

######################################################################

# SET UP

  # Set working directory
  setwd("E:/Documents/GitHub/SM3_Group_Project")
  
  # Import libraries
  library(caret)
  library(ggplot2)

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
  
  
  # get number of predictors and records in the dataset
  n_predictors = ncol(dat)-1
  n_row = nrow(dat)
  
  # Set the number of records in validation set (pick a %)
  N = 0.2 * n_row
  
  # Separate into training and validation sets
  validInd = sample(1:nrow(dat), N)
  train = dat[-validInd, -1]
  val = dat[validInd, -1]
  train.x = train[ ,1:(ncol(train)-1)]
  train.y = train[ ,ncol(train)]
  val.x = val[ ,1:(ncol(train)-1)]
  val.y = val[ ,ncol(train)]
  
  train.x <- as.matrix(train.x)
  
  model = glm(train.y~train.x[,1:2], family = binomial(logit))
  
  # Set up containers for error estimates
  training = rep(0,2^4)
  validation = rep(0,2^4)
  model.size = rep(0,2^4)
  
# 2 - LOOP OVER ALL POSSIBLE MODELS, STORE ERROR ESTIMATES FOR EACH
  
  # Firstly get error estimates for the null model
  k = 1
  model = glm(train.y~1, family = binomial(logit))
  training[k] = mean((train.y-fitted(model))^2)
  validation[k] = mean((val.y-mean(train.y))^2) # <--- don't understand this
  
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
      
      val.z = cbind(1,val.x[,combs[,j]])
      
      model = glm(train.y~train.z, family = binomial(logit))
      
      val.fit = val.z %*% coef(model)
      
      # Increment k to store results to correct position in error vectors
      k = k+1
      
      # Set the model size to the current number of predictors 
      model.size[k] = i
      
      # Store the error results of the current model
      training[k] = mean((train.y-fitted(model))^2)
      validation[k] = mean((val.y-val.fit)^2)
    }
  }

  # Concat results vectors into single dataframe
  results = data.frame(index=c(1:16), model.size, training, validation)

  

  




