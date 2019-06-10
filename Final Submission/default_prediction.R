
##########     CREDIT CARD DEFAULT PREDICTION      ###########

# Function to read in dataset and make predictions for 
# whether a borrower is going to default in the next month 
default_prediction = function(file) {
  
  # Read in the headers
  headers <- read.csv(file, skip=1, header=F, nrows=1, as.is=T)
  
  # Read in complete dataset
  dat <- read.csv(file = "train.csv", skip = 2, header = F)
  
  # Add the headers to dataset
  colnames(dat) <- headers
  
  # Remove the ID field
  dat <- dat[ ,-1]
  
  # Rename response variable for brevity
  names(dat)[ncol(dat)]<-"y"
  
  # Convert the necessary columns to factors
  dat$SEX = as.factor(dat$SEX)
  dat$EDUCATION = as.factor(dat$EDUCATION)
  dat$MARRIAGE = as.factor(dat$MARRIAGE)
  dat$PAY_0 = as.factor(dat$PAY_0)
  dat$PAY_2 = as.factor(dat$PAY_2)
  dat$PAY_3 = as.factor(dat$PAY_3)
  dat$PAY_4 = as.factor(dat$PAY_4)
  dat$PAY_5 = as.factor(dat$PAY_5)
  dat$PAY_6 = as.factor(dat$PAY_6)
  dat$y = as.factor(dat$y)
  
  # Read in the final model
  mdl_final <- readRDS("./final_model.rds")
  
  # Obtain the predicted classes
  predictions <- predict(mdl_final,dat)
  
  return(predictions)
}


