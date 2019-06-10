
# Test file for binary classification model

#setwd("E:/Documents/GitHub/SM3_Group_Project/Final Submission")

source('default_prediction.R')
library(tidyverse)
library(gridExtra)
library(caret)
library(MASS)
library(kableExtra)
library(broom)
library(reshape2)
library(modelr)
library(mlbench)
library(Hmisc)
library(randomForest)
library(leaps)
library(e1071)
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

# Set y as a factor
dat$y = as.factor(dat$y)

# Get the predictions
predictions <- default_prediction("train.csv")

cm <- confusionMatrix(predictions, dat$y)
accuracy <- cm$overall[1]
accuracy
