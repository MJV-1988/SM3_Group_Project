
loadData = function(file = "train.csv", N = 1000) {
  #loads the data in file (e.g. "data.csv"), with the response variable in the last column
  #Returns validation data set of size N, train data set with the rest of the data
  
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