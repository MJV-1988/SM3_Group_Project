# Accepts a column of observations of a variable and a 
# significance level alpha and retursn the lower bound
# of the mean's (1-alpha)*100% confidence interval
lowerCI <- function(data, alpha = 0.05){
  # Calulate the mean of the data
  mean <- data %>%
    mean(na.rm = TRUE)
  
  # Calculate the sd of the data
  stdDev <- data %>%
    sd(na.rm = TRUE)
  
  # Calulate the population sd of the data
  popSD <- data %>%
    length() %>%
    sqrt()
  
  popSD <- stdDev/popSD
  
  # One Tailed significance
  alpha <- alpha
  
  # Gives the cut off point from the t-distribution for
  # a given area, lower the degrees for freedom by one
  # a set lower.tail so P[X>x] is calculated
  tCutOff <- qt(alpha, df = length(data)-1, lower.tail = FALSE)
  
  # Return Lower Bound
  return(mean - tCutOff * popSD)
}

# Accepts a column of observations of a variable and a 
# significance level alpha and retursn the upper bound
# of the mean's (1-alpha)*100% confidence interval
upperCI <- function(data, alpha = 0.05){
  # Calulate the mean of the data
  mean <- data %>%
    mean(na.rm = TRUE)
  
  # Calculate the sd of the data
  stdDev <- data %>%
    sd(na.rm = TRUE)
  
  # Calulate the population sd of the data
  popSD <- data %>%
    length() %>%
    sqrt()
  
  popSD <- stdDev/popSD
  
  # One Tailed significance
  alpha <- alpha
  
  # Gives the cut off point from the t-distribution for
  # a given area, lower the degrees for freedom by one
  # a set lower.tail so P[X>x] is calculated
  tCutOff <- qt(alpha, df = length(data)-1, lower.tail = FALSE)
  
  # Return Lower Bound
  return(mean + tCutOff * popSD)
}

# Accepts a column of observations and a significance level alpha,
# returns the (1-alpha)*100% confidence interval for the variable's mean
# as a vector in the form [lwr, mean, upper]
CI <- function(data, alpha = 0.05){
  return (c(lowerCI(data, alpha/2), mean(data), upperCI(data, alpha/2)))
}
