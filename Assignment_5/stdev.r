stdev <- function(data) {
  # Calculate the mean of the data
  n <- length(data)
  mean_value <- sum(data) / n
  
  # Calculate the differences between the data points and the mean
  diff_values <- numeric(n)
  for (i in 1:n) {
    diff_values[i] <- data[i] - mean_value
  }
  
  # Calculate the squared differences
  squared_diff <- numeric(n)
  for (i in 1:n) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }
  
  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / n
  
  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}