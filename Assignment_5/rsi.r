rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  n <- length(data)
  diff_values <- numeric(n - 1)
  for (i in 1:(n - 1)) {
    diff_values[i] <- data[i + 1] - data[i]
  }
  
  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA, n)
  
  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- ((avg_gain * (period - 1)) + gains[i - 1]) / period
    avg_loss <- ((avg_loss * (period - 1)) + losses[i - 1]) / period
    
    if (avg_loss == 0) {
      rs <- Inf
    } else {
      rs <- avg_gain / avg_loss
    }
    
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}