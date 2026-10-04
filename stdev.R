stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from the mean
  diff_values <- data - mean_value
  
  # Calculate squared differences
  squared_diff <- diff_values^2
  
  # Calculate variance
  variance <- sum(squared_diff) / length(squared_diff)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Test Standard Deviation
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
stdev_result <- stdev(data)
print(stdev_result)