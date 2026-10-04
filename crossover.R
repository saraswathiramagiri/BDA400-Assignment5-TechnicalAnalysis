crossover <- function(data1, data2) {
  
  # Check that both vectors have the same length
  if (length(data1) != length(data2)) {
    stop("data1 and data2 must have the same length")
  }
  
  # Initialize crossover results
  result <- rep(FALSE, length(data1))
  
  # Check for crossover
  for (i in 2:length(data1)) {
    if (data1[i - 1] <= data2[i - 1] &&
        data1[i] > data2[i]) {
      result[i] <- TRUE
    }
  }
  
  return(result)
}

# Test Crossover
data1 <- c(1, 2, 3, 4, 5)
data2 <- c(2, 2, 2, 3, 4)

crossover_result <- crossover(data1, data2)
print(crossover_result)