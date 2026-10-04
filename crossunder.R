crossunder <- function(data1, data2) {
  
  # Check that both vectors have the same length
  if (length(data1) != length(data2)) {
    stop("data1 and data2 must have the same length")
  }
  
  # Initialize crossunder results
  result <- rep(FALSE, length(data1))
  
  # Check for crossunder
  for (i in 2:length(data1)) {
    if (data1[i - 1] >= data2[i - 1] &&
        data1[i] < data2[i]) {
      result[i] <- TRUE
    }
  }
  
  return(result)
}

# Test Crossunder
data1 <- c(5, 4, 3, 2, 1)
data2 <- c(4, 4, 4, 3, 2)

crossunder_result <- crossunder(data1, data2)
print(crossunder_result)