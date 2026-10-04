sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  sma_values <- numeric(length(data) - period + 1)
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  return(sma_values)
}
  # Test SMA
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  sma_result <- sma(data, period = 3)
  print(sma_result)
