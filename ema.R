ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]
  for (i in 2:length(data)) {
    ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
  }
  return(ema_values)
}
# Test EMA
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)