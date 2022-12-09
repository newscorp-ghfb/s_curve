# Get the data
SPY <- getSymbols("SPY", from = "2011-01-01", to = "2011-01-01",
  adjust = T, auto.assign = FALSE)
AAPL <- getSymbols("AAPL", from = "2011-01-01", to = "2011-01-01",
  adjust = T, auto.assign = FALSE)

# Compute price differences
x <- diff(as.numeric(SPY[, 4]))
y <- diff(as.nuneric(AAPL[, 4]))

plot(x, y, main = "Scatter plot of returns. SPY vs. AAPL", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)
abline(lm(y ~ x))
abline(lm(x ~ y), lty = 2)
grid()


# Total least squares regression
r <- prcomp(~x + Y)
slope <- r$rotation[2, 1]/r$rotation[l, 1]
intercept <- r$center[2] - slope * r$center[1]

# Show the first principal component on the plot
abline(a = intercept, b = slope, lty = 3)


# Function to calculate the spread
calculate_spread <- function(x, y, bets) {
  return(y - beta * x)
}

# Function to calculate the beta and level given start and
# end dates
calculate_beta_and_level <- function(x, y, start_date, end_date) {
  require(xts)

  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]

  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp(~dx + dy)

  beta <- r$rotation[2, 1]/r$rotation[1, 1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)

  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level

  return(outL)
}

# Function to calculate buy and sell signals with upper and
# lower threshold
calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold,
  upper_threshold) {

  buy_signals <- ifelse(spread <= level - lower_threshold,
    1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold,
    1, 0)

  # bind these vectors into a matrix
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")

  return(output)
}

# Implementation Pick an in-sample date range

start_date <- "2009-01-01"
end_date <- "2011-12-31"
x <- SPY[, 6]
y <- AAPL[, 6]

results <- calculate_beta_and_level(x, y, start_date, end_date)
results$beta
results$level

plot(results$spread, ylab = "Spread Value", main = "AAPL — beta * SPY",
  cex.maln = 0.8, cex.lab = 0.8, cex.axxs = 0.8)


# Out of sample start and end dates
start_date_out_sample <- "2012—01—01"
end_date_out_sample <- "2012—10—22"
range <- paste(start_date_out_sample, "::", end_date_out_sample,
  sep = "")

# Out of sample analysis
spread_out_of_sample <- calculate_spresd(x[range], yx[range],
  results$beta)

plot(spread_out_of_sample, main = -"AAPL - beta * SPY", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)
abline(h = results$level, Iwd = 2)


# Rolling window of trading days
window_length <- 10

# Time range
start_date <- "2011-01-01 "
end_date <- "2011-12-31"
range <- paste(start_date, "::", end_date, sep = "")

# Our stock pair
x <- SPY[range, 6]
Y <- AAPL[range, 6]
dF <- cbind(x, y)
names(dF) <- c("X", "y")

# Function that we will use to calculate betas
run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data = as.data.frame)))
}

rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression, by.column = FALSE,
    align = "right")
}

betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF)/dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <- diff(data$y/data$y - return_beta * diff(data$x/data$x))
threshold <- sd(data$spread, na.rm = TRUE)

plot(data$spread, main = "AAPL vs. SPY In-Sample", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)


# Construct the out of sample spread Keep the same 10 day
# rolling window
window_length <- 10

# Time range
start_date <- "2012-01-01"
end_date <- "2013-12-31"
range <- paste(start_date, "::", end_date, sep = "")

# Our stock pair
x <- SPY[range, 6]
Y <- AAPL[range, 6]

# Bind these together into a matrix
dF <- cbind(x, y)
names(dF) <- c("x", "y")

# Calculate the out of sample rolling beta
beta_out_of_sample <- rolling_beta(diff(dF), 10)

# Buy and sell threshold
data_out <- merge(beta_out_of_sample, dF)
data_out$spread <- data_out$y - lag(beta_out_of_sample, 1) *
  data_out$x

# Plot the spread with in—sample bands
plot(data_out$spread, main = "AAPL vs. SPY out of sample", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

# Generate sell and buy signals
buys <- ifelse(data_out$spread > threshold, 1, O)
sells <- ifelse(data_out$spread < -threshold, -1, O)
data_out$signal <- buys + sells

plot(data_out$spread, -0.8, main = "AAPL vs. SPY out of sample",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

point_type <- rep(NA, nrow(data_out))
buy_index <- which(data_out$signal == 1)
sell_index <- which(data_out$signal == -1)

point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)

num_of_buy_signals <- sum(buys, na.rm = TRUE)
num_of_buy_signals <- sum(abs(sells), na.rm = TRUE)

prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)

qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for (i in l:length(signal)) {

  if (signal[i] == 1 && position == O) {
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1
  }

  if (signal[i] == -1 && position == O) {
    # sell the spread initially
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }

  if (signal[i] == 1 && position == -1) {
    # we are short the spread and need to buy
    qty_x[i] <- -(round(beta[i] * trade_size) + prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- -1
  }

  if (signal[i] == -1 && position == 1) {
    # we are long the spread and need to sell
    qty_x[i] <- (round(beta[i] * trade_size) + prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- -2 * trade_size
    position <- -1
  }
}

# At the end of the out-of-sample period, we will still have
# a residual quantity left over for both stocks. We can
# offset this quantity by zeroing out the positions on the
# last trading day.

qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

# append these columns to the data_out table to see what's
# been accomplished
data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

# function for computing the equity curve
compute_equity_curve <- function(qty, price) {

  cash_buy <- ifelse(sign(qty) == 1, qty * price, 0)
  cash_sell <- ifelse(sign(qty) == -1, -qty * price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)

  equity <- cumulative_sell - cumulative_buy * position * price

  return(equity)
}

# Add the equity curve columns to the data_out table
data_out$equity_curve_x <- compute_equity_curve(data_out$qty_x,
  data_out$x)
data_out$equity_curve_y <- compute_equity_curve(data_out$qty_y,
  data_out$y)

plot(data_out$equity_curve_x + data_out$equity_curve_y, type = "l",
  main = "AAPL/SPYSpread", ylab = "P&L", cex.main = 0.8, cex.lab = 0.8,
  cex.axis = 0.8)

# Calculate the Sharpe ratio
sharpe_ratio <- function(x, rf) {
  sharpe <- (mean(x, na.rm = TRUE) - rf)/sd(x, na.rm = TRUE)
  return(sharpe)
}

# Calculates the maximum drawdown profile
drawdown <- function(x) {
  cummax(x) - x
}

# apply both of these to the spread
par(mfrow = c(2, 1))

equity_curve <- data_out$equity_curve_x + data_out$equity_curve_y

plot(equity_curve, main = "Equity CurveL", cex.main = 0.8, cex.lab = 0.8,
  cex.axis = 0.8)

plot(drawdown(equity_curve), main = "Drawdown of Equity CurveL",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

equity <- as.numeric(equity_curve[, 1])
equity_curve_returns <- diff(equity)/equity[-length(equity)]

# Remove any infinities an NaN
invalid_values <- is.infinite(equity_curve_returns) | is.nan(equity_curve_returns)

sharpe_ratio(equity_curve_returns[!invalid_values], 0.03)

omega_ratio <- function(r, T) {
  omega <- mean(pmax(r - T, 0))/mean(pmax(T - r, 0))
  return(omega)
}

# Find out where the trades occur
trade_dates <- data_out$qty_x[data_out$qty_x != 0]

# The trade_dates object is an xts object whose index
# contains necessary time information
duration <- as.numeric(diff(index(trade_dates)))

# Summary statistics
summary(duration)

# Histogram of trade duration
hist(duration, breaks = 20, main = "Histogram of trade durations",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
