# ------------------------------------------------------------------------------
# 01. opis_volume                                                            ---
# ------------------------------------------------------------------------------
################################################################################
## When should a function accept a dataframe as an argument?                 ###
## https://tinyurl.com/8f64hduu                                              ###
################################################################################
library(tidyr)
library(ggplot2)

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
# add class to stock
class(stocks) <- "stock"

# this has no class
# or could be a class not named stock
not_stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

# this is like an abstract base method
getStockPlot <- function(stocks_df) {
  UseMethod("getStockPlot")
}

# this is the implementation for "stock" objects,
# you could have more for other "class" objects
getStockPlot.stock <- function(stocks_df){
  print("Plot Stocks")
}

# this captures unsupported objects
getStockPlot.default <- function(stocks_df) {
  stop("class not supported")
}

# this calls getStockPlot.stock
getStockPlot(stocks)
#> [1] "Plot Stocks"
#this calls getStockPlot.default
getStockPlot(not_stocks)
#> Error in getStockPlot.default(not_stocks): class not supported
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version = "1.0.0"
a00.ModDate = as.Date("2022-02-01")
# ------------------------------------------------------------------------------
# 2022.02.01 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
