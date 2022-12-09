install.packages('caTools')
library(caTools)                                                                # Contains several basic utility functions including: moving (rolling, running) window statistic functions,
library(tidyquant)                                                              # Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(TTR)                                                                    # Technical Trading Rules package
# Date Processing --------------------------------------------------------------
#end <- ymd("2017-10-30")
end <-  today()
from <- today() - years(10)
start <- end - weeks(52)
# SPL Processing ---------------------------------------------------------------
SPL <- tq_get("SPL.AX")                                                         # Get SPL Stock Prices
SPL<-SPL[complete.cases(SPL),]                                                  # Delete NA
date <- SPL$date                                                                # Create date variable
# Example 1: Annual Returns ----------------------------------------------------                                                                                
tblRetAnnual <- SPL %>%
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic")
# Example 2: Use tq_mutate_xy to use functions with two columns required--------
# SPL %>%
#  tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA,
#               col_rename = "EVWMA")

# Example 2B: Getting Daily Log Returns-----------------------------------------
tblRetDailyLog <- SPL %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily.returns")
# Example 3: MACD to Visualize Moving Average Convergence Divergence------------
tblMACD <- SPL %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = SMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(open:volume))
# Example 4: Max and Min Price for Each Quarter---------------------------------
tblRetMaxByQtr <- SPL %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = max, 
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)

tblRetMinByQtr <- SPL %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = min, 
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)


tblRetByQtr <- left_join(tblRetMaxByQtr, tblRetMinByQtr,
                         by = c("year.qtr" = "year.qtr"))
# Example 5: Chaikan AD --------------------------------------------------------
# tblchaikanAD <- SPL %>%
#   tq_transmute(
#                mutate_fun = chaikinAD, 
#                HLC = high:low:close,
#                volume = volume
#                )
# Delete Table NA --------------------------------------------------------------  
tblMACD <-tblMACD[complete.cases(tblMACD),]                                      # Delete MACD NA  
# Reformat Table ---------------------------------------------------------------
# tblchaikanAD
tblMACD <- data.frame("spl",tblMACD)                                            # Create MACD Table
tblRetAnnual <- data.frame("spl",tblRetAnnual)                                  # Create Annual Returns Table
tblRetDailyLog <- data.frame("spl",tblRetDailyLog)                              # Create Daily Log Returns Table
tblRetMaxByQtr <- data.frame("spl",tblRetMaxByQtr)                              # Create Max Returns by Quarter Table
tblRetMinByQtr <- data.frame("spl",tblRetMinByQtr)                              # Create Min Returns by Quarter Table
tblRetByQtr <- data.frame("spl",tblRetByQtr)                                    # Create Min Returns by Quarter Table
# Rename columns ---------------------------------------------------------------
colnames(tblMACD) <- c( "symbol", "date", "adjusted", "macd", "signal","diff")  # Rename tblMACD Table Columns
colnames(tblRetAnnual) <- c( "symbol", "date", "retAnnual")                     # Rename tblRetAnnual Table Columns
colnames(tblRetDailyLog) <- c( "symbol", "date", "retDaily")                    # Rename tblDailyLogReturns Table Columns
colnames(tblRetMaxByQtr) <- c( "symbol", "close", "dateYrQtr")                  # Rename tblRetMaxByQtr Table Columns
colnames(tblRetMinByQtr) <- c( "symbol", "close", "dateYrQtr")                  # Rename tblRetMinByQtr Table Columns
colnames(tblRetByQtr) <- c( "symbol", "closeMax", "dateYrQtr", "closeMin")      # Rename tblRetByQtr Table Columns
# Reorder  columns -------------------------------------------------------------
tblMACD <- tblMACD[, c(2, 1, 3, 4, 5, 6)]                                       # Reorder tblMACD columns
tblRetAnnual <- tblRetAnnual[, c(2, 1, 3)]                                      # Reorder tblRetAnnual columns
tblRetDailyLog <- tblRetDailyLog[, c(2, 1, 3)]                                  # Reorder tblDailyLogReturns columns
tblRetMaxByQtr <- tblRetMaxByQtr[, c(3, 1, 2)]                                  # Reorder tblRetMaxByQtr columns
tblRetMinByQtr <- tblRetMinByQtr[, c(3, 1, 2)]                                  # Reorder tblRetMinByQtr columns
tblRetByQtr <- tblRetByQtr[, c(3, 1, 2, 4)]                                     # Reorder tblRetByQtr columns