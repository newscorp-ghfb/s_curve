library(quantmod)                                                               # Load quantmod
library(tidyquant)                                                              # Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(TTR)                                                                    # Technical Trading Rules package
# SPL Processing -----------------------------------------------------------------------------------
SPL <- tq_get("SPL.AX")                                                         # Get SPL Stock Prices
SPL<-SPL[complete.cases(SPL),]                                                  # Delete NA
date_1 <- SPL$date                                                              # Create date variable
date <- date_1 + 1
key<-paste(as.character(format(date, "%Y")), as.character(format(date, "%m")),  # Create Key
           as.character(format(date, "%d")), "spl", sep = "")
# Create Table -------------------------------------------------------------------------------------
tblPrice <- data.frame(key,SPL,"spl", date)                                     # Create Stock Price Table
# Delete Table NA ----------------------------------------------------------------------------------
tblPrice<-tblPrice[complete.cases(tblPrice),]                                   # Delete tblPrice NA's
# Rename Table columns -----------------------------------------------------------------------------
colnames(tblPrice) <- c("key","date-1", "open", "high", "low", "close", "volume", 
                        "adjusted", "symbol", "date" )                                              # Rename tblPBands Table Columns
# Indexing  ----------------------------------------------------------------------------------------
tblPrice <- tibble::rowid_to_column(tblPrice, "Index")                          # Add Index to tblPrice
# Reorder  -----------------------------------------------------------------------------------------
tblPrice <- tblPrice[, c(2, 1, 11, 3, 10, 4, 5, 6, 7, 9, 8)]                    # Reorder tblPrice columns
# Create Simulation Matrix  -----------------------------------------------------------------------------------
mtxDeltPrice <- data.frame(Delt(tblPrice$adjusted, k=1:200))                    # Create Adjusted Price Matrix
mtxDeltPrice[is.infinite(mtxDeltPrice) | is.na(mtxDeltPrice)] <- 0              # Check for inf or na's and convert to 0
rownames(mtxDeltPrice) <- rownames(mtxDeltPrice, do.NULL = FALSE, prefix = "")  # Add Row Name - Index Value 
colnames(mtxDeltPrice) <- c(1:200)                                              # Add Column Name Value 1 - 200
# Create Simulation Data Frame  -----------------------------------------------------------------------------
tblDeltPrice <- data.frame(mtxDeltPrice)
sprintf("x%03d", tblDeltPrice")
# Top / Bottom n Values  ------------------------------------------------------------------------------------
tblDeltPriceBotN <- data.frame(head(sort(mtxDeltPrice), 50))                    # Bottom N Prices
tblDeltPriceBotIdx<- data.frame(which(mtxDeltPrice<=sort(mtxDeltPrice,          # Bottom N Prices Index 
                                                         decreasing = F)[50], arr.ind = T))
tblDeltPriceTopN <- data.frame(head(sort(mtxDeltPrice,decreasing = TRUE), 50))  # Top N Prices
tblDeltPriceTopIdx<- data.frame(which(mtxDeltPrice>=sort(mtxDeltPrice,          # Top N Prices Index
                                                         decreasing = T)[50], arr.ind = T))
