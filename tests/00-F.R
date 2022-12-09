library(tidyquant)                                                              # Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(TTR)                                                                    # Technical Trading Rules package
# SPL Processing ---------------------------------------------------------------
SPL <- tq_get("SPL.AX")                                                         # Get SPL Stock Prices
SPL<-SPL[complete.cases(SPL),]                                                  # Delete NA
date <- SPL$date                                                                # Create date variable
# TTR Processing --------------------------------------------------------------- 
adx <- ADX(SPL[,c("high","low","close")])                                       # Welles Wilder’s Directional Movement Index
aroon <- aroon(SPL[,c("high","low")],n=20)                                      # Aroon Indicator
atr <- ATR(SPL[,c("high","low","close")],n=14)                                  # Average True Range Indicator
# Bollinger Bands Start
bb.20 <- BBands(SPL$close,20,sd=2,maType=EMA)                                   # BBands - 20 Day EMA
disp <- Delt(bb.20[,"dn"],bb.20[,"up"])                                         # Create Dispersion Column
dispDiff <-Delt(disp)                                                           # Create Daily Dispersion Difference Pct Column
# Bollinger Bands End

cmf <- CMF(SPL[,c("high","low","close")],SPL[,"volume"])                        # Chaikan Money Flow
cVol <- chaikinVolatility(SPL[,c("high","low")],20,maType=EMA)                  # Chaikan Volatility
clv <- CLV(SPL[,c("high","low","close")])                                       # Close Location Value
dc <- DonchianChannel(SPL[,c("high","low")])                                    # Donchian Channel
# emv <- EMV(SPL[,c("high","low")],SPL[,"volume"])                              # Arms’ Ease of Movement Value
gmma <-GMMA(SPL[,c("close")])                                                   # Guppy Multiple Moving Averages
macd <-MACD(SPL[,c("close")], 12, 26, 9, maType="EMA")                          # Moving Average Convergence/Divergence (MACD)
mfi <- MFI(SPL[,c("high","low","close")],SPL[,"volume"])                        # Money Flow Index
# Momentum Function
cci <- CCI(SPL[,c("high","low","close")])                                       # Commodity Channel Index
cmo <- CMO(SPL[,c("close")])                                                    # Chande Momentum Oscillator
mom <- momentum(SPL[,c("close")])                                               # Momentum Indicator
momDPOprice <- DPO(SPL$close)                                                   # De-Trended Price Oscillator Momentum Indicator
momDPOvolume <- DPO(SPL$volume)                                                 # De-Trended Volume Oscillator Momentum Indicator
roc <- ROC(SPL[,c("close")])                                                    # Rate of Change Indicator
rsi <- RSI(SPL[,c("close")], 14, maType="WMA",SPL[,"volume"])                   # Relative Strength Indicator - 14 days Weighted Moving Average
stoch <- stoch(SPL[,c("high","low","close")])                                   # Stochastic Oscillator / Stochastic Momentum Index
wpr <- WPR(SPL[,c("high","low","close")], 14)                                   # William's %R Indicator

obv <- OBV(SPL$close, SPL$volume )                                              # On Balance Volume
pbands <- PBands(SPL[,c("close")])                                              # PBands
rpr <- runPercentRank(SPL[,c("close")], 260, FALSE, 0.5)                        # Run Percent Rank
runsd <- runSD(SPL[,c("close")], 10, TRUE, FALSE)                               # Running Standard Deviation
runWilderSum <- wilderSum(SPL[,c("close")], 10)                                 # retuns a Welles Wilder style weighted sum over a n-period moving window
tdi <- TDI(SPL$close, n=30)                                                     # Trend Detection Index
ult.osc <- ultimateOscillator(SPL[,c("high","low","close")])                    # Ultimate Oscillator
vhf.close <- VHF(SPL$close)                                                     # Vertical Horizontal Filter
vhf.hilow <- VHF(SPL[,c("high","low","close")])                                 # Vertical Horizontal Filter Hi / Low
# Volatility Tables
ohlc <- SPL[,c("open", "high","low","close")]                                   # Volatility OHLC parameter
tblVolatilityClose <- volatility(ohlc, calc="close")                            # Volatility
# Williams Indicators
wad <- williamsAD(SPL[,c("high","low","close")])                                # William's Williams Accumulation / Distribution 
# Moving Averages
alma.20 <- ALMA(SPL$close, 20)                                                  # Arnaud Legoux moving average - ALMA inspired by Gaussian filters. Tends to put less weight on most recent observations, reducing tendency to overshoot.
dema.20 <- DEMA(SPL$close, 20)                                                  # Double Exponential Moving Average
ema.20 <-EMA(SPL$close, 20)                                                     # Exponential Moving Average - EMA calculates an exponentially-weighted mean, giving more weight to recent observations
evwma.20 <-EVWMA(SPL$close, SPL$volume, 20)                                     # Elastic Volume-weighted Moving Average
hma.20 <- HMA(SPL$close, 20)                                                    # Hull moving average.
sma.20 <-SMA(SPL$close, 20)                                                     # Simple Moving Average - SMA calculates the arithmetic mean of the series over the past n observations.
#vma <- VMA(SPL$close, w, ratio = 1, ...)     
trix <- TRIX(SPL$close)                                                         # Triple Smoothed Exponential Oscillator
vwas.20 <- VWAP(SPL$close, SPL$volume, 20)                                      # Volume-weighted moving average - VWAP calculate the volume-weighted moving average price.
wma.20 <- WMA(SPL$close, 20)                                                    # Weighted Moving Average - MA is similar to an EMA, but with linear weighting if the length of wts is equal to n. If the length of wts is equal to the length of x, the WMA will use the values of wts as weights.       
zlema.20 <- ZLEMA(SPL$close, 20)                                                # ZLEMA - 20 Day; ZLEMA is similar to an EMA, as it gives more weight to recent observations, but attempts to remove lag by subtracting data prior to (n-1)/2 periods (default) to minimize the cumulative effect.
# Create Table -----------------------------------------------------------------
tblADX <- data.frame(date,"spl",adx)                                            # Create Welles Wilder's ADX Table
tblAROON <- data.frame(date,"spl",aroon)                                        # Create Aroon Indicator Table
tblATR14 <- data.frame(date,"spl",atr)                                          # Create Average True Range Indicator Table
tblBBands20 <- data.frame(date,"SPL", bb.20, disp, dispDiff)                    # Create BBands Table
tblChaikinMF <- data.frame(date,"SPL",cmf)                                      # Create Chaikan Money Flow A/D Table
tblCLV <- data.frame(date,"spl",clv)                                            # Create Close Location Value Table
tblDC <- data.frame(date,"spl",dc)                                              # Create Donchian Channel Table
# tblEMV <- data.frame(date,"spl",emv)                                          # Create Arms’ Ease of Movement Table
tblGMMA <- data.frame(date,"spl",gmma)                                          # Create Guppy Multiple Moving Averages Table
tblMACD <- data.frame(date,"spl",macd)                                          # Create Moving Average Convergence/Divergence (MACD) Table
tblMFI <- data.frame(date,"spl",mfi)                                            # Create Money Flow Index Table

# Momentum Tables
tblMOM <- data.frame(date,"spl",mom)                                            # Create Momentum Table
tblMOM_CCI <- data.frame(date,"spl",cci)                                        # Create Commodity Channel Index Table
tblMOM_CMO <- data.frame(date,"spl",cmo)                                        # Create Chande Momentum Oscillator Table
tblMOM_DPOprice <- data.frame(date,"spl",momDPOprice)                           # Create DeTrended Price Oscillator Table
tblMOM_DPOvolume <- data.frame(date,"spl",momDPOvolume)                         # Create DeTrended Volume Oscillator Table
tblMOM_ROC <- data.frame(date,"spl",roc)                                        # Create Rate of Change Table
tblMOM_RSI <- data.frame(date,"spl",rsi)                                        # Create Relative Strength Indicator Table
tblMOM_stoch <- data.frame(date,"spl",stoch)                                    # Create Stochastic Oscillator / Stochastic Momentum Index Table
tblMOM_WPR <- data.frame(date,"spl",wpr)                                        # Create William's %R Indicator Table

tblOBV <- data.frame(date,"SPL",obv)                                            # Create OBV Table
tblPBands <- data.frame(date,"spl",pbands)                                      # Create PBands Table
tblPrice <- data.frame(SPL,"spl", date + 1)                                     # Create Stock Price Table
tblRPR <- data.frame(date,"spl",rpr)                                            # Create Run Percent Rank Table
tblRunSD <- data.frame(date,"spl",SPL$close, runsd)                             # Create Running Standard Deviation Table

# Trend Tables
tblTrend_TDI <- data.frame(date,"spl",tdi)                                      # Create Trend Detection Index Table
tblTrend_VHF <- data.frame(date,"spl",vhf.close)                                # Create Vertical Horizontal Filter (Close) Table
tblTrend_VHF_HiLo <- data.frame(date,"spl",vhf.hilow)                           # Create Vertical Horizontal Filter (High / Low) Table
tblUltOsc <-data.frame(date,"spl",ult.osc)                                      # Create Ultimate Oscillator Table
# Volatility Tables
tblVolatilityChaikin <- data.frame(date,"SPL",cVol)                             # Create Chaikan Volatility Table
tblVolatilityClose <- data.frame(date, "spl", tblVolatilityClose)               # CreateVolatility (Close) Table

tblWilderSum <- data.frame(date,"spl",runWilderSum)                             # Create Welles Wilder style weighted sum Table
tblWAD <- data.frame(date,"spl",wad)                                            # Create Williams Accumulation / Distribution Table

# Moving Average Tables
tblMA_ALMA20 <- data.frame("spl", alma.20)                                      # Create Arnaud Legoux moving average table
tblMA_DEMA20 <- data.frame(date,"spl", dema.20)                                 # Create Double Exponential Moving Average table
tblMA_EMA20 <- data.frame(date,"spl", ema.20)                                   # Create Exponential Moving Average table
tblMA_EVWMA20 <- data.frame(date,"spl", evwma.20)                               # Create # Elastic Volume-weighted Moving Average table
tblMA_HMA20 <- data.frame(date,"spl", hma.20)                                   # Create Hull moving average table
tblMA_SMA20 <- data.frame(date,"spl", sma.20)                                   # Create Simple Moving Average table
#vma <- 
tblMA_TRIX <- data.frame(date,"spl",trix)                                       # Create Triple Smoothed Exponential Oscillator Moving Average Table
tblMA_VWAS20 <- data.frame(date,"spl", vwas.20)                                 # Create Volume-weighted moving average table
tblMA_WMA20 <- data.frame(date,"spl", wma.20)                                   # Create # Weighted Moving Average table
tblMA_ZLEMA20 <- data.frame(date,"SPL", zlema.20)                               # Create ZLEMA Table
# Delete Table NA --------------------------------------------------------------
tblADX<-tblADX[complete.cases(tblADX),]                                         # Delete tblADX NA
tblAROON<-tblAROON[complete.cases(tblAROON),]                                   # Delete tblAROON NA
tblATR14<-tblATR14[complete.cases(tblATR14),]                                   # Delete tblATR14 NA
tblBBands20<-tblBBands20[complete.cases(tblBBands20),]                          # Delete BBands20 NA
tblChaikinMF<-tblChaikinMF[complete.cases(tblChaikinMF),]                       # Delete tblChaikinMF NA
tblCLV<-tblCLV[complete.cases(tblCLV),]                                         # Delete tblCLV NA
tblDC<-tblDC[complete.cases(tblDC),]                                            # Delete tblDC NA
# tblEMV<-tblEMV[complete.cases(tblEMV),]                                       # Delete tblEMV NA
tblGMMA<-tblGMMA[complete.cases(tblGMMA),]                                      # Delete tblGMMA NA
tblMACD<-tblMACD[complete.cases(tblMACD),]                                      # Delete tblMACD NA
tblMFI<-tblMFI[complete.cases(tblMFI),]                                         # Delete tblMFI NA
# Momentum Tables
tblMOM<-tblMOM[complete.cases(tblMOM),]                                         # Delete tblATR14 NA's
tblMOM_CCI<-tblMOM_CCI[complete.cases(tblMOM_CCI),]                             # Delete tblMOM_CCI NA's
tblMOM_CMO<-tblMOM_CMO[complete.cases(tblMOM_CMO),]                             # Delete tblMOM_CMO NA's
tblMOM_DPOprice<-tblMOM_DPOprice[complete.cases(tblMOM_DPOprice),]              # Delete tblMOM_DPOprice NA's
tblMOM_DPOvolume<-tblMOM_DPOvolume[complete.cases(tblMOM_DPOvolume),]           # Delete tblMOM_DPOprice NA's
tblMOM_ROC<-tblMOM_ROC[complete.cases(tblMOM_ROC),]                             # Delete tblMOM_ROC NA's
tblMOM_RSI<-tblMOM_RSI[complete.cases(tblMOM_RSI),]                             # Delete tblMOM_RSI NA's
tblMOM_stoch<-tblMOM_stoch[complete.cases(tblMOM_stoch),]                       # Delete Stochastic Oscillator / Stochastic Momentum Index NA
tblMOM_WPR<-tblMOM_WPR[complete.cases(tblMOM_WPR),]                             # Delete William's %R Indicator NA

tblOBV<-tblOBV[complete.cases(tblOBV),]                                         # Delete OBV NA
tblPBands<-tblPBands[complete.cases(tblPBands),]                                # Delete tblPBands NA
tblRPR<-tblRPR[complete.cases(tblRPR),]                                         # Delete tblRPR NA
tblRunSD<-tblRunSD[complete.cases(tblRunSD),]                                   # Delete tblRunSD NA
# Trend Tables
tblTrend_TDI<-tblTrend_TDI[complete.cases(tblTrend_TDI),]                       # Delete Trend Detection Index NA
tblTrend_VHF<-tblTrend_VHF[complete.cases(tblTrend_VHF),]                       # Delete Vertical Horizontal Filter (Close) NA
tblTrend_VHF_HiLo<-tblTrend_VHF_HiLo[complete.cases(tblTrend_VHF_HiLo),]        # Delete Vertical Horizontal Filter (High / Low) NA
# Volatility Tables
tblVolatilityChaikin<-tblVolatilityChaikin[complete.cases(tblVolatilityChaikin),]                    # Delete tblChaikinVol NA
tblVolatilityClose <-tblVolatilityClose[complete.cases(tblVolatilityClose),]    # Delete Volatility NA's

tblUltOsc<-tblUltOsc[complete.cases(tblUltOsc),]                                # Delete Ultimate Oscillator NA
tblWilderSum<-tblWilderSum[complete.cases(tblWilderSum),]                       # Delete tblWilderSum NA
tblWAD<-tblWAD[complete.cases(tblWAD),]                                         # Delete William's Accumulation / Distribution NA's

# Moving Average Tables
tblMA_ALMA20 <- tblMA_ALMA20[complete.cases(tblMA_ALMA20),]                     # Delete Arnaud Legoux moving average NA
tblMA_DEMA20 <- tblMA_DEMA20[complete.cases(tblMA_DEMA20),]                     # Delete Double Exponential Moving Average NA
tblMA_EMA20 <- tblMA_EMA20[complete.cases(tblMA_EMA20),]                        # Delete Exponential Moving Average NA
tblMA_EVWMA20 <- tblMA_EVWMA20[complete.cases(tblMA_EVWMA20),]                  # Delete # Elastic Volume-weighted Moving Average NA
tblMA_HMA20 <- tblMA_HMA20[complete.cases(tblMA_HMA20),]                        # Delete Hull moving average NA
tblMA_SMA20 <- tblMA_SMA20[complete.cases(tblMA_SMA20),]                        # Delete Simple Moving Average NA
#vma <- 
tblMA_TRIX <- tblMA_TRIX[complete.cases(tblMA_TRIX),]                           # Delete Triple Smoothed Exponential Oscillator NA
tblMA_VWAS20 <- tblMA_VWAS20[complete.cases(tblMA_VWAS20),]                     # Delete Volume-weighted moving average NA
tblMA_WMA20 <- tblMA_WMA20[complete.cases(tblMA_WMA20),]                        # Delete # Weighted Moving Average NA
tblMA_ZLEMA20<-tblMA_ZLEMA20[complete.cases(tblMA_ZLEMA20),]                    # Delete ZLEMA NA
# Rename Table columns ---------------------------------------------------------
colnames(tblADX) <- c("date", "symbol", "DIp", "DIn","DX","ADX")                # Rename tblADX Table Columns
colnames(tblAROON) <- c("date", "symbol", "aroonUp", "aroonDn", "oscillator"  ) # Rename tblAROON Table Columns
colnames(tblATR14) <- c("date", "symbol", "tr", "atr", "trueHigh", "trueLow" )  # Rename tblATR14 Table Columns
colnames(tblBBands20) <- c("date", "symbol", "dn", "mavg", "up", "pctB", 
                           "bbDisp", "bbDispDiff")                                                       # Rename BBands Table Columns
colnames(tblChaikinMF) <- c("date", "symbol", "cmf")                            # Rename ChaikinMF Table Columns
colnames(tblCLV) <- c("date", "symbol", "clv" )                                 # Rename tblCLV Table Columns
colnames(tblDC) <- c("date", "symbol", "high", "mid", "low" )                   # Rename tblDC Table Columns
# colnames(tblEMV) <- c("date", "symbol", "emv", "emvMA")                       # Rename tblEMV Table Columns
colnames(tblGMMA) <- c("date", "symbol", "short.lag.3", "short.lag.5", 
                       "short.lag.8", "short.lag.10", "short.lag.12", "short.lag.15", "long.lag.30", 
                       "long.lag.35", "long.lag.40", "long.lag.45", "long.lag.50", "long.lag.60"  )  # Rename tblGMMA Table Columns
colnames(tblMACD) <- c("date", "symbol","macd", "signal"  )                     # Rename tblMACD Table Columns
colnames(tblMFI) <- c("date", "symbol", "mfi" )                                 # Rename tblMFI Table Columns
# Momentum Tables
colnames(tblMOM) <- c("date", "symbol", "mom")                                  # Rename tblMOM Table Columns
colnames(tblMOM_CCI) <- c("date", "symbol", "cci" )                             # Rename tblMOM_CCI Table Columns
colnames(tblMOM_CMO) <- c("date", "symbol", "cmo" )                             # Rename tblMOM_CMO Table Columns
colnames(tblMOM_DPOprice) <- c("date", "symbol", "DPOprice" )                   # Rename tblMOM_DPOprice Table Columns
colnames(tblMOM_DPOvolume) <- c("date", "symbol", "DPOvolume" )                 # Rename tblMOM_DPOprice Table Columns
colnames(tblMOM_ROC) <- c("date", "symbol", "roc" )                             # Rename tblMOM_ROC Table Columns
colnames(tblMOM_RSI) <- c("date", "symbol", "rsi" )                             # Rename tblMOM_RSI Table Columns
colnames(tblMOM_stoch) <- c("date", "symbol", "fastK", "fastD", "slowD")        # Rename tblMOM_Stoch Table Columns
colnames(tblMOM_WPR) <- c("date", "symbol", "wpr" )                                 # Rename tblWPR Table Columns

colnames(tblOBV) <- c("date", "symbol", "obv")                                  # Rename OBV Table Columns
colnames(tblPBands) <- c("date", "symbol", "dn", "center", "up" )               # Rename tblPBands Table Columns
colnames(tblPrice) <- c("date_1", "open", "high", "low", "close", "volume", 
                        "adjusted", "symbol", "date" )                                                # Rename tblPBands Table Columns


colnames(tblRPR) <- c("date", "symbol", "rpr" )                                 # Rename tblRPR Table Columns
colnames(tblRunSD) <- c("date", "symbol", "close", "runsd" )                    # Rename tblRunSD Table Columns
# Trend Tables
colnames(tblTrend_TDI) <- c("date", "symbol", "tdi", "di" )                     # Rename tblTrend_TCI Table Columns
colnames(tblTrend_VHF) <- c("date", "symbol", "vhfClose")                       # Rename tblTrend_TCI Table Columns
colnames(tblTrend_VHF_HiLo) <- c("date", "symbol", "vhfHiLo")                   # Rename tblTrend_TCI Table Columns
# Volatility Tables
colnames(tblVolatilityChaikin) <- c("date", "symbol", "volatilityChaikin")      # Rename ChaikinVol Table Columns
colnames(tblVolatilityClose) <- c("date", "symbol", "volatilityClose")          # Rename tblVolatility_Close Table Columns

colnames(tblUltOsc) <- c("date", "symbol", "ultOsc")                            # Rename tblTrend_TCI Table Columns  
colnames(tblWilderSum) <- c("date", "symbol", "runWilderSum")                   # Rename tblWilderSum Table Columns
colnames(tblWAD) <- c("date", "symbol", "williamsAD" )                          # Rename tblWPR Table Columns

# Moving Average Tables
colnames(tblMA_ALMA20) <- c("symbol", "alma20")                                 # Rename Arnaud Legoux moving average columns
colnames(tblMA_DEMA20) <- c("date", "symbol", "dema20")                         # Rename Double Exponential Moving Average columns
colnames(tblMA_EMA20) <- c("date", "symbol", "ema20")                           # Create Exponential Moving Average table
colnames(tblMA_EVWMA20) <- c("date", "symbol", "evma20")                        # Create # Elastic Volume-weighted Moving Average table
colnames(tblMA_HMA20) <- c("date", "symbol", "hma20")                           # Create Hull moving average table
colnames(tblMA_SMA20) <- c("date", "symbol", "sma20")                           # Create Simple Moving Average table
#vma <- 
colnames(tblMA_TRIX) <- c("date", "symbol", "trix", "signal")                   # Create Triple Smoothed Exponential Oscillator table
colnames(tblMA_VWAS20) <- c("date", "symbol", "vwas20")                         # Create Volume-weighted moving average table
colnames(tblMA_WMA20) <- c("date", "symbol", "wma20")                           # Create # Weighted Moving Average table
colnames(tblMA_ZLEMA20) <- c("date", "symbol", "zlema20")                       # Rename ZLEMA Table Columns
# Indexing  --------------------------------------------------------------------
tblPrice <- tibble::rowid_to_column(tblPrice, "Index")                          # Add Index to tblPrice
# Reorder  ---------------------------------------------------------------------
tblPrice <- tblPrice[, c(1, 10, 2, 9, 8, 3, 4, 5, 6, 7)]                        # Reorder tblPrice columns

# Output -----------------------------------------------------------------------
# tblADX
# tblAROON
# tblATR14
# tblBBands20
# tblChaikinMF
# tblVolatilityChaikin
# tblCLV
# tblDC
# tblEMV
# tblGMMA
# tblMACD
# tblMFI
# tblMOM
# tblMOM_CCI
# tblMOM_CMO
# tblMOM_DPOprice
# tblMOM_DPOvolume
# tblMOM_ROC
# tblMOM_RSI
# tblMOM_stoch
# tblMOM_WPR
# tblOBV
# tblPBands
# tblPrice
# tblRPR
# tblRunSD
# tblTrend_TDI
# tblUltOsc
# tblWilderSum
# tblWAD
# tblMA_ALMA20 
# tblMA_DEMA20
# tblMA_EMA20
# tblMA_EVWMA20
# tblMA_HMA20
# tblMA_SMA20
# tblMA_TRIX
# tblUltOsc
# tblMA_VWAS20
# tblMA_WMA20
# tblMA_ZLEMA20