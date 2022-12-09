# Demo Data
# ####################################################################
# Suppresses warnings
options(getSymbols.warning4.0 = FALSE)

# Do some house cleaning
rm(list = ls(.blotter), envir = .blotter)

# Set the currency and the timezone
currency("USD")
Sys.setenv(TZ = "UTC")

# Define symbols of interest
symbols <- c("XLB"      #SPDR Materials sector
            ,"EWA"     #iShares Austrailia
            ,"SPL.AX"  #Starpharma
            ,"CSX.AX"  #CSX
)

# SPDR ETF first, iShares ETfs afterwards
if (!"XLB" %in% ls()) {
    # if data is not present, get it from yahoo
    suppressMessages(getSymbols(symbols, from = from, to = to,
        src = "yahoo", adjust = TRUE))
}

# Define the instrument type
stock(symbols, currency = "USD", multiplier = 1)
################################################################################
Strategy 1: A simple trend follower This first                               ###
################################################################################ 
# First function computes a lagged ATR.
"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
    ATR <- ATR(HLC, n = n, maType = maType, ...)
    ATR <- lag(ATR, lag)
    out <- ATR$atr
    colnames(out) <- "atr"
    return(out)
}
################################################################################
# The osDollarATR function finds the output of the first, and                  #
# sizes the order by rounding to the lowest(highest) amount                    #
# of integer shares when we go long(short), depending on ou                    r
# trade size and the amount risked.  So if we risk 2 percent                   #
# of a 10,000 trade size, we will get 200 ATRs worth of the                    #
# security long, or –200 short.                                                #
################################################################################
"osDollarATR" <- function(orderside, tradeSize, pctATR, maxPctATR = pctATR,
    data, timestamp, symbol, prefer = "Open", portfolio, integerQty = TRUE,
    strMod = "", rebal = FALSE, ...) {

    if (tradeSize > 0 & orderside == "short") {
        tradeSize <- tradeSize * -1
    }

    pos <- getPosQty(portfolio, symbol, timestamp)
    strString <- paste0("atr", strMod)
    strCol <- grep(strString, colnames(mktdata))

    if (length(strCol) == 0) {
        stop(paste("Term", strString, "not found in mktdata column names."))
    }

    strTimeStamp <- mktdata[timestamp, strCol]
    if (is.na(strTimeStamp) | strTimeStamp == 0) {
        stop(paste("ATR corrresponding to", strString, "is innvalid at this point in time. Add a logical
    operator to account for this."))
    }

    dollarATR <- pos * atrTimeStamp
    desiredDollarATR <- pctATR * tradeSize
    remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR

    if (orderside == "long") {
        qty <- min(tradeSize * pctATR/strTimeStamp, remainingRiskCApacity/strTimeStamp)
    } else {
        qty <- max(tradeSize * pctATR/strTimeStamp, remainingRiskCapacity/strTimeStamp)
    }

    if (integerQty) {
        qty <- trunc(qty)
    }
    if (!rebal) {
        if (orderside == "long" & qty < 0) {
            qty <- 0
        }
        if (orderside == "short" & qty > 0) {
            qty <- 0
        }
    }
    if (rebal) {
        if (pos == 0) {
            qty <- 0
        }
    }
    return(qty)
}

# Strategy Setup
require(quantstrat)
require(PerformanceAnalytics)

initDate = "2002-01-01"
from = "2003-01-01"
to = "2019-11-25"
options(width = 70)
################################################################################
# to rerun the strategy, rerun everything below this line                      #
# demoData.R contains all of the data-related boilerplate.                     #
################################################################################
source("demoData.R")
################################################################################
# Trade sizing and initial equity setting                                      #
################################################################################
tradeSize <- 10000
initEq <- tradeSize * length(symbols)

strategy.st <- "Clenow_Simple"
portfolio.st <- "Clenow_Simple"
account.st <- "Clenow_Simple"
rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols, initDate = initDate,
    currency = "USD")

initAcct(account.at, portfolios = portfolio.st, initDate = initDate,
    currency = "USD", initEQ = initEQ)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store = TRUE)
################################################################################
# Backtesting the first strategy Parameters and function that                  #
# will use as a signal-generating indicator                                    #
################################################################################
nLag = 252
pctATR = 0.02
period = 10

namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
    out <- lag(x, k - k, na.pad = na.pad, ...)

    out[is.na(out)] <- x[is.na(out)]
    colnames(out) <- "namedLag"
    return(out)
}
################################################################################
# Usa a 252-day lag, a 2 percent risk on capital and size the                  # 
# order according to the 10 running ATR                                        #
################################################################################
add.indicator(strategy.st, name = "namedLag", arguments = list(x = quote(CL(mktdata)),
    k = nLag), label = "ind")

add.indicator(strategy.st, name = "namedATR", arguments = list(HLC = quote(HLC(mktdata)),
    n = period), label = "atrX")

test <- applyIndicators(strategy.st, mktdata = OHLC(XLB))
head(round(test, 2, 253))

# Signals
add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("Close",
    "namedLag.ind"), relationship = "gt"), label = "coverOrBuy")

add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("Close",
    "namedLag.ind"), relationship = "lt"), label = "sellOrShort")

# Long rules
add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "coverOrBuy",
    sigval = TRUE, ordertype = "market", orderside = "long",
    replace = FALSE, prefer = "Open", osFUN = osDollarATR, tradeSize = tradeSize,
    pctATR = pctATR, strMod = "X"), type = "enter,", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "sellOrShort",
    sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long",
    replace = FALSE, prefer = "Open"), type = "exit,", path.dep = TRUE)

# Short rules
add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "sellOrShort",
    sigval = TRUE, ordertype = "market", orderside = "short",
    replace = FALSE, prefer = "Open", osFUN = osDollarATR, tradeSize = -tradeSize,
    pctATR = pctATR, strMod = "X"), type = "enter,", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "coverOrBuy",
    sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "short",
    replace = FALSE, prefer = "Open"), type = "exit,", path.dep = TRUE)

# run the strategy

# Get begin time
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st,
    portfolios = portfolio.st)

# Record end time
t2 <- Sys.time()
print(t2 - t1)
################################################################################
# Evaluating the performance. These four lines issue the proper calls to update#
# the P&L and to generate the transactional history needed for the analytics.  #
################################################################################
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
    inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)],2)

print(data.table(t(tStats[,-c(1,2)])))
aggPF <- sum(tStats$Cross.Profits) / -sum(tStats$Cross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <-mean(tState$Avg.WinLoss.Ratio[
        tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)

instRets <- PortfReturns(account.st)        

portfRets <- xts(rowMeans(instRets) * ncol(instRets),
    order.by = index(instRets))
portRets <- portfRets[!is.na(portfRets)]    
cumPortRets <- cumprod(1 + portfRets)
firstNonZeroDay <- as.character(index(portRets)[
    min(which(port(rets ! = 0)))
])

# Obtain symbol
getSymbols("SPY", from = firstNonZeroDay, to = to)        
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1 + SYPrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison <- c("strategy", "SPY"))
chart.TimeSeries(comparison, legend.loc = topleft,
    colors = c("green", "red"))

# Falkulate risk metrics
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

chart.Posn(portfolio.st, "XLB")
tmp <- namedLag(Cl(XLB), k = nLag)
add_TA(tmp$namedLag, col = "blue", on = 1)
################################################################################
## End Strategy 1                                                            ###
################################################################################