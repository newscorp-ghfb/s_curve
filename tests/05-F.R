install.packages('caTools')
install.packages('dplyr')
install.packages('ggplot2')
install.packages("ggthemes") # Install 
install.packages('plotly')
install.packages('reshape')
install.packages("reshape2")
install.packages("tibble")
# ############################################################################
library(caTools)
library(dplyr)
library(ggplot2) 
library(ggthemes) # Load
library(reshape)
library(quantmod)                                                               # Load quantmod
library(tibble)
library(tidyquant)                                                              # Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(TTR)   
# ############################################################################
foo <-
  matrix(rnorm(80),nrow=20,dimnames=list(letters[1:20],LETTERS[23:26]))
apply(foo,2,function(xx)tail(sort(xx),2))
apply(foo,2,function(xx)tail(names(sort(xx)),2))
# ############################################################################
m <- data.matrix(Delt(tblPrice$close, k=1:200))
m[is.infinite(m) | is.na(m)] <- 0
rownames(m) <- rownames(m, do.NULL = FALSE, prefix = "")
colnames(m) <- c(1:200)
sprintf("%03s", tblDeltPrice)

mm <- apply(m,2,function(xx)tail(sort(xx),2))
mmm <- apply(m,2,function(xx)tail(names(sort(xx)),2))

tm <- t(mm)
tmmm <- t(mmm)
order(tm)


colnames(mmm) <- c(list(1:200))

# find the 5 largest values
x <- which(m>=sort(m, decreasing = T)[5], arr.ind = T)
# determine the order of the 5 largest values in decreasing order
x.order <- order(m[x], decreasing = T)
x[x.order, ]

x <- matrix(nrow=list(1:length(x)),dimnames=list(1:length(x)),1:22)
x[ is.na(x) ] <- 0
x <- data.frame(key, x)
y <- apply(x,2,function(xx)tail(sort(xx),2))
z  <- apply(y,2,function(xx)tail(names(sort(xx)),2))

x <- apply(deltVolume,2,function(xx)tail(sort(xx),2))
y <- apply(deltVolume,2,function(xx)tail(nrow(sort(xx)),2))
z <- apply(deltVolume, 1,  function(x) rownames(deltVolume)[which.max(x)])


m <- matrix(deltVolume, 500, 4, dimnames=list([1:20],LETTERS[23:26]))
apply(m,2,function(xx)tail(sort(xx),7))
s <- apply(m,2,function(xx)tail(names(sort(xx)),2))

# Clear the R Environment and clear memory
ls()                      # View R Environment
remove(list = ls())       # To clear your environment
gc()                      # clear any occupied memory by running garbage collector using gc()
