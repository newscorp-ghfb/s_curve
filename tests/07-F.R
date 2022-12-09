# Load Systematic Investor Toolbox (SIT)
# github.com/systematicinvestor/SIT
###############################################################################
library(curl)
con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
source(con)
close(con)


###############################################################################
# Example Usage:
###############################################################################
# Run a plot test
plota.test()