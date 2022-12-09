################################################################################
# How to dynamically assign names to dataframes? https://tinyurl.com/2mc7p3rm ##
################################################################################
zip <- paste('zip',1:5, sep ='')
ll <- sapply(zip, function(x)
   {
     data.frame ()
   })

ll

ll[['zip1']]
ll[['zip1']] <- tail(dt_volume,-11)

ll
#################################################################################
## Step 00.02 dynamically create dataframe                                    ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm  ###
#################################################################################
x00 <- grep(pattern = 'AL*|UT', ls(), value = TRUE)

lapply(x00, function(nm) 
    {
    df <- get(nm)
    g[[paste0("dx", "_", nm)]] <- tail(df,-11)
    data.table::setDT(g[[paste0("dx", "_", nm)]], keep.rownames = TRUE)
    }
)

g <- new.env()
g[[paste0("dt", "_", "glen")]] <- tail(dt_volume,-11)
g[["dt_glen"]]
g[['dt_glen]]
g$dt_glen


mdata <-g[[paste(setupTrend[i,5], "mktdata", "ind", sep = "_")]] <<-
  applyIndicators(
    strategy                = setupTrend[i,5],
    mktdata                 = SPL.AX)

#################################################################################
## Step 00.03 nested functions                  https://tinyurl.com/2p8dar8s  ###
#################################################################################
outer <- function(d1) {
  lapply(d1, function(nm)
    {
    d1 <- d1 + 1
    }
  d2 <- 10  # local variable
  inner <- function(d1) {
    # sub function
    (d1 + d2) / 5  # inner function have an access to d2
  }
  inner(d1)
}
outer(5)