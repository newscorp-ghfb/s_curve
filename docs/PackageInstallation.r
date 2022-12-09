if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")
install.packages("remotes") #install remotes
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T)
if (!require("devtools")) install.packages("devtools")
devtools::install_github("KentonWhite/ProjectTemplate")

devtools::install_github('patzaw/ReDaMoR')
devtools::install_github("r-lib/usethis")
install.packages("here")
devtools::install_github("gagolews/stringi")
devtools::install_github("sfirke/janitor")

install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
install.packages('curl', repos = 'http://cran.r-project.org')

devtools::install_github('systematicinvestor/SIT.date')
library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')
install.packages("C:/Users/gfalk/Downloads/SIT-20210514T173311Z-001.zip", repos = NULL, type = "win.binary")
devtools::install_github('R-Finance/blotter')
devtools::install_github("R-Finance/FinancialInstrument") #if not installed
devtools::install_github("braverock/PerformanceAnalytics") #if not installed
devtools::install_github("braverock/quantstrat")
remotes::install_github('rstudio/DT')
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
remotes::install_github('rstudio/flexdashboard')
1
devtools:install_github('Swechhya/excelR')
devtools::install_github("renkun-ken/formattable")
devtools::install_github("rstudio/gt")
devtools::install_github("jennybc/here_here")
yes
install.packages("here")
remotes::install_github("jbkunst/highcharter")
devtools::install_github("haozhu233/kableExtra")
devtools::install_github("hzambran/hydroTSM")
devtools::install_github("ropensci/plotly")
install.packages("shinydashboard")
devtools::install_github("dreamRs/shinyWidgets")
install.packages("shinydashboard")
devtools::install_github("business-science/tidyquant")
devtools::install_github("tidyverse/tidyverse")
install_github("cvitolo/r_treemap", subdir = "treeMap")
install.packages("treemap")
devtools::install_github('gadenbuie/rsthemes')
rsthemes::install_rsthemes(include_base16 = TRUE)
devtools::install_github("r-lib/httr")
devtools::install_github("statisticsNZ/simplevis")
devtools::install_github("timelyportfolio/sunburstR")
remotes::install_github("HenrikBengtsson/R.utils@develop")
devtools::install_github('yihui/formatR')
devtools::install_github("bergant/datamodelr")
devtools::install_github("lenkiefer/darklyplot")
devtools::install_github("gadenbuie/grkstyle")
install.packages('installr'); install.Rtools()
devtools::install_github("ropensci/plotly") # you will probably benefit from the latest version of plotly
devtools::install_github('talgalili/heatmaply')
devtools::install_github("dreamRs/shinyWidgets")
devtools::install_github("emilopezcano/SixSigma")
devtools::install_github("r-lib/cli")
remotes::install_github("dreamRs/esquisse")
devtools::install_github('r-lib/keyring')
devtools::install_github("btskinner/rscorecard")
install.packages("rJava", repos="http://rforge.net")
remotes::install_github("johnmyleswhite/log4r")
install_github("aryoda/tryCatchLog", ref = "v1.1.7")
################################################################################
## Step 00.01 Exploratory Data Analysis      https://tinyurl.com/j8dxy87n    ###
################################################################################
remotes::install_github("IndrajeetPatil/ggstatsplot")
# ------------------------------------------------------------------------------DataExplorer
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("boxuancui/DataExplorer")
# ------------------------------------------------------------------------------code
DataExplorer::create_report(starwars %>% select(-films, -vehicles, -starships))
# ------------------------------------------------------------------------------
# explore                                           https://tinyurl.com/af8kczbb
# ------------------------------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("rolkra/explore")
# ------------------------------------------------------------------------------inspectdf
devtools::install_github("alastairrushworth/inspectdf")
# ------------------------------------------------------------------------------code
inspectdf::inspect_cat(starwars) %>% show_plot()
# ------------------------------------------------------------------------------skimr
# install.packages("devtools")
devtools::install_github("ropensci/skimr")
# ------------------------------------------------------------------------------SmartEDA
install.packages("devtools")
devtools::install_github("daya6489/SmartEDA",ref = "develop")
# ------------------------------------------------------------------------------summarytools
install.packages("remotes")        # Using devtools is also possible
library(remotes)
install_github("rapporter/pander") # Strongly recommended
install_github("dcomtois/summarytools", build_vignettes = TRUE)
# ------------------------------------------------------------------------------visdat
# install.packages("devtools")
devtools::install_github("ropensci/visdat")
# ------------------------------------------------------------------------------
renv::status()
renv::snapshot()
# category:themes fortnite

# usethis::create_github_token()
