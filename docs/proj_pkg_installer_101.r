if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")
# -----------------------------------------------------------------------------
devtools::install_github("r-lib/cli")
# -----------------------------------------------------------------------------
setwd(getwd())
renv::init(bare = TRUE)
renv::activate()
if (!require(devtools)) install.packages("devtools")
devtools::install_github("r-lib/devtools")
devtools::install_github('KentonWhite/ProjectTemplate')
# -----------------------------------------------------------------------------
library('devtools')
library('ProjectTemplate')
if (!require(devtools)) install.packages("devtools")
devtools::install_github("r-lib/devtools")
devtools::install_github('KentonWhite/ProjectTemplate')
library('devtools')
library('ProjectTemplate')
library(devtools)
devtools::install_github('KentonWhite/ProjectTemplate')
library(ProjectTemplate)
install.packages("ProjectTemplate")
''
devtools::install_github("r-lib/devtools")
setwd("~/GitHub/ProjectTemplate")
install.packages(c("httpgd", "languageserver", "rmarkdown"), Ncpus = 6)
remotes::install_github("ManuelHentschel/vscDebugger")
install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("radiant.update", repos = "https://radiant-rstats.github.io/minicran/")
options(repos = c(RSM = "https://radiant-rstats.github.io/minicran", CRAN = "https://cloud.r-project.org"))
install.packages("radiant.update")
remotes::install_github("REditorSupport/languageserver", force = TRUE)
# -----------------------------------------------------------------------------
devtools::install_git(
url = "https://github.com/kieranjmartin/getfunctionargs.git",
)
devtools::install_github("r-lib/cli")
# -----------------------------------------------------------------------------
renv::snapshot()
## Step 00.02.b Install data.table # latest development version:            ###
###############################################################################
install.packages("data.table")
data.table::update.dev.pkg()
library(data.table)
remove.packages("data.table", lib="~/github/volatility/renv/library/R-4.1/x86_64-w64-mingw32")
install.packages("data.table")
data.table::update.dev.pkg()
folder_root             <- "C:/Users/glen.falk/OneDrive - IHS Markit/Documents"
folder_github           <- "/github"
folder_new              <- c("/cheatsheet/", "/dashboard/", "/rds/")
folder_project_template <- paste0(folder_root,folder_github, "/ProjectTemplate")
folder_target           <- getwd()
# -----------------------------------------------------------------------------
dt_project_template     <- data.table::as.data.table(folder_project_template)
dt_target               <- data.table::as.data.table(folder_target)
# -----------------------------------------------------------------------------
sub_folder              <- c("/config/",
"/data/",
"/docs/",
"/lib/",
"/munge/",
"/reports")
# -----------------------------------------------------------------------------
dt_folder_new           <- data.table::as.data.table(matrix(folder_new))
dt_subfolder            <- data.table::as.data.table(matrix(sub_folder))
# -----------------------------------------------------------------------------
dt_folder_new           <- dt_target[,as.list(dt_folder_new), by = dt_target]
# -----------------------------------------------------------------------------
dt_folder_copy_from     <- dt_project_template[,as.list(dt_subfolder), by = dt_project_template]
dt_folder_copy_from     <- dt_folder_copy_from[,path_from:= paste0(folder_project_template,V1)][,3]
# -----------------------------------------------------------------------------
# Step 00.03.b cartesian join                      https://tinyurl.com/ysy8jpen
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_target[,as.list(dt_subfolder), by=dt_target]
# -----------------------------------------------------------------------------
# Step 00.03.c combine two columns of a data.table https://tinyurl.com/a9jv99cf
# -----------------------------------------------------------------------------
dt_folder_new           <- dt_folder_new[,path:= paste0(folder_target,V1)][,3]
dt_folder_copy_to       <- dt_folder_copy_to[,path_to:= paste0(folder_target,V1)][,3]
# -----------------------------------------------------------------------------
dt_folder_copy          <- cbind.data.frame(dt_folder_copy_from, dt_folder_copy_to)
# -----------------------------------------------------------------------------
# Step 00.03.d copy the files to the new folder
# -----------------------------------------------------------------------------
# Break data.table chain into two lines of code for readability
# -----------------------------------------------------------------------------
# You have to give a return between the [ and ] of each line.
# bar <- foo[, .(long_name_here = sum(foo2)), by = var
#           ][order(-long_name_here)]
# -----------------------------------------------------------------------------
# You can also give a return before / after each comma. An example with a
# return before the comma (my preference):             https://is.gd/6RFsyK ###
# -----------------------------------------------------------------------------
# bar <- foo[, .(long_name_here = sum(foo2))
#            , by = var
#           ]
# -----------------------------------------------------------------------------
mapply(dir.create, paths = dt_folder_new[,1], showWarnings = TRUE,
recursive = FALSE, mode = "0777")
# -----------------------------------------------------------------------------
mapply(file.copy, from=dt_folder_copy[,1], to=dt_target,
overwrite = TRUE, recursive = TRUE)
install.packages(
"datapasta",
repos = c(mm = "https://milesmcbain.r-universe.dev", getOption("repos")))
# -----------------------------------------------------------------------------
install.packages("languageserversetup")
languageserversetup::languageserver_install()
languageserversetup::languageserver_add_to_rprofile()
devtools::install_github("ChiHangChen/KeyboardSimulator")
###############################################################################
# Install addins                                                            ###
###############################################################################
devtools::install_github('daattali/addinslist')
devtools::install_github("BAAQMD/copydat")
devtools::install_github("ChrisDienes/SeaClass")
devtools::install_github("LudvigOlsen/insertImage")
devtools::install_github("LudvigOlsen/splitChunk")
devtools::install_github("MilesMcBain/datapasta")
devtools::install_github("RhoInc/CRANsearcher")
devtools::install_github("Stan125/GREA")
devtools::install_github("Stan125/limoaddin")
devtools::install_github("ThinkR-open/littleboxes")
devtools::install_github("YvesCR/arimaUI")
devtools::install_github("alan-y/objectremover")
devtools::install_github("benmarwick/snakecaser")
devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)
devtools::install_github("bnosac/taskscheduleR")
devtools::install_github("calligross/ggthemeassist")
devtools::install_github("crsh/citr")
devtools::install_github("csgillespie/addinmanager")
devtools::install_github("daattali/addinslist")
devtools::install_github("daattali/ggExtra")
devtools::install_github("daranzolin/ViewPipeSteps")
devtools::install_github("daranzolin/compareAreas")
devtools::install_github("daranzolin/typeStringsGadget")
devtools::install_github("dkilfoyle/rpivotGadget")
devtools::install_github("dokato/todor")
devtools::install_github("donlelef/tsviz")
devtools::install_github("dracodoc/mischelper")
devtools::install_github("dracodoc/namebrowser")
devtools::install_github("dreamRs/addinit")
devtools::install_github("dreamRs/viewxl")
devtools::install_github("erictleung/unnestIfElse")
devtools::install_github("fkeck/quickview")
devtools::install_github("flaviobarros/shinyExams")
devtools::install_github("gadenbuie/ermoji")
devtools::install_github("haozhu233/giphyr")
devtools::install_github("homerhanumat/addinplots")
devtools::install_github("jeffjjohnston/RStudioConsoleRender")
devtools::install_github("jennybc/jadd")
devtools::install_github("kieranjmartin/viewenhance")
devtools::install_github("lbusett/insert_table")
devtools::install_github("lorenzwalthert/strcode")
devtools::install_github("milesmcbain/mufflr")
devtools::install_github("mlysy/rdoxygen")
devtools::install_github("mrjoh3/mapedit.addin")
devtools::install_github("mvkorpel/uniscape")
devtools::install_github("mwip/beautifyR")
devtools::install_github("omegahat/RDCOMClient")
devtools::install_github("nevrome/wellspell.addin")
devtools::install_github("petermeissner/assignparams")
devtools::install_github("r-hub/pkgsearch")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("seasmith/AlignAssign")
devtools::install_github('stla/bracketify')
devtools::install_github("sfr/RStudio-Addin-Snippets")
devtools::install_github("strboul/caseconverter")
devtools::install_github("tjmahr/WrapRmd")
devtools::install_github('Timag/imageclipr')
devtools::install_github('dcomtois/sortLines')
devtools::install_github('n8thangreen/jagsAddIn')
devtools::install_github('paulgovan/QRAGadget')
devtools::install_github('xiaoa6435/RmdImgPaste')
# ------------------------------------------------------------------------------
devtools::install_github("b-rodrigues/loud")
###############################################################################
## Step 00.04 install devtools packages                                     ###
###############################################################################
if (!require(devtools)) install.packages("devtools")
# -----------------------------------------------------------------------------
devtools::install_github("bergant/datamodelr")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("btskinner/rscorecard")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("cynkra/dm")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("dreamRs/shinyWidgets")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("emilopezcano/SixSigma")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("gadenbuie/grkstyle")
# keybd.press('1')flexdashboard
# keybd.press("Enter")
devtools::install_github('adeckmyn/maps')
devtools::install_github('alexsanjoseph/compareDF')
devtools::install_github('baptiste/gridExtra')
devtools::install_github('cwickham/munsell')
devtools::install_github('flow-r/flowr')
devtools::install_github("gadenbuie/rsthemes")
devtools::install_github("gagolews/stringi")
devtools::install_github("gavinrozzi/zipcodeR")
devtools::install_github("hzambran/hydroTSM")
devtools::install_github("haozhu233/kableExtra")
devtools::install_github("jennybc/here_here")
devtools::install_github("jiho/autoplot")
devtools::install_github("joshuaulrich/xts")
devtools::install_github("jsugarelli/packagefinder")
devtools::install_github("lenkiefer/darklyplot")
devtools::install_github('markfairbanks/tidytable')
devtools::install_github('Mikata-Project/ggthemr')
devtools::install_github("patzaw/ReDaMoR")
devtools::install_github("pzhaonet/bookdownplus")
devtools::install_github("renkun-ken/formattable")
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
devtools::install_github('ramnathv/rCharts')
devtools::install_github("R-CoderDotCom/ggdogs")
devtools::install_github("R-Finance/blotter")
devtools::install_github("r-lib/cli")
devtools::install_github("r-lib/httr")
devtools::install_github("r-lib/keyring")
devtools::install_github("r-lib/usethis")
devtools::install_github("ropensci/plotly")
devtools::install_github("rspatial/raster")
devtools::install_github("r-rust/gifski")
devtools::install_github("rstudio/gt")
devtools::install_github("rstudio/rsconnect")
devtools::install_github("sfirke/janitor")
devtools::install_github('stla/bracketify')
devtools::install_github('stla/pasteAsComment')
devtools::install_github("statisticsNZ/simplevis")
devtools::install_github("Swechhya/excelR")
devtools::install_github("talgalili/d3heatmap")
devtools::install_github("talgalili/heatmaply")
devtools::install_github("tidyverse/tidyverse")
devtools::install_github("tidyverse/ggplot2")
devtools::install_github("tidyverse/googlesheets4")
devtools::install_github("timelyportfolio/sunburstR")
devtools::install_github("yihui/formatR")
# ------------------------------------------------------------------------------
# good packages to install for heatmaply to work smoothly:
# ------------------------------------------------------------------------------
install.packages(c("Rcpp","ggplot2","munsell","htmltools","DBI","assertthat",
                   "gridExtra","digest","fpc","TSP","registry","gclus","gplots","RColorBrewer",
                   "stringr","labeling","yaml"))
###############################################################################
## Step 00.05 install cran packages                                         ###
## Faster R package installation               https://tinyurl.com/mxcf9fcd ###
###############################################################################
install.packages('curl', repos = 'http://cran.r-project.org', Ncpus = 6)
# ------------------------------------------------------------------------------
install.packages( c(" ARTofR","billboarder", "beepr", "bookdown", "cheatsheet,", "checkpoint",
"compare", "compareDF", "daff", "DataExplorer", "dataMeta", "datamodelr","datawizard", "deepdep",
"devtools", "DBI", "DiagrammeR", "dlookr", "diffobj", "dplyr", "dtplyr,", "DT",
"flexdashboard", "flextable", "formatR", "formattable", "ftExtra", "futile.logger",
"fuzzyjoin", "ggalt", "gganimate", "ggdark", "ggforce", "ggiraph", "ggimage",
"ggplot2", "ggpp", "ggrepel", "ggshadow", "ggsignif", "ggstatsplot", "ggtext",
"ggthemes", "gifski", "glue", "googledrive", "googlesheets4", "grid", "gridExtra",
"gt","here", "hrbrthemes", "htmltools","humaniformat", "janitor", "keyring", "knitr",
"lintr", "logger", "lubridate", "modelsummary", "openxlsx", "orca",
"pacman", "paletteer", "plotluck,", "plyr", "poorman", "png", "prettydoc", "prettycode",
"processR", "qdap", "readxl", "reactable", "reprex", "reshape2", "rex", "rio",
"RMariaDB", "rmarkdown", "rmdformats","rmiscutils", "RMySQL", "RODBC", "reprex",
"rscorecard", "rstudioapi",   "RVerbalExpressions", "scales", "shiny", "shinydashboard",
"shinyWidgets","SixSigma", "slider", "SmartEDA", "snakecase",  "splot", "stargazer",
"stringdist", "stringi", "stringr", "styler", "sunburstR", "tabulizer", "tabulizerjars", "tidyverse",
"timetk", "timevis",  "tint", "tinytex", "tor", "treemap", "tryCatchLog", "tufte",
"waldo", "webshot", "xfun", "xlsx", "xtable", "zoo"), Ncpus = 6)
install.packages( c("beepr", "DataExplorer", "dygraphs", "flexdashboard", "formattable", "fortunes",
"ggplot2movies", "gt", "here", "highcharter",  "hydroTSM",	"inspectdf", "kableExtra",
"keyring", "plotly", "shinydashboard", "tidyverse", "timetk", "treemap"),
Ncpus = 6)
renv::status()
rsthemes::install_rsthemes(include_base16 = TRUE)
install.packages(
"rsthemes",
repos = c(gadenbuie = 'https://gadenbuie.r-universe.dev', getOption("repos"))
)
renv::status()
renv::snapshot()
devtools::install_github("aryoda/tryCatchLog", ref = "v1.1.7")
devtools::install_github("aryoda/tryCatchLog", ref = "v1.1.7")
usethis::browse_github_pat()
usethis::create_github_token()
gitcreds::gitcreds_set('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')
gitcreds::gitcreds_set('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')
usethis::edit_r_profile()
usethis::browse_github_token()
usethis::edit_git_config()
devtools::install_github("aryoda/tryCatchLog", ref = "v1.1.7")
usethis::edit_r_environ()
install.packages(
"datapasta",
repos = c(mm = "https://milesmcbain.r-universe.dev", getOption("repos")))
###############################################################################
## Step 00.04 install devtools packages                                     ###
###############################################################################
if (!require(devtools)) install.packages("devtools")
# -----------------------------------------------------------------------------
devtools::install_github("bergant/datamodelr")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("btskinner/rscorecard")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("cynkra/dm")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("dreamRs/shinyWidgets")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("emilopezcano/SixSigma")
# keybd.press('1')
# keybd.press("Enter")
devtools::install_github("gadenbuie/grkstyle")
# keybd.press('1')flexdashboard
# keybd.press("Enter")
devtools::install_github('adeckmyn/maps')
devtools::install_github('alexsanjoseph/compareDF')
devtools::install_github("gadenbuie/rsthemes")
install.packages("gitgadget", repos = "https://radiant-rstats.github.io/minicran/", Ncpus = 6)
devtools::install_github("MangoTheCat/tidyshiny")
devtools::install_github("juba/questionr")
devtools::install_github("Techtonique/ahead")           #  forecasting
# ------------------------------------------------------------------------------
remotes::install_github("arcruz0/inexact")
remotes::install_github("MarkEdmondson1234/googleAuthR")
remotes::install_github("ThinkR-open/remedy")
remotes::install_github("bquast/datasets.load")
remotes::install_github("brry/rskey")
remotes::install_github("christophsax/tsbox")
remotes::install_github("daattali/colourpicker")
remotes::install_github("dcomtois/pathToClip")
remotes::install_github("dreamRs/esquisse")
remotes::install_github("dreamRs/prefixer")
remotes::install_github('jorvlan/raincloudplots')
remotes::install_github('vincentarelbundock/modelsummary')
# ------------------------------------------------------------------------------
options(repos = c(
  yihui = 'https://yihui.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'
))
install.packages('knitr')
# ------------------------------------------------------------------------------
addinslist:::addinslistAddin()
CRANsearcher:::CRANsearcher()
# ------------------------------------------------------------------------------
# Financial packages
# ------------------------------------------------------------------------------
devtools::install_github("braverock/blotter")
install.packages("FinancialInstrument")
devtools::install_github("braverock/quantstrat", force = TRUE )
devtools::install_github("business-science/tidyquant")
devtools::install_github("joshuaulrich/TTR")
