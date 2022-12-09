###############################################################################
## Step 00.00 MsVscode-REditor Support         https://tinyurl.com/mrytruwx ###
## Writing R in VSCode: A Fresh Start          https://tinyurl.com/tv2h44ef ###
## In setting type either "r-", "r."                                        ###
###############################################################################
# Finally add these lines in the settings.json of your editor
# "r.bracketedPaste": true,
# "r.rterm.windows": "*Path to radian executable*", //Use this only for Windows
# "r.rterm.linux": "*Path to radian executable*", //Use this only for Linux
# "r.rterm.mac": "*Path to radian executable*", //Use this only for a Mac
# "r.lsp.path": "*Path to your R executable*",
# "r.lsp.debug": true,
# "r.lsp.diagnostics": true,
# "r.rterm.option": [
# "--no-save",
# "--no-restore",
# "--r-binary=*Path to R executable*"
# ],
###############################################################################
## Step 00.00.a Setting up R with Visual Studio Code https://tinyurl.com/2445f3cz
###############################################################################
install.packages("languageserversetup")
languageserversetup::languageserver_install()
languageserversetup::languageserver_add_to_rprofile()
###############################################################################
## Step 00.00.a Project Template setup         https://tinyurl.com/2p8rrnff ###
###############################################################################
setwd("~/GitHub")
# -----------------------------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("r-lib/devtools")
devtools::install_github('KentonWhite/ProjectTemplate')
# -----------------------------------------------------------------------------
library('devtools')
library('ProjectTemplate')
create.project('compare_dt')
# -----------------------------------------------------------------------------
setwd("~/GitHub/compare_dt")
###############################################################################
## Step 00.00.b create new project in RStudio                                 #
###############################################################################
###############################################################################
## Step 00.01 renv package                                                    #
###############################################################################
if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")
# -----------------------------------------------------------------------------
devtools::install_github("r-lib/cli")
# -----------------------------------------------------------------------------
setwd("~/GitHub/ProjectName")
renv::init(bare = TRUE)
renv::activate()
# -----------------------------------------------------------------------------
renv::status()
renv::snapshot()
###############################################################################
## Step 00.02 packages required for MsVsCode                                  #
###############################################################################
# install.packages("devtools", "httpgd", "languageserver", "rmarkdown")
install.packages(c("httpgd", "languageserver", "rmarkdown"), Ncpus = 6)
# -----------------------------------------------------------------------------
if (!require("devtools")) install.packages("devtools")
devtools::install_github("ChiHangChen/KeyboardSimulator")
library(KeyboardSimulator)
# -----------------------------------------------------------------------------
# install devtools development copy
# -----------------------------------------------------------------------------
# devtools::install_github("r-lib/devtools")
remotes::install_github("ManuelHentschel/vscDebugger")
install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
# -----------------------------------------------------------------------------
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
###############################################################################
## Step 00.02.b Install data.table # latest development version:            ###
###############################################################################
install.packages("data.table")
data.table::update.dev.pkg()                      # latest development version:
###############################################################################
## Step 00.03 Copying files with R             https://tinyurl.com/ycy8wnab ###
## Check existence of directory and create if doesn't exist                 ###
###############################################################################
# -----------------------------------------------------------------------------
# Create new directories
# -----------------------------------------------------------------------------
#   ~/cheatsheet
#   ~/dashboard
#   ~/rds
# -----------------------------------------------------------------------------
# Copy following file(s) from existing ProjectTemplate folder:
# -----------------------------------------------------------------------------
#   ~/data/xlsx.file
#   ~/docs/readmeMenu.md
#   ~/docs/readmeMetaData.md
#   ~/munge/00-A.R
#   ~/munge/01-A.R
#   ~/munge/99-Z.R
# -----------------------------------------------------------------------------
# Rename:
# -----------------------------------------------------------------------------
#   ~/ProjectTemplate.r
#   'rename to "projectName".r
# -----------------------------------------------------------------------------
# Replace following file(s) in target directory
# -----------------------------------------------------------------------------
#   ~/.gitignore
#   ~/config/global.dcf
#   ~/lib/globals.R
#   ~/lib/helpers.R
# -----------------------------------------------------------------------------
# Step 00.03.a identify the folders
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------
###############################################################################
#  Break data.table chain into two lines of code for readability             ##
#                                                       https://is.gd/6RFsyK ##
# -----------------------------------------------------------------------------
# Regex multiple values                                 https://is.gd/WxzP0S ##
# Exclude Graduate School College %like% "^B.*Inst$|^VP.*|^G")               ##
###############################################################################
runsumCollege.tbl <- na.omit(setorder(fafsa.profile                          %>%
    .[, c(21,93)], College, Rounded.Unmet.need))                             %>%
    .[!(College %like% "Inst|Law$|Medicine$|Pharmacy$|^G|^N") & Rounded.Unmet.need>0,] %>%
    .[, .N, by = list(Rounded.Unmet.need, College)]                          %>%
    .[,runsum := cumsum(N), by = list(College)]
runsumCollege <- dcast(runsumCollege.tbl, Rounded.Unmet.need ~ College, value.var=("runsum"))
################################################################################
runsumGPA.tbl <- na.omit(setorder(fafsa.profile         %>%
    .[, c(21,75)], Current_GPA, Rounded.Unmet.need))    %>%
    .[Rounded.Unmet.need > 0]                           %>%
    .[, .N, by = list(Rounded.Unmet.need, Current_GPA)] %>%
    .[,runsum := cumsum(N), by = list(Current_GPA)]
 runsumGPA <- dcast(runsumGPA.tbl, Rounded.Unmet.need ~ Current_GPA, value.var=("runsum"))
###############################################################################
## CREATE, DELETE, MOVE, AND MORE WITH FILES   https://tinyurl.com/2p87tr3s ###
###############################################################################
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
devtools::install_github("gagolews/stringi")
devtools::install_github("gavinrozzi/zipcodeR")
devtools::install_github("hzambran/hydroTSM")
devtools::install_github("haozhu233/kableExtra")
devtools::install_github("jennybc/here_here")
devtools::install_github("lenkiefer/darklyplot")
devtools::install_github('markfairbanks/tidytable')
devtools::install_github('Mikata-Project/ggthemr')
devtools::install_github("patzaw/ReDaMoR")
devtools::install_github("renkun-ken/formattable")
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
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
devtools::install_github("sfirke/janitor")
devtools::install_github('stla/bracketify')
devtools::install_github('stla/pasteAsComment')
devtools::install_github("statisticsNZ/simplevis")
devtools::install_github("Swechhya/excelR")
devtools::install_github("talgalili/d3heatmap")
devtools::install_github("talgalili/heatmaply")
devtools::install_github("tidyverse/tidyverse")
devtools::install_github("tidyverse/googlesheets4")
devtools::install_github("timelyportfolio/sunburstR")
devtools::install_github("yihui/formatR")
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
"lintr", "logger", "lubridate", "modelsummary", "openxlsx", "orca", "packagefinder",
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
"

# -----------------------------------------------------------------------------
devtools::install_github("aryoda/tryCatchLog", ref = "v1.1.7")
devtools::install_github("cvitolo/r_treemap", subdir = "treeMap")
devtools::install_github("Swechhya/excelR")
# -----------------------------------------------------------------------------
remotes::install_github("arcruz0/inexact")
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T)
remotes::install_github("dreamRs/esquisse")
remotes::install_github('rstudio/flexdashboard')
remotes::install_github("HenrikBengtsson/R.utils@develop")
remotes::install_github("joshuaulrich/xts")
remotes::install_github("jbkunst/highcharter")
remotes::install_github("johnmyleswhite/log4r")
remotes::install_github("rstudio/DT")
remotes::install_github("talgalili/installr")
rsthemes::install_rsthemes(include_base16 = TRUE)
# -----------------------------------------------------------------------------
renv::status()
renv::snapshot()
###############################################################################
# Step 00.06 Install addins                                                 ###
###############################################################################
devtools::install_github('daattali/addinslist')
devtools::install_github("BAAQMD/copydat")
devtools::install_github("ChrisDienes/SeaClass")
devtools::install_github("LudvigOlsen/insertImage")
devtools::install_github("LudvigOlsen/splitChunk")
devtools::install_github("MilesMcBain/datapasta")
devtools::install_github("RhoInc/CRANsearcher")
devtools::install_github("rolkra/explore")                              # eda
devtools::install_github("Stan125/GREA")
devtools::install_github("alastairrushworth/inspectdf")                 # eda
devtools::install_github("Stan125/limoaddin")
devtools::install_github("ThinkR-open/littleboxes")
devtools::install_github("YvesCR/arimaUI")
devtools::install_github("alan-y/objectremover")
devtools::install_github("benmarwick/snakecaser")
devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)
devtools::install_github("bnosac/taskscheduleR")
devtools::install_github("business-science/correlationfunnel")
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
devtools::install_github("ggobi/ggally")
devtools::install_github("gowerc/diffdf")
devtools::install_github("haozhu233/giphyr")
devtools::install_github("hms-dbmi/UpSetR")
devtools::install_github("homerhanumat/addinplots")
devtools::install_github("jeffjjohnston/RStudioConsoleRender")
devtools::install_github("jennybc/jadd")
devtools::install_github("kieranjmartin/viewenhance")
devtools::install_github("lbusett/insert_table")
devtools::install_github("lorenzwalthert/strcode")
devtools::install_github('mayoverse/arsenal')
devtools::install_github("milesmcbain/mufflr")
devtools::install_github("mlysy/rdoxygen")
devtools::install_github("mrjoh3/mapedit.addin")
devtools::install_github("mvkorpel/uniscape")
devtools::install_github("mwip/beautifyR")
devtools::install_github("nevrome/wellspell.addin")
devtools::install_github("petermeissner/assignparams")
devtools::install_github("r-hub/pkgsearch")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("sarupurisailalith/commonUtilAddins")
devtools::install_github("seasmith/AlignAssign")
devtools::install_github("sfr/RStudio-Addin-Snippets")
devtools::install_github("strboul/caseconverter")
devtools::install_github("tjmahr/WrapRmd")
devtools::install_github('Timag/imageclipr')
devtools::install_github('dcomtois/sortLines')
devtools::install_github('n8thangreen/jagsAddIn')
devtools::install_github('paulgovan/QRAGadget')
devtools::install_github('xiaoa6435/RmdImgPaste')
###############################################################################
# Step 00.07 Faster R package installation     https://tinyurl.com/mxcf9fcd ###
###############################################################################
install.packages("gitgadget", repos = "https://radiant-rstats.github.io/minicran/", Ncpus = 6)
install_github("MangoTheCat/tidyshiny")
install_github("juba/questionr")
install_github("juba/questionr")
# ------------------------------------------------------------------------------
remotes::install_github("MarkEdmondson1234/googleAuthR")
remotes::install_github("ThinkR-open/remedy")
remotes::install_github("bquast/datasets.load")
remotes::install_github("brry/rskey")
remotes::install_github("christophsax/tsbox")
remotes::install_github("daattali/colourpicker")
remotes::install_github("dcomtois/pathToClip")
remotes::install_github("dreamRs/esquisse")
remotes::install_github("dreamRs/prefixer")
remotes::install_github("fkeck/quickview")
remotes::install_github("ginolhac/upnews")
remotes::install_github("jorvlan/raincloudplots")
remotes::install_github("konradzdeb/extraInserts")
remotes::install_github("liao961120/linguisticsdown")
remotes::install_github("matt-dray/blogsnip")
remotes::install_github("miraisolutions/compareWith")
remotes::install_github("njtierney/naniar")
remotes::install_github("R-CoderDotCom/ggdogs@main")
remotes::install_github("r-lib/styler")
remotes::install_github("robjhyndman/forecast")     # forecasting
remotes::install_github("tidyverts/fable")          # forecasting
remotes::install_github("ropensci/rcrossref")
remotes::install_github("s-fleck/testthis")
remotes::install_github("yonicd/ggedit")
remotes::install_github('famuvie/straddin')
remotes::install_github('yonicd/rsam')
remotes::install_github('yonicd/sinew')
remotes::install_github(repo = "baslat/bbb")
remotes::install_github('rstudio/bookdown')
# ------------------------------------------------------------------------------
renv::status()
renv::snapshot()
###############################################################################
# Step 00.08 ggplot extensions                 https://tinyurl.com/bddn7xnw ###
###############################################################################
devtools::install_github("dgkf/ggpackets", build_vignettes = TRUE)            # https://tinyurl.com/ye27758j
devtools::install_github("davidhodge931/ggblanket")                           # https://tinyurl.com/2p8aumzn
remotes::install_github("thomas-neitmann/ggcharts", upgrade = "never")        # https://tinyurl.com/5n89xu8z
remotes::install_github("jonocarroll/ggeasy")                                 # https://tinyurl.com/5n8pwbv7
devtools::install_github("yutannihilation/gghighlight")                       # https://tinyurl.com/2p8tt4r9
devtools::install_github(c("hadley/ggplot2", "jrnold/ggthemes"))              # https://tinyurl.com/yze23zx7
remotes::install_github("nanxstats/ggsci")                                    # https://tinyurl.com/bdcnbtz7
devtools::install_github('Mikata-Project/ggthemr')                            # https://tinyurl.com/n223k64y
devtools::install_github("hrbrmstr/hrbrthemes")                               # https://tinyurl.com/ydantu47
devtools::install_github('bbc/bbplot')                                        # https://tinyurl.com/yc3w89ec
devtools::install_github("EmilHvitfeldt/paletteer")                           # https://tinyurl.com/mrr9tsyn
remotes::install_github("wilkelab/ggtext")                                    # https://tinyurl.com/3ysyv94t
devtools::install_github("mjskay/ggdist")                                     # https://tinyurl.com/4zyzcdrv
###############################################################################
# Step 00.09 fuzzy logic packages              https://tinyurl.com/3u4u9k2k ###
###############################################################################
# [stringdist](https://github.com/markvanderloo/stringdist)
# [fuzzyjoin](https://github.com/dgrtwo/fuzzyjoin)
# [inexact](remotes::install_github("arcruz0/inexact"))
# [refinr](https://cran.r-project.org/web/packages/refinr/vignettes/refinr-vignette.html)
# [fuzzywuzzyR](https://github.com/mlampros/fuzzywuzzyR)
###############################################################################
#Check if the folder "Data" exists in the current directory, if not creates it
# https://tinyurl.com/5efa9xk8
###############################################################################
mainDir <- here::here()
subDir  <- here::here("dashboard/")
project_folder <- "C:/Users/gfalk/Documents/GitHub/ProjectTemplate/dashboard/"
#-------------------------------------------------------------------------------
if (!dir.exists(here(subDir))) {dir.create(here(subDir))}
file.copy(paste0(project_folder, dir(project_folder)), subDir)
#-------------------------------------------------------------------------------
subDir  <- here::here("rds/")
if (!dir.exists(here(subDir))) {dir.create(here(subDir))}
###############################################################################
# copy files to new folder
# https://tinyurl.com/bdeufm87
###############################################################################
# identify the folders
project_folder <- "C:/Users/gfalk/Documents/GitHub/ProjectTemplate/"
project_files  <- paste0(project_folder,list.files(project_folder, pattern = git))
new_folder     <- "C:/Users/gfalk/Documents/GitHub/payroll/"
file.copy(project_files, new_folder, overwrite = TRUE)

# find the files that you want
list_of_files <- list.files(project_folder, pattern = "xlsx")

# copy the files to the new folder
# https://tinyurl.com/2p87tr3s
file.copy(list.of.files, new.folder)
###############################################################################
# File Management
###############################################################################
# -----------------------------------------------------------------------------
# Create new directories
# -----------------------------------------------------------------------------
#   ~/cheatsheet
#   ~/dashboard
#   ~/rds
# -----------------------------------------------------------------------------
# Copy following file(s) from existing ProjectTemplate folder:
# -----------------------------------------------------------------------------
#   ~/data/xlsx.file
#   ~/docs/readmeMenu.md
#   ~/docs/readmeMetaData.md
#   ~/munge/00-A.R
#   ~/munge/01-A.R
#   ~/munge/99-Z.R
# -----------------------------------------------------------------------------
# Rename:
# -----------------------------------------------------------------------------
#	~/ProjectTemplate.r
#	 'rename to "projectName".r
# -----------------------------------------------------------------------------
# Replace following file(s)
# -----------------------------------------------------------------------------
# ~/.gitignore
# ~/config/global.dcf
# ~/lib/globals.R
# ~/lib/helpers.R
###############################################################################
# Copying list of files from one folder to other in R
# https://tinyurl.com/2p9x6scv
###############################################################################
origindir <- c("C:/Users/gfalk/Documents/GitHub/ProjectTemplate/data/")
targetdir <- c("C:/Users/gfalk/Documents/GitHub/payroll/data/")
filestocopy <- c("xlsx.file")

file.copy(from=filestocopy, to=targetdir,  copy.mode = TRUE)
###############################################################################
#  Choose a file interactively
#  https://tinyurl.com/yu7nrphp
###############################################################################
file.choose(new = FALSE)
###############################################################################
#  RStudio Initialization Commands
###############################################################################
if (!require(devtools)) install.packages("devtools")
devtools::install_github('KentonWhite/ProjectTemplate')

install.packages("languageserver")
install.packages("rmarkdown")
install.packages("httpgd")

remotes::install_github("ManuelHentschel/vscDebugger")
install.packages("vscDebugger")
remotes::install_github("ManuelHentschel/vscDebugger")

library(devtools)
devtools::install_git(
url = "https://github.com/kieranjmartin/getfunctionargs.git",
)

install.packages("radiant.update", repos = "https://radiant-rstats.github.io/minicran/")
options(repos = c(RSM = "https://radiant-rstats.github.io/minicran", CRAN = "https://cloud.r-project.org"))
install.packages("radiant.update")

remotes::install_github("REditorSupport/languageserver")
remotes::install_github("REditorSupport/languageserver", force = TRUE)

install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...)
library('jsonlite')


install.packages("jsonlite", dependencies = TRUE, repos = "http://cran.rstudio.com/")
if (!require(devtools)) install.packages("devtools")


if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")

install.packages("addinsJoaoMelo")

install.packages('curl', repos = 'http://cran.r-project.org', Ncpus = 6)
install.packages("scales")
radiant.update:::radiant.update()

devtools::install_github('daattali/addinslist')
install.packages("addinslist")
addinslist:::addinslistAddin()

install.packages(c("broom", "carData", "class", "DiagrammeR", "foreign", "igraph", "MASS", "Matrix", "NeuralNetTools", "nlme", "nloptr", "nnet", "openxlsx", "parallelly", "polycor", "progressr", "quantreg", "radiant.model", "RcppArmadillo", "readr", "rlang", "rpart", "spatial", "vroom"))
install.packages("rlang")
install.packages("blogsnip", lib="C:/Users/glen.falk/OneDrive - IHS Markit/Documents/R/R-4.1.2/library")
devtools::install_github('gadenbuie/rsthemes')
rsthemes::install_rsthemes(include_base16 = TRUE)
rsthemes::list_rsthemes()
rsthemes::list_rsthemes()
devtools::install_github("r-lib/devtools")
folder_root             <- "C:/Users/glen.falk/OneDrive - IHS Markit/Documents"
folder_github           <- "/github"
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
dt_subfolder            <- data.table::as.data.table(matrix(sub_folder))
# -----------------------------------------------------------------------------
dt_folder_copy_from     <- dt_project_template[,as.list(dt_subfolder), by=dt_project_template]
dt_folder_copy_from     <- dt_folder_copy_from[,path_from:=paste0(folder_project_template,V1)][,3]
# -----------------------------------------------------------------------------
# Step 00.03.b cartesian join                      https://tinyurl.com/ysy8jpen
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_target[,as.list(dt_subfolder), by=dt_target]
# -----------------------------------------------------------------------------
# Step 00.03.c combine two columns of a data.table https://tinyurl.com/a9jv99cf
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_folder_copy_to[,path_to:=paste0(folder_target,V1)][,3]
# -----------------------------------------------------------------------------
dt_folder_copy          <- cbind.data.frame(dt_folder_copy_from, dt_folder_copy_to)
# -----------------------------------------------------------------------------
# Step 00.03.d copy the files to the new folder
# -----------------------------------------------------------------------------
mapply(file.copy, from=dt_folder_copy[,1], to=dt_target,
overwrite = TRUE, recursive = TRUE)
# -----------------------------------------------------------------------------
# Step 99.99.a update packages
# -----------------------------------------------------------------------------
install.packages(c("broom", "carData", "class", "DiagrammeR", "foreign",
  "igraph", "MASS", "Matrix", "NeuralNetTools", "nlme", "nloptr", "nnet",
  "openxlsx","parallelly", "polycor", "progressr", "quantreg", "radiant.model",
  "RcppArmadillo", "readr", "rpart", "spatial", "vroom"))
# -----------------------------------------------------------------------------
install.packages("rlang")
# -----------------------------------------------------------------------------
# Step 99.99.b RStudio themes
# -----------------------------------------------------------------------------
devtools::install_github('gadenbuie/rsthemes')
###############################################################################
## Step 99.99.c install data.table               https://tinyurl.com/3vuv998k ###
###############################################################################
install.packages("data.table")

# latest development version:
data.table::update.dev.pkg()
###############################################################################
## Step 99.99.d install packages via devtools  https://tinyurl.com/3vuv998k ###
###############################################################################
devtools::install_github("bergant/datamodelr")
devtools::install_github("dreamRs/shinyWidgets")
devtools::install_github("gadenbuie/grkstyle")
devtools::install_github("gagolews/stringi")
devtools::install_github("haozhu233/kableExtra")
devtools::install_github("jennybc/here_here")
devtools::install_github("patzaw/ReDaMoR")
devtools::install_github("renkun-ken/formattable")
devtools::install_github('ramnathv/rCharts')
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
devtools::install_github("rjake/headliner")
devtools::install_github("r-lib/keyring")
devtools::install_github("r-lib/usethis")
devtools::install_github("ropensci/plotly")
devtools::install_github("rstudio/gt")
devtools::install_github("sfirke/janitor")
devtools::install_github("statisticsNZ/simplevis")
devtools::install_github("Swechhya/excelR")
devtools::install_github("tidyverse/tidyverse")
devtools::install_github("timelyportfolio/sunburstR")
devtools::install_github("yihui/formatR")

###############################################################################
## Step 99.99.e visual studio sidebar extensions   https://tinyurl.com/4y4ewk9z
###############################################################################
# Code time
# Github
# Powershell Command Explorer
# R
###############################################################################
## Step 99.99.e visual studio code extensions      https://tinyurl.com/5yeyft9p
###############################################################################
# Better Comments
# Bookmarks
# Code Spell Checker
# Color Highlight
# GitHub Copilot
# GitHub Pull Requests and Issues
# GitLink
# :emojisense:
# Live Preview
# Path Intellisense
# Peacock
# Random Everything
# Rainbow Brackets
###############################################################################
## Step 99.99.f RHistory file
###############################################################################
install.packages("languageserver")
install.packages("rmarkdown")
install.packages("httpgd")
if (!require(devtools)) install.packages("devtools")
install.packages("devtools")
remotes::install_github("ManuelHentschel/vscDebugger")
install.packages("vscDebugger")
remotes::install_github("ManuelHentschel/vscDebugger")
library(devtools)
devtools::install_git(
url = "https://github.com/kieranjmartin/getfunctionargs.git",
)
install.packages("radiant.update", repos = "https://radiant-rstats.github.io/minicran/")
options(repos = c(RSM = "https://radiant-rstats.github.io/minicran", CRAN = "https://cloud.r-project.org"))
install.packages("radiant.update")
remotes::install_github("REditorSupport/languageserver")
remotes::install_github("REditorSupport/languageserver", force = TRUE)
install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...)
library('jsonlite')
install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("jsonlite", dependencies = TRUE, repos = "http://cran.rstudio.com/")
install.packages("jsonlite", dependencies = TRUE, repos = "http://cran.rstudio.com/")
if (!require(devtools)) install.packages("devtools")
devtools::install_github('KentonWhite/ProjectTemplate')
if (!requireNamespace("remotes"))
install.packages("remotes")
remotes::install_github("rstudio/renv")
install.packages("addinsJoaoMelo")
install.packages('curl', repos = 'http://cran.r-project.org'), Ncpus = 6)
install.packages('curl', repos = 'http://cran.r-project.org', Ncpus = 6)
radiant.update:::radiant.update()
install.packages("scales")
radiant.update:::radiant.update()
devtools::install_github('daattali/addinslist')
install.packages("addinslist")
addinslist:::addinslistAddin()
install.packages(c("broom", "blastula", "carData", "class", "DiagrammeR", "foreign", "igraph", "MASS", "Matrix", "NeuralNetTools", "nlme", "nloptr", "nnet", "openxlsx", "parallelly", "polycor", "progressr", "quantreg", "radiant.model", "RcppArmadillo", "readr", "rlang", "rpart", "spatial", "vroom"))
install.packages("rlang")
devtools::install_github('gadenbuie/rsthemes')
rsthemes::install_rsthemes(include_base16 = TRUE)
rsthemes::list_rsthemes()
rsthemes::list_rsthemes()
devtools::install_github("r-lib/devtools")
folder_root             <- "C:/Users/glen.falk/OneDrive - IHS Markit/Documents"
folder_github           <- "/github"
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
dt_subfolder            <- data.table::as.data.table(matrix(sub_folder))
# -----------------------------------------------------------------------------
dt_folder_copy_from     <- dt_project_template[,as.list(dt_subfolder), by=dt_project_template]
dt_folder_copy_from     <- dt_folder_copy_from[,path_from:=paste0(folder_project_template,V1)][,3]
# -----------------------------------------------------------------------------
# Step 00.03.b cartesian join                      https://tinyurl.com/ysy8jpen
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_target[,as.list(dt_subfolder), by=dt_target]
# -----------------------------------------------------------------------------
# Step 00.03.c combine two columns of a data.table https://tinyurl.com/a9jv99cf
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_folder_copy_to[,path_to:=paste0(folder_target,V1)][,3]
# -----------------------------------------------------------------------------
dt_folder_copy          <- cbind.data.frame(dt_folder_copy_from, dt_folder_copy_to)
# -----------------------------------------------------------------------------
# Step 00.03.d copy the files to the new folder
# -----------------------------------------------------------------------------
mapply(file.copy, from=dt_folder_copy[,1], to=dt_target,
overwrite = TRUE, recursive = TRUE)
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T)
remotes::install_github("dreamRs/esquisse")
remotes::install_github('rstudio/flexdashboard')
remotes::install_github("HenrikBengtsson/R.utils@develop")
remotes::install_github('talgalili/installr')
remotes::install_github("jbkunst/highcharter")
remotes::install_github("johnmyleswhite/log4r")
remotes::install_github("SebKrantz/collapse")
remotes::install_github('rstudio/DT')
remotes::install_github('rstudio/flexdashboard')
remotes::install_github('vincentarelbundock/modelsummary')
rsthemes::install_rsthemes(include_base16 = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/qdapRegex")
# -----------------------------------------------------------------------------
renv::status()
renv::snapshot()
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
# Faster R package installation                https://tinyurl.com/mxcf9fcd ###
###############################################################################
install.packages("gitgadget", repos = "https://radiant-rstats.github.io/minicran/", Ncpus = 6)
install_github("MangoTheCat/tidyshiny")
install_github("juba/questionr")
install_github("juba/questionr")
# ------------------------------------------------------------------------------
remotes::install_github("MarkEdmondson1234/googleAuthR")
remotes::install_github("ThinkR-open/remedy")
remotes::install_github("bquast/datasets.load")
remotes::install_github("brry/rskey")
remotes::install_github("daattali/colourpicker")
remotes::install_github("daattali/colourpicker")
remotes::install_github("dcomtois/pathToClip")
remotes::install_github("dreamRs/esquisse")
remotes::install_github("dreamRs/prefixer")
remotes::install_github("ginolhac/upnews")
remotes::install_github("konradzdeb/extraInserts")
remotes::install_github("liao961120/linguisticsdown")
remotes::install_github("miraisolutions/compareWith")
remotes::install_github("r-lib/styler")
remotes::install_github("ropensci/rcrossref")
remotes::install_github("s-fleck/testthis")
remotes::install_github("yonicd/ggedit")
remotes::install_github('famuvie/straddin')
remotes::install_github('yonicd/rsam')
remotes::install_github('yonicd/sinew')
remotes::install_github(repo = "baslat/bbb")
# ------------------------------------------------------------------------------
renv::status()
renv::snapshot()
addinslist:::addinslistAddin()
CRANsearcher:::CRANsearcher()
dt_volume[,I:=.I,]
dt_volume[I %between% c(dt_title_line_num[1,2],dt_title_line_num[1,3])]
sapply(dt_title_line_num[,1], function(x) dt_volume[x,1])
sapply(dt_title_line_num[,1], function(x) dt_volume[-x,1])
z<-data.table::setorder((unique(setDT(sapply(dt_title_line_num[,1], function(x) dt_volume[-x,1])))))
sessionInfo()
require(remotes)
install_version("DiagrammeR ", version = "1.0.6.1", repos = "http://cran.us.r-project.org")
install.packages("C:/Users/glen.falk/Downloads/DiagrammeR_1.0.6.1.tar.gz", repos = NULL, type = "source", lib="C:/Users/glen.falk/OneDrive - IHS Markit/Documents/R/R-4.1.2/library")
install.packages("C:/Users/glen.falk/Downloads/DiagrammeR_1.0.6.1.tar.gz", repos = NULL, type = "source", lib="C:/Users/glen.falk/OneDrive - IHS Markit/Documents/R/R-4.1.2/library")
