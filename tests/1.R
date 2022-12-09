#################################################################################
## Step 00.00 fuzzy join                        https://tinyurl.com/2x3sb69k ###
#################################################################################
# fuzzy join
inexact:::inexact_addin()
# ------------------------------------------------------------------------------
inexact::inexact_join(
  x  = data_a,
  y  = data_b,
  by = 'country',
  method = 'osa',
  mode = 'left',
  custom_match = c(
   'Bolivia' = 'Bolivia (Plurinational State of)'
  )
)
# ------------------------------------------------------------------------------

# If you are fluent in {dplyr} and wish to learn how to write SQL queries, it can be nifty to have a function that can provide a syntax translation between the two.

# The {show_query} function from {dbplyr} Package does just that! Anticlockwise downwards and upwards open circle arrows

# https://dbplyr.tidyverse.org/articles/sql.html

#rstats #DataScience
# https://dbplyr.tidyverse.org/articles/sql.html

mf %>%
  mutate(z = foofify(x, y)) %>%
  show_query()
#> <SQL>
#> SELECT `x`, `y`, foofify(`x`, `y`) AS `z`
#> FROM `dbplyr_001`

mf %>%
  mutate(z = FOOFIFY(x, y)) %>%
  show_query()
#> <SQL>
#> SELECT `x`, `y`, FOOFIFY(`x`, `y`) AS `z`
#> FROM `dbplyr_001`

mf %>%
  filter(x %LIKE% "%foo%") %>%
  show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_001`
#> WHERE (`x` LIKE '%foo%')

mf %>%
  transmute(z = x %||% y) %>%
  show_query()
#> <SQL>
#> SELECT `x` || `y` AS `z`
#> FROM `dbplyr_001`
#################################################################################
# Nested If Else in R                                https://tinyurl.com/yc7njdxu
#################################################################################
if (all.equal(x, y)) {
#   Boolean_Expression 1 result is TRUE then, it will check for Boolean_Expression 2
    print("x is equal to y")
    if (all.equal(sapply(dfCorrect,class),sapply(dfWrong,class))) {
#     Boolean_Expression 2 result is TRUE, then these statements will be executed
#     Boolean_Expression 2 True statements
      print("x class is equal to y class")
    } else {
#     Boolean_Expression 2 result is FALSE then, these statements will be executed
#     Boolean_Expression 2 False statements
      print("x classes not equal y classes")
  }
} else {
#   If the Boolean_Expression 1 result is FALSE, these statements will be executed
#   Boolean_Expression 1 False statements  
  print("x not equal to y")
}
#################################################################################
## Step 00.02 dynamically create dataframe      https://tinyurl.com/y3adrqwa ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm ###
#################################################################################
x00 <- grep(pattern = 'AL*|UT', ls(), value = TRUE)
# ------------------------------------------------------------------------------
lapply(x00, function(nm) {
    df <- get(nm)
    g[[paste0("dx", "_", nm)]] <- tail(df,-11)
    data.table::setDT(g[[paste0("dx", "_", nm)]], keep.rownames = TRUE)


    g[[paste0("dx", "_", nm)]]$date_run <- as.Date(g[[paste0("dx", "_", nm)]]$date_run, format('%m/%d/%Y'))
#    data.table::setkey(g[[paste0("dx", "_", nm)]], "id")
# ------------------------------------------------------------------------------pct to dbl
    g[[paste0("dx", "_", nm)]][,c(2,6,9,24:25,27:28,32:35,37:46)] <-
        lapply(g[[paste0("dx", "_", nm)]][,c(2,6,9,24:25,27:28,32:35,37:46)],
        function(x) parse_number(x))
# ------------------------------------------------------------------------------chr to dbl
    g[[paste0("dx", "_", nm)]][,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
        lapply(g[[paste0("dx", "_", nm)]][,c(3:4,8,12,14,16:23,26,29:31,36,47:54)],
        function(x) parse_number(x))
    }
)

# ------------------------------------------------------------------------------
# R: copy/move one environment to another       https://tinyurl.com/2p94pajk ###
# Detect if environment is global environment   https://tinyurl.com/yckhmhxp ###
# ------------------------------------------------------------------------------
for(n in ls(g, all.names=TRUE)) assign(n, get(n, g), .GlobalEnv)
# ------------------------------------------------------------------------------
#################################################################################
# Example Unit Testing Script -
context("Example tests")
expect_equal(1, 1)

# Get the list of installed packages by user in R [shorturl.at/glCV5]
ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
ip

# Installing older version of R package                                         https://tinyurl.com/5w4yvarc
require(remotes)
install_version("ggplot2", version = "0.9.1", repos = "http://cran.us.r-project.org")

# install yesterday's version of checkpoint, by date
install.dates('checkpoint', Sys.Date() - 1)

# install earlier versions of checkpoint and devtools
install.versions(c('checkpoint', 'devtools'), c('0.3.3', '1.6.1'))


# Got knit issue with R pdfcrop not working with ghostscript                    https://tinyurl.com/zzn695vk
Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.53.3/bin/gswin64.exe")

# Convert docx to Rmarkdown                                                     https://tinyurl.com/yjh26dan
pandoc_convert(examplefile,to="markdown",output = "example.rmd", options=c("--extract-media=."))

# How to convert R Markdown to PDF?                                             https://tinyurl.com/k48sf2b2
render("input.Rmd", "pdf_document")

# Resume                                                                        # Font Awesome Web App Icons                                http://tinyurl.com/yb8avt9t
xaringan:::inf_mr()

# Glen C. Falk website                                                          # https://tinyurl.com/y35xuv48
servr::daemon_stop()
blogdown::serve_site()                                                          # refresh site
blogdown::new_site()                                                            # build site
# Warning: You are recommended to ignore certain files in config.toml: set the option ignoreFiles = ["\\.Rmd$", "\\.Rmarkdown$", "_files$", "_cache$"]

# edit rprofile
file.edit("~/.Rprofile")
# or file.edit('.Rprofile')

# get a list of dataframes                                                      https://tinyurl.com/yyu9q52s
ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
ls()[sapply(ls(), function(x) is.data.table(get(x)))]
ls()[sapply(ls(), function(x) is.xts(get(x)))]
str(lapply(family1, typeof))                                                    # https://tinyurl.com/y3qeekrt
str(lapply(family1, function(x) typeof(x)))                                     # same code as previous line.
x<-data.table(ls()[sapply(ls(), function(x) is.data.frame(get(x)) | is.xts(get(x)))])
rm(list = ls()[grepl("(SQL|X2016Tuition)", ls())])                             # remove dataframes with 'SQL' in its name

a<-list("a", list("b","c"), list("d","e","f"))

ProjectTemplate::migrate.project()                                              # migrate ProjectTemplate project to latest version.

# Get the list of installed packages by user in R                               # https://tinyurl.com/yxoaj5zl
ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = data.table(ip[is.na(ip$Priority),1:2,drop=FALSE])
ip

# Change all columns except the 1st to dollar format                            https://tinyurl.com/y4nvpd8w
df[-1] <- lapply(df[-1], dollar)

# The definitive replace Na's                                                   https://tinyurl.com/y4alytbp
# https://tinyurl.com/ybcssoh8
dtSPL[is.na(dtSPL)] <- 0                                                        # replace na's with zero
# Getting Started in R?                                                         # https://github.com/eddelbuettel/gsir-te/blob/master/Getting-Started-in-R.R
# https://eddelbuettel.github.io/gsir-te/Getting-Started-in-R.pdf               btw c() implies 'combine'
# https://github.com/Rdatatable/data.table/wiki

iris.dt[, ncol(iris.dt), with = FALSE]                                          # Last column (need with = FALSE) https://tinyurl.com/rf8o7fu

# ----------------------------------------------------------https://tinyurl.com/yc7lohmk
# dynamically select column by index number                 https://is.gd/G1c2PS
# format(dt_hz_tot[,1], justify = "left")
# format(dt_hz_tot, justify = "left")
# dt_hz_tot[,2    := lapply(.SD, function(x) format(x, big.mark= ",", scientific = F)), .SDcols = 2]
# dt_hz_tot[,3    := lapply(.SD, function(x) format_dol_fun(dt_hz_tot$amt)), .SDcols = 3]
# dt_hz_tot[,4:5  := lapply(.SD, function(x) paste0(round(x,2), '%')), .SDcols = 4:5]
# format(dt_hz_tot[,2:5], justify = "right")
# ------------------------------------------------------------------------------

# --------------------------------------------------------------------------
# Assign multiple columns using := in data.table, by group https://tinyurl.com/y5sct5sq
# --------------------------------------------------------------------------
x <- data.table(a = 1:3, b = 1:6)
f <- function(x) {list("hi", "hello")}
x[ , c("col1", "col2") := f(), by = a][]
#    a b col1  col2
# 1: 1 1   hi hello
# 2: 2 2   hi hello
# 3: 3 3   hi hello
# 4: 1 4   hi hello
# 5: 2 5   hi hello
# 6: 3 6   hi hello

x[ , c("mean", "sum") := list(mean(b), sum(b)), by = a][]
#    a b col1  col2 mean sum
# 1: 1 1   hi hello  2.5   5
# 2: 2 2   hi hello  3.5   7
# 3: 3 3   hi hello  4.5   9
# 4: 1 4   hi hello  2.5   5
# 5: 2 5   hi hello  3.5   7
# 6: 3 6   hi hello  4.5   9

mynames = c("Name1", "Longer%")
x[ , (mynames) := list(mean(b) * 4, sum(b) * 3), by = a]
#     a b col1  col2 mean sum Name1 Longer%
# 1: 1 1   hi hello  2.5   5    10      15
# 2: 2 2   hi hello  3.5   7    14      21
# 3: 3 3   hi hello  4.5   9    18      27
# 4: 1 4   hi hello  2.5   5    10      15
# 5: 2 5   hi hello  3.5   7    14      21
# 6: 3 6   hi hello  4.5   9    18      27

x[ , get("mynames") := list(mean(b) * 4, sum(b) * 3), by = a][]  # same
#    a b col1  col2 mean sum Name1 Longer%
# 1: 1 1   hi hello  2.5   5    10      15
# 2: 2 2   hi hello  3.5   7    14      21
# 3: 3 3   hi hello  4.5   9    18      27
# 4: 1 4   hi hello  2.5   5    10      15
# 5: 2 5   hi hello  3.5   7    14      21
# 6: 3 6   hi hello  4.5   9    18      27

x[ , eval(mynames) := list(mean(b) * 4, sum(b) * 3), by = a][]   # same
#    a b col1  col2 mean sum Name1 Longer%
# 1: 1 1   hi hello  2.5   5    10      15
# 2: 2 2   hi hello  3.5   7    14      21
# 3: 3 3   hi hello  4.5   9    18      27
# 4: 1 4   hi hello  2.5   5    10      15
# 5: 2 5   hi hello  3.5   7    14      21
# 6: 3 6   hi hello  4.5   9    18      27

# --------------------------------------------------------------------------
# lapply multiple functions                   # https://tinyurl.com/y9hxhq2p
# -------------------------------------------------------------------------
iOdd <- seq(7, 21, 2)
iEven <- seq(6, 20, 2)
htmlpages = lapply(
  urls,
  function(x)
  {
    y <- readLines(x)
    Sys.sleep(0.3)
    y
  }
)
# --------------------------------------------------------------------------
# dynamic column index                        # https://tinyurl.com/y7pjxqd6
#  -------------------------------------------------------------------------
iEven  <- seq(6, 20, 2)
iOdd   <- seq(7, 53, 2)
paste(as.character(iOdd), collapse = ", ")
dt_osfa_term[, ..iOdd]
# -------------------------------------------------------------------------
dt01_aid_coa_non <- as.numeric(                                                 # https://tinyurl.com/y97odeef
  gsub(",", "",                                                                 # replace $, , wiht blanks
    gsub("\\.", "",
      gsub("\\$", "",
  dt01_aid_coa_non$NonResident))))

dt[,`:=`(avg=mean(mpg), med=median(mpg), min=min(mpg)), by=cyl]                 # data.table add multiple columns with := in one statement  https://tinyurl.com/y68ok9mc
data.table(geoID.UrbanRural, key = 'geoid')                                     # data.table another way to set a key
geoID.UrbanRural[jctzipfip, on = c("geoid", "fip")

df <- mydata[ -c(1,3:4) ]                                                       # data.table delete column by index reference               https://tinyurl.com/ybczklhq
cw[, weightKg := weight/1000]                                                   # data.table add column
DT[,w:=1:3]                                                                     # data.table add a column named "w"
cw[, Diet := paste0("Diet_", Diet)]                                             # data.table modify column
cw[, .(Chick, Time, Diet, weightKg)]                                            # data.table select columns
setnames(cw, c("Diet", "weight"), c("Group", "Weight"))                         # data.table rename columns
names(data)[3]<-"new_name"                                                      # data.table rename columns
setcolorder(cw, c(4,1:3))                                                       # data.table reorder columns                                https://tinyurl.com/y5dquu3m
cw[Time == 21 & Weight > 300]                                                   # data.table select rows
cw[order(Weight)]                                                               # data.table sort
setkey(cw, Chick, Time)                                                         # data.table key setting a key                              https://tinyurl.com/yxqsjfbu
DT[J("a",3:6)]                                                                  # data.table inner join (J is an alias of data.table)       https://tinyurl.com/y99q5se6
dt[is.na(dt)] <- 0                                                              # data.table replace na's with zero                         https://tinyurl.com/yxqtjuq3
x[, .SD, .SDcols = sapply(x, is.numeric)]                                       # data.table find numeric columns                           https://tinyurl.com/y36gdfhv

breaks <- c(0,18,25,35,45,65,Inf)                                               # data.table group by custom range                          https://tinyurl.com/y2nje6qb
DT[,list(mean=mean(value)),by=list(age=cut(age,breaks=breaks))][order(age)]
# data.table chaining chain commands
cw[Time %in% c(0,21)][                                                          # i: select rows
, Weight := weight][                                                            # j: mutate
, Group := factor(paste0("Diet_", Diet))][
, .(Chick,Group,Time,Weight)][                                                  # j: arrange
order(Chick,Time)][                                                             # i: order
1:5]                                                                            # i: subset

# Chick Group Time Weight
# 1: 1 Diet_1   0      42
# 2: 1 Diet_1  21     205
# 3: 2 Diet_1   0      40
# 4: 2 Diet_1  21     215
# 5: 3 Diet_1   0      43

# Summary Statistics                                                            number of observations and the mean of weight grouped by diet and time.
cw[, .(N = .N,                                                                  # .N is nb per group
Mean = mean(weight)),                                                           # compute mean
by=.(Diet, Time)][                                                              # group by Diet + Time
1:5]                                                                            # display rows 1 to 5

# calculate the standard deviation, median, minimum and maximum values—only at days 0 and 21
cws <-

# data.table format the numeric values as text
cws[, Mean_SD := paste0(format(Mean,digits=1),
" (",
format(SDev,digits=2),
")")]
cws[, Range := paste(Min, "-", Max)]
prettySum <- cws[ , .(Diet, Time, N, Mean_SD,
Median, Range)][
order(Diet, Time)]
# ------------------------------------------------------------------------------
# R data.table cube generate subtotal               https://tinyurl.com/yckph6mv       
# Subtraction to combine two R data.tables by matching rows
# https://tinyurl.com/ywnr49h9
# ------------------------------------------------------------------------------
datapasta::dt_paste(head(dt.opis_gas_vol_curr,10))
# ------------------------------------------------------------------------------
dt.opis_gas_vol_curr <- data.table::data.table(
        Week.Ending = c("2022-10-08","2022-10-08",
                        "2022-10-08","2022-10-08","2022-10-08","2022-10-08",
                        "2022-10-08","2022-10-08","2022-10-08","2022-10-08"),
       Time.Summary = c("Weekly","Weekly","Weekly",
                        "Weekly","Weekly","Weekly","Weekly","Weekly",
                        "Weekly","Weekly"),
         Station.ID = c(1, 2, 3, 4, 6, 8, 9, 11, 12, 13),
       Station.Name = c("PTC","PTC","PTC","PTC",
                        "PTC","PTC","PTC","PTC","PTC","PTC"),
            ADDRESS = c("5868 NITTANY VALLEY DRIVE",
                        "2246 OH-45","1150 NORTH CANFIELD-NILES ROAD",
                        "39115 COLORADO ROAD","61700 SOUTHGATE ROAD","25600 US-23",
                        "6830 FRANKLIN-LEBANON ROAD","10920 MARKET STREET",
                        "3430 LIBBEY ROAD","8924 LAKE ROAD"),
               CITY = c("MILL HALL","AUSTINBURG",
                        "AUSTINTOWN","AVON","CAMBRIDGE","CIRCLEVILLE",
                        "FRANKLIN","NORTH LIMA","PERRYSBURG","SEVILLE"),
              STATE = c("PA","OH","OH","OH","OH",
                        "OH","OH","OH","OH","OH"),
           Zip.Code = c(17751,44010,44515,44011,
                        43725,43113,45005,44452,43551,44273),
            COUNTRY = c("US","US","US","US","US",
                        "US","US","US","US","US"),
  Weekly.UNL.Volume = c(40926,55840,17560,15802,
                        29212,20827,34820,13691,18918,31762),
  Weekly.MID.Volume = c(1903, 1736, 584, 650, 1322, 740, 1018, 244, 554, 867),
  Weekly.PRE.Volume = c(2733, 2263, 841, 792, 1617, 741, 1512, 395, 589, 1120)
)
# ------------------------------------------------------------------------------
datapasta::dt_paste(head(dt.opis_gas_vol_prev,10))
# ------------------------------------------------------------------------------
dt.opis_gas_vol_curr <- data.table::data.table(
        Week.Ending = c("2022-10-01","2022-10-01",
                        "2022-10-01","2022-10-01","2022-10-01","2022-10-01",
                        "2022-10-01","2022-10-01","2022-10-01","2022-10-01"),
       Time.Summary = c("Weekly","Weekly","Weekly",
                        "Weekly","Weekly","Weekly","Weekly","Weekly",
                        "Weekly","Weekly"),
         Station.ID = c(1, 2, 3, 4, 6, 8, 9, 11, 12, 13),
       Station.Name = c("PTC","PTC","PTC","PTC",
                        "PTC","PTC","PTC","PTC","PTC","PTC"),
            ADDRESS = c("5868 NITTANY VALLEY DRIVE",
                        "2246 OH-45","1150 NORTH CANFIELD-NILES ROAD",
                        "39115 COLORADO ROAD","61700 SOUTHGATE ROAD","25600 US-23",
                        "6830 FRANKLIN-LEBANON ROAD","10920 MARKET STREET",
                        "3430 LIBBEY ROAD","8924 LAKE ROAD"),
               CITY = c("MILL HALL","AUSTINBURG",
                        "AUSTINTOWN","AVON","CAMBRIDGE","CIRCLEVILLE",
                        "FRANKLIN","NORTH LIMA","PERRYSBURG","SEVILLE"),
              STATE = c("PA","OH","OH","OH","OH",
                        "OH","OH","OH","OH","OH"),
           Zip.Code = c(17751,44010,44515,44011,
                        43725,43113,45005,44452,43551,44273),
            COUNTRY = c("US","US","US","US","US",
                        "US","US","US","US","US"),
  Weekly.UNL.Volume = c(42312,53012,18078,13422,
                        30744,23841,31950,14473,17716,33368),
  Weekly.MID.Volume = c(1704, 1675, 616, 754, 1143, 732, 1244, 353, 526, 886),
  Weekly.PRE.Volume = c(2544, 2108, 812, 657, 1249, 1010, 1288, 691, 760, 1606)
)
# ------------------------------------------------------------------------------
dt_curr_sub <- cube(dt.opis_gas_vol_curr,
                    j = lapply(.SD, sum),
                    by = c("CITY","STATE"),
                    id=TRUE, .SDcols=10:12)
dt_prev_sub <- cube(dt.opis_gas_vol_prev,
                    j = lapply(.SD, sum),
                    by = c("CITY","STATE"),
                    id=TRUE, .SDcols=10:12)
# ------------------------------------------------------------------------------
dt_diff <- dt_prev_sub[dt_curr_sub, nomatch = 0][
  , .(grouping, CITY, STATE,
      UNL = Weekly.UNL.Volume - i.Weekly.UNL.Volume,
      MID = Weekly.MID.Volume - i.Weekly.MID.Volume,
      PRE = Weekly.PRE.Volume - i.Weekly.PRE.Volume)]

# janitor package                                                               https://garthtarr.github.io/meatR/janitor.html
tabyl(cw, Diet)                                                                 # janitor summarize
janitor::adorn_totals(mydf, where = "col")                                      # janitor Add a total column to a data frame
janitor::adorn_totals(mydf)                                                     # janitor Add a total row to a data frame
x %>%                                                                           # # row and column totals
  tabyl(meat_colour, plant) %>%
  adorn_totals(where = c("row","col"))

 x %>%                                                                          # convert to percentage
  tabyl(meat_colour, plant) %>%
  adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0)

  x %>%                                                                         # add counts
  tabyl(meat_colour, plant) %>%
  adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front")

# add_row ---------------------------------                                     # https://tibble.tidyverse.org/reference/add_row.html
df <- tibble(x = 1:3, y = 3:1)
add_row(df, x = 4, y = 0)

# Replace NA's with zero in “xts” “zoo” object in R                             https://tinyurl.com/y8gosabx
NEW_OBJECT <- na.fill(MY_OBJECT, fill = 0.00))

# purrr
fafsa.profile %>% select_if(is.numeric)                                         # purrr find numeric columns
