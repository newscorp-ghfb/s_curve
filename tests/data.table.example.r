################################################################################
## Break data.table chain into two lines of code for readability             ### https://is.gd/6RFsyK
## Regex multiple values                                                     ### https://is.gd/WxzP0S  Exclude Graduate School College %like% "^B.*Inst$|^VP.*|^G")
################################################################################
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
################################################################################ 
# data.table left join merge / class=TRUE
dt00_emp.addr_all <- merge(dt00_emp.addr_mail,dt00_emp.addr_home, all=TRUE)[, c(1,10:13,4:7)]
################################################################################
# Conditional data.table if/else in R               https://tinyurl.com/h8x9cx3f
################################################################################
dt00_emp.form_answers[dt00_emp.person,  on = .(id_uga), date := data.table::fifelse(id_questions == 19, date_last, date)]
################################################################################
# Conditional merge/replacement/replace in R       https://tinyurl.com/dp3rwtxc
################################################################################
df1
x1  x2
1   a
2   b
3   c
4   d

df2
x1  x2
2   zz
3   qq

dt.X202105_pp_inactive_b[dt.X202105_pp_inactive_a, on = .(UserEmail), nomatch = 0]         # data.table join merge 

df1 <- data.frame(x1=1:4,x2=letters[1:4],stringsAsFactors=FALSE)
df2 <- data.frame(x1=2:3,x2=c("zz","qq"),stringsAsFactors=FALSE)

library(data.table)
setDT(df1); setDT(df2)
df1[df2, on = .(x1), x2 := i.x2]

df1[df2, on=c(id_mgr="OSFA.ID"), id_mgr := i.id_uga, all = TRUE]                # data.table left_join update column

dt00_emp.person[dt00_emp.mgr, on = .(id_mgr = id_osfa), id_mgr := i.id_uga]
merge.data.table(dt00_emp.person,dt00_emp.mgr, by.x="id_mgr", by.y="OSFA.ID", all.x = TRUE)

df1
x1  x2
1   a
2   zz
3   qq
4   d
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
dt.opis_gas_vol_prev <- data.table::data.table(
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
glimpse(dt_prev_sub)
1  $ Week.Ending       <chr> "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-01", "2022-10-~
2  $ Time.Summary      <chr> "Weekly", "Weekly", "Weekly", "Weekly", "Weekly", "Weekly", "Weekly", "Weekly", "Weekly", "Weekly"
3  $ Station.ID        <dbl> 1, 2, 3, 4, 6, 8, 9, 11, 12, 13
4  $ Station.Name      <chr> "PTC", "PTC", "PTC", "PTC", "PTC", "PTC", "PTC", "PTC", "PTC", "PTC"
5  $ ADDRESS           <chr> "5868 NITTANY VALLEY DRIVE", "2246 OH-45", "1150 NORTH CANFIELD-NILES ROAD", "39115 COLORADO ROAD", "61700 SOUTHGATE ROAD~
6  $ CITY              <chr> "MILL HALL", "AUSTINBURG", "AUSTINTOWN", "AVON", "CAMBRIDGE", "CIRCLEVILLE", "FRANKLIN", "NORTH LIMA", "PERRYSBURG", "SEV~
7  $ STATE             <chr> "PA", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH"
8  $ Zip.Code          <dbl> 17751, 44010, 44515, 44011, 43725, 43113, 45005, 44452, 43551, 44273
9  $ COUNTRY           <chr> "US", "US", "US", "US", "US", "US", "US", "US", "US", "US"
10 $ Weekly.UNL.Volume <dbl> 42312, 53012, 18078, 13422, 30744, 23841, 31950, 14473, 17716, 33368
11 $ Weekly.MID.Volume <dbl> 1704, 1675, 616, 754, 1143, 732, 1244, 353, 526, 886
12 $ Weekly.PRE.Volume <dbl> 2544, 2108, 812, 657, 1249, 1010, 1288, 691, 760, 1606
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

#------------------------------------------------------------------------------
# Sample random rows within each group in a data.table
# https://tinyurl.com/yckydpp6
#------------------------------------------------------------------------------
DT[ DT[, sample(.N, 3), by=a], b[i.V1], on="a", by=.EACHI]
dt.fla.tampa <- dt.fla[,.SD[sample(.N,min(.N,2))],by = as.ITime(dt.fla$date, format = "%H")][County == 'Hillsborough']
#------------------------------------------------------------------------------
# sample size by minimum cell size
# https://tinyurl.com/yc5vasnd
#------------------------------------------------------------------------------
#This follows your description of how to do it step-by-step.
library(data.table)
setDT(data)
data[, N := .N, .(SCHOOL, GRADE)]
data[, N := min(N), GRADE]
data[, .(SCORE = sample(SCORE, N)), .(SCHOOL, GRADE, N)][, -'N']
#------------------------------------------------------------------------------#-
# If you have multiple SCORE-like columns and you want keep the same rows from
# each then you can use .SD like in your attempt:
#------------------------------------------------------------------------------#-
data[, .SD[sample(.N, N)], .(SCHOOL, GRADE, N)][, -'N']
#------------------------------------------------------------------------------#-
# Convert data.frame columns from factors to characters
# https://tinyurl.com/3563sxku
#------------------------------------------------------------------------------#-
library(dplyr)
bob %>% mutate_if(is.factor, as.character) -> bob
#-----------------------------------------------------------------------------#
# data.table  get the first row of a data frame grouped by an identifier
# https://tinyurl.com/32pjz595
#-----------------------------------------------------------------------------#
setkey(t, ID)
t[, .SD[1,], by=ID]
#-----------------------------------------------------------------------------#
t<-na.omit(dt_sc[,c(1:9,26)])
setkey(t, opeid)
t[group=='a', .SD[1,], by=unitid]
#-----------------------------------------------------------------------------#
# anti-join  (don't forget setkey)                         


# Omit rows containing specific column of NA na.omit one column 1 column        https://tinyurl.com/19hq3dg3
DF %>% drop_na(y,z)

# Move a column to first position in a data frame / last column to first column https://tinyurl.com/54ecsxvx
# For the any column scenario, I needed to change 1:ncol(df)-1) to which(colnames(df) != "your_column_name_here") to get the remaining columns rather than always removing the last column, as suggested by @HT_079 below. –

df <- df[,c(ncol(df),1:ncol(df)-1)]

## move last column to first columm               https://tinyurl.com/bdf3rfy2
order(df, neworder = "I")

## data.table exclude last n columns
osfa.employee <- osfa.employee[,.SD, .SDcols = !c(ncol(osfa.employee)-3):ncol(osfa.employee)]

## data.table left join
osfa.employee <- merge(osfa.employee, dt00_emp_area, all.x=TRUE)

## data.table  ignore or exclude first column                                   https://tinyurl.com/y9l965w2
osfa.employee <- merge(osfa.employee, dt00_emp_lunch, all.x=TRUE)[,-1]

## data.table rename non-contiguous columns                                     https://tinyurl.com/y9ua8sq5
names(df)[c(2,ncol(df))]  <- c("name12", "name32")

# rename multiple columns by index                                              https://tinyurl.com/23vzcfjw
names(dtCOA)[2:11]<-paste0('ay', dtCOA[1,-1])

# dplyr left join                                                               https://tinyurl.com/5xmbwjcs
left_join(x, dt00_emp.race, by = c("Race" = "abbv"))

# Adding ' " ' quote inside a paste command in R                                https://is.gd/fLrvwI
# Escape " with backslash \\"
# ------------------------------------------------------------------------------
# unrelated dataframe cartesian join                https://tinyurl.com/4hk5knad
# ------------------------------------------------------------------------------
dt_A <- dt00_emp.person[active == 1,][, as.list(dt00_emp.form_questions), by = dt00_emp.person[active == 1]][ , c(2,22:27) ]
dt00_emp.person[active == 0,][, as.list(dt00_emp.form_questions), by = dt00_emp.person[active == 0]]
# -----------------------------------------------------------------------------
# Step 00.03.b cartesian join                      https://tinyurl.com/ysy8jpen
# -----------------------------------------------------------------------------
dt_folder_copy_from <- dt_project_template[,as.list(dt_subfolder),by=dt_project_template]
# ------------------------------------------------------------------------------
# compare two dataframes                            https://tinyurl.com/b5fx66tb
# ------------------------------------------------------------------------------
library(dompareDF)
library(daff)
library(diffobj)
library(waldo)
# ------------------------------------------------------------------------------
df1 <- data.frame(a = 1:5, b=letters[1:5])
df2 <- data.frame(a = 1:3, b=letters[1:3])
# ------------------------------------------------------------------------------
df_compare = compare_df(df1, df2, "row")
diff_data(data_ref = a2,data = a1)
render_diff(diff_data(data_ref = a2, data = a1))
diffPrint(a1, a2)
diffObj(a1, a2)
# ------------------------------------------------------------------------------
# fuzzy logic join                            https://github.com/arcruz0/inexact
# ------------------------------------------------------------------------------
inexact::inexact_join(dt00_emp.position, dt00_emp.position_new, by = 'Position.Number', method = 'osa', mode = 'left')
# ------------------------------------------------------------------------------
# Find Duplicates                                   https://tinyurl.com/5jtbcwxm
# ------------------------------------------------------------------------------
myDT <- fread(
"id,fB,fC
 1, b1,c1
 2, b2,c2
 3, b1,c1
 4, b3,c3
 5, b1,c1
")
# ------------------------------------------------------------------------------# ------------------------------------------------------------------------------
setkeyv(myDT, c('fB', 'fC'))                                                    # IMPORTANT - need to setkey
# ------------------------------------------------------------------------------# Method 1
myDT[, fD := .N > 1, by = key(myDT)]
# ------------------------------------------------------------------------------# Method 2 - You can explicitly call duplicated.data.frame....
myDT[,fD := duplicated.data.frame(.SD)|duplicated.data.frame(.SD, fromLast=TRUE),
  .SDcols = key(myDT)
# ------------------------------------------------------------------------------
# Select subset of columns in data.table R                                      https://tinyurl.com/4tf8a7sn
# ------------------------------------------------------------------------------
cols = grep("Train", names(osfa.employee), value = TRUE)
osfa.employee[, ..cols]
# ------------------------------------------------------------------------------
iris_DT[, .SD, .SDcols = patterns(".e.al")]                                     https://tinyurl.com/27ax4tjn

# ------------------------------------------------------------------------------
# Rounding selected columns of data.table in R                                  https://tinyurl.com/ydd2e6s8
# ------------------------------------------------------------------------------
mydf[, lapply(.SD, round, 1), vch1]
mydf[, lapply(.SD, round, digits = 1), by = vch1]
mydf[, 1:2 := lapply(.SD, round, digits = 1), by = vch1]
mydf[, lapply(.SD, round, digits = 1), by = vch1, .SDcols = "vnum1"]
# ------------------------------------------------------------------------------ .SDcols
# .SDcols = can be supplied with column name or it's number,
# as a single column by name  .SDcols = "vnum1" or by number .SDcols = 1
# as a multi columns by names .SDcols = c("vnum2", "vnum1") or by numbers .SDcols = c(2, 1)
# as a columns range by names .SDcols = vnum1:vnum2 or by numbers.SDcols = 1:2
# ------------------------------------------------------------------------------
# R data.table generate unique values of each column https://tinyurl.com/msmwcf9x
# ------------------------------------------------------------------------------
library(data.table)
library(lubridate)
sourceDT <- data.table(
               ID = c(1,2,3,4),
               date = c(ymd("20110101"),ymd("20110101"),ymd("20130101"),ymd("20150101")),
               text = c("A","B","C","C")
               )

for (i in seq_along(sourceDT)) {
  dupes <- which(duplicated(sourceDT[[i]]))
  if (length(dupes > 0)) {
    set(sourceDT, dupes, i, NA)
  }
}
# ------------------------------------------------------------------------------https://tinyurl.com/y52o5car
# Find Duplicates                                                               https://tinyurl.com/2hkqgstd
# ------------------------------------------------------------------------------https://tinyurl.com/y52o5car
duplicated(df)                                                                  # Is each row a repeat?
df[duplicated(df),]                                                             # Show the repeat entries
unique(df[duplicated(df),])                                                     # Show unique repeat entries (row names may differ, but values are the same)
unique(df)                                                                      # Original data with repeats removed. These do the same:
df[!duplicated(df),]                                                            https://tinyurl.com/yvwr97eb
dups <-voc.dups[duplicated(voc.dups$Position.Number)|duplicated(voc.dups$Position.Number, fromLast=TRUE),]
# ------------------------------------------------------------------------------
# Unique Rows of Data Frame Based On Selected Columns 
# i.e. Removing Rows Duplicated in Certain Variables https://tinyurl.com/yenmy6hb
# ------------------------------------------------------------------------------
data[!duplicated(data[ , c("id1", "id2")]), ]  # Delete duplicate rows
dt_volume_files[wkyy == wkyr, ][!duplicated(dt_volume_files[wkyy == wkyr, c("company", "wkyy")]), ]
# ------------------------------------------------------------------------------https://tinyurl.com/y52o5car
# data.table pct change same column
# dt[ , `percentage(counts)` := `sum(count)` / sum( `sum(count)` ) * 100 , by = "x" ]
# ------------------------------------------------------------------------------https://tinyurl.com/y259at76
dt00_emp_area           <- dt00_emp_area[, id := .I][,c(2:1)]                   # row number to column id
# ------------------------------------------------------------------------------https://tinyurl.com/yc7lohmk
# dynamically select column by index number                                     https://is.gd/G1c2PS
# format(dt_hz_tot[,1], justify = "left")
# format(dt_hz_tot, justify = "left")
# dt_hz_tot[,2    := lapply(.SD, function(x) format(x, big.mark= ",", scientific = F)), .SDcols = 2]
# dt_hz_tot[,3    := lapply(.SD, function(x) format_dol_fun(dt_hz_tot$amt)), .SDcols = 3]
# dt_hz_tot[,4:5  := lapply(.SD, function(x) paste0(round(x,2), '%')), .SDcols = 4:5]
# format(dt_hz_tot[,2:5], justify = "right")
# ------------------------------------------------------------------------------https://is.gd/cyYkaA
# search / filter column names and change to currency
dt[ ,colnames(dt) %like% 'Amount', with=FALSE]               # data.table way
dt %>% mutate_at(vars(matches("_Amount")), dollar)           # dplyr method
# ------------------------------------------------------------------------------
# How to divide each value in an R data frame by 100? https://tinyurl.com/24fd4xu3
# ------------------------------------------------------------------------------
dt_data[,3]/100
df1[,1:ncol(df1)]/100
# ------------------------------------------------------------------------------
# data.table NOT LIKE                               https://tinyurl.com/5h4adb8p
# Negate %like% in Data.Table Package | R-Data.Table
# ------------------------------------------------------------------------------
dt_super_admin[!UserEmail %like% 'opis',]
dt_super_admin[!UserEmail %like% 'opis' & !UserEmail %like% 'ihs'  & !UserEmail %like% 'gbb',]
# ------------------------------------------------------------------------------https://tinyurl.com/y8l7njjv
# A and B are the "id" variables within which the
#   "data" variables C and D vary meaningfully
DT = data.table(A = rep(1:3, each = 5), B = rep(1:5, 3),
                C = sample(15), D = sample(15))
DT
#     A B  C  D
#  1: 1 1 14 11
#  2: 1 2  3  8
#  3: 1 3 15  1
#  4: 1 4  1 14
#  5: 1 5  5  9
#  6: 2 1  7 13
#  7: 2 2  2 12
#  8: 2 3  8  6
#  9: 2 4  9 15
# 10: 2 5  4  3
# 11: 3 1  6  5
# 12: 3 2 12 10
# 13: 3 3 10  4
# 14: 3 4 13  7
# 15: 3 5 11  2
Compare the following:
#Sum all columns
DT[ , lapply(.SD, sum)]
#     A  B   C   D
# 1: 30 45 120 120

#Sum all columns EXCEPT A, grouping BY A
DT[ , lapply(.SD, sum), by = A]
#    A  B  C  D
# 1: 1 15 38 43
# 2: 2 15 30 49
# 3: 3 15 52 28

#Sum all columns EXCEPT A
DT[ , lapply(.SD, sum), .SDcols = !"A"]
#     B   C   D
# 1: 45 120 120

#Sum all columns EXCEPT A, grouping BY B
DT[ , lapply(.SD, sum), by = B, .SDcols = !"A"]

# ------------------------------------------------------------------------------
# use first row data as column names in r           https://tinyurl.com/ya3v4edm
# ------------------------------------------------------------------------------Janitor
library(janitor)
x <- data.frame(X_1 = c(NA, "Title", 1:3),
           X_2 = c(NA, "Title2", 4:6))
x %>%
  row_to_names(row_number = 2)
# ------------------------------------------------------------------------------data.table way
setnames(t1, as.character(t1[1,]))
t1 <- t1[-1,]
# ------------------------------------------------------------------------------
# How to convert a data frame column to numeric type? https://tinyurl.com/yaq7ky6n
dt12_pell_tbl[, c(6:9)] <- lapply(dt12_pell_tbl[, c(6:9)], as.integer)

# Select last n columns from a data.table     https://is.gd/yqyual
x[,(ncol(x)-3):ncol(x)]

# Get the  of a data table                https://tinyurl.com/rf8o7fu
library(data.table)
iris.dt <- data.table(iris)
iris.dt[, ncol(iris.dt), with = FALSE]

# Update several columns in a data.table      https://tinyurl.com/vjwfj73
vars <- names(X04aid.sources.long) %>% .[grepl("pct_ug", .)]
X04aid.sources.long[, (vars) := .SD * 100, .SDcols = vars]

#  set several columns as the key in data.table
# https://tinyurl.com/w2ng9pj
# three simple alternatives:
# 1
setkeyv(dt, c("A","B","C"))

# 2
keycols = c("A","B","C")
setkeyv(dt, keycols)

# 3, or you can setkey for the whole data.table
setkey(data)

setkeyv(z, c("id","i.id","i.id.1"))

# ------------------------------------------------------------------------------Percent Change Bar chart
dt_hope_awd         <- cbind(X06hope.amt.wide[,1],                              # pct change
                             X06hope.amt.wide[,..last_col - 5],
                             X06hope.amt.wide[,..last_col])
# percentage-change-between-two-columns-same-row                                https://tinyurl.com/vv6wvah
dt_hope_awd$pct_chg <- apply(dt_hope_awd[,c(1:2)], 1, function(x) { (x[1]-x[2])/x[2] * 100 })
# -----------------------------------------------------------------------------
# Calculate percent of column in R                                              https://tinyurl.com/yxm3nr3n
dt12_pell_tbl = mutate(dt12_pell_tbl,
                pell_pct = pell_recipients/ugds,
                pell_avg = pell_disbursements/pell_recipients)
# ------------------------------------------------------------------------------
# Counting rows by group                            https://tinyurl.com/yckncjku
# ------------------------------------------------------------------------------
dt_client_active_cnt <- dt_pp_active[, .N, by=.(ClientName)]                
# -----------------------------------------------------------------------------
# Advanced tips and tricks with data.table
#  https://is.gd/GTxELj
#  Using shift for to lead/lag vectors and lists
# Note this feature is only available in version 1.9.5 (currently on Github, not CRAN)
# Base R surprisingly does not have great tools for dealing with leads/lags of vectors that most social science
# statistical software (Stata, SAS, even FAME which I used in my formative data years) come equipped with out of the box.
dt <- data.table(mtcars)[,.(mpg, cyl)]
dt[,mpg_lag1:=shift(mpg, 1)]
dt[,mpg_forward1:=shift(mpg, 1, type='lead')]
head(dt)


# data.table pivot table                                                        https://is.gd/zo00t5
dtEMApct <- data.table::cube(trendEMA,.(tradeDay = sum(tradeDays)),by = c("catName"))
dtEMApct <- head(dtEMApct[, pctDays := tradeDay/tail( dtEMApct$tradeDay, 1)],-1)

#[Use data.table to calculate the percentage of occurrence depending on the category in another column](https://is.gd/2Jo60h)
setDT(DT)[ order(event), .(event = unique(event), percentage = tabulate(event)/.N), by = group_ind]

DF = data.frame(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)

# basic row subset operations
DT[2]                          # 2nd row
   x y v
1: b 3 2

DT[3:2]                        # 3rd and 2nd row
   x y v
1: b 6 3
2: b 3 2

DT[order(x)]                   # no need for order(DT$x)
   x y v
1: a 1 4
2: a 3 5
3: a 6 6
4: b 1 1
5: b 3 2
6: b 6 3
7: c 1 7
8: c 3 8
9: c 6 9

DT[order(x), ]                 # same as above. The ',' is optional
   x y v
1: a 1 4
2: a 3 5
3: a 6 6
4: b 1 1
5: b 3 2
6: b 6 3
7: c 1 7
8: c 3 8
9: c 6 9

DT[y>2]                        # all rows where DT$y > 2
   x y v
1: b 3 2
2: b 6 3
3: a 3 5
4: a 6 6
5: c 3 8
6: c 6 9

DT[y>2 & v>5]                  # compound logical expressions
   x y v
1: a 6 6
2: c 3 8
3: c 6 9

DT[!2:4]                       # all rows other than 2:4
   x y v
1: b 1 1
2: a 3 5
3: a 6 6
4: c 1 7
5: c 3 8
6: c 6 9

DT[-(2:4)]                     # same
   x y v
1: b 1 1
2: a 3 5
3: a 6 6
4: c 1 7
5: c 3 8
6: c 6 9

# select|compute columns data.table way
DT[, v]                        # v column (as vector)
[1] 1 2 3 4 5 6 7 8 9

DT[, list(v)]                  # v column (as data.table)
   v
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9

DT[, .(v)]                     # same as above, .() is a shorthand alias to list()
   v
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9

DT[, sum(v)]                   # sum of column v, returned as vector
[1] 45

DT[, .(sum(v))]                # same, but return data.table (column autonamed V1)
   V1
1: 45

DT[, .(sv=sum(v))]             # same, but column named "sv"
   sv
1: 45

DT[, .(v, v*2)]                # return two column data.table, v and v*2
   v V2
1: 1  2
2: 2  4
3: 3  6
4: 4  8
5: 5 10
6: 6 12
7: 7 14
8: 8 16
9: 9 18

# subset rows and select|compute data.table way
DT[2:3, sum(v)]                # sum(v) over rows 2 and 3, return vector
[1] 5

DT[2:3, .(sum(v))]             # same, but return data.table with column V1
   V1
1:  5

DT[2:3, .(sv=sum(v))]          # same, but return data.table with column sv
   sv
1:  5

DT[2:5, cat(v, "\n")]          # just for j's side effect
2 3 4 5
NULL

# select columns the data.frame way
DT[, 2]                        # 2nd column, returns a data.table always
   y
1: 1
2: 3
3: 6
4: 1
5: 3
6: 6
7: 1
8: 3
9: 6

colNum = 2                     # to refer vars in `j` from the outside of data use `..` prefix

DT[, ..colNum]                 # same, equivalent to DT[, .SD, .SDcols=colNum]
   y
1: 1
2: 3
3: 6
4: 1
5: 3
6: 6
7: 1
8: 3
9: 6

DT[["v"]]                      # same as DT[, v] but much faster
[1] 1 2 3 4 5 6 7 8 9

# grouping operations - j and by
DT[, sum(v), by=x]             # ad hoc by, order of groups preserved in result
   x V1
1: b  6
2: a 15
3: c 24

DT[, sum(v), keyby=x]          # same, but order the result on by cols
   x V1
1: a 15
2: b  6
3: c 24

DT[, sum(v), by=x][order(x)]   # same but by chaining expressions together
   x V1
1: a 15
2: b  6
3: c 24

# fast ad hoc row subsets (subsets as joins)
DT["a", on="x"]                # same as x == "a" but uses binary search (fast)
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

DT["a", on=.(x)]               # same, for convenience, no need to quote every column
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

DT[.("a"), on="x"]             # same
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

DT[x=="a"]                     # same, single "==" internally optimised to use binary search (fast)
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

DT[x!="b" | y!=3]              # not yet optimized, currently vector scan subset
   x y v
1: b 1 1
2: b 6 3
3: a 1 4
4: a 3 5
5: a 6 6
6: c 1 7
7: c 3 8
8: c 6 9

DT[.("b", 3), on=c("x", "y")]  # join on columns x,y of DT; uses binary search (fast)
   x y v
1: b 3 2

DT[.("b", 3), on=.(x, y)]      # same, but using on=.()
   x y v
1: b 3 2

DT[.("b", 1:2), on=c("x", "y")]             # no match returns NA
   x y  v
1: b 1  1
2: b 2 NA

DT[.("b", 1:2), on=.(x, y), nomatch=NULL]   # no match row is not returned
   x y v
1: b 1 1

DT[.("b", 1:2), on=c("x", "y"), roll=Inf]   # locf, nomatch row gets rolled by previous row
   x y v
1: b 1 1
2: b 2 1

DT[.("b", 1:2), on=.(x, y), roll=-Inf]      # nocb, nomatch row gets rolled by next row
   x y v
1: b 1 1
2: b 2 2

DT["b", sum(v*y), on="x"]                   # on rows where DT$x=="b", calculate sum(v*y)
[1] 25

# all together now
DT[x!="a", sum(v), by=x]                    # get sum(v) by "x" for each i != "a"
   x V1
1: b  6
2: c 24

DT[!"a", sum(v), by=.EACHI, on="x"]         # same, but using subsets-as-joins
   x V1
1: b  6
2: c 24

DT[c("b","c"), sum(v), by=.EACHI, on="x"]   # same
   x V1
1: b  6
2: c 24

DT[c("b","c"), sum(v), by=.EACHI, on=.(x)]  # same, using on=.()
   x V1
1: b  6
2: c 24

# joins as subsets
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))

X
   x v foo
1: c 8   4
2: b 7   2

DT[X, on="x"]                         # right join
   x y v i.v foo
1: c 1 7   8   4
2: c 3 8   8   4
3: c 6 9   8   4
4: b 1 1   7   2
5: b 3 2   7   2
6: b 6 3   7   2

X[DT, on="x"]                         # left join
   x  v foo y i.v
1: b  7   2 1   1
2: b  7   2 3   2
3: b  7   2 6   3
4: a NA  NA 1   4
5: a NA  NA 3   5
6: a NA  NA 6   6
7: c  8   4 1   7
8: c  8   4 3   8
9: c  8   4 6   9

DT[X, on="x", nomatch=NULL]           # inner join
   x y v i.v foo
1: c 1 7   8   4
2: c 3 8   8   4
3: c 6 9   8   4
4: b 1 1   7   2
5: b 3 2   7   2
6: b 6 3   7   2

DT[!X, on="x"]                        # not join
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

DT[X, on=c(y="v")]                    # join using column "y" of DT with column "v" of X
      x y  v i.x foo
1: <NA> 8 NA   c   4
2: <NA> 7 NA   b   2

DT[X, on="y==v"]                      # same as above (v1.9.8+)
      x y  v i.x foo
1: <NA> 8 NA   c   4
2: <NA> 7 NA   b   2

DT[X, on=.(y<=foo)]                   # NEW non-equi join (v1.9.8+)
   x y v i.x i.v
1: b 4 1   c   8
2: b 4 2   c   8
3: a 4 4   c   8
4: a 4 5   c   8
5: c 4 7   c   8
6: c 4 8   c   8
7: b 2 1   b   7
8: a 2 4   b   7
9: c 2 7   b   7

DT[X, on="y<=foo"]                    # same as above
   x y v i.x i.v
1: b 4 1   c   8
2: b 4 2   c   8
3: a 4 4   c   8
4: a 4 5   c   8
5: c 4 7   c   8
6: c 4 8   c   8
7: b 2 1   b   7
8: a 2 4   b   7
9: c 2 7   b   7

DT[X, on=c("y<=foo")]                 # same as above
   x y v i.x i.v
1: b 4 1   c   8
2: b 4 2   c   8
3: a 4 4   c   8
4: a 4 5   c   8
5: c 4 7   c   8
6: c 4 8   c   8
7: b 2 1   b   7
8: a 2 4   b   7
9: c 2 7   b   7

DT[X, on=.(y>=foo)]                   # NEW non-equi join (v1.9.8+)
   x y v i.x i.v
1: b 4 3   c   8
2: a 4 6   c   8
3: c 4 9   c   8
4: b 2 2   b   7
5: b 2 3   b   7
6: a 2 5   b   7
7: a 2 6   b   7
8: c 2 8   b   7
9: c 2 9   b   7

DT[X, on=.(x, y<=foo)]                # NEW non-equi join (v1.9.8+)
   x y v i.v
1: c 4 7   8
2: c 4 8   8
3: b 2 1   7

DT[X, .(x,y,x.y,v), on=.(x, y>=foo)]  # Select x's join columns as well
   x y x.y v
1: c 4   6 9
2: b 2   3 2
3: b 2   6 3

DT[X, on="x", mult="first"]           # first row of each group
   x y v i.v foo
1: c 1 7   8   4
2: b 1 1   7   2

DT[X, on="x", mult="last"]            # last row of each group
   x y v i.v foo
1: c 6 9   8   4
2: b 6 3   7   2

DT[X, sum(v), by=.EACHI, on="x"]      # join and eval j for each row in i
   x V1
1: c 24
2: b  6

DT[X, sum(v)*foo, by=.EACHI, on="x"]  # join inherited scope
   x V1
1: c 96
2: b 12

DT[X, sum(v)*i.v, by=.EACHI, on="x"]  # 'i,v' refers to X's v column
   x  V1
1: c 192
2: b  42

DT[X, on=.(x, v>=v), sum(y)*foo, by=.EACHI] # NEW non-equi join with by=.EACHI (v1.9.8+)
   x v V1
1: c 8 36
2: b 7 NA

# setting keys
kDT = copy(DT)                        # (deep) copy DT to kDT to work with it.

setkey(kDT,x)                         # set a 1-column key. No quotes, for convenience.

setkeyv(kDT,"x")                      # same (v in setkeyv stands for vector)

v="x"

setkeyv(kDT,v)                        # same

# key(kDT)<-"x"                       # copies whole table, please use set* functions instead
haskey(kDT)                           # TRUE
[1] TRUE

key(kDT)                              # "x"
[1] "x"

# fast *keyed* subsets
kDT["a"]                              # subset-as-join on *key* column 'x'
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

kDT["a", on="x"]                      # same, being explicit using 'on=' (preferred)
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

# all together
kDT[!"a", sum(v), by=.EACHI]          # get sum(v) for each i != "a"
   x V1
1: b  6
2: c 24

# multi-column key
setkey(kDT,x,y)                       # 2-column key

setkeyv(kDT,c("x","y"))               # same

# fast *keyed* subsets on multi-column key
kDT["a"]                              # join to 1st column of key
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

kDT["a", on="x"]                      # on= is optional, but is preferred
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

kDT[.("a")]                           # same, .() is an alias for list()
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

kDT[list("a")]                        # same
   x y v
1: a 1 4
2: a 3 5
3: a 6 6

kDT[.("a", 3)]                        # join to 2 columns
   x y v
1: a 3 5

kDT[.("a", 3:6)]                      # join 4 rows (2 missing)
   x y  v
1: a 3  5
2: a 4 NA
3: a 5 NA
4: a 6  6

kDT[.("a", 3:6), nomatch=NULL]        # remove missing
   x y v
1: a 3 5
2: a 6 6

kDT[.("a", 3:6), roll=TRUE]           # locf rolling join
   x y v
1: a 3 5
2: a 4 5
3: a 5 5
4: a 6 6

kDT[.("a", 3:6), roll=Inf]            # same as above
   x y v
1: a 3 5
2: a 4 5
3: a 5 5
4: a 6 6

kDT[.("a", 3:6), roll=-Inf]           # nocb rolling join
   x y v
1: a 3 5
2: a 4 6
3: a 5 6
4: a 6 6

kDT[!.("a")]                          # not join
   x y v
1: b 1 1
2: b 3 2
3: b 6 3
4: c 1 7
5: c 3 8
6: c 6 9

kDT[!"a"]                             # same
   x y v
1: b 1 1
2: b 3 2
3: b 6 3
4: c 1 7
5: c 3 8
6: c 6 9

# more on special symbols, see also ?"special-symbols"
DT[.N]                                  # last row
   x y v
1: c 6 9

DT[, .N]                                # total number of rows in DT
[1] 9

DT[, .N, by=x]                          # number of rows in each group
   x N
1: b 3
2: a 3
3: c 3

DT[, .SD, .SDcols=x:y]                  # select columns 'x' through 'y'
   x y
1: b 1
2: b 3
3: b 6
4: a 1
5: a 3
6: a 6
7: c 1
8: c 3
9: c 6

DT[ , .SD, .SDcols = !x:y]              # drop columns 'x' through 'y'
   v
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9

DT[ , .SD, .SDcols = patterns('^[xv]')] # select columns matching '^x' or '^v'
   x v
1: b 1
2: b 2
3: b 3
4: a 4
5: a 5
6: a 6
7: c 7
8: c 8
9: c 9

DT[, .SD[1]]                            # first row of all columns
   x y v
1: b 1 1

DT[, .SD[1], by=x]                      # first row of 'y' and 'v' for each group in 'x'
   x y v
1: b 1 1
2: a 1 4
3: c 1 7

DT[, c(.N, lapply(.SD, sum)), by=x]     # get rows *and* sum columns 'v' and 'y' by group
   x N  y  v
1: b 3 10  6
2: a 3 10 15
3: c 3 10 24

DT[, .I[1], by=x]                       # row number in DT corresponding to each group
   x V1
1: b  1
2: a  4
3: c  7

DT[, grp := .GRP, by=x]                 # add a group counter column
   x y v grp
1: b 1 1   1
2: b 3 2   1
3: b 6 3   1
4: a 1 4   2
5: a 3 5   2
6: a 6 6   2
7: c 1 7   3
8: c 3 8   3
9: c 6 9   3

X[, DT[.BY, y, on="x"], by=x]           # join within each group
   x V1
1: c  1
2: c  3
3: c  6
4: b  1
5: b  3
6: b  6

# add/update/delete by reference (see ?assign)
print(DT[, z:=42L])                   # add new column by reference
   x y v grp  z
1: b 1 1   1 42
2: b 3 2   1 42
3: b 6 3   1 42
4: a 1 4   2 42
5: a 3 5   2 42
6: a 6 6   2 42
7: c 1 7   3 42
8: c 3 8   3 42
9: c 6 9   3 42

print(DT[, z:=NULL])                  # remove column by reference
   x y v grp
1: b 1 1   1
2: b 3 2   1
3: b 6 3   1
4: a 1 4   2
5: a 3 5   2
6: a 6 6   2
7: c 1 7   3
8: c 3 8   3
9: c 6 9   3

print(DT["a", v:=42L, on="x"])        # subassign to existing v column by reference
   x y  v grp
1: b 1  1   1
2: b 3  2   1
3: b 6  3   1
4: a 1 42   2
5: a 3 42   2
6: a 6 42   2
7: c 1  7   3
8: c 3  8   3
9: c 6  9   3

print(DT["b", v2:=84L, on="x"])       # subassign to new column by reference (NA padded)
   x y  v grp v2
1: b 1  1   1 84
2: b 3  2   1 84
3: b 6  3   1 84
4: a 1 42   2 NA
5: a 3 42   2 NA
6: a 6 42   2 NA
7: c 1  7   3 NA
8: c 3  8   3 NA
9: c 6  9   3 NA

DT[, m:=mean(v), by=x][]              # add new column by reference by group
   x y  v grp v2  m
1: b 1  1   1 84  2
2: b 3  2   1 84  2
3: b 6  3   1 84  2
4: a 1 42   2 NA 42
5: a 3 42   2 NA 42
6: a 6 42   2 NA 42
7: c 1  7   3 NA  8
8: c 3  8   3 NA  8
9: c 6  9   3 NA  8

                                      # NB: postfix [] is shortcut to print()
# advanced usage
DT = data.table(x=rep(c("b","a","c"),each=3), v=c(1,1,1,2,2,1,1,2,2), y=c(1,3,6), a=1:9, b=9:1)

DT[, sum(v), by=.(y%%2)]              # expressions in by
   y V1
1: 1  9
2: 0  4

DT[, sum(v), by=.(bool = y%%2)]       # same, using a named list to change by column name
   bool V1
1:    1  9
2:    0  4

DT[, .SD[2], by=x]                    # get 2nd row of each group
   x v y a b
1: b 1 3 2 8
2: a 2 3 5 5
3: c 2 3 8 2

DT[, tail(.SD,2), by=x]               # last 2 rows of each group
   x v y a b
1: b 1 3 2 8
2: b 1 6 3 7
3: a 2 3 5 5
4: a 1 6 6 4
5: c 2 3 8 2
6: c 2 6 9 1

DT[, lapply(.SD, sum), by=x]          # sum of all (other) columns for each group
   x v  y  a  b
1: b 3 10  6 24
2: a 5 10 15 15
3: c 5 10 24  6

DT[, .SD[which.min(v)], by=x]         # nested query by group
   x v y a b
1: b 1 1 1 9
2: a 1 6 6 4
3: c 1 1 7 3

DT[, list(MySum=sum(v),
dt.tbl+           MyMin=min(v),
dt.tbl+           MyMax=max(v)),
dt.tbl+     by=.(x, y%%2)]                    # by 2 expressions
   x y MySum MyMin MyMax
1: b 1     2     1     1
2: b 0     1     1     1
3: a 1     4     2     2
4: a 0     1     1     1
5: c 1     3     1     2
6: c 0     2     2     2

DT[, .(a = .(a), b = .(b)), by=x]     # list columns
   x     a     b
1: b 1,2,3 9,8,7
2: a 4,5,6 6,5,4
3: c 7,8,9 3,2,1

DT[, .(seq = min(a):max(b)), by=x]    # j is not limited to just aggregations
    x seq
 1: b   1
 2: b   2
 3: b   3
 4: b   4
 5: b   5
 6: b   6
 7: b   7
 8: b   8
 9: b   9
10: a   4
11: a   5
12: a   6
13: c   7
14: c   6
15: c   5
16: c   4
17: c   3

DT[, sum(v), by=x][V1<20]             # compound query
   x V1
1: b  3
2: a  5
3: c  5

DT[, sum(v), by=x][order(-V1)]        # ordering results
   x V1
1: a  5
2: c  5
3: b  3

DT[, c(.N, lapply(.SD,sum)), by=x]    # get number of observations and sum per group
   x N v  y  a  b
1: b 3 3 10  6 24
2: a 3 5 10 15 15
3: c 3 5 10 24  6

DT[, {tmp <- mean(y);
dt.tbl+       .(a = a-tmp, b = b-tmp)
dt.tbl+       }, by=x]                        # anonymous lambda in 'j', j accepts any valid
   x       a       b
1: b -2.3333  5.6667
2: b -1.3333  4.6667
3: b -0.3333  3.6667
4: a  0.6667  2.6667
5: a  1.6667  1.6667
6: a  2.6667  0.6667
7: c  3.6667 -0.3333
8: c  4.6667 -1.3333
9: c  5.6667 -2.3333

                                      # expression. TO REMEMBER: every element of
                                      # the list becomes a column in result.
pdf("new.pdf")

DT[, plot(a,b), by=x]                 # can also plot in 'j'
Hit <Return> to see next plot:
Hit <Return> to see next plot:
Hit <Return> to see next plot:
Empty data.table (0 rows and 1 cols): x

dev.off()
null device
          1

# using rleid, get max(y) and min of all cols in .SDcols for each consecutive run of 'v'
DT[, c(.(y=max(y)), lapply(.SD, min)), by=rleid(v), .SDcols=v:b]
   rleid y v y a b
1:     1 6 1 1 1 7
2:     2 3 2 1 4 5
3:     3 6 1 1 6 3
4:     4 6 2 3 8 1

# Support guide and links:
# https://github.com/Rdatatable/data.table/wiki/Support
dt.tblR>
## Not run:
##D if (interactive()) {
##D   vignette("datatable-intro")
##D   vignette("datatable-reference-semantics")
##D   vignette("datatable-keys-fast-subset")
##D   vignette("datatable-secondary-indices-and-auto-indexing")
##D   vignette("datatable-reshape")
##D   vignette("datatable-faq")
##D
##D   test.data.table()          # over 6,000 low level tests
##D
##D   # keep up to date with latest stable version on CRAN
##D   update.packages()
##D
##D   # get the latest devel version
##D   update.dev.pkg()
##D   # compiled devel binary for Windows available -- no Rtools needed
##D   update.dev.pkg(repo="https://Rdatatable.github.io/data.table")
##D   # read more at:
##D   # https://github.com/Rdatatable/data.table/wiki/Installation
##D }
## End(Not run)
## https://github.com/Rdatatable/data.table/wiki
dt.tblR>
