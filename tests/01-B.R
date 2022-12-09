# Example preprocessing script.
# require(stats)

#https://stackoverflow.com/questions/49016063/how-to-import-most-recent-csv-file-into-rstudio
data_files <- file.info(Sys.glob("y:/Reports/Banner/Argos/Test/*.csv"))                                                                     
tbl.scholar<-read.csv(row.names(data_files)[which.max(data_files[["ctime"]])])                                                               #find the most recent version
#import .csv file
#tbl.scholar<-read.csv(row.names(data_files)[which.max(data_files[["ctime"]])], header=TRUE, sep=",", colClasses=c("Date", rep("character",2),"numeric","character", rep("numeric",8)))
#tbl.scholar$spriden_id  <- as.character(tbl.scholar$spriden_id)                                    #convert spriden to character
names(tbl.scholar)[3] <- "studentID"                                                                #rename to studentID
names(tbl.scholar)[6] <- "fundCode"                                                                 #rename to studentID
#tbl.scholar$SYSDATE <- as.Date(as.character(tbl.scholar$SYSDATE), "%m/%d/%Y")                      #convert SYSDATE to date only
tbl.scholar[ is.na(tbl.scholar) ] <- 0                                                              #replace na's with 0

# dup.DF(tbl.scholar, "cohort")

colNames <- c("ay_2223", "ay_2324")
#colNames <- list("ay_2223", "ay_2324")
#colNames <- paste0("ay_", c(ay$V1))
# z<-head(paste0("ay_", c(ay$V1)),4)
a<-as.character(rep(head(ay$V1,1),4))
b<-head(paste0("ay_", ay$V2),4)

tbl.scholar1 <- tbl.scholar
names(tbl.scholar1) <- tolower(names(tbl.scholar))
tbl.scholar1<-tbl.scholar1%>%mutate_cond(fixed("cohort", ignore_case = TRUE) == currentAY, ay_1819=ay_1718, ay_1920=ay_1718, ay_2021=ay_1718)
tbl.scholar1<-tbl.scholar1%>%mutate_cond(fixed("cohort", ignore_case = TRUE) == currentAY+101, ay_1920=ay_1819, ay_2021=ay_1819, ay_2122=ay_1819)

#tbl.scholar1<-dup.DF(tbl.scholar1, currentAY, 4, "ay_1718", "ay_2324")
#tbl.scholar1<-dup.DF(tbl.scholar1, currentAY, 4, "ay_1718", "ay_2324")
#tbl.scholar1<-dup.DF(tbl.scholar1, currentAY, 4, "ay_1718", colNames)                              #20180606b




#Dynamic Approach to propagate scholarship amount to the ensuing three years ####
st_pos <- 7                                                                                         #concerned column's start position in the given dataframe
df <- tbl.scholar                                                                                   #data backup
names(df)<-tolower(names(df))                                                                       #change column names to lower case since don't know how input will be formatted

#df<-df[c(1:6,15:27,7:14)]

#rename concerned columns as "ay_1718", "ay_1819" etc
#names(df)[st_pos:ncol(df)-4] <- paste("AY", paste0(as.numeric(substr(min(df$cohort), 1, 2)) + 0:(ncol(df) - st_pos),
#                                                 as.numeric(substr(min(df$cohort), 3, 4)) + 0:(ncol(df) - st_pos)), 
#                                    sep="_")

#copy "year" column's value to the ensuing three columns
cols <- names(df)[st_pos:(ncol(df)-13)]                                                              #renamed columns (-4) is the number of extraneous columns at the end of the data frame
mapply(function(x, y) 
  df[df$cohort == x & df$studentid == y, which(grepl(x, cols)) + (st_pos-1):(st_pos+2)] <<- 
    df[df$cohort == x & df$studentid == y, which(grepl(x, cols)) + (st_pos-1)],
  df$cohort, df$studentid)

#End of Dynamic Approach ####

#funcCode
fundCode <- c(tbl.Cancels[,9], tbl.scholarshipAwd[,4], tbl.scholarshipCohorts[,4], tbl.GaCommit[,3])   #concatenate fundCode columns
fundCode <- melt(fundCode)                                                                          #stack columns on top of each other
fundCode <- fundCode[,1]                                                                            #extract the first column only
fundCode <- as.data.table(fundCode)                                                                 #convert to a data.table
fundCode <- subset(fundCode, fundCode!=0)                                                           #remove rows containing zero
fundCode <- unique(fundCode)                                                                        #extract the unique values
fundCode <- setorder(fundCode, fundCode)                                                            #sort the fundCode column
fundCode <- fundCode %>% left_join(select(tbl.GaCommitmentFundList.OSFA, fundCode, fundName), by.x = "fundCode")
fundCode$fundName[is.na(fundCode$fundName)] <- ""                                                   #replace na's with blanks in fund name

#jctCode
jctCode <- c(tbl.scholarshipCohorts[,4], tbl.scholarshipCohorts[,3])                                            #concatenate jctCode columns
names(jctCode) <- c("fundCode", "osfaCode")                                                                     #rename columns
jctCode <- as.data.table(jctCode)                                                                               #convert to a data.table
jctCode <- subset(jctCode, fundCode!=0 & osfaCode !=0)                                                          #remove rows containing zero
jctCode <- unique(jctCode)                                                                                      #extract the unique values
jctCode <- setorder(jctCode, fundCode)                                                                          #sort the fundCode column
jctCode<-jctCode %>% left_join(select(fundCode, fundCode, fundName), by.x = "fundCode")                         #left_ join add column <- fundCode

#jctCohortCode
jctCohortCode <- c(tbl.BannerCohorts[,2], tbl.BannerCohorts[,5])                                                #concatenate Banner Cohort columns
names(jctCohortCode) <- c("studentID", "fundCode")                                                              #rename columns
jctCohortCode <- as.data.table(jctCohortCode)                                                                   #convert to a data.table
jctCohortCode <- subset(jctCohortCode, fundCode!=0 & studentID !=0)                                             #remove rows containing zero
jctCohortCode <- unique(jctCohortCode)                                                                          #extract the unique values
jctCohortCode <- setorder(jctCohortCode, studentID)                                                             #sort the studentID column

#osfaCode
osfaCode <- c(tbl.BannerCohorts[,5], tbl.scholarshipAwd[,3], tbl.scholarshipCohorts[,3])                        #concatenate fundCode columns
osfaCode <- melt(osfaCode)                                                                                      #stack columns on top of each other
osfaCode <- osfaCode[,1]                                                                                        #extract the first column only
names(osfaCode) <- c("osfaCode")                                                                                #rename columns
osfaCode <- as.data.table(osfaCode)                                                                             #convert to a data.table
osfaCode <- subset(osfaCode, osfaCode!=0)                                                                       #remove rows containing zero
osfaCode <- unique(osfaCode)                                                                                    #extract the unique values
osfaCode <- setorder(osfaCode, osfaCode)                                                                        #sort the osfaCode column

#studentID
studentID <- c(tbl.BannerCohorts[,2], tbl.Cancels[,5], tbl.scholarshipAwd[,5], tbl.scholarshipCohorts[,11])     #concatenate studentID columns
studentID <- melt(studentID)                                                                                    #stack columns on top of each other
studentID <- studentID[,1]                                                                                      #extract the first column only
studentID <- as.data.table(studentID)                                                                           #convert to a data.table
studentID <- subset(studentID, studentID!=0 & studentID!="")                                                    #remove rows containing zero
studentID <- unique(studentID)                                                                                  #extract the unique values
#studentID <- studentID[!apply(is.na(studentID) | studentID == "", 1, all),]                                    #remove rows containing blanks & na's
studentID <- setorder(studentID, studentID)                                                                     #sort the studentID column

#Join three Tables - use `nomatch` argument-> i.e., nomatch-0 nomatch is for an inner join          https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
tbl.CohortAwd<-tbl.BannerCohorts[tbl.scholarshipAwd,nomatch=0,on=c("studentID", "osfaCode")][tbl.scholarshipCohorts,nomatch=0,on=c("studentID", "osfaCode")]
# tbl.CohortAwd<-tbl.BannerCohorts[tbl.scholarshipAwd, nomatch=0, on=c("studentID", "osfaCode")]                #Join Banner Cohorts + scholarshipAwd tables
tbl.CohortAwd <- tbl.CohortAwd %>%
    mutate_all(funs(replace(., is.na(.), 0)))                                                                   #20180516b
as.numeric(as.character(tbl.CohortAwd$Award))                                                                   #covert CohortAwd$Award from character to numeric


tbl.CohortAwdByFY <- data.table(tbl.CohortAwd)[,.(FY, Award)]
tbl.CohortAwdByFY[,meanAwdByFY:=mean(Award), by=FY]

x<-tbl.CohortAwd %>% select(FY, Award) %>%
    group_by(FY) %>%
    dplyr::summarize(AvgAward = mean(Award),
                     MedianAward = as.numeric(median(Award)),
                     total = n()) %>%
    arrange(FY)

tbl.GaCommit <- tbl.CohortAwd %>% 
select(FY, osfaCode, fundCode, AccountName, Requirement, Active, Award, MajorChanges, ReplacementScholar, studentID, NAME, AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324,AY_2425, OSFA.Notes, Rural, FundAgreement, message, eMail, Special.Circumstances, SCR.Complete ) %>%
group_by(FY, osfaCode, studentID) %>%
mutate_all(funs(replace(., is.na(.), 0)))

tbl.fundBalance<-tbl.GaCommitmentFundList.OSFA %>% 
    group_by(fundCode, fundName) %>%
    select(fundCode, fundName, incomeBalance, principleBalance, invested.Balance) %>%
    summarize(incBal=sum(incomeBalance), principleBal=sum(principleBalance), invBal=sum(invested.Balance)) %>%
    mutate(totBalFund = incBal+principleBal+invBal) %>% 
    arrange(fundCode, fundName)

#tbl.fundBalance<-inner_join(tbl.fundBalance, jctCode)

tbl.BannerCohorts[ is.na(tbl.BannerCohorts) ] <- 0
tbl.BannerCohorts %>% 
    mutate_at(vars(AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425), as.numeric)

tbl.studentBalance<-tbl.BannerCohorts %>% 
    group_by(osfaCode) %>%
    select(osfaCode, AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425 ) %>%
    summarize(totBalStudent=sum(AY_1718, AY_1819, AY_1920, AY_2021, AY_2122, AY_2223, AY_2324, AY_2425)) %>%
    arrange(osfaCode)

#tbl.balance<- inner_join(tbl.fundBalance, tbl.studentBalance, by = "osfaCode")

# Totals preprocessing script.
tbl.fundSummary<-as.data.table(unique.data.frame(select(select(inner_join(tbl.fundBalance,jctCode, by='fundCode')%>%inner_join(., tbl.studentBalance, by='osfaCode'), -ends_with(".y")),osfaCode, fundCode:totBalFund, totBalStudent)%>%mutate(totBalRemain = totBalFund - totBalStudent)))


#convert tables to data.tables
fundCode<-data.table(fundCode)
jctCode<-data.table(jctCode)
tbl.CohortAwd<-data.table(tbl.CohortAwd)
tbl.fundBalance<-data.table(tbl.fundBalance)
tbl.GaCommit<-data.table(tbl.GaCommit)


#write output .csv files
write.csv(jctCode, "output/jct_Code.csv", row.names=F)
write.csv(fundCode, "output/tbl_fundCode.csv", row.names=F)
write.csv(jctCohortCode, "output/jct_CohortCode.csv", row.names=F)
write.csv(osfaCode, "output/tbl_osfaCode.csv", row.names=F)
write.csv(studentID, "output/tbl_studentId.csv", row.names=F)

#write.csv(tbl.balance, "output/tbl_balance.csv", row.names=F)
write.csv(tbl.CohortAwd, "output/tbl_CohortAwd.csv", row.names=F)
write.csv(tbl.fundBalance, "output/tbl_fundBalance.csv", row.names=F)
write.csv(tbl.fundSummary, "output/tbl_fundSummary.csv", row.names=F)
write.csv(tbl.GaCommit, "output/tbl_GaCommit.csv", row.names=F)
write.csv(tbl.scholar, "output/tbl_scholar.csv", row.names=F)
write.csv(df, "output/tbl_scholar2.csv", row.names=F)                                                                                       #output from the copy "year" column's value to the ensuing three columns process
write.csv(tbl.studentBalance, "output/tbl_studentBalance.csv", row.names=F)

# #write output .xlsx files
# # write.xlsx(fundCode, "output/tbl_fundCode.xlsx", row.names=F, sheetName="tblFundCode", append=FALSE)
# # write.xlsx(jctCode, "output/tbl_jctCode.xlsx", row.names=F, sheetName="tbl_jctCode")
# # write.xlsx(jctCohortCode, "output/tbl_jctCohortCode.xlsx", row.names=F, sheetName="tbl_jctCohortCode")
# # write.xlsx(osfaCode, "output/tbl_osfaCode.xlsx", row.names=F, sheetName="tbl_osfaCode")
# # write.xlsx(studentID, "output/tbl_studentID.xlsx", row.names=F, sheetName="tbl_studentID")

# # write.xlsx(as.data.frame(tbl.balance), "output/tbl_balance.xlsx", row.names=F, sheetName="tbl_balance")
# # write.xlsx(as.data.frame(tbl.BannerCohorts), "output/tbl_bannerCohorts.xlsx", row.names=F, sheetName="tbl_bannerCohorts")
# # write.xlsx(as.data.frame(tbl.CohortAwd), "output/tbl_cohortAwd.xlsx", row.names=F, sheetName="tbl_cohortAwd")
# # write.xlsx(as.data.frame(tbl.fundBalance), "output/tbl_fundBalance.xlsx", row.names=F, sheetName="tbl_fundBalance")
# # write.xlsx(as.data.frame(tbl.GaCommit), "output/tbl_GaCommit.xlsx", row.names=F, sheetName="tbl_GaCommit")
# # write.xlsx(as.data.frame(tbl.GaCommitmentFundList.OSFA), "output/tbl_GaCommitFundList.xlsx", row.names=F, sheetName="tbl_GaCommitFundList")
# # write.xlsx(tbl.scholar, "output/tbl_scholar.xlsx", row.names=F, sheetName="tbl_scholar")
# # write.xlsx(as.data.frame(tbl.studentBalance), "output/tbl_studentBalance.xlsx", row.names=F, sheetName="tbl_studentBalance")

# write.xlsx(jctCode, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_jctCode", append=FALSE)
# write.xlsx(fundCode, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tblFundCode", append=TRUE)
# write.xlsx(jctCohortCode, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_jctCohortCode", append=TRUE)
# write.xlsx(osfaCode, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_osfaCode", append=TRUE)
# write.xlsx(studentID, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_studentID", append=TRUE)
# write.xlsx(tbl.scholar, "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_scholar", append=TRUE)
# #write.xlsx(as.data.frame(tbl.balance), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_balance", append=TRUE)
# write.xlsx(as.data.frame(tbl.BannerCohorts), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_bannerCohorts", append=TRUE)
# write.xlsx(as.data.frame(tbl.CohortAwd), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_cohortAwd", append=TRUE)
# write.xlsx(as.data.frame(tbl.fundBalance), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_fundBalance", append=TRUE)
# write.xlsx(as.data.frame(tbl.GaCommit), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_GaCommit", append=TRUE)
# write.xlsx(as.data.frame(tbl.GaCommitmentFundList.OSFA), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_GaCommitFundList", append=TRUE)
# write.xlsx(as.data.frame(tbl.studentBalance), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl_studentBalance", append=TRUE)
# write.xlsx(as.data.frame(tbl.fundSummary), "output/tbl_scholarships.xlsx", row.names=F, sheetName="tbl.fundSummary", append=TRUE)

# write.xlsx(tbl.scholar, "output/tbl_scholar.xlsx", row.names=F, sheetName="tbl_scholar", append=FALSE)
# write.xlsx(tbl.scholar1, "output/tbl_scholar1.xlsx", row.names=F, sheetName="tbl_scholar1", append=FALSE)
# write.xlsx(df, "output/tbl_scholar2.xlsx", row.names=F, sheetName="tbl_scholar2", append=FALSE)

# RegEx
# ([\sI])\w+

# zfundcode <- fundCode
# as.data.table(zfundcode)
# zfundcode<-zfundcode[tbl.GaCommitmentFundList.OSFA,mult = "first",on = "fundCode", nomatch=0]
# zfundcode<-zfundcode[, c(1,4)]
# zfundcode<-zfundcode[order(fundCode, fundName)][, .SD[c(1)], by=fundCode]

# zfundcode$fundName<-gsub(pattern = "[IV|X]|\\(1\\)|\\(2\\)", "", zfundcode$fundName)
# zfundcode$fundName<-trim(zfundcode$fundName)
# zfundcode$fundName

# zfundcode < left_join(zfundcode,jctCode, by = "fundCode")

#PIPES
# jctCode  <- c(tbl.scholarshipCohorts[,3], tbl.scholarshipCohorts[,4])                                         #concatenate scholarship codes
# jctCodes <- jctCode %>% 
#     as.data.table(jctCode) %>%
#     setorder(jctCode, AccountNumber) %>%
#     unique(jctCode)

# tbl.BannerCohorts$studentID <- as.character(as.numeric(tbl.BannerCohorts$studentID))
# tbl.Cancels$studentID <- as.character(as.numeric(tbl.Cancels$studentID))
# tbl.scholarshipAwd$studentID <- as.character(as.numeric(tbl.scholarshipAwd$studentID))
# tbl.scholarshipCohorts$studentID <- as.character(as.numeric(tbl.scholarshipCohorts$studentID))

# tbl.Cancels$fundCode <- as.character(as.numeric(tbl.Cancels$fundCode))
# tbl.scholarshipAwd$fundCode <- as.character(as.numeric(tbl.scholarshipAwd$fundCode))
# tbl.scholarshipCohorts$fundCode <- as.character(as.numeric(tbl.scholarshipCohorts$fundCode))

# tbl.BannerCohorts.osfaCode <-as.character(as.numeric(tbl.BannerCohorts$osfaCode))
# tbl.scholarshipAwd$osfaCode <- as.character(as.numeric(tbl.scholarshipAwd$osfaCode))
# tbl.scholarshipCohorts$osfaCode <- as.character(as.numeric(tbl.scholarshipCohorts$osfaCode))

#as.character(c(tbl.BannerCohorts$studentID, tbl.Cancels$studentID, tbl.scholarshipAwd$studentID, tbl.scholarshipCohorts$studentID))
#as.character(c(tbl.GaCommitmentFundList.OSFA$fundCode, tbl.scholarshipAwd$fundCode))
#as.character(c(tbl.BannerCohorts$osfaCode,tbl.scholarshipAwd$osfaCode))

#Ensure key's are formatted as character
# tbl.BannerCohorts  %>% mutate_if(is.integer,as.character)
# tbl.Cancels %>% mutate_if(is.integer,as.character)
# tbl.scholarshipAwd %>% mutate_if(is.integer,as.character)
# tbl.scholarshipCohorts %>% mutate_if(is.integer,as.character)
# # 
# tbl.BannerCohorts[] <- lapply(tbl.BannerCohorts, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.Cancels[] <- lapply(tbl.Cancels, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.scholarshipAwd[] <- lapply(tbl.scholarshipAwd, function(x) if(is.numeric(x)) as.character(x) else x)
# tbl.scholarshipCohorts[] <- lapply(tbl.scholarshipCohorts, function(x) if(is.numeric(x)) as.character(x) else x)