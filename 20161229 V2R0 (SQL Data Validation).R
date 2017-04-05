library(RODBC)
edw_bi <- odbcConnect("PulseDB_EDW_BI", uid = "edw", pwd = "BqSC&*UV^Skx6RtH")
head(sqlTables(edw_bi, tableType = "Table"))
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees"); nrow(active)
# inactive <- (sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees"))
attach(active)
### Active Table ####
## 1.NA ####
na_list <-sapply(active, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_list); na_count


zero_list <-sapply(active, function(y) sum(length(which((y==0)))))
zero_count <- data.frame(zero_list); zero_count

uniq_list <- sapply(active, function(y) sum(length(unique(y))))
uniq_count <- data.frame(uniq_list); uniq_count

# merge all these tables
active_prop <- cbind(na_count, zero_count, uniq_count)
active_prop$variable <-rownames(active_prop)
rownames(active_prop) <- NULL
active_prop 


table(format(DateofBirth, '%Y'))
table(format(DateofJoin, '%Y'))

sum(is.na(DateOfRelieving))
table(format(DateOfRelieving, '%Y'))
table(AttritionMonth)
table(AGSExperienceInMonths, ExpAGSRange)
table(Shift)
table(Shift, PrevShift)
table(PrevShift)
table(MaritalStatus)
table(Gender)
table(TransportMode)
table(WorkFacility)
table(WorkLocation)
table(PrevWorkFacility)
table(PrevWorkFacility, WorkFacility)
table(PrevWorkLocation)
table(JobRole)
table(ReasonofLeaving)
table(ExperienceType)
table(ExitType)

Fresher <- subset(active, ExperienceType=='Fresher')
sum(length(which(!is.na(Fresher$PreviousExperienceInMonths))))
sum(length(which(Fresher$PreviousExperienceInMonths>6)))
sum(length(which(!is.na(Fresher$PrevEmployer))))


IFresher <- subset(active, ExperienceType=='Industry Fresher')
sum(length(which(is.na(IFresher$PreviousExperienceInMonths))))
sum(length(which(is.na(IFresher$PrevEmployer))))
sum(length(which(IFresher$PreviousExperienceInMonths==0)))

Lateral <- subset(active, ExperienceType=='Lateral')
sum(length(which(is.na(Lateral$PreviousExperienceInMonths))))
sum(length(which(Lateral$PreviousExperienceInMonths==0)))
sum(length(which(is.na(Lateral$PrevEmployer))))


table(ExitType)
table(CurrentAddressCity)
sum(table(format(as.Date(RptEffectiveFrom), '%Y')))
nrow(active)
table(RptSpanofControl); sum(table(RptSpanofControl))
table(LastReviewType)
sum(table(format(as.Date(LastReviewDate), '%Y')))


PrevEmployerA <- subset(active, !is.na(PrevEmployer))
table(PrevEmployerA$ExperienceType)
PrevEmployerNA <- subset(active, is.na(PrevEmployer))
table(PrevEmployerNA$ExperienceType)
table()