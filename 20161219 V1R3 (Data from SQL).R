# install.packages("RODBC")
library(RODBC)
edw_bi <- odbcConnect("UAT_DEV - EDW_BI", uid="", pwd="") 
# head(sqlTables(edw_bi, tableType = "TABLE", schema = "dbo"), 10)
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees")
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees")
data <- rbind(active, inactive); nrow(data)
# str(data)
AR <- subset(data, data$VerticalName == "Accounts Receivable");nrow(AR) # 4409
TM <- subset(AR, JobRole == "Team Member" |  JobRole == "Team Leader" |  JobRole == "Desk Head / Team Co-ordinators"); nrow(TM); # 4310
# TM <- subset(AR, JobRole == "Team Member"); nrow(TM); # 4157
Current <- subset(TM, is.na(TM$DateOfRelieving) == TRUE | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 ); nrow(Current) # 3198
Current$Attrition = ifelse(is.na(Current$DateOfRelieving), 0, 1)
Current$Available = ifelse(!is.na(Current$DateOfRelieving), 0, 1)
Current$Status = as.factor(ifelse(is.na(Current$DateOfRelieving), "Current", "Past")) 
# only 2 > on the basis that we'll not have date for current employees as told by sid (his discussion with IT) on 23 Dec


######### Create Shift ####

Current$Shift2 <- substr(Current$Shift, 1, 17)
table(Current$Shift2, Current$Status)

Current$Shift3 <- factor(Current$Shift2)
levels(Current$Shift3) <- c("01:00 PM-10:00 PM", "04:00 PM-01:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
                            "06:30 AM-03:30 PM" ,"06:30 PM-03:30 AM", "08:00 AM-05:00 PM", "08:00 PM-05:00 AM",
                            "10:00 AM-07:00 PM" ,"11:00 AM-08:00 PM", "12:00 PM-09:00 PM", "9:00 AM-06:00 PM" )
table(Current$Shift3, Current$Status)
Current$Shift=NULL
Current$Shift2=NULL

class(Current$DistanceInKms); head(Current$DistanceInKms)
D_str <- as.character(Current$DistanceInKms)
D1 <- gsub("km","", D_str, fixed=FALSE); class(D1); head(D1)
D2 <- gsub(",", "", D1, fixed=FALSE); class(D2); head(D2)
Current$Distance <- as.numeric(D2)

summary(Current$Distance)
Current$DistanceInKms <- NULL


########### Employee Age ############
# install.packages("lubridate")
library(lubridate)
age <- function(dob, age.day = today(), units = "years", floor = FALSE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

Current$EmployeeAge <- ifelse(Current$Status=="Current", age(Current$DateofBirth), age(Current$DateofBirth, age.day = Current$DateOfRelieving))
summary(Current$EmployeeAge)


################### Exploratory Data Analysis #############

sum(Current$Attrition) # 1557
sum(Current$Available) # 1641

table(format(Current$DateofJoin, '%Y'))
table(Current$Status, format(Current$DateofJoin, '%Y'))
table(format(Current$DateOfRelieving, '%Y'))
table(format(Current$DateofJoin, '%Y'), format(Current$DateOfRelieving, '%Y'))
table(Current$Status, Current$JobRole)

sort(tapply(Current$Attrition, Current$RptEmployeeName, sum, na.rm=TRUE), decreasing = TRUE)
sort(tapply(Current$Attrition, Current$Rpt2EmployeeName, sum, na.rm=TRUE), decreasing = TRUE)
table(Current$Status, format(Current$DateofBirth, '%Y'))

table(Current$JobRole, Current$Status)

table(Current$Shift, Current$Status)


# table(Current$MaritalStatus, Current$JobRole, Current$Status)
table(Current$MaritalStatus, Current$Gender, Current$Status)
table(Current$TransportMode, Current$Status)
table(Current$WorkFacility, Current$Status)
table(Current$WorkLocation, Current$Status)
table(Current$PrevWorkFacility, Current$Status)
table(Current$PrevWorkLocation, Current$Status)
table(Current$Function, Current$Status)
table(Current$ReasonofLeaving)
table(Current$ExitType)

table(Current$ExperienceType, Current$Status)
table(Current$LastReviewType, Current$Status)
tapply(Current$ProdAvgLast3Months, Current$Status, mean, na.rm=TRUE)
tapply(Current$QualAvgLast3Months, Current$Status, mean, na.rm=TRUE)
table(Current$Course, Current$Status)
table(Current$CourseLevels, Current$Status)
table(Current$Specialization, Current$Status)
tapply(Current$Last30DaysLeaveCount, Current$Status, mean)
table(Current$EngagementIndex, Current$Status)
tapply(Current$TotalExtraHoursWorked, Current$Status, mean)
summary(Current$TotalExtraHoursWorked)
table(Current$StaffingEmployeeStatus, Current$Status)
tapply(Current$PreviousExperienceInMonths, Current$Status, mean, na.rm=TRUE)
table(Current$Process, Current$Status)

table(Current$LastReviewType)

table(Current$StaffingEmployeeStatus)
table(Current$LongLeave)

nlevels(Current$PrevEmployer)
head(Current$PrevEmployer)

table(Current$ExperienceType)

table(Current$CourseLevels)
table(Current$CourseLevels, Current$Status)
subset(Current, CourseLevels=="Professional")["Course"]

table(Current$EngagementIndex) ## 
table(Current$EngagementIndex, Current$Status)
table(Current$JobRole)
table(Current$Gender, Current$TransportMode)
tapply(Current$PreviousExperienceInMonths, Current$ExperienceType, mean, na.rm=TRUE)
sum(is.na(Current$PreviousExperienceInMonths))
sum(Current$PreviousExperienceInMonths==2)
table(Current$ExperienceType, Current$PreviousExperienceInMonths)

################ Graphical ##################
boxplot(Current$ProdAvgLast3Months ~ Current$Status) # 825 zeroes (ie 25.8 %) 241 Currrent and 584 past
hist(Current$ProdAvgLast3Months, xlab = "Prod Avg Last 3Months", main="Frequency Plot Prod Avg", breaks = 100)
library("Hmisc")
describe(Current$ProdAvgLast3Months ~ Current$Status ) 
sum(Current$ProdAvgLast3Months==0)
sm.density.compare(Current$ProdAvgLast3Months, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)


dt2 <- subset(Current[c("QualAvgLast3Months", "Status")], QualAvgLast3Months==0)
describe(dt2$QualAvgLast3Months ~ dt2$Status)

boxplot(Current$QualAvgLast3Months ~ Current$Status )
hist(Current$QualAvgLast3Months, breaks = 100, xlab = "Qual Avg Last 3Months", main="Frequency Plot for Qual Avg ") # 716 zeroes (ie 22.4%) 191 Current and 525 past 

boxplot(Current$AGSExperienceInMonths)
boxplot(Current$AGSExperienceInMonths ~ Current$Status)
hist(Current$AGSExperienceInMonths, breaks = 50, xlab = "AGS Experience In Months", main="Frequency Plot for AGS Experience ")
hist(Current$LastReviewRating, breaks=50)
sum(is.na(Current$LastReviewRating)) # 1622 NA ie 50.72%
sm.density.compare(Current$QualAvgLast3Months, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)
## create custom function to understand how many zeroes in a column

boxplot(Current$EmployeeAge)
boxplot(Current$EmployeeAge ~ Current$Status)
h <- hist(Current$EmployeeAge, breaks = 100, col="skyblue")
xfit <- seq(min(Current$EmployeeAge), max(Current$EmployeeAge), length=100)
yfit <- dnorm(xfit, mean=mean(Current$EmployeeAge), sd=sd(Current$EmployeeAge))
yfit <- yfit*diff(h$mids[1:2])*length(Current$EmployeeAge)
lines(xfit, yfit, col="blue", lwd=2)
# install.packages("sm")
library("sm")
sm.density.compare(Current$EmployeeAge, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)


boxplot(Current$Last30DaysLeaveCount)
boxplot(Current$Last30DaysLeaveCount ~ Current$Status)
hist(Current$Last30DaysLeaveCount, breaks = 30,xlab="Last 30 Days Leave Count", main="Freqency of Last 30 Days Leave Count")
plot(density(Current$Last30DaysLeaveCount))  
# sm.density.compare(Current$Last30DaysLeaveCount, Current$Status) # not working
sum(Current$Last30DaysLeaveCount==0) # 1682 zeroes  ie 52.59%
sum(Current$Last30DaysLeaveCount==1) # 62 ones  ie 1.93%
sum(Current$Last30DaysLeaveCount==2) # 91 zeroes  ie 2.84%
sum(Current$Last30DaysLeaveCount==15) # 72 zeroes  ie 2.25%



boxplot(Current$TotalExtraHoursWorked)
boxplot(Current$TotalExtraHoursWorked ~ Current$Status)
hist(Current$TotalExtraHoursWorked,breaks=25, xlab = "Total Extra Hours Worked", main="Freqency of Total Extra Hours Worked") # 1682 zeroes  ie 52.59%
sum(Current$Last30DaysLeaveCount==0)
sum(Current$Distance > 50, na.rm = TRUE) # 86 (>100 #49)

head(Current$Distance)
boxplot(Current$Distance)
boxplot(Current$Distance ~ Current$Status)
hist(Current$Distance, breaks=100)

library(ggplot2)
ggplot(subset(Current, Distance>30), aes(x=Distance)) + geom_density()
# ggplot(Current, aes(x=Distance)) + geom_density()

toolong <- which(Current$Distance > 30)
length(toolong)
Current$Distance[toolong]=NA # 139 to already 1576 ie 1715 ie 53.62%
summary(Current$Distance)

################# Naive Data #################

nrows <- nrow(Current)
ncomplete <- sum(complete.cases(Current))
100*ncomplete/nrows # usable data %

####Naive model accuracy - 51.32%#####
100*max(table(Current$Status))/nrow(Current)
################################################
names(Current)
nlevels(Current$RptEmployeeCode)

dataset2 <- Current[c("EmployeeCode", "AGSExperienceInMonths", "EmployeeAge", "Gender", "MaritalStatus",  "WorkLocation", 
                      "JobRole", "ExperienceType", "ProdAvgLast3Months", "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount",
                      "TotalExtraHoursWorked", "Shift3", "TransportMode","EngagementIndex", "PreviousExperienceInMonths", "Status", "Attrition", "Available")]

summary(dataset2)

##################   ###################
sum(is.na(dataset2$PreviousExperienceInMonths))
with(dataset2[is.na(dataset2$PreviousExperienceInMonths),],table(Status))


# odbcClose(edw_bi)