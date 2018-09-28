

# Load the required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caTools)
library(MASS)
library(car)
library(caret)
library(e1071)
library(ROCR)
library(cowplot)


# Load all the CSV files into data frames
employee_survey_data <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv",stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)

#Check if upload looks good
head(employee_survey_data)
head(manager_survey_data)
head(general_data)
head(in_time)
head(out_time)

#Check the structure
str(employee_survey_data)
str(manager_survey_data)
str(general_data)
str(in_time)
str(out_time)


###  CRISP-DM Framework : Stage 1 - Business Understanding (through comments)  ###
##################################################################################
# Objective / Goal :
#   Analyze the data provided in multiple files to understand key variables that affect attrition
#   Build a model for depicting probability of attrition using a logistic regression. 
#   The results thus obtained will be used by the management to understand what changes they should make to their workplace, 
#   in order to get most of their employees to stay.
# Data Provided : All the data is provided by Employee ID as common key
#   Employee Survey Data
#   Manager Survey Data
#   General Data about employees
#   In time and out times for employees   



###  CRISP-DM Framework : Stage 2 & 3 - Data Understanding & Data Preparation  ###
##################################################################################

## Employee Survey Data
# Check Duplicates
nrow(employee_survey_data) == length(unique(employee_survey_data$EmployeeID))
#OR
#which(duplicated(employee_survey_data))
#OR
#sum(duplicated(employee_survey_data$EmployeeID))

# Check if any column has same value across all the rows
sapply(employee_survey_data, function(x) length(unique(x))>1)
#   EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
#     TRUE                    TRUE                    TRUE                    TRUE
# All columns have more than 1 distinct values across rows

# Check NA values
sapply(employee_survey_data, function(x) sum(is.na(x)))
#   EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
#       0                      25                      20                      38 
# We will revist NA after merging for any trends

# Outliers
# Outlier treatment is not needed as all are categorical varilables. We will revisit while during Univariate analysis.


## Manager Survey Data
nrow(manager_survey_data) == length(unique(manager_survey_data$EmployeeID))
sapply(manager_survey_data, function(x) length(unique(x))>1)
#   EmployeeID    JobInvolvement PerformanceRating 
#     TRUE              TRUE              TRUE
# All columns have more than 1 distinct values across rows
sapply(manager_survey_data, function(x) sum(is.na(x)))
#   EmployeeID    JobInvolvement PerformanceRating 
#       0                 0                 0 
# Outlier treatment is not needed as all are categorical varilables

## General Data about employees
nrow(general_data) == length(unique(general_data$EmployeeID))
sapply(general_data, function(x) length(unique(x))>1)
#   Age               Attrition          BusinessTravel              Department        DistanceFromHome 
#   TRUE                    TRUE                    TRUE                    TRUE                    TRUE 
#   Education          EducationField           EmployeeCount              EmployeeID                  Gender 
#   TRUE                    TRUE                   FALSE                    TRUE                    TRUE 
#   JobLevel                 JobRole           MaritalStatus           MonthlyIncome      NumCompaniesWorked 
#   TRUE                    TRUE                    TRUE                    TRUE                    TRUE 
#   Over18       PercentSalaryHike           StandardHours        StockOptionLevel       TotalWorkingYears 
#   FALSE                    TRUE                   FALSE                    TRUE                    TRUE 
#   TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager 
#   TRUE                    TRUE                    TRUE                    TRUE
# EmployeeCount, Over18 and StandardHours columns have same values across all rows. So we would be removing these columns
general_data <- general_data %>% dplyr::select(-c(which(sapply(general_data,function(x) length(unique(x)) == 1))))
#OR
# general_data <- general_data[ , !(names(general_data) %in% c("EmployeeCount","Over18", "StandardHours"))]

sapply(general_data, function(x) sum(is.na(x)))
#   Age               Attrition          BusinessTravel              Department        DistanceFromHome 
#     0                       0                       0                       0                       0 
#   Education          EducationField           EmployeeCount              EmployeeID                  Gender 
#     0                       0                       0                       0                       0 
#   JobLevel                 JobRole           MaritalStatus           MonthlyIncome      NumCompaniesWorked 
#     0                       0                       0                       0                      19 
#   Over18       PercentSalaryHike           StandardHours        StockOptionLevel       TotalWorkingYears 
#     0                       0                       0                       0                       9 
#   TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager 
#     0                       0                       0                       0 
# We will revist NA after merging for any trends
# Outlier treatment is not needed as all are categorical varilables


## In-Time and Out-Time for employees
# Add missing EmployeeID as column name and remove "X" character from column names
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub(pattern="X","",colnames(in_time))

colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub(pattern="X","",colnames(out_time))

# Check duplicate values
nrow(in_time) == length(unique(in_time$EmployeeID))
nrow(out_time) == length(unique(out_time$EmployeeID))

# Check the columns that have NA values for all rows 
colnames(in_time)[which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))]
colnames(out_time)[which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))]
# Both in & out dataframes have all rows NA for same dates. These could be holidays so can be safely removed
in_time <- in_time %>% dplyr::select(-c(which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))))
out_time <- out_time %>% dplyr::select(-c(which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))))

# No columns have same value across rows
sapply(in_time, function(x) length(unique(x))>1)
sapply(out_time, function(x) length(unique(x))>1)


in_time_format <- in_time[,-1]
in_time_format <- as.data.frame(sapply(in_time_format, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
out_time_format <- out_time[,-1]
out_time_format <- as.data.frame(sapply(out_time_format, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
TimeInOffice <- out_time_format - in_time_format

View(TimeInOffice)
class(TimeInOffice)
str(TimeInOffice)
# Datatype is chr; we need to convert it to number
TimeInOffice<-as.data.frame(sapply(TimeInOffice,function(x) as.numeric(x)))
str(TimeInOffice)
View(TimeInOffice)

#OR
# in_time_epochSecs <- as.data.frame((sapply(c(2:ncol(in_time)),function(x) ymd_hms(in_time[,x]))))
# colnames(in_time_epochSecs) <- colnames(in_time)[-1]
# out_time_epochSecs <- as.data.frame((sapply(c(2:ncol(out_time)),function(x) ymd_hms(out_time[,x]))))
# colnames(out_time_epochSecs) <- colnames(out_time)[-1]
# Lets calculate the time in hours for each employee
# TimeInOffice <- as.data.frame((as.matrix(out_time_epochSecs) - as.matrix(in_time_epochSecs))/(60*60))



# Average hours present in office / employee
TimeInOffice$AvgWorkHrs<-apply(TimeInOffice,1,mean,na.rm=TRUE)
TimeInOffice$OffDays<-apply(TimeInOffice,1,function (x) sum(is.na(x),na.rm=TRUE))

colnames(TimeInOffice) <- gsub(pattern="X","",colnames(TimeInOffice))

EmployeeAvgHours <- cbind(in_time$EmployeeID,TimeInOffice$AvgWorkHrs, TimeInOffice$OffDays)
View(EmployeeAvgHours)
class(EmployeeAvgHours)
# Convert Matrix to Data Frame
EmployeeAvgHours<-as.data.frame(EmployeeAvgHours)
class(EmployeeAvgHours)
View(EmployeeAvgHours)
colnames(EmployeeAvgHours)
# Update Column Names
names(EmployeeAvgHours) <- c("EmployeeID", "AvgWorkingHours","OffDays")
colnames(EmployeeAvgHours)

# Merge All DFs into Single DF
# First merge employee_survey_data & manager_survey_data
EmployeeHRData <- merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
View(EmployeeHRData)

# Merge general_data with New DF
EmployeeHRData<-merge(EmployeeHRData,general_data,by="EmployeeID",all=F)
View(EmployeeHRData)

# Merge EmployeeAvgHours into New DF
EmployeeHRData<-merge(EmployeeHRData,EmployeeAvgHours,by="EmployeeID",all = F)
View(EmployeeHRData)
colnames(EmployeeHRData)

# Check how many columns have NA
colnames(EmployeeHRData)[which((sapply(EmployeeHRData,function(x) sum(is.na(x))) > 0))]
# [1] "EnvironmentSatisfaction" "JobSatisfaction"         "WorkLifeBalance"         "NumCompaniesWorked"      "TotalWorkingYears"      

# NA values in TotalWorkingYears
# We will take the median of the age when other employees generally started working and use it to approximately calculate NA values
round(mean((EmployeeHRData %>% 
              filter(!is.na(TotalWorkingYears)) %>% 
              dplyr::select(EmployeeID,Age,TotalWorkingYears) %>% 
              mutate(started_working_at=(Age-TotalWorkingYears)))$started_working_at),0)
# Average age when employees started working is 26 years
# Assuming employees didn't skip years, based on average age above, totalWorkingYears for NAs is calculated
EmployeeHRData[which(is.na(EmployeeHRData$TotalWorkingYears)),'TotalWorkingYears'] <- (EmployeeHRData %>% filter(is.na(TotalWorkingYears)))$Age - 26

# NA values for remaining columns NumCompaniesWorked, EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance
# Check impact of removing NA values on Attrition variable 
EmployeeHRData %>% group_by(Attrition) %>% summarize(cnt=length(EmployeeID))
#Attrition   cnt
#    <chr> <int>
#       No  3699
#      Yes   711

# The attrition count if we remove the lines which have NA in the above columns
EmployeeHRData[(which(is.na(EmployeeHRData$NumCompaniesWorked) | is.na(EmployeeHRData$EnvironmentSatisfaction) | 
                          is.na(EmployeeHRData$JobSatisfaction) | is.na(EmployeeHRData$WorkLifeBalance))),] %>%
                  group_by(Attrition) %>% summarize(cnt=length(EmployeeID))
#Attrition   cnt
#    <chr> <int>
#       No    88
#      Yes    14

# % reduction in NO values of Attrition = 88/3699 ~ 2.38%
# % reduction in YES values of Attrition = 14/711 ~ 1.97%
# Both percentages are almost same so if we remove NA values there wouldn't be any discrepancy introduced in Attrition data
EmployeeHRData <- as.data.frame(EmployeeHRData %>% 
                                    filter(!is.na(NumCompaniesWorked), !is.na(EnvironmentSatisfaction), 
                                           !is.na(JobSatisfaction), !is.na(WorkLifeBalance)))

# Confirm No NAs are present
sum(is.na(EmployeeHRData)) 

# Let's summarise the data
summary(EmployeeHRData)


# Separate-out Caterorical and Continuous variables

# Catergorical variables --
#   EmployeeID - numeric
#   EnvironmentSatisfaction - numeric
#   JobSatisfaction - numeric
#   WorkLifeBalance - numeric
#   JobInvolvement - numeric 
#   PerformanceRating - numeric 					 
#   Attrition
#   BusinessTravel
#   Department 
#   Education - numeric
#   EducationField
#   Gender
#   JobLevel - numeric
#   JobRole 
#   MaritalStatus
#   StockOptionLevel 


# Continuous variables --
#   Age
#   DistanceFromHome
#   MonthlyIncome
#   NumCompaniesWorked
#   PercentSalaryHike
#   TotalWorkingYears
#   TrainingTimesLastYear
#   YearsAtCompany
#   YearsSinceLastPromotion
#   YearsWithCurrManager
#   AvgWorkingHours
#   OffDays

EmployeeHRData$Attrition <- as.factor(EmployeeHRData$Attrition)
EmployeeHRData$BusinessTravel <- as.factor(EmployeeHRData$BusinessTravel)
EmployeeHRData$Department <- as.factor(EmployeeHRData$Department)
EmployeeHRData$EducationField <- as.factor(EmployeeHRData$EducationField)
EmployeeHRData$Gender <- as.factor(EmployeeHRData$Gender)
EmployeeHRData$JobRole <- as.factor(EmployeeHRData$JobRole)
EmployeeHRData$MaritalStatus <- as.factor(EmployeeHRData$MaritalStatus)
EmployeeHRData$StockOptionLevel <- as.factor(EmployeeHRData$StockOptionLevel)


## Univariate & Bivariate Analysis

# Starting with Categorical Variables

Chart01 <- ggplot(EmployeeHRData) + 
            geom_bar(aes(x=BusinessTravel,fill=Attrition),position="dodge",alpha=0.7) + 
            scale_y_continuous(breaks=seq(0,3000,500)) +
            labs(title="BusinessTravel v/s Attrition",x="Business Travel",y="No of Employees",fill="Attrition") +
            theme_bw()

Chart02 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=Department,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,2400,400)) +
  labs(title="Department v/s Attrition",x="Department",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart03 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=EducationField,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="EducationField v/s Attrition",x="EducationField",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart04 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=Gender,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,2400,400)) +
  labs(title="Gender v/s Attrition",x="Gender",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart05 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=MaritalStatus,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,2400,200)) +
  labs(title="MaritalStatus v/s Attrition",x="MaritalStatus",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart06 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=StockOptionLevel,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="StockOptionLevel v/s Attrition",x="StockOptionLevel",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart07 <- ggplot(EmployeeHRData) + 
  geom_bar(aes(x=JobRole,fill=Attrition),position="dodge",alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,1000,100)) +
  labs(title="JobRole v/s Attrition",x="JobRole",y="No of Employees",fill="Attrition") +
  theme_bw()

grid.arrange(Chart01,Chart02,Chart03,Chart04,Chart05,Chart06, nrow=2, ncol=3)
grid.arrange(Chart07, nrow=1, ncol=1)

# Other Numeric Categorical Variables

Chart08 <- ggplot(EmployeeHRData) + geom_bar(aes(x=EnvironmentSatisfaction,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="EnvironmentSatisfaction v/s Attrition",x="EnvironmentSatisfaction", y="No of Employees",fill="Attrition") +
  theme_bw()

Chart09 <- ggplot(EmployeeHRData) + geom_bar(aes(x=JobSatisfaction,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="JobSatisfaction v/s Attrition",x="JobSatisfaction",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart10 <- ggplot(EmployeeHRData) + geom_bar(aes(x=WorkLifeBalance,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,2400,400)) +
  labs(title="WorkLifeBalance v/s Attrition",x="WorkLifeBalance",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart11 <- ggplot(EmployeeHRData) + geom_bar(aes(x=JobInvolvement,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,2400,400)) +
  labs(title="JobInvolvement v/s Attrition",x="JobInvolvement",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart12 <- ggplot(EmployeeHRData) + geom_bar(aes(x=as.factor(PerformanceRating),fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,3500,500)) +
  labs(title="PerformanceRating v/s Attrition",x="PerformanceRating",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart13 <- ggplot(EmployeeHRData) + geom_bar(aes(x=Education,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="Education v/s Attrition",x="Education",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart14 <- ggplot(EmployeeHRData) + geom_bar(aes(x=JobLevel,fill=Attrition),position="dodge",alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="JobLevel v/s Attrition",x="JobLevel",y="No of Employees",fill="Attrition") +
  theme_bw()

grid.arrange(Chart08,Chart09,Chart10,Chart11,Chart12,Chart13,Chart14, nrow=3, ncol=3)

# Continuous Variables

Chart15 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=Age,fill=Attrition)) +
  scale_y_continuous(breaks=seq(22,64,4)) +
  labs(title="Age v/s Attrition",x="Attrition",y="Age") +
  theme_bw()

Chart15_hist <- ggplot(EmployeeHRData) + geom_histogram(aes(x=Age,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,250,25)) +
  scale_x_continuous(breaks=seq(25,60,2)) +
  labs(title="Age distribution",x="Age",y="No of Employees") +
  theme_bw()

Chart16 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=DistanceFromHome,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,30,4)) +
  labs(title="Distance From Home v/s Attrition",x="Attrition",y="Distance From Home") +
  theme_bw()

Chart16_hist <- ggplot(EmployeeHRData) + geom_histogram(aes(x=DistanceFromHome,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,50)) +
  scale_x_continuous(breaks=seq(0,30,2)) +
  labs(title="Distance From Home Distribution",x="Distance from home",y="No of Employees") +
  theme_bw()

Chart17 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=MonthlyIncome,fill=Attrition)) +
  scale_y_continuous(breaks=seq(5000,200000,25000)) +
  labs(title="MonthlyIncome v/s Attrition",x="Attrition",y="MonthlyIncome") +
  theme_bw()

Chart17_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=MonthlyIncome,fill=Attrition),binwidth=5000,col="black") +
  scale_y_continuous(breaks=seq(0,600,50)) +
  scale_x_continuous(breaks=seq(0,200000,50000)) +
  labs(title="Monthly Income Distribution",x="Monthly Income",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart18 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=NumCompaniesWorked,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  labs(title="NumCompaniesWorked v/s Attrition",x="Attrition",y="NumCompaniesWorked") +
  theme_bw()

Chart18_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=NumCompaniesWorked,fill=Attrition),binwidth=1,col="black") +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  scale_x_continuous(breaks=seq(0,15,1)) +
  labs(title="Number of Companies Worked Distribution",x="Number of Companies Worked",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart19 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=PercentSalaryHike,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,30,2)) +
  labs(title="PercentSalaryHike v/s Attrition",x="Attrition",y="PercentSalaryHike") +
  theme_bw()

Chart19_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=PercentSalaryHike,fill=Attrition),binwidth=1,col="black") +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  scale_x_continuous(breaks=seq(0,30,2)) +
  labs(title="PercentSalaryHike Distribution",x="Percent Salary Hike",y="No of Employees",fill="Attrition") +
  theme_bw()

Chart20 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=TotalWorkingYears,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="TotalWorkingYears v/s Attrition",x="Attrition",y="TotalWorkingYears") +
  theme_bw()

Chart20_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=TotalWorkingYears,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,50)) +
  scale_x_continuous(breaks=seq(0,40,4)) +
  labs(title="TotalWorkingYears Distribution",x="Total Working Years",y="No of Employees") +
  theme_bw()

Chart21 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=TrainingTimesLastYear,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,10,1)) +
  labs(title="TrainingTimesLastYear v/s Attrition",x="Attrition",y="TrainingTimesLastYear") +
  theme_bw()

Chart21_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=TrainingTimesLastYear,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1800,200)) +
  labs(title="TrainingTimesLastYear Distribution",x="Training Times Last Year",y="No of Employees") +
  theme_bw()

Chart22 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=YearsAtCompany,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="YearsAtCompany v/s Attrition",x="Attrition",y="YearsAtCompany") +
  theme_bw()

Chart22_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,100)) +
  labs(title="YearsAtCompany Distribution",x="Years At Company",y="No of Employees") +
  theme_bw()

Chart23 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=YearsSinceLastPromotion,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,20,2)) +
  labs(title="YearsSinceLastPromotion v/s Attrition",x="Attrition",y="YearsSinceLastPromotion") +
  theme_bw()

Chart23_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=YearsSinceLastPromotion,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  labs(title="YearsSinceLastPromotion Distribution",x="Years Since Last Promotion",y="No of Employees") +
  theme_bw()

Chart24 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=YearsWithCurrManager,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,20,2)) +
  labs(title="YearsWithCurrManager v/s Attrition",x="Attrition",y="YearsWithCurrManager") +
  theme_bw()

Chart24_hist <- ggplot(EmployeeHRData) + 
  geom_histogram(aes(x=YearsWithCurrManager,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1000,200)) +
  labs(title="YearsWithCurrManager Distribution",x="Years With Current Manager",y="No of Employees") +
  theme_bw()

Chart25 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=OffDays,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,24,4)) +
  labs(title="Total Off Days v/s Attrition",x="Attrition",y="Total Off Days") +
  theme_bw()

Chart25_hist <- ggplot(EmployeeHRData) + geom_histogram(aes(x=OffDays,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,300,50)) +
  labs(title="Total Off Days Distribution",x="Total Off Days",y="No of Employees") +
  theme_bw()

Chart26 <- ggplot(EmployeeHRData) + geom_boxplot(aes(x=Attrition,y=AvgWorkingHours,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,14,2)) +
  labs(title="AvgWorkingHours v/s Attrition",x="Attrition",y="AvgWorkingHours") +
  theme_bw()

Chart26_hist <- ggplot(EmployeeHRData) + geom_histogram(aes(x=AvgWorkingHours,fill=Attrition),col="black",binwidth=0.01) +
  scale_y_continuous(breaks=seq(0,50,10)) +
  labs(title="AvgWorkingHours Distribution",x="Average Working Hours",y="No of Employees") +
  theme_bw()


grid.arrange(Chart15,Chart15_hist,Chart16,Chart16_hist,Chart17,Chart17_hist,Chart18,Chart18_hist,Chart19,Chart19_hist,Chart20,Chart20_hist,nrow=3,ncol=4)

## Addressing OUTLIERS

# MonthlyIncome
# As per Chart17_hist there is higher attrition rate in lower income group.
# So removing the outliers on higher side of income will not be disturbing attrition data.
quantile(EmployeeHRData$MonthlyIncome,seq(0,1,0.01))

# ggplot() + geom_line(aes(x=c(0:100),y=quantile(EmployeeHRData$MonthlyIncome,seq(0,1,0.01)))) + 
#   scale_y_continuous(breaks=seq(0,200000,25000)) +
#   labs(title="Monthly Income Quantile",x="Quantile",y="MonthlyIncome") +
#   theme_bw()

length(EmployeeHRData[which(EmployeeHRData$MonthlyIncome >170000),'MonthlyIncome'])
length(EmployeeHRData[,"EmployeeID"])

# Referring to box plot Chart17 and the quantile result, if we cap the income to 170000 
# 282 employees are outliers having income higher than 170000 which is 282/4308 ~ 6.5% of the total rows in EmployeeHRData
EmployeeHRData[which(EmployeeHRData$MonthlyIncome > 170000),'MonthlyIncome'] <- 170000


# TotalWorkingYears
# As per Chart20_hist there is higher attrition rate in lower working years.
# So removing the outliers on higher side of working years will not be disturbing attrition data.
quantile(EmployeeHRData$TotalWorkingYears,seq(0,1,0.01))

# ggplot() + geom_line(aes(x=c(0:100),y=quantile(EmployeeHRData$TotalWorkingYears,seq(0,1,0.01)))) + 
#   scale_x_continuous(breaks=seq(0,100,10)) +
#   scale_y_continuous(breaks=seq(0,40,5)) +
#   labs(title="Total Working Years Quantile",x="Quantile",y="TotalWorkingYears") +
#   theme_bw()

length(EmployeeHRData[which(EmployeeHRData$TotalWorkingYears >26),'TotalWorkingYears'])
length(EmployeeHRData[,"EmployeeID"])

# Referring to box plot Chart20 and the quantile result, if we cap the working years to 26 
# 247 employees are outliers having working years greater than 26 years which is 247/4308 ~ 5.7% of the total rows in EmployeeHRData
EmployeeHRData[which(EmployeeHRData$TotalWorkingYears > 26),'TotalWorkingYears'] <- 26


grid.arrange(Chart21,Chart21_hist,Chart22,Chart22_hist,Chart23,Chart23_hist,Chart24,Chart24_hist,Chart25,Chart25_hist,nrow=3,ncol=4)
grid.arrange(Chart26,Chart26_hist,nrow=2,ncol=1)

## Addressing OUTLIERS CONTD..

# YearsAtCompany
# As per Chart22_hist there is higher attrition rate in lower years at company (<10 years).
# So removing the outliers on higher side of years at company will not be disturbing attrition data.
quantile(EmployeeHRData$YearsAtCompany,seq(0,1,0.01))

# ggplot() + geom_line(aes(x=c(0:100),y=quantile(EmployeeHRData$YearsAtCompany,seq(0,1,0.01)))) + 
#   scale_x_continuous(breaks=seq(0,100,10)) +
#   scale_y_continuous(breaks=seq(0,40,5)) +
#   labs(title="Years at Company Quantile",x="Quantile",y="YearsAtCompany") +
#   theme_bw()

length(EmployeeHRData[which(EmployeeHRData$YearsAtCompany >22),'YearsAtCompany'])
length(EmployeeHRData[,"EmployeeID"])

# Referring to box plot Chart22 and the quantile result, if we cap the years at company to 22 
# 111 employees are outliers having working years greater than 22 years which is 111/4308 ~ 2.6% of the total rows in EmployeeHRData
EmployeeHRData[which(EmployeeHRData$YearsAtCompany > 22),'YearsAtCompany'] <- 22

# YearsSinceLastPromotion
# As per Chart23_hist there is higher attrition rate in lower years since last promotion.
# So removing the outliers on higher side of years since last promotion will not be disturbing attrition data.
quantile(EmployeeHRData$YearsSinceLastPromotion,seq(0,1,0.01))

# ggplot() + geom_line(aes(x=c(0:100),y=quantile(EmployeeHRData$YearsSinceLastPromotion,seq(0,1,0.01)))) + 
#   scale_x_continuous(breaks=seq(0,100,10)) +
#   scale_y_continuous(breaks=seq(0,40,5)) +
#   labs(title="Years Since Last Promotion Quantile",x="Quantile",y="YearsSinceLastPromotion") +
#   theme_bw()

length(EmployeeHRData[which(EmployeeHRData$YearsSinceLastPromotion >11),'YearsSinceLastPromotion'])
length(EmployeeHRData[,"EmployeeID"])

# Referring to box plot Chart23 and the quantile result, if we cap the years at company to 11 (there are 70 records with value 11, so 9 is not taken at cap)
# 126 employees are outliers having working years greater than 11 years which is 126/4308 ~ 2.9% of the total rows in EmployeeHRData
EmployeeHRData[which(EmployeeHRData$YearsSinceLastPromotion > 11),'YearsSinceLastPromotion'] <- 11


# Check out if there are any highly corelated numeric variables
EmployeeHRData_Cor <- cor(scale(select_if(EmployeeHRData[,-1],is.numeric)))
dimnames(EmployeeHRData_Cor) <- list(colnames(select_if(EmployeeHRData[,-1],is.numeric)), colnames(select_if(EmployeeHRData[,-1],is.numeric)))
corrplot(EmployeeHRData_Cor,method = "number", 
         title = "Correlation Matrix", 
         type = "lower", 
         order = "FPC",
         number.cex = 0.8,
         tl.cex = 0.7,
         bg="light green",number.digits = 2)

# There is no numerical variable that can be eliminiated due to high corelation



## Data Preparation

# Scaling numerical columns
EmployeeHRData_Scaled <- EmployeeHRData
EmployeeHRData_Scaled <- cbind(EmployeeHRData_Scaled$EmployeeID, scale(dplyr::select_if(EmployeeHRData_Scaled[,-1],is.numeric)), dplyr::select_if(EmployeeHRData_Scaled[,-1],is.factor))
colnames(EmployeeHRData_Scaled)[1] <- "EmployeeID"

# Converting Categorical variables to numeric
# Replacing binary values to 1 & 0
EmployeeHRData_Scaled$Attrition <- sapply(EmployeeHRData_Scaled$Attrition, function(x) ifelse(x == "Yes",1,0))
EmployeeHRData_Scaled$Gender <- sapply(EmployeeHRData_Scaled$Gender, function(x) ifelse(x == "Male",1,0))
str(EmployeeHRData_Scaled)

# --- Working on BusinessTravel,Department,EducationField,JobRole,MaritalStatus,StockOptionLevel for conversion to numerical elements --- #
# Expanding factor variables having multiple values using model.matrix
EmployeeHRData_Scaled_Cat <- dplyr::select_if(EmployeeHRData_Scaled,is.factor)
EmployeeHRData_Dummy <- sapply(EmployeeHRData_Scaled_Cat,function(x) 
                          as.data.frame(model.matrix(~x, data=EmployeeHRData_Scaled_Cat)))
class(EmployeeHRData_Dummy)
EmployeeHRData_Dummy <- as.data.frame(EmployeeHRData_Dummy)
# Remove all the "..Intercept." columns
EmployeeHRData_Dummy <- dplyr::select(EmployeeHRData_Dummy, -ends_with("..Intercept."))

# Update scaled dataframe for further use
EmployeeHRData_Scaled <- cbind(dplyr::select_if(EmployeeHRData_Scaled,is.numeric), EmployeeHRData_Dummy)
EmployeeHRData_Scaled <- EmployeeHRData_Scaled %>% dplyr::select(-EmployeeID)

#Create Test and Train Datasets
set.seed(100)
train <- which(sample.split(EmployeeHRData_Scaled$Attrition,SplitRatio = 0.7))
test <- which(!c(1:nrow(EmployeeHRData_Scaled) %in% train))

EmployeeHRData_Train <- EmployeeHRData_Scaled[train,]
EmployeeHRData_Test <- EmployeeHRData_Scaled[test,]



###  CRISP-DM Framework : Stage 4 - Model Preparation  ###
##########################################################


## Build Model

## Eliminated Columns
model_1 <- glm(Attrition~., data=EmployeeHRData_Train, family="binomial")
summary(model_1)
step <- stepAIC(model_1, direction="both")

model_2 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  JobInvolvement + Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + OffDays + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + EducationField.xMarketing + EducationField.xTechnical.Degree + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
              data=EmployeeHRData_Train, family="binomial")
sort(vif(model_2), decreasing = TRUE)
summary(model_2)
# Department.xSales removed after VIF analysis (VIF > 4)

model_3 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 JobInvolvement + Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + OffDays + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_3), decreasing = TRUE)
summary(model_3)
# VIF will not be used from here onwards (VIF < 4). Will be working only with p-value
# JobInvolvement removed after p-value analysis

model_4 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + OffDays + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_4), decreasing = TRUE)
summary(model_4)
# OffDays removed after p-value analysis

model_5 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_5), decreasing = TRUE)
summary(model_5)
# Department.xResearch...Development removed after p-value analysis

model_6 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_6), decreasing = TRUE)
summary(model_6)
# EducationField.xTechnical.Degree removed after p-value analysis

model_7 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 EducationField.xMarketing + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_7), decreasing = TRUE)
summary(model_7)
# EducationField.xMarketing removed after p-value analysis

model_8 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_8), decreasing = TRUE)
summary(model_8)
# JobRole.xSales.Representative removed after p-value analysis

model_9 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_9), decreasing = TRUE)
summary(model_9)
# StockOptionLevel.x3 removed after p-value analysis

model_10 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 ,
               data=EmployeeHRData_Train, family="binomial")
sort(vif(model_10), decreasing = TRUE)
summary(model_10)
# StockOptionLevel.x1 removed after p-value analysis

model_11 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  Age + JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle ,
                data=EmployeeHRData_Train, family="binomial")
sort(vif(model_11), decreasing = TRUE)
summary(model_11)
# JobLevel removed after p-value analysis

model_12 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle ,
                data=EmployeeHRData_Train, family="binomial")
sort(vif(model_12), decreasing = TRUE)
summary(model_12)
# JobRole.xManager removed after p-value analysis

model_13 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle ,
                data=EmployeeHRData_Train, family="binomial")
sort(vif(model_13), decreasing = TRUE)
summary(model_13)
# BusinessTravel.xTravel_Rarely removed after p-value analysis

model_14 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle ,
                data=EmployeeHRData_Train, family="binomial")
sort(vif(model_14), decreasing = TRUE)
summary(model_14)

Final_Model <- model_14


###  CRISP-DM Framework : Stage 5 - Model Evaluation  ###
#########################################################


## Evaluate Model on TEST data

# Lets see what are the predictions made
EmployeeHRData_Attrition_Predicted <- predict(Final_Model, type="response", newdata = dplyr::select(EmployeeHRData_Test, -Attrition))
EmployeeHRData_Test$Attrition_Predicted <- EmployeeHRData_Attrition_Predicted
View(EmployeeHRData_Test)

# Maximum predicted probability
max(EmployeeHRData_Test$Attrition_Predicted)
# 0.8556364

# Minimum predicted probability
min(EmployeeHRData_Test$Attrition_Predicted)
# 0.0003195233

# Let's use the probability cutoff of 50%.
EmployeeHRData_Test_pred_attr <- factor(ifelse(EmployeeHRData_Attrition_Predicted >= 0.50, "Yes", "No"))
EmployeeHRData_Test_actual_attr <- factor(ifelse(EmployeeHRData_Test$Attrition == 1,"Yes","No"))

#str(EmployeeHRData_Test_pred_attr)
#View(EmployeeHRData_Test_pred_attr)
#str(EmployeeHRData_Test_actual_attr)
#View(EmployeeHRData_Test_actual_attr)

table(EmployeeHRData_Test_actual_attr, EmployeeHRData_Test_pred_attr)
#                             EmployeeHRData_Test_pred_churn
# EmployeeHRData_actual_churn   	  No    	Yes
#                         No  		  1059   	24
#                         Yes  		  168   	41

EmployeeHRData_Test_conf <- confusionMatrix(EmployeeHRData_Test_pred_attr, EmployeeHRData_Test_actual_attr, positive = "Yes")
EmployeeHRData_Test_conf

#Confusion Matrix and Statistics

#                   Reference
# Prediction      No      Yes
#          No     1059    168
#          Yes    24      41

#Accuracy                   : 0.8514          
#95% CI                     : (0.8308, 0.8704)
#No Information Rate        : 0.8382          
#P-Value [Acc > NIR]        : 0.1054          

#Kappa                      : 0.241           
#Mcnemar's Test P-Value     : <2e-16          

#Sensitivity                : 0.19617         
#Specificity                : 0.97784         
#Pos Pred Value             : 0.63077         
#Neg Pred Value             : 0.86308         
#Prevalence                 : 0.16176         
#Detection Rate             : 0.03173         
#Detection Prevalence       : 0.05031         
#Balanced Accuracy          : 0.58701        

#'Positive' Class : Yes  


## Find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(EmployeeHRData_Attrition_Predicted >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, EmployeeHRData_Test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(EmployeeHRData_Attrition_Predicted)

# Creating cutoff values from 0.0003195 to 0.8556364 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.85,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plotting ROC Curve having Sensitivity,Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.41,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
View(cutoff)

# Let's choose a cutoff value = 0.1627273 at the point where sensitivity & specificity are nearest to eachother
EmployeeHRData_Test_cutoff_attr <- factor(ifelse(EmployeeHRData_Attrition_Predicted >=0.1627273, "Yes", "No"))

conf_final <- confusionMatrix(EmployeeHRData_Test_cutoff_attr, EmployeeHRData_Test_actual_attr, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

#Accuracy :: 0.7167183 
#Sensitivity :: 0.7320574 
#Specificity :: 0.7137581


## KS Statistic - Test Data

EmployeeHRData_Test_cutoff_attr <- ifelse(EmployeeHRData_Test_cutoff_attr == "Yes", 1, 0)
EmployeeHRData_Test_actual_attr <- ifelse(EmployeeHRData_Test_actual_attr == "Yes", 1, 0)

#on testing  data
pred_object_test<- prediction(EmployeeHRData_Test_cutoff_attr, EmployeeHRData_Test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
                  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.4458155


## Lift & Gain Chart 

lift <- function(labels, predicted_prob, groups=10) 
{
  if(is.factor(labels)) labels  <- as.integer(as.character(labels))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  
  helper = data.frame(cbind(labels, predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
                summarise_at(vars(labels ), funs(total = n(), totalresp=sum(., na.rm = TRUE))) %>%
                mutate(Cumresp = cumsum(totalresp), Gain=Cumresp/sum(totalresp)*100,
                  Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(EmployeeHRData_Test_actual_attr, EmployeeHRData_Attrition_Predicted, groups = 10)

Chart27 <- ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain)) + 
  geom_line(color = "red") + geom_point(color = "blue") + 
  geom_text(aes(label=round(Attrition_decile$Gain, digits = 2)), color="red", hjust=0, vjust=1) +
  labs(title="Gain Chart",x="Decile",y="Gain (%)")

Chart28 <- ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift)) + 
  geom_line(color = "black") + geom_point(color = "blue") + 
  geom_text(aes(label=round(Attrition_decile$Cumlift, digits = 2)), color="red", hjust=0, vjust=0) +
  labs(title="Lift Chart",x="Decile",y="Lift")

grid.arrange(Chart27,Chart28,nrow=2,ncol=1)




#################################################################################################################################



### ADDITIONALREFERENCES ###
#   ====================

## VIF and p-values during data modelling ##
#|-------------------------------------------------------------------------------------------------------|
#| Model No. |  Variable Removed to Build Model 	|      VIF        |    p-value      |Current AIC Value | 
#|-------------------------------------------------------------------------------------------------------|
#|Model-1    | NA					                        | NA              |                 | 2103.4           |
#|-------------------------------------------------------------------------------------------------------|
#|StepAIC    | NA					                        | NA              |                 | 2078.85          |
#|-------------------------------------------------------------------------------------------------------|
#|Model-2    | Department.xSales			            | 4.681447        | 0.009076        | 2078.9           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-3    | JobInvolvement				              |  Not Referred   | 0.108687        | 2083.4           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-4    | OffDays					                  |  Not Referred   | 0.138509        | 2084             |
#|-------------------------------------------------------------------------------------------------------|
#|Model-5    | Department.xResearch...Development	|  Not Referred   | 0.114222        | 2084.2           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-6    | EducationField.xTechnical.Degree		|  Not Referred   | 0.073851        | 2084.6           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-7    | EducationField.xMarketing		      |  Not Referred   | 0.092632        | 2086             |
#|-------------------------------------------------------------------------------------------------------|
#|Model-8    | JobRole.xSales.Representative		  |  Not Referred   | 0.06827         | 2087             |
#|-------------------------------------------------------------------------------------------------------|
#|Model-9    | StockOptionLevel.x3			          |  Not Referred   | 0.057679        | 2088.5           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-10   | StockOptionLevel.x1			          |  Not Referred   | 0.109259        | 2090.4           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-11   | JobLevel					                  |  Not Referred   | 0.027047        | 2090.9           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-12   | JobRole.xManager				            |  Not Referred   | 0.013137        | 2094             |
#|-------------------------------------------------------------------------------------------------------|
#|Model-13   | BusinessTravel.xTravel_Rarely		  |  Not Referred   | 0.006853        | 2098.7           |
#|-------------------------------------------------------------------------------------------------------|
#|Model-14   |						                        |                 |                 | 2104.7           |
#|-------------------------------------------------------------------------------------------------------|

