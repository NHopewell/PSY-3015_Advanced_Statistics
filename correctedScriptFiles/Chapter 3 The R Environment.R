#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 3 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

##################################
#WHERE TO GET THE FILES FROM!!!
#https://us.sagepub.com/en-us/nam/discovering-statistics-using-r/book236067#resources
##################################



##################################
####HOW TO INSTALL PACKAGES FOR RUNNING SPECIFIC STATS####
##################################

install.packages("foreign")
install.packages("Hmisc")
#install.packages("Rcmdr", dependencies = TRUE)
install.packages("reshape")


##################################
####HOW TO LOAD THE PACKAGES THAT YOU HAVE DOWNLOADED#####
##################################
library(forign)
library(Rcmdr)
library(reshape)

##################################
#### SET THE WORKING DIRECTORY SO THAT YOU CAN ACCESS DATA FILES EASILY ####
##################################
#setwd("~/Public/Academic/Data/DSU_R/Chapter 03 (The R Environment)")
#setwd("~/Documents/Academic/Data/DSU_R/Chapter 03 (The R Environment)")

##################################
#-----------Metallica Data---------------------------------------------------------------------------
##################################

# USE THE CONCATENATE FUNCTION TO PUT 4 CHARACTER STRINGS INTO A SINGLE VARIABLE
metallica<-c("lars", "james", "Jason", "Kirk")

#VIEW WHAT IS IN THE VARIABLE
metallica 

# SET METALLICA TO METALLICA WHERE METALLICA IS NOT EQUAL TO JASON
# BE AWARE OF THE [] USED TO INDEX A VARIABLE
metallica<-metallica[metallica != "Jason"]
metallica

# USE THE CONCATENATE COMMAND TO ADD ROB TO THE END OF METALLICA
metallica<-c(metallica, "Rob")
metallica

# USE THE CONCATENATE COMMAND TO CREATE TWO NEW VARIABLES
metallicaNames<-c("Lars", "James", "Kirk", "Rob")
metallicaAges<-c(47, 47, 48, 46)


##################################
# FROM R'S SOULS' TIP 3.5
##################################


# TWO WAYS OF COMBINING VARIABLES.  WE PROBABLY WILL NOT USE THESE VERY OFTEN
# THE LIST COMMAND CREATES A VARIABLE WITH TWO ORGANIZED LISTS, WHERE EACH LIST CONSISTS OF ONE OF THE VARIABLES
metallica<-list(metallicaNames, metallicaAges)
metallica

# CBIND CREATES A MATRIX FROM THE VARIABLES WERE EACH VARIABLE FORMS A NEW COLUMN
metallica<-cbind(metallicaNames, metallicaAges)
metallica

#### DATA.FRAMES!!!!
#### THE MOST COMMON WAY OF COMBINING VARIABLES.
metallica<-data.frame(Name = metallicaNames, Age = metallicaAges)
View(metallica)

#ADD A NEW VARIABLE TO OUR DATAFRAME
metallica$childAge<-c(12, 12, 4, 6)

# CREATE A NEW VARIABLE FROM EXISTING ONES IN OUR DATA FRAME.
metallica$fatherhoodAge<-metallica$Age-metallica$childAge
View(metallica)

# THE NAMES COMMAND (BOTTOM OF PAGE 42.), RETURNS THE VARIALBE NAMES IN THE DATA.FILE
names(metallica)



#-----------Lecturer Data---------------------------------------------------------------------------
##################################
#STARTS IN SECTION 3.5.4.1 ON PAGE 87
##################################
#CREATE A STRING VARIABLE
name<-c("Ben", "Martin","Andy","Paul", "Graham","Carina","Karina","Doug","Mark", "Zoe")
name


#Default date format is yyyy-mm-dd
#IF VALUES ARE ADDED USING THE AS.DATE FUNCTION, THEN THEY CAN BE SUBTRACTED AND ADDED.
birth_date<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1949-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))

#CREATE A FACTOR JOB
#ENTER THE VALUES MANUALLY
job<-c(1,1,1,1,1,2,2,2,2,2)

#ENTER THE VALUES USING THE REP COMMAND  rep(value, number of times)
job<-c(rep(1, 5),rep(2, 5))  #NOTE THE USE OF THE CONCATENATE COMMAND AGAIN!!!

#NORMALLY R TREATS NUMERICAL VALUES AS NUMBERS (CONTINOUS) WE WANT THIS TO BE A FACTOR WHICH MEANS CATEGORIES
#TO DO THIS WE USE THE FACTOR COMMAND
job<-factor(job, levels = c(1:2), labels = c("Lecturer", "Student"))

levels(job)  # returns the levels of the factor "job"

#ANOTHER WAY OF CREATING A FACTOR
# REQUIRES SPECIFYING THE NUMBER OF LEVELS AND THE NUMBER OF OBSERVATIONS PER GROUP
job<-gl(2, 5, labels = c("Lecturer", "Student"))  


# CREATING NUMERICAL VARIABLES  -- SECTION 3.5.4.4
friends<-c(5,2,0,4,1,10,12,15,12, 17)
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
neurotic<-c(10,17,14,13,21,7,13,9,14,13)

#COMBINE THE VARIABLES USING THE DATA FRAME COMMAND.
lecturerData<-data.frame(name, birth_date, job, friends, alcohol,income, neurotic)


##################################
#--------R souls tip 3.5-----------
##################################

#DEMONSTRATES THE SUBTRACTION OF AS.DATE VARIABLES

#FIRST SHOW THAT ENTERING THE DATES AS STRINGS DOES NOT ALLOW SUBTRACTION
husband<-c("1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24")
wife<-c("1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23")
agegap <- husband-wife

#NEXT DEMONSTRATE THAT USING THE AS.DATE COMMAND ALLOWS SUBTRACTION
husband<-as.Date(c("1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24"))
wife<-as.Date(c("1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23"))
agegap <- husband-wife
agegap

#CAN COVERT INTO YEARS FOR THE STUDENTS IF YOU LIKE
agegap/365


#WE WILL NOT DO THE DEMONSTRATIONS USING R COMMANDER 
##################################
#### 3.7.1 IMPORTING DATA
##################################

#--------Importing files-----------

#LOAD IN A COMMA DELIMITED FILE
#IF THERE ARE PROBLEMS, MAKE SURE THAT THEY HAVE THEIR WORKING DIRECTORY SET PROPERLY!!!!
#### FOR THE FIRST ONE. WE NEED TO CREATE THE FILE IN EXCEL FROM THE ".DAT" VERSION OF THE FILE
lecturerData<-read.csv("Lecturer Data.csv", header = TRUE)
lecturerData$job<-factor(lecturerData$job, levels = c(1:2), labels = c("Lecturer", "Student"))

lecturerData<-read.delim("Lecturer Data.dat", header = TRUE)
View(lecturerData)
lecturerData<-read.delim("Lecturer Data.txt", header = TRUE)
View(lecturerData)

#SO THE STUDENTS CAN SEE THE CONSEQUENCES OF CHANGING JOB INTO A FACTOR
lecturerData$job<-factor(lecturerData$job, levels = c(1:2), labels = c("Lecturer", "Student"))
View(lecturerData)

#WE CAN SKIP THESE AS WE WILL NOT BE USING SPSS
#library(foreign)
#lecturerData<- read.spss("Lecturer Data.sav",use.value.labels=TRUE, to.data.frame=TRUE)
#lecturerData$birth_date <- as.Date(as.POSIXct(lecturerData$birth_date , origin="1582-10-14"))

##################################
##### 3.8 SAVING DATA 
##################################
#--------Exporting files-----------
write.table(metallica, "Metallica Data.txt", sep="\t", row.names = FALSE)
write.csv(metallica, "Metallica Data.csv")

##################################
# SECTION 3.9
#--------Selecting Data-----------
##################################

#CREATE A NEW VARIABLE WITH ALL OF THE ROWS FOR THE COLUMNS FRIENDS, ALCOHOL AND NEUROTIC
lecturerPersonality <- lecturerData[, c("friends", "alcohol", "neurotic")]
lecturerPersonality

#CREATE A NEW VARIABLE WHERE THE ROWS OF JOB = LECTURER, RETURN ALL THE COLUMNS 
lecturerOnly <- lecturerData[job=="Lecturer",]
lecturerOnly

#COMBINE THE PREVIOUS TWO EXAMPLES SO THAT ONLY ROWS WERE ALCOHOL IS > 10 AND 3 COLUMNS ARE RETURNED

#NOTE THIS ONE DIDN'T WORK FOR ME I HAD TO USE THE REVISED ONE BELOW:
alcoholPersonality <- lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")]

alcoholPersonality <- lecturerData[lecturerData$alcohol > 10, c("friends", "alcohol", "neurotic")]
alcoholPersonality


#I'M NOT SURE WHY THESE ARE IN THE SCRIPT PAGE, WE WILL NOT LIKELY USE THEM.
#alcoholPersonalityMatrix <- as.matrix(alcoholPersonality)
#alcoholPersonalityMatrix

#alcoholPersonalityMatrix <- as.matrix(lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")])


##################################
# SECTION 3.9.2
#--------Subset-----------
##################################

lecturerOnly <- subset(lecturerData, job=="Lecturer")
alcoholPersonality <- subset(lecturerData, alcohol > 10, select = c("friends", "alcohol", "neurotic"))


##################################
# GET THE STUDENTS TO DO THIS ON THEIR OWN
# P 106.
#--------self test-----------
##################################

highEarners <- lecturerData[income>=10000, c("name", "job", "income")]
highEarners <- subset(lecturerData, income>=10000, select = c("name", "job", "income"))

soberPeople <- lecturerData[alcohol<=12, c("name", "job", "income",  "friends")]
soberPeople <- subset(lecturerData, alcohol<=12, select = c("name", "job", "income",  "friends"))

neuroticOrAlcoholic <- lecturerData[alcohol>=20|neurotic > 14,]
neuroticOrAlcoholic <- subset(lecturerData, alcohol>=20|neurotic > 14)

##################################
# SECTION 3.9.4
#--------Restructuring Data-----------
##################################

#OPEN A NEW DATA FILE
satisfactionData = read.delim("Honeymoon Period.dat",  header = TRUE)
View(satisfactionData)

#CONVERT SATISFCATIONDATA INTO LONG FORMAT
satisfactionStacked<-stack(satisfactionData, select = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
View(satisfactionStacked)

#CREATE A WIDE DATA FRAME BY UNSTACKING
# THE PROBLEM WITH THE STACK AND UNSTACK METHODS IS THAT WE LOSE OUR INDEX VALUES
satisfactionUnstacked<-unstack(satisfactionStacked, values~ind)
View(satisfactionUnstacked)


#CONVERT INTO LONG FORMAT USING RESHAPE
#USE RESHAPE TO TURN OUR TIME VARIABLES INTO A SINGLE TIME VARIABLE WITH MULTIPLE LEVELS
restructuredData<-reshape(satisfactionData, idvar = c("Person", "Gender"),  #THE VARIABLES TO KEEP AND REPEAT
        varying = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"),  # THE VARIABLES THAT WILL BE COMBIEND INTO THE NEW VARIABLE
        v.names = "Life_Satisfaction", timevar = "Time", times = c(0:3), direction = "long")  #THE NEW VARIABLE NAMES AND DIRECTION

View(restructuredData) # VIEW THE NEW DATAFRAME

#USE THE ORDER COMMAND TO RESORT THE DATA (ROWS) SO THAT ALL OF AN INDIVIDUAL DATA POINTS ARE TOGETHER
restructuredData.sorted<-restructuredData[order(restructuredData$Person),]

View(restructuredData.sorted)

#USE THE MELT COMMAND TO CONVERT A WIDE FORMAT INTO A LONG FORMAT
#IF IT DOES NOT WORK, MAKE SURE THAT THE "RESHAPE" PACKAGE IS LOADED
restructuredData2<-melt(satisfactionData, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
View(restructuredData2)

# THE COMMAND HAS CHANGED TO INDICATE WHAT TYPE OF VARIABLE THAT YOU WANT TO USE
#CHANGE "cast" to "dcast"
wideData<-dcast(restructuredData2, Person + Gender ~ variable, value = "value")
View(wideData)

##################################
----Smart Alex Task 1-------
##################################

write.table(lecturerData, "Lecturer Data.txt", sep="\t", row.names = FALSE)
write.csv(lecturerData, "Lecturer Data.csv")

----Smart Alex Task 2-------
Method<-c(rep(1,10), rep(2,10))
Method<-factor(Method, levels = c(1:2), labels = c("Electric Shock", "Being Nice"))
Gender<-c(rep(0, 5),rep(1, 5), rep(0, 5),rep(1, 5))
Gender<-factor(Gender, levels = c(0:1), labels = c("Male", "Female"))
Mark<-c(15,14,20,13,13,6,7,5,4,8,10,9,8,8,7,12,10,7,8,13)
teachingMethodData<-data.frame(Method, Gender, Mark)
teachingMethodData
write.table(teachingMethodData, "teachingMethodData.txt", sep="\t", row.names=FALSE)

----Task 3-----
Gender<-c(rep(0,12),rep(1,12)
Gender<-factor(Gender,levels=c(0:1),labels=c("Male","Female"))
Partner<-c(69,76,70,76,72,65,82,71,71,75,52,34,70,74,64,43,51,93,48,51,74,73,41,84)
Self<-c(33,26,10,51,34,28,27,9,33,11,14,46,97,80,88,100,100,58,95,83,97,89,69,82)
infidelityData<-data.frame(Gender, Partner, Self)
infidelityData
write.csv(infidelityData, "Infidelity Data.csv")

##################################
#  WORKSHOP TASK
##################################

# CREATE A NEW DATAFILE WITH THE FOLLOWING INFORMATION
# LNAME
# FNAME
# SEX
# BIRTHDATE
# HEIGHT (CM)
# RELATIONSHIP (Y/N) ARE YOU CURRENTLY IN A RELATIONSHIP
# PHEIGHT PARTNER'S HEIGHT



#use the following variable names
#lname
#fname
#sex
#bdate
#height
#relationship
#pheight


#SAVE THE FILE AS W1G1R1C1.TXT
# W = WEEK
# G = SEMINAR GROUP
# R = ROW YOU ARE IN
# C = COLUMN YOU ARE IN

