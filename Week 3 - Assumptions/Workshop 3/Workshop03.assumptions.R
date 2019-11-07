#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 5 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

##########################################
#----Set the working directory------
##########################################
 
setwd("C:/Users/nicho/Desktop/My Files/advanced stats 3015/Data files")


##########################################
#----INSTALL PACKAGES-----
#----Check to see if these are installed unter the packages window.
#----If you are using a University computer they should already be installed.
#----If they are not installed on a unversity computer, let the instructer know right away.
#----If you are using your own computer you may have to install them.
##########################################
install.packages("car")   #Companion to Applied Regression
install.packages("ggplot2")  #graphing package
install.packages("pastecs")  #Regulation, decomposition and analysis of space-time series.
install.packages("psych") #A general purpose toolbox for personality, psychometrics and experimental psychology


##########################################
#----LOAD PACKAGES-----
##########################################
library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(grid)
library(gridExtra)

#READ IN THE DOWNLOAD FESTIVAL DATA AGAIN:
dlf <- read.delim("DownloadFestival(No Outlier).dat", header=TRUE)
View(dlf)



#view structure of dlf
str(dlf)
#change ticknumb to factor
dlf$ticknumb = factor(dlf$ticknumb)
str(dlf)

#get descriptives
describe(dlf)



##########################################
#Histogram for day 1:
#NOTE: HERE THE GRAPH OBJECT IS BUILD AND FILLED IN A SINGLE STEP
#BECAUSE THE <- OPERATOR IS USED ALL OF THE FEATURES WE ADD TO THE GRAPH
#ARE LOADED INTO THE GRAPH OBJECT.


##########################################
#Add the curves to the Histograms:
##########################################

#stat_function - add a statistical function to the graph
#fun = dnorm - add a normal density function
#args (we need to add two arguments for a normal distribution (mean and sd))
# set the mean to the mean of dlf$day1
#set the sd to the sd of dlf$day1
#na.rm = TRUE (remove the NA(missing data))

##########################################
hist.day1 <- ggplot(dlf, aes(day1)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="white") +
      labs(x="Hygiene score on day 1", y = "Density")

hist.day1

hist.day1+
      stat_function(fun = dnorm, 
              args = list(mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), 
              colour = "black", size = 1)


 
###creating the histogram and normal curve at the same time:

hist.day1 <- ggplot(dlf, aes(day1)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Hygiene score on day 1", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), 
                colour = "black", size = 1)

hist.day1  # DO THIS TO VIEW THE GRAPH



#create the histograms for Day2 and Day3

hist.day2 <- ggplot(dlf, aes(day2)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Hygiene score on day 2", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(dlf$day2, na.rm = TRUE), sd = sd(dlf$day2, na.rm = TRUE)), 
                colour = "black", size = 1)

hist.day3 <- ggplot(dlf, aes(day3)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Hygiene score on day 3", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(dlf$day3, na.rm = TRUE), sd = sd(dlf$day3, na.rm = TRUE)), 
                colour = "black", size = 1)





#YOU CAN USE THE FOLLOWING CODE TO GRAPH THEM TOGETHER
library(grid)
library(gridExtra)
grid.arrange(hist.day1, 
             hist.day2,
             hist.day3,
            nrow=1, ncol = 3)  #NCOL * NROW MUST BE > THAN THE NUMBER OF GRAPHS



#CREATE GRAPHS FOR MULTIPLE GROUPS OF ONE DEPENDENT VARIABLE

#HOW I PREFER TO DO CREATE THE SUBGROUPS
#NOTE THIS IS DIFFERENT THAN THE TEXTBOOK
maleData<-dlf[dlf$gender=="Male",]
femaleData<-dlf[dlf$gender=="Female",]


#CREATE HISTOGRAMS FOR DAY1 AND DAY2 FOR MALES AND FEMALES

#CREATE A HISTOGRAM WITH THE DENSITY PLOT FOR THE MALEDATA, DAY1

hist.day1.male <- ggplot(maleData, aes(day1)) + 
  labs(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
  labs(x = "MALE HYGIENE DAY 1", y = "Density") + 
  stat_function(fun=dnorm, 
                args=list(mean = mean(maleData$day1, na.rm = TRUE), sd = sd(maleData$day1, na.rm = TRUE)),
                colour = "red", size=1)
hist.day1.male 

#CREATE A HISTOGRAM WITH THE DENSITY PLOT FOR THE MALE DATA, DAY2

hist.day2.male <- ggplot(maleData, aes(day2)) + 
  labs(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
  labs(x = "MALE HYGIENE DAY 2", y = "Density") + 
  stat_function(fun=dnorm, 
                args=list(mean = mean(maleData$day2, na.rm = TRUE), sd = sd(maleData$day2, na.rm = TRUE)),
                colour = "red", size=1)
hist.day2.male


#CREATE A HISTOGRAM WITH THE DENSITY PLOT FOR THE FEMALE DATA, DAY1

hist.day1.female <- ggplot(femaleData, aes(day1)) + 
  labs(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
  labs(x = "FEMALE HYGIENE DAY 1", y = "Density") + 
  stat_function(fun=dnorm, 
                args=list(mean = mean(femaleData$day1, na.rm = TRUE), sd = sd(femaleData$day1, na.rm = TRUE)), 
                colour = "red", size=1)
hist.day1.female

#CREATE A HISTOGRAM WITH THE DENSITY PLOT FOR THE FEMALE DATA, DAY2

hist.day2.female <- ggplot(femaleData, aes(day2)) + 
  labs(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
  labs(x = "FEMALE HYGIENE DAY 2", y = "Density") + 
  stat_function(fun=dnorm, 
                args=list(mean = mean(femaleData$day2, na.rm = TRUE), sd = sd(femaleData$day2, na.rm = TRUE)), 
                colour = "red", size=1)
hist.day2.female


#VIEW THE GRAPHS ALL TOGETHER
grid.arrange(hist.day1.male, 
             hist.day1.female,
             hist.day2.male,
             hist.day2.female,
             ncol = 2, nrow=2)



##########################################
##########################################
#Q-Q plot for day 1:
## FIXED FROM PAGE 171
##########################################
##########################################
par(mfrow=c(1,1))
qqplot.day1 <- qqnorm(dlf$day1,
                      xlab = "Theoretical Quantiles", 
                      ylab = "Sample Quantiles DAY 1")
qqline(dlf$day1)



# CREATE QQPLOTS FOR DAY 2 AND DAY 3

qqplot.day2 <- qqnorm(dlf$day2,
                      xlab = "Theoretical Quantiles", 
                      ylab = "Sample Quantiles DAY 2")
qqline(dlf$day2)



qqplot.day3 <- qqnorm(dlf$day3,
                      xlab = "Theoretical Quantiles", 
                      ylab = "Sample Quantiles DAY 3")
qqline(dlf$day3)


#IF YOU WANT TO VIEW THEM SIDE BY SIDE SET THE DISPLAY
#TO SHOW 3 GRAPHS AND THEN CREATE THE GRAPHS AGAIN.
par(mfrow=c(1,3))  #TURNS THE DISPLAY INTO 1 ROW OF 3 COLUMNS



##########################################
##########################################
#qqplots for the day 1 and day 2 variables
# split by group
##########################################
##########################################

par(mfrow=c(1,2))  #TURNS THE DISPLAY INTO 1 ROW OF 2 COLUMNS

qplot.day1.male <- qqnorm(maleData$day1,
                          xlab = "Theoretical Quantiles", 
                          ylab = "Sample Quantiles MALE DAY1")

qqline(maleData$day1)

qplot.day1.female<- qqnorm(femaleData$day1,
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles FEMALE DAY1")

qqline(femaleData$day1)

#create the qplot for male and female on day 2

qplot.day2.male <- qqnorm(maleData$day2,
                          xlab = "Theoretical Quantiles", 
                          ylab = "Sample Quantiles MALE DAY2")

qqline(maleData$day2)

qplot.day2.female<- qqnorm(femaleData$day2,
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles FEMALE DAY2")

qqline(femaleData$day2)



#PART TWO GETTING NUMBERS
##########################################
#Quantifying normality with numbers
# SECTION 5.5.2,  PAGE 173
##########################################



#GET DESCRIPTIVE STATISTICS FOR A SINGLE VARIABLE
#REMEMBER THIS FUNCTION GIVE USE THE VALUES NECESSARY TO COMPARE SKEW AND KURTOSIS ON THE Z DISTRIBUTION
#ALSO GIVES US THE SHAPIRO-WILK TEST (normtest.W, normtest.p)
# stat.desc(variable, basic, norm)
# basic = TRUE, give only basic statistics
# norm = TRUE, give normality statistics
stat.desc(dlf$day1, basic = FALSE, norm = TRUE)
round(stat.desc(dlf$day1, basic = FALSE, norm = TRUE),3)
# ^^^ rounds the output to 3 decimal places

#TRY IT FOR DAY 2
stat.desc(dlf$day2, basic = FALSE, norm = TRUE)
round(stat.desc(dlf$day2, basic = FALSE, norm = TRUE),3)



#GET DESCRIPTIVE STATISTICS FOR MULTIPLE VARIABLES
#ROUND THE VALUES TO 3 DECIMAL PLACES
#round(values, digits = )
# dlf[,c("day1","day2")]  # says return all of the rows for columns "day1"and "day2" 
round(stat.desc(dlf[, c("day1", "day2")], basic = FALSE, norm = TRUE), digits = 3)


#CAN YOU DO IT FOR DAY3???
#WHAT ABOUT ALL OF THEM 
#Look at skew, kurtosis, skew.2SE, kurtosis.2SE - What does it tell you?

round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE), digits = 3)

##########################################
##########################################
# BECAUSE OUR DESIGN HAS TWO GROUPS (GENDER) IT IS NECESSARY
# TO EXAMINE THE CHARACTERISTICS OF EACH DV SEPARATELY FOR EACH LEVEL
# OF THE IV (GENDER)
#
#Use by() to get descriptives for one variable, split gender
#SECTION 5.5.3.2, PAGE 179
##########################################
##########################################


#by(data,INDICES,FUNCTION)  # SEE PAGE 179
by(dlf$day1, dlf$gender, stat.desc, basic = FALSE, norm = TRUE)  #STAT.DESC, ONE VARIABLE


### >>Use by() to get descriptives for all days split by gender!!!! 
# Can also ROUND the output to 3 decimal places
by(dlf[, c("day1", "day2", "day3")], dlf$gender, stat.desc,basic = FALSE, norm = TRUE)

#look at the skewness, kurtosis, skew.2E and kurtosis.2E - what does it tell you 

##########################################
##########################################
#SECTION 5.6.1, PAGE 182
#
#TESTING WHETHER A DISTRIBUTION IS NORMAL
##########################################
##########################################



#Shapiro-Wilks test for day 1 and day 2 for whole sample

shapiro.test(dlf$day1)  # RUN THE SHAPIRO-WILKS TEST 


#CAN YOU RUN IT FOR DAYS 2 AND 3???
shapiro.test(dlf$day2) 

shapiro.test(dlf$day3)
##########################################
#Shapiro-Wilks test for day1,2,3 split by gender
##########################################
by(dlf$day1, dlf$gender, shapiro.test)
by(dlf$day2, dlf$gender, shapiro.test)
by(dlf$day3, dlf$gender, shapiro.test)

# result:VIOLATIONS OF NORMALITY EXCEPT FOR MALES Day 1



##########################################
##########################################
#Levene's test for comparison of variances of day 1 scores by gender.
#SECTION 5.7.1.1
##########################################
##########################################
#leveneTest(outcomevariable, group variable, center =median/mean)
# For levenes you need two groups to compare. Here we use gender. Basically a t test of variances.
# We the median here, this is actually called the  Brown-Forsythe test techncally. 
# median is best according to dr chan reynolds.
leveneTest(dlf$day1, dlf$gender)  #DEFAULT USING THE MEDIAN A S THE CENTER
leveneTest(dlf$day1, dlf$gender, center = mean)  #CHANGE THE CENTER TO MEAN

leveneTest(dlf$day2, dlf$gender)  # MEDIAN AS CENTER
# REMEMBER THE LEVENE TEST IS JUST LIKE ANY OTHER IF THE P<.05 THEN THERE IS A DIFFERENCE
# if the p is >.05 then the variances are not even.



