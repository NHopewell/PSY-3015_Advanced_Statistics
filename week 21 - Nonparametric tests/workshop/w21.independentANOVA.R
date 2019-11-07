
################################
#------------------------------#
#---START OF WORKSHOP----------#
#------------------------------#
################################


#...ONE WAY INDEPENDENT ANOVA

#...set the working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")

#...open the data file
viagraData= read.delim("Viagra.dat", 
                       header = TRUE)

#...visually inspect the data
View(viagraData)

#...check the coding of variables
str(viagraData)

#...fix coding errors - convert person and dose into factors
viagraData$person = factor(viagraData$person)
viagraData$dose=factor(viagraData$dose,
                       labels=c("Placebo","Low Dose","High Dose"))
str(viagraData)

#...explore your data
summary(viagraData)

#...explore your dependent variable
library(psych)
describeBy(viagraData$libido, #..DV
           viagraData$dose,   #..IV(s)
           mat=TRUE)  #...return matrix


#...examine your DV 
#...probably want to split by group
par(mfrow=c(1,3))
by(viagraData,  #...data to split
   viagraData$dose,  #...IV for splitting
   function(x)   #...going to run a function
     hist(x$libido,   #run a histogram on DV
          main = unique(x$dose),  #add titles
          xlim=c(0,8)))  #..set the x-axis

#...Examine the assumptions:
#...Predictor/IV must be Categorical- use structure function 
#...Outcome/DV must be measured on an interval or better scale - use structure function to assist
#...Outcome/DV is Unbounded (Practical pursposes-no ceiling/floor effect)- check histogram of DV
#...Homogeneity of Variance - use Levenes Test
#...Normamally Distribution of Errors(Residuals) 

#...STEP 1  RUN THE ANOVA
viagraModel = aov(libido~dose, #...formula
                  data=viagraData)  #...dataframe
summary(viagraModel)  #...view model

#...calculate predicted values and add them to the data.frame
viagraData$predicted = predict(viagraModel)

#...calculate the residual values and add them to the data.frame
viagraData$residuals = resid(viagraModel)

#...calculate the standardized residual values and 
#...add them to the data.frame
viagraData$stdResiduals = rstandard(viagraModel)
head(viagraData,n=10)


#ASSUMPTION - NORMALLY DISTRIBUTED ERRORS

#...use histogram
par(mfrow=c(1,1))
hist(viagraData$residuals)


#...use QQPlot of residuals

qqplot.residuals <- qqnorm(viagraData$residuals,
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles RESIDUALS")
qqline(viagraData$residuals)



#...use shapiro-wilks test
shapiro.test(viagraData$residuals)


#...use skew and kurtosis
library(pastecs)
stat.desc(viagraData$residuals, 
          basic = FALSE, 
          norm = TRUE)

#...can look for outliers
viagraData[abs(viagraData$stdResiduals)>=1.96,]

#...get the values for reporting
#..Make an APA table 
library(apaTables)
apa.1way.table(iv=dose,
               dv=libido,
               show.conf.interval = TRUE,
               data = viagraData)

#Create an APA table for report 
#...Set the working directory
setwd("F:/Workshop Psyc3015 2017 2018/Lab 13  one wy ANOVA")
#you can view the table in your working directory
apa.1way.table(iv=dose,
               dv=libido,
               data = viagraData, 
               show.conf.interval = TRUE,
               filename = "onewayTable.doc",
               landscape = FALSE)
#effect size 
library(lsr)
etaSquared(viagraModel)
