setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")
ViagraData= read.delim("Viagra.dat", header = TRUE)

View(ViagraData)

#...check the coding of variables
str(ViagraData)

#...convert sjNum and dose into a factor
ViagraData$person = factor(ViagraData$person)
ViagraData$dose=factor(ViagraData$dose,labels=c("Placebo","Low Dose","High Dose"))
str(ViagraData)

#...describe your data
library(psych)
describe(ViagraData)
summary(ViagraData)


#...examine your DV 
hist(ViagraData$libido)

#...probably want to split by group
par(mfrow=c(1,3))
by(ViagraData,ViagraData$dose,function(x)
  hist(x$libido, 
       main = unique(x$dose),
       xlim=c(0,8)))

#...Examine the assumptions:
#...Predictor/IV must be Categorical- use structure function 
#...Outcome/DV must be measured on an interval or better scale - use structure function to assist
#...Outcome/DV is Unbounded (Practical pursposes-no ceiling/floor effect)- check histogram of DV
#...Homogeneity of Variance - use Levenes Test
#...Noraml Distribution of Errors(Residuals) 

describeBy(ViagraData$libido,ViagraData$dose,mat= TRUE,digits = 1)
#ASSUMPTION - HOMOGENEITY OF VARIANCE
library(car)

#Levene's test for comparison of variances of libido scores by dose.
#leveneTest(outcomevariable, group variable, center =median/mean)
leveneTest(ViagraData$libido, ViagraData$dose)  #DEFAULT USING THE MEDIAN AS THE CENTER

#ASSUMPTION - NORMALLY DISTRIBUTED ERRORS
#run the ANOVA
ViagraModel = aov(libido~dose, data=ViagraData)
summary(ViagraModel)

#...calculate predicted values and add them to the data.frame
#...notice that they match our Model predictions
ViagraData$predicted = predict(ViagraModel)
head(ViagraData,n=10)

#...calculate the residual values and add them to the data.frame
#...notice that match our hand calculations
ViagraData$residuals = resid(ViagraModel)
head(ViagraData,n=10)

#ASSUMPTION - NORMALLY DISTRIBUTED ERRORS
#METHOD # 1 - histogram of residuals
par(mfrow=c(1,1))
hist(ViagraData$residuals)


#METHOD # 2 CAN  USE QQPLOTS
#QQPlot of residuals

qqplot.residuals <- qqnorm(ViagraData$residuals,
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles RESIDUALS")
qqline(ViagraData$residuals)



#METHOD NUMBER #3 USE SHAPIRO-WILKS TEST
shapiro.test(ViagraData$residuals)


#METHOD #4 USE SKEW AND KURTOSIS
library(pastecs)
stat.desc(ViagraData$residuals, basic = FALSE, norm = TRUE)


#To make Tables
#Method 1
#...create an ANOVA
ViagraModel<- aov(libido~dose,data=ViagraData)
summary(ViagraModel)

#...get the means
print(model.tables(ViagraModel,"means"),digts=3)

#Method 2
#...can also use the describeBy command

describeBy(ViagraData$libido,ViagraData$dose,mat=TRUE,digits=1)

#Method 3...Make an APA table 
library(apaTables)
apa.1way.table(iv=dose,dv=libido,data = ViagraData)

#Create an APA table for report 
#...Set the working directory
setwd("F:/Workshop Psyc3015 2017 2018/Lab 13  one wy ANOVA")
#you can view the table in your working directory
apa.1way.table(iv=dose,
               dv=libido,
               data = ViagraData, 
               show.conf.interval = TRUE,
               filename = "onewayTable.doc",
               landscape = FALSE)
#effect size 
library(lsr)
etaSquared(ViagarModel)
