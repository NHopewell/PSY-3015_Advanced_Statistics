
################################
#------------------------------#
#---START OF WORKSHOP----------#
#------------------------------#
################################


#######Workshop Dependent t-test############### 

#...set the working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")

#...load data file
spiderData=read.delim("spiderLong.dat",header=TRUE)

#...visually inspect the data
head(spiderData)
View(spiderData)

#...N.B. the data is in long format.
#...to illustrate the assumptions of a dependent t-test
#...we will reshape the data.
#...This will require adding subject numbers so that we
#...can keep track of which observations belong to which person



#....lets add subject numbers
spiderData$sj = c(seq(1,12,1),seq(1,12,1))

#...visually inspect the data to make sure
#...the subject numbers were added properly
head(spiderData)

#...check the structure
str(spiderData)

#... sj should be a factor
spiderData$sj = factor(spiderData$sj)
str(spiderData)

#...explore the data
library(psych)
describe(spiderData)

#...NEXT WE WILL RESHAPE THE DATA
library(reshape2)
#....convert to wide format
#GENERIC
#wideData = dcast(longData, #the name of the dataframe you want to reshape
#                 SJ  #row variables 
#                 ~y, #row variables ~ column variables
#                 value.var = "score") 


#ACTUAL
wideData = dcast(spiderData, #the name of the dataframe you want to reshape
                 sj  #row variables 
                 ~Group, #row variables ~ column variables
                 value.var = "Anxiety") 
View(wideData)
#...see  how the take is linked to the sj number that we added!
#...Nice!!!

#...just in case you ever need it
#...here is how you convert from wide to long format
longData = melt(wideData, #the name of the dataframe you want to reshape
                id = c("sj"), #the variables that you want to leave unchanged - they correspond to the subject
                variable.name="condition", #the new variable that labels the conditions
                value.name="Anxiety") #the name of the outcome variable

#...visually inspect the data
head(longData)
View(longData)

#...CALCULATE THE DIFFERENCE SCORES
#...assumptions for the dependent t-test can be examined
#...on the difference scores
wideData$diff = wideData$Picture-wideData$'Real Spider'
head(wideData)

#...TEST ASSUMPTIONS
#...normality

#...histogram of the difference scores
hist(wideData$diff)

#...skew and kurtosis
library(pastecs)
round(stat.desc(wideData[, c("Picture", "Real Spider","diff")], 
                basic = FALSE, 
                norm = TRUE), 
      digits = 3)

#...qqplots of the difference scores
qqnorm(wideData$diff)
qqline(wideData$diff)

#...shapiro wilks test of the difference scores
shapiro.test(wideData$diff)



#...run the t-test from long format
t.test(Anxiety~Group,
       data=spiderData,
       paired = TRUE,
       alternative = "two.sided")


#...calculate effect size
#...Effect sizes
dep.t.test = t.test(Anxiety~Group,data=spiderData,paired = TRUE)
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

