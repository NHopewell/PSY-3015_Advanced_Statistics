#######Workshop Dependent t-test############### 

#load data file
setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")
spiderData=read.delim("spiderLong.dat",header=TRUE)
head(spiderData)
View (spiderData)

library(ggplot2)
library(reshape2)
library(psych)
library(pastecs)

#....lets add subject numbers

spiderData$sj = c(seq(1,12,1),seq(1,12,1))

head(spiderData)

#check the structure
str(spiderData)

spiderData$sj = factor(spiderData$sj)
str(spiderData)

#describe your data
describe(spiderData)


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

#...convert from wide to long format
longData = melt(wideData, #the name of the dataframe you want to reshape
                id = c("sj"), #the variables that you want to leave unchanged - they correspond to the subject
                variable.name="condition", #the new variable that labels the conditions
                value.name="Anxiety") #the name of the outcome variable
head(longData)
View(longData)

#....CALCULATE THE DIFFERENCE SCORES
wideData$diff = wideData$Picture-wideData$'Real Spider'
head(wideData)

#....TEST ASSUMPTIONS
# normality

hist(wideData$diff)

round(stat.desc(wideData[, c("Picture", "Real Spider","diff")], basic = FALSE, norm = TRUE), digits = 3)

qqnorm(wideData$diff)
qqline(wideData$diff)

shapiro.test(wideData$diff)



#...run the t-test from long format
t.test(Anxiety~Group,data=spiderData,paired = TRUE)




#calculate effect size
#Effect sizes
dep.t.test = t.test(Anxiety~Group,data=spiderData,paired = TRUE)
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

