setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")
bushData = read.delim ("Bushtucker.dat", header =TRUE)
View(bushData)
library(ggplot2)
library(pastecs)
library(reshape2)
library(Hmisc)
library(psych)

#...convert from wide to long format
longBushData = melt(bushData, #the name of the dataframe you want to reshape
                id = c("participant"), #the variables that you want to leave unchanged - they correspond to the subject
                variable.name="Food", #the new variable that labels the conditions
                value.name="Retch") #the name of the outcome variable

View(longBushData)
str(longBushData)

#descriptives by food types
by(longBushData$Retch, longBushData$Food, stat.desc, basic = FALSE, norm = TRUE)

#Plotting the data
by(longBushData, longBushData$Food, function(x) hist(x$Retch,main=unique(x$Food)))


#Checking Normality
by(longBushData, longBushData$Food, function(x) shapiro.test(x$Retch) )



#Contrasts

PartsvsWhole=c(1,-1,-1,1)
TesticlevsEye=c(0,-1,1,0)
StickvsGrub=c(-1,0,0,1)
contrasts(longBushData$Food)<- cbind(PartsvsWhole,TesticlevsEye,StickvsGrub)

#alternative 
#...number of contrasts
#contrasts(longBushData$Food)=contr.poly(4)

#Repeated ANOVA
library(ez)

longBushModel=ezANOVA(longBushData, dv = .(Retch),wid= .(participant),within = .(Food),detailed = TRUE, type=3)
longBushModel

#Follow up tests - Pairwise using bonferroni
pairwise.t.test(longBushData$Retch,longBushData$Food,paired=TRUE,p.adjust.method = "bonferroni",pooled.sd=FALSE)


#bar chart 
# add the confidence intervals
bar <- ggplot(longBushData, aes(Food, Retch))
bar + 
  stat_summary(fun.y = mean, ### Plot the means
               geom = "bar")+ ### plot as a bar graph
  stat_summary(fun.data = mean_cl_normal, #gets the summary statistics 95%confidnence interval
               geom = "errorbar") + #plots a line with a value for a point
  labs(x = "Food Type", y = "Mean Retch Time") 

library(apaTables)
apa.1way.table(Food,Retch,data=longBushData)