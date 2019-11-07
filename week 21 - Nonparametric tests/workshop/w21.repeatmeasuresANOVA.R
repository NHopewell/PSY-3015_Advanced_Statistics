
################################
#------------------------------#
#---START OF WORKSHOP----------#
#------------------------------#
################################


#...ONE WAY REPEATED MEASURES ANOVA

#...set working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")

#...open data
bushData = read.delim ("Bushtucker.dat", header =TRUE)

#...visually inspect data
View(bushData)
#...UGHH it is in wide format!!!

library(ggplot2)
library(pastecs)

library(Hmisc)
library(psych)

#...convert from wide to long format
library(reshape2)
longBushData = melt(bushData, #the name of the dataframe you want to reshape
                id = c("participant"), #the variables that you want to leave unchanged - they correspond to the subject
                variable.name="Food", #the new variable that labels the conditions
                value.name="Retch") #the name of the outcome variable
#...visually inspect the data
View(longBushData)

#...check variable coding
str(longBushData)

#...examine data
summary(longBushData)
#...multiple observations per participant
#...DV from 1 to 12??? is this what is expected???


#...examine dependent variable 
#...split by condition 
library(pastecs)
by(longBushData$Retch, #...DV
   longBushData$Food,  #...IV
   stat.desc,   #... function
   basic = FALSE, 
   norm = TRUE)  #return skew and kurtosis

#...visuallize the data 
#...plot the DV using histograms
par(mfrow=c(1,4))
by(longBushData,   #..dataframe
   longBushData$Food, #...IV
   function(x)   #...use function
     hist(x$Retch,  #... histogram on DV
          main=unique(x$Food))) #...add title

#...EXAMINE ASSUMPTIONS
#...normality in each cell
#...homogeneity of variance across cells
#...sphericity

#...Checking Normality
by(longBushData, #...dataframe
   longBushData$Food, #...IV
   function(x) #use a function
     shapiro.test(x$Retch) ) #run shapiro on DV

#...Test homogeneity of variance
library(car)
leveneTest(Retch~Food,  #...formula
           data= longBushData,  #..dataframe
           center = "median")  #.use median center


#...Contrasts
contrasts(longBushData$Food) = contr.poly(4)


#...Repeated ANOVA
library(ez)

longBushModel=ezANOVA(longBushData, #...dataframe
                      dv = .(Retch),  #...DV
                      wid= .(participant),  #..subject ID
                      within = .(Food),  #within IV
                      detailed = TRUE,  #give sphericity
                      type=3)  #...type 3 sums of squares
longBushModel

#Follow up tests - Pairwise using bonferroni
pairwise.t.test(longBushData$Retch,  #DV
                longBushData$Food,  #IV
                paired=TRUE,  #repeated measures
                p.adjust.method = "bonferroni",
                pooled.sd=FALSE) #...doesn't matter here


#...Graph the data with confidence intervals
#...for report  
# add the confidence intervals
library(ggplot2)
bar <- ggplot(longBushData, aes(Food, Retch))
bar + 
  stat_summary(fun.y = mean, ### Plot the means
               geom = "bar")+ ### plot as a bar graph
  stat_summary(fun.data = mean_cl_normal, #gets the summary statistics 95%confidnence interval
               geom = "errorbar",
               width = .3) + #plots a line with a value for a point
  labs(x = "Food Type", y = "Mean Retch Time") 

#...make a table for the report
library(apaTables)
apa.1way.table(iv = Food,
               dv = Retch,
               data=longBushData)
