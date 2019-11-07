

#...concatenate the sequence 1 to 5 by 1s and the sequence 1 to 5 by 1s
sj= c(seq(1,5,1),seq(1,5,1))

#...create our IV by concatenating the number 1 repeated five times 
#...and the number 2 repeated five times
x <- c(rep("cond1",5),rep("cond2",5))

#...concatenate a sequence of numbers as our DV
y<-c(6,7,6,10,9,13,19,11,18,16)

#...combine the variables sj, x, and y into a data.frame called depData
depData= data.frame(sj,x,y)
head(depData)

depData$sj=as.factor(depData$sj)
depData$x=as.factor(depData$x)

#run the t-test
t.test(y~x,             #...formula
       data=depData,    #...dataframe
       paired = TRUE,   #...TRUE for dependent t-test
       alternative = "two.sided",  #...althernative hypothesis
       conf.level = .95)  #...opposite of alpha

library(ggplot2)
bar <- ggplot(depData, aes(x, y))
bar + 
  stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black",width=.5) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               width = .2) +
  labs(x = "X", y = "mean y")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))+
  theme(strip.text.x = element_text(size = 20, face = "bold",colour = "black", angle = 0),
        strip.text.y = element_text(size = 20, face = "bold",colour = "black", angle = 0))
           

#end of class


################################
#..............................#
#...CONVERT BETWEEN LONG.......#
#.....AND WIDE FORMATS.........#
#..............................#
################################

#activate the "reshape2" library
library(reshape2)

#...convert from long to wide format

#GENERIC
#wideData = dcast(longData, #the name of the dataframe you want to reshape
#            SJ  #row variables 
#              ~y, #row variables ~ column variables
#              value.var = "score") 

head(depData)

#ACTUAL
wideData = dcast(depData, #the name of the dataframe you want to reshape
                 sj  #row variables 
                 ~x, #row variables ~ column variables
                 value.var = "y") 

head(wideData)


#...convert from wide to long format
longData = melt(wideData, #the name of the dataframe you want to reshape
                   id = c("sj"), #the variables that you want to leave unchanged - they correspond to the subject
                   variable.name="x", #the new variable that labels the conditions
                   value.name="y") #the name of the outcome variable
head(longData)


#...get the difference scores
wideData$diff = wideData$cond1-wideData$cond2
head(wideData)  

#....TEST Normality ASSUMPTIONS

hist(wideData$diff)
shapiro.test(wideData$diff)
library(pastecs)
stat.desc(wideData$diff,basic=FALSE,norm=TRUE)


#...CALCUALTE EFFECT SIZE (r)

#...put the output from the t-test into a variable
dep.t.test = t.test(y~x,             #...formula
                    data=depData,    #...dataframe
                    paired = TRUE,   #...TRUE for dependent t-test
                    alternative = "two.sided",  #...althernative hypothesis
                    conf.level = .95)  #...opposite of alpha

t<-dep.t.test$statistic[[1]] #...get the t value from the output
df<-dep.t.test$parameter[[1]]  #...get the df from the output
r <- sqrt(t^2/(t^2+df))  #...calculate r
round(r, 3)  #...ouput the r value rounded to 3 decimals



################################
#..............................#
#....TRY AN EXAMPLE FROM.......#
#........THE TEXTBOOK..........#
################################

#load data file
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")
spiderData=read.delim("spiderLong.dat",header=TRUE)
head(spiderData)

#....lets add subject numbers

spiderData$sj = c(seq(1,12,1),seq(1,12,1))

head(spiderData)

str(spiderData)
spiderData$sj = factor(spiderData$sj)

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
head(wideData)

#....SEE IF YOU CAN CONVERT IT BACK TO LONG FORMAT 
#....GIVE THE NEW DATA.FRAME A NEW NAME


#....CALCULATE THE DIFFERENCE SCORES
wideData$diff = wideData$Picture-wideData$`Real Spider`
head(wideData)

#....TEST ASSUMPTIONS
# normality????

hist(wideData$diff)


#....FOR THE KEENERS

#find outliers
#z-score the difference scores.
wideData$zDiff = scale(wideData$diff)
head(wideData)
cor(wideData$Picture,wideData$`Real Spider`)


#...run the t-test
t.test(Anxiety~Group,data=spiderData,paired = TRUE)


#calculate effect size
#Effect sizes
dep.t.test = t.test(Anxiety~Group,data=spiderData,paired = TRUE)
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

