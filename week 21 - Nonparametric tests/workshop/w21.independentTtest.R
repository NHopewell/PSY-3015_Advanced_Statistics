

################################
#------------------------------#
#---START OF WORKSHOP----------#
#------------------------------#
################################

#...set working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")




#...load data file
spiderData=read.delim("SpiderLong.dat",header=TRUE)
View(spiderData)


str(spiderData)

#...basic descriptive statistics

#using the by command
library(pastecs)
by(spiderData, 
   spiderData$Group, 
   function(x) 
     stat.desc(x$Anxiety))


##########################
#plotting histograms 
##########################

#...using the by command
par(mfrow=c(1,2))
by(spiderData, 
   spiderData$Group, 
   function(x) 
     hist(x$Anxiety,main=unique(x$Group)))

##########################
#plotting QQPLOTS
##########################

#using the by command
by(spiderData, 
   spiderData$Group, 
   function(x) {
     qqnorm(x$Anxiety); 
     qqline(x$Anxiety)})

##########################
#CALCULATING SHAPIRO WILKS
##########################

#...using the by command
by(spiderData, 
   spiderData$Group, 
   function(x) 
     shapiro.test(x$Anxiety) )


##########################
#CHECK HOMOGENITY OF VARIANCE
##########################
library(car)
leveneTest(Anxiety~Group, data=spiderData)


##########################
#PLOT DATA AS A BOX PLOT
##########################
par(mfrow=c(1,1))
boxplot(Anxiety~Group, 
        data=spiderData, 
        ylab="Anxiety")

##########################
#RUN YOUR T-TESTS
##########################
t.test(Anxiety~Group, 
       data=spiderData,
       paired=FALSE,
       var.equal=TRUE,
       alternative= "two.sided") #assume equal variance


#calculate effect size
#Effect sizes

ind.t.test = t.test(Anxiety~Group, 
                    data=spiderData,
                    paired=FALSE,
                    var.equal=TRUE,
                    alternative= "two.sided") #independent samples
t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##########################
##########################
#LOOKING AT ASSUMPTIONS FOR A DEPENDENT T-TEST
##########################
##########################
wideData=read.delim("SpiderWide.dat",header=TRUE)
#View(wideData)

#calculate difference scores
wideData$D = wideData$picture-wideData$real
View(wideData)


##########################
#getting basic statistics
##########################


round(stat.desc(wideData$D,basic = FALSE, norm = TRUE),3)



##########################
#plotting histograms 
##########################

#manually
hist(wideData$D)

##########################
#plotting QQPLOTS
##########################

#manually
qqnorm(wideData$D)
qqline(wideData$D)

##########################
#CALCULATING SHAPIRO WILKS
##########################


#manually
shapiro.test(wideData$D) 

##########################
#identifying outliers
##########################
library(plyr)

#for between subjects you need to split by grouping variable
spiderData = ddply(spiderData, .(Group), mutate, zAnxiety = scale(Anxiety))
View(spiderData)

#for within subjects you need to run it on the distribution of differences
wideData$zD = scale(wideData$D)
View(wideData)
