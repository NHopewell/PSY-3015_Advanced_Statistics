

################################
#------------------------------#
#---START OF WORKSHOP----------#
#------------------------------#
################################

#ACTIVATE THE FOLLOWING LIBRARIES
library(ggplot2)
library(pastecs)
library(reshape)
library(Hmisc)


#load data file
spiderData=read.delim("SpiderLong.dat",header=TRUE)
View(spiderData)


str(spiderData)

#basic descriptive statistics

#SPLITTING THE DATA MANUALLY
pictureSpider = spiderData[spiderData$Group=="Picture",]
realSpider = spiderData[spiderData$Group != "Picture",] #

##########################
#getting basic statistics
##########################

#manually
round(stat.desc(pictureSpider$Anxiety,basic = FALSE, norm = TRUE),3)
round(stat.desc(realSpider$Anxiety,basic = FALSE, norm = TRUE),3)

#using the by command
by(spiderData, spiderData$Group, function(x) stat.desc(x$Anxiety))



####### something from the text book (STUDENTS DONT HAVE THIS)!!
by(spiderData$Anxiety, spiderData$Group, stat.desc, basic = FALSE, norm = TRUE)


spiderData %>%
  group_by()

###  HOW TO FIX OUTPUT FRMAE
par(mfrow=c(2,2))
dev.off() 

 ##########################
#plotting histograms 
##########################

#manually
hist(pictureSpider$Anxiety)
hist(realSpider$Anxiety)

#using the by command
by(spiderData, spiderData$Group, function(x) hist(x$Anxiety,main=unique(x$Group)))


##########################
#plotting QQPLOTS
##########################

#manually
qqnorm(pictureSpider$Anxiety)
qqline(pictureSpider$Anxiety)

qqnorm(realSpider$Anxiety)
qqline(realSpider$Anxiety)

#using the by command
by(spiderData, spiderData$Group, function(x) {qqnorm(x$Anxiety); qqline(x$Anxiety)})

##########################
#CALCULATING SHAPIRO WILKS
##########################


#manually
shapiro.test(pictureSpider$Anxiety)
shapiro.test(realSpider$Anxiety)

#using the by command
by(spiderData, spiderData$Group, function(x) shapiro.test(x$Anxiety) )


##########################
#CHECK HOMOGENITY OF VARIANCE
##########################
library(car)
leveneTest(Anxiety~Group, data=spiderData)


##########################
#PLOT DATA AS A BOX PLOT
##########################
boxplot(Anxiety~Group, data=spiderData, ylab="Anxiety")

##########################
#RUN YOUR T-TESTS
##########################
t.test(Anxiety~Group, data=spiderData,paired=FALSE) #independent samples
t.test(Anxiety~Group, data=spiderData,paired=FALSE,var.equal=TRUE) #assume equal variance
t.test(Anxiety~Group, data=spiderData,paired=TRUE) #dependent samples


## using with() command: AGAIN, students dont have this
with(spiderData, t.test(Anxiety ~ Group))  # independant t-test as default


#calculate effect size
#Effect sizes

ind.t.test = t.test(Anxiety~Group, data=spiderData,paired=FALSE, var.equal = TRUE) #independent samples
t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


## IF atudents ask about choosing items from a list show them

str(ind.t.test)



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
