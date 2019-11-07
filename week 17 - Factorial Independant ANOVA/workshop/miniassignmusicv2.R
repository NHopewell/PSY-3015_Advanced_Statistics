#setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")
#setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")
setwd("D:/Workshop Psyc3015 2017 2018/Data files/Data files")
#...read in file 
musicData= read.delim("fugazi.dat", header = TRUE, sep= "\t")


#...view the data.frame
#View(musicData)
head(musicData, n=20)

#...check the coding of variables
str(musicData)

#...reorder the levels of music and age
musicData$music=factor(musicData$music,labels=c("Fugazi","ABBA","BROOKS"))
musicData$age=factor(musicData$age,labels=c("under 40","over 40"))
str(musicData)


library(psych)
summary(musicData)

describe(musicData)


#...WHEN EXAMINING THE DATA, IT IS IMPORTANT TO THINK IN TERMS OF THE FACTORIAL DESIGN
#...main effect of music
describeBy(musicData$liking,
           musicData$music,
           mat= TRUE,digits = 1)

#...main effect of age
describeBy(musicData$liking,
           musicData$age,
           mat= TRUE,digits = 1)

#...interaction of age and music
describeBy(musicData$liking,
           list(musicData$age,musicData$music),
           mat= TRUE,digits = 1)


library(car)

#...examine your DV 

#...main effect of music
par(mfrow=c(1,3))
      
by(musicData,musicData$music,function(x)
  hist(x$liking, 
       main = unique(x$music),
       xlim=c(-118,122)))

#...main effect of age
par(mfrow=c(1,2))
by(musicData,musicData$age,function(x)
  hist(x$liking, 
       main = unique(x$age),
       xlim=c(-118,122)))

#...interaction
par(mfrow=c(2,3))
by(musicData,list(musicData$music,musicData$age),
   function(x) hist(x$liking, 
       main = paste(unique(x$age),"\n",unique(x$music)),
       xlim=c(-118,122)))


#...RUN THE Two Way ANOVA MODEL


#...specify orthogonal contrasts
contrasts(musicData$age) <-c(-1,1)
contrasts(musicData$music)<- cbind(c(-2,1,1), 
                                   c(0,-1,1))

musicModel<-aov (liking ~ age * music,data = musicData)
Anova(musicModel, type = "III")



#...ASSUMPTIONS....

#...get the predicted, residuals and standardized residuals
musicData$predicted = predict(musicModel)
musicData$residuals = residuals(musicModel)
musicData$sresiduals = rstandard(musicModel)


#Assumption Homogeneity of variance
leveneTest(musicData$residuals,
           interaction(musicData$music,musicData$age),
           center = median)


#...Normality distributed error
shapiro.test(musicData$residuals)

#...No outliers
#...Are there any outliers?  Here we use 1.96 SD (p = .05)
musicData[abs(musicData$sresiduals)>=1.96,]  
#...based on this criterion we have 3 outliers.  
#...It is probably worth while removing them.




#...Create a new data.frame with the outliers removed
musicNoOutliers = musicData[abs(musicData$sresiduals)<1.96,]  

################################
#
#...RE RUN THE MODEL WITH THE OUTLIERS REMOVED.
#
################################

#...Note:  this requires going through all of the steps again.

#...GET THE DESCRIPTIVE STATISTICS

#...main effect of music
describeBy(musicNoOutliers$liking,musicNoOutliers$music,mat= TRUE,digits = 1)

#...main effect of age
describeBy(musicNoOutliers$liking,musicNoOutliers$age,mat= TRUE,digits = 1)
#...interaction of age and music
describeBy(musicNoOutliers$liking,
           list(musicNoOutliers$age,musicNoOutliers$music),
           mat= TRUE,
           digits = 1)

noOutlierModel<-aov (liking ~ age * music,data = musicNoOutliers  )
Anova(noOutlierModel, type = "III")
#...the effects are that much larger


#...TEST ASSUMPTIONS...AGAIN

#...get the predicted, residuals and standardized residuals (AGAIN)
musicNoOutliers$predicted = predict(noOutlierModel)
musicNoOutliers$residuals = residuals(noOutlierModel)
musicNoOutliers$sresiduals = rstandard(noOutlierModel)



#...Homogeneity of variance (AGAIN)
leveneTest(musicNoOutliers$residuals,
           interaction(musicNoOutliers$music,musicNoOutliers$age),
           center = median)

#...Normality distributed error (AGAIN)
shapiro.test(musicNoOutliers$residuals)

#...No outliers (AGAIN)  
#...we will only remove outliers once.  Though this could
#...go on forever.
musicNoOutliers[abs(musicNoOutliers$sresiduals)>1.96,]  

#Graphs####

#...PLOT THE MAIN EFFECT OF age
library(ggplot2)
bar <- ggplot(musicNoOutliers, aes(age, liking))
bar + 
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               colour = "Black") + 
  stat_summary(aes(age, liking),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "age", 
       y = "liking") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE MAIN EFFECT OF music
bar <- ggplot(musicNoOutliers, aes(music, liking))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(music, liking),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "music", 
       y = "liking") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE INTERACTION OF age AND music
bar <- ggplot(musicNoOutliers, aes(music, liking, linetype=age))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=age), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(music, liking),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width =.2, 
               position=position_dodge(width=.1)) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "music", 
       y = "Attractivness") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

