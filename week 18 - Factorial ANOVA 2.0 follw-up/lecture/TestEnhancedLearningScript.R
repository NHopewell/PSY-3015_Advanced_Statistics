#...ENTER DATA FROM CLASS
sj = paste("pid",seq(1,18,1),sep="")
studyMethod <- c(rep("Reread",9),rep("Test",9))
retentionInterval <-c(rep(c(rep("20 Minutes",3),rep("2 Days",3),rep("1 Week",3)),2))
grade<-c(89,80,71,55,51,44,43,42,38,80,75,70,72,64,59,57,55,53)


#...combine the variables into a data.frame
learningData <- data.frame(sj,studyMethod,retentionInterval,grade)

#...view the data
head(learningData,n=18)

#...check the structure to make sure variables are coded correctly
str(learningData)

#...re-order the variables to match hypothesized effects and orders
learningData$studyMethod <-factor(learningData$studyMethod, levels = c("Reread","Test"))
learningData$retentionInterval <-factor(learningData$retentionInterval, levels = c("20 Minutes","2 Days","1 Week"))
str(learningData)


#...specify orthogonal contrasts
contrasts(learningData$studyMethod) = contr.poly(2)
contrasts(learningData$retentionInterval) = contr.poly(3)

#...Run the complete ANOVA
learningModel <- aov(grade~studyMethod*retentionInterval, data=learningData)

#...get the summary using type III sums of squares
library(car)
Anova(learningModel, type="III")


#...TEST ASSUMPTIONS

#...get residuals and predicted values
learningData$predicted = predict(learningModel)
learningData$residuals = resid(learningModel)
learningData$rstandard = rstandard(learningModel)

#...test normality
shapiro.test(learningData$residuals)

#...test homogeneity of variance
leveneTest(residuals~studyMethod*retentionInterval, data=learningData)

#...test for outliers???
learningData[abs(learningData$rstandard)>=1.96,]

#... need to decide if you are going to remove the value.
#... if so, then you will need to remove the item and then run the model again


#...PLOT THE MAIN EFFECT OF study method 
library(ggplot2)
bar <- ggplot(learningData, aes(studyMethod, grade))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(studyMethod, grade),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width = .2) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Time of Day", 
       y = "Duration in minutes") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE MAIN EFFECT OF retention interval
bar <- ggplot(learningData, aes(retentionInterval, grade))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(retentionInterval, grade),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Method", 
       y = "Duration in Minutes") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#...PLOT THE INTERACTION OF retentionInterval and study method
bar <- ggplot(learningData, aes(retentionInterval, grade, linetype=studyMethod))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=studyMethod), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(retentionInterval, grade,group=studyMethod),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width =.2, 
               position=position_dodge(width=.1)) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Method", 
       y = "Duration in Minutes") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


###################################
#   
#    SIMPLE EFFECTS
#
###################################

#...SPLIT BASED ON THE @ VARIABLE
#...the effect of time of day @ retentionInterval
#...remember that your data.frame is indexed using [rows, columns]
minutesData = learningData[learningData$retentionInterval=="20 Minutes",]
daysData = learningData[learningData$retentionInterval=="2 Days",]
weekData = learningData[learningData$retentionInterval=="1 Week",]
View(minutesData)

#...provide test of simple effect
#...contrasts will carry over from original data.frame
minutesModel = aov(grade~studyMethod, data=minutesData)
summary(minutesModel)
#...don't need type III sums of squares because there is only one variable

dayModel = aov(grade~studyMethod, data=daysData)
summary(dayModel)

weekModel = aov(grade~studyMethod, data=weekData)
summary(weekModel)




###################################
#   
#    SIMPLE EFFECTS V2.0
#
###################################

#...SPLIT BASED ON THE @ VARIABLE
#...the effect of transportation retentionInterval @ time of day
rereadData = learningData[learningData$studyMethod=="Reread",]
testData = learningData[learningData$studyMethod=="Test",]
View(rereadData)
#...provide test of transportation retentionInterval at MORNING
rereadModel = aov(grade~retentionInterval, data=rereadData)
summary(rereadModel)

#...provide test of transportation retentionInterval at AFTERNOON
testModel = aov(grade~retentionInterval, data=testData)
summary(testModel)


#...follow up simple effect with pairwise comparisons
#...pairwise t tests of the morning data
pairwise.t.test(rereadData$grade, 
                rereadData$retentionInterval, 
                paired=FALSE, 
                pool.sd = TRUE)

#...pairwise t tests of the afternoon data
pairwise.t.test(testData$grade, 
                testData$retentionInterval, 
                paired=FALSE, 
                pool.sd = TRUE)



###################################
#   
#    EFFECT SIZE
#
###################################

library(sjstats)
eta_sq(learningModel)
omega_sq(learningModel)


library(apaTables)

apa.2way.table(studyMethod,retentionInterval,grade,data=learningData,landscape=TRUE, show.marginal.means = TRUE)
