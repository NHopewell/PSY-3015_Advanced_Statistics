#...ENTER DATA FROM CLASS
sj = paste("pid",seq(1,18,1),sep="")
tod <- c(rep("Morning",9),rep("Afternoon",9))
method <-c(rep(c(rep("walking",3),rep("bike",3),rep("car",3)),2))
duration<-c(89,73,75,30,28,38,13,12,14,81,79,74,25,28,40,26,32,35)


#...combine the variables into a data.frame
exampleData <- data.frame(sj,tod,method,duration)

#...view the data
head(exampleData,n=18)

#...check the structure to make sure variables are coded correctly
str(exampleData)

#...re-order the variables to match hypothesized effects and orders
exampleData$tod <-factor(exampleData$tod, levels = c("Morning","Afternoon"))
exampleData$method <-factor(exampleData$method, levels = c("walking","bike","car"))
str(exampleData)


#...specify orthogonal contrasts
contrasts(exampleData$tod) = contr.poly(2)
contrasts(exampleData$method) = contr.poly(3)

#...Run the complete ANOVA
exampleModel <- aov(duration~tod*method, data=exampleData)

#...get the summary using type III sums of squares
library(car)
Anova(exampleModel, type="III")


#...TEST ASSUMPTIONS

#...get residuals and predicted values
exampleData$predicted = predict(exampleModel)
exampleData$residuals = resid(exampleModel)
exampleData$rstandard = rstandard(exampleModel)

#...test normality
shapiro.test(exampleData$residuals)

#...test homogeneity of variance
leveneTest(residuals~tod*method, data=exampleData)

#...test for outliers???
exampleData[abs(exampleData$rstandard)>=1.96,]

#... need to decide if you are going to remove the value.
#... if so, then you will need to remove the item and then run the model again


#...PLOT THE MAIN EFFECT OF TIME OF DAY 
library(ggplot2)
bar <- ggplot(exampleData, aes(tod, duration))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(tod, duration),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width = .2) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Time of Day", 
       y = "Duration in minutes") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE MAIN EFFECT OF TRANSPORTATION
bar <- ggplot(exampleData, aes(method, duration))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(method, duration),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Method", 
       y = "Duration in Minutes") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#...PLOT THE INTERACTION OF transportation method and time of day
bar <- ggplot(exampleData, aes(method, duration, linetype=tod))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=tod), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(method, duration,group=tod),
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
#...the effect of time of day @ method
#...remember that your data.frame is indexed using [rows, columns]
walkingData = exampleData[exampleData$method=="walking",]
bikeData = exampleData[exampleData$method=="bike",]
carData = exampleData[exampleData$method=="car",]

#...provide test of simple effect
#...contrasts will carry over from original data.frame
walkingModel = aov(duration~tod, data=walkingData)
summary(walkingModel)
#...don't need type III sums of squares because there is only one variable

bikeModel = aov(duration~tod, data=bikeData)
summary(bikeModel)

carModel = aov(duration~tod, data=carData)
summary(carModel)




###################################
#   
#    SIMPLE EFFECTS V2.0
#
###################################

#...SPLIT BASED ON THE @ VARIABLE
#...the effect of transportation method @ time of day
morningData = exampleData[exampleData$tod=="Morning",]
afternoonData = exampleData[exampleData$tod=="Afternoon",]

#...provide test of transportation method at MORNING
morningModel = aov(duration~method, data=morningData)
summary(morningModel)

#...provide test of transportation method at AFTERNOON
afternoonModel = aov(duration~method, data=afternoonData)
summary(afternoonModel)


#...follow up simple effect with pairwise comparisons
#...pairwise t tests of the morning data
pairwise.t.test(morningData$duration, 
                morningData$method, 
                paired=FALSE, 
                pool.sd = TRUE)

#...pairwise t tests of the afternoon data
pairwise.t.test(afternoonData$duration, 
                afternoonData$method, 
                paired=FALSE, 
                pool.sd = TRUE)



###################################
#   
#    EFFECT SIZE
#
###################################

library(sjstats)
eta_sq(exampleModel)
omega_sq(exampleModel)




