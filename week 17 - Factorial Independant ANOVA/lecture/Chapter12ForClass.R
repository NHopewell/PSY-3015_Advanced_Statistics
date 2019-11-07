#...Enter the data from class
sj = seq(1,16,1)
anxiety = c(rep("High",8),rep("Low",8))
year = c(rep("2nd",4),rep("3rd",4),rep("2nd",4),rep("3rd",4))
grade = c(64,72,63,73,51,61,56,60,84,82,80,90,88,93,84,83)

# Build our Data Object
dataFile = data.frame(sj,anxiety,year,grade)

#...check to see if variables are classified properly
str(dataFile)

#convert sj to a factor
dataFile$sj = factor(dataFile$sj)


head(dataFile, n=16)


#...EXAMINE DATA

#...descriptives
library(psych)
describeBy(dataFile$grade,list(dataFile$year,dataFile$anxiety), mat=TRUE, digits=1)
describeBy(dataFile$grade,list(dataFile$year), mat=TRUE, digits=1)
describeBy(dataFile$grade,list(dataFile$anxiety), mat=TRUE, digits=1)


#...histograms
par(mfrow=c(2,2))  #levels of IV1, #levels of IV2
by(dataFile, 
   list(dataFile$year,dataFile$anxiety), #IV1, IV2
   function(x) hist(x$grade, 
                    main=paste("Anxiety: ", 
                               unique(x$anxiety),
                               "\n",
                               "Year: ", 
                               unique(x$year)),
                    ylim=c(0,4),  #We have 4 per group, so it is possible to have a frequency count greater than 4
                    xlim=c(50,100)))



#...specify orthogonal contrasts  
#...when in doubt, use the contr.poly()  funciton
contrasts(dataFile$anxiety) = c(-1,1)
contrasts(dataFile$year) = c(-1,1)

#...RUN THE MODEL
library(car)
dataModel = aov(grade~anxiety*year, data=dataFile)

#...get the type 3 sums of squares
Anova(dataModel, type="III")

#...GET PREDICTED AND RESIDUAL VALUES

dataFile$predicted = predict(dataModel)
dataFile$residuals = resid(dataModel)
dataFile$sresiduals = rstandard(dataModel)


#...TEST HOMOGENEITY OF VARIANCE


leveneTest(dataFile$residuals,
           interaction(dataFile$year,dataFile$anxiety),
           center=median) 

#...test normality
shapiro.test(dataFile$residuals)

#...check for outliers
#...No outliers
#...Are there any outliers?  Here we use 1.96 SD (p = .05)
dataFile[abs(dataFile$sresiduals)>=1.96,]  
#...based on this criterion we have 0 outliers.  




#...GRAPH THE DATA
library(ggplot2)


#...PLOT THE MAIN EFFECT OF ANXIETY
bar <- ggplot(dataFile, aes(anxiety, grade))
bar + 
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               colour = "Black") + 
  stat_summary(aes(anxiety, grade),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Anxiety Level", 
       y = "Final Grade (%)") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#PLOT THE MAIN EFFECT OF YEAR
bar <- ggplot(dataFile, aes(year, grade))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(year, grade),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Year", 
       y = "Final Grade (%)") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#PLOT THE INTERACTION OF Anxiety AND YEAR
bar <- ggplot(dataFile, aes(anxiety, grade, linetype=year))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=year), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(anxiety, grade),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width =.2, 
               position=position_dodge(width=.1)) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Anxeity", 
       y = "Final Grade (%)") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))
