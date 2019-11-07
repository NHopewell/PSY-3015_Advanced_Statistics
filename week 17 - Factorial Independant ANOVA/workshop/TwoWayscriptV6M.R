
setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")
#setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")

#...read in file 
gogglesData= read.delim("goggles.csv", header = TRUE, sep =",")

#...view the data.frame
View(gogglesData)
head(gogglesData, n=20)

#...check the coding of variables
str(gogglesData)

#...reorder the levels of alcohol
gogglesData$alcohol=factor(gogglesData$alcohol,levels=c("None","2 Pints","4 Pints"))
str(gogglesData)

library(psych)
summary(gogglesData)

describe(gogglesData)

#...WHEN EXAMINING THE DATA, IT IS IMPORTANT TO THINK IN TERMS OF THE FACTORIAL DESIGN
#...main effect of alcohol
describeBy(gogglesData$attractiveness,gogglesData$alcohol,mat= TRUE,digits = 1)

#...main effect of gender
describeBy(gogglesData$attractiveness,gogglesData$gender,mat= TRUE,digits = 1)

#...interaction of gender and alcohol
describeBy(gogglesData$attractiveness,
           list(gogglesData$gender,gogglesData$alcohol)
           ,mat= TRUE,digits = 1)


library(car)

#...Graph your DV 

#...main effect of alcohol
par(mfrow=c(1,length(unique(gogglesData$alcohol))))
by(gogglesData,gogglesData$alcohol,function(x)
  hist(x$attractiveness, 
       main = unique(x$alcohol),
       xlim=c(20,90)))

#...main effect of gender
par(mfrow=c(1,length(unique(gogglesData$gender))))
by(gogglesData,gogglesData$gender,function(x)
  hist(x$attractiveness, 
       main = unique(x$gender),
       xlim=c(20,90)))

#...interaction
par(mfrow=c(length(unique(gogglesData$gender)),
            length(unique(gogglesData$alcohol))))
by(gogglesData,list(gogglesData$alcohol,gogglesData$gender),function(x)
  hist(x$attractiveness, 
       main = paste(unique(x$gender),"\n",unique(x$alcohol)),
       xlim=c(20,90)))


#...RUN THE TWO WAY ANOVA MODEL

#...specify orthogonal contrasts
contrasts(gogglesData$gender) <-c(-1,1)
contrasts(gogglesData$alcohol)<- cbind(c (-2,1,1), c(0,-1,1))

gogglesModel<-aov (attractiveness ~ gender * alcohol,data = gogglesData)
Anova(gogglesModel, type = "III")

#...get the predicted, residuals and standardized residuals
gogglesData$predicted = predict(gogglesModel)
gogglesData$residuals = residuals(gogglesModel)
gogglesData$sresiduals = rstandard(gogglesModel)

#...TEST ASSUMPTIONS

#...Homogeneity of variance
#...here you want to test the hypothesis that the varianc is the same
#...across all of the cells.  To do this, you use the "interaction()" function
leveneTest(gogglesData$residuals,
           interaction(gogglesData$alcohol,gogglesData$gender),
           center = median)


#...Normality distributed error
shapiro.test(gogglesData$residuals)

#...No outliers
#...Are there any outliers?  Here we use 1.96 SD (p = .05)
gogglesData[abs(gogglesData$sresiduals)>=1.96,]  


#...based on this criterion we have 4 outliers.  
#...It is probably worth while removing them.


#...Create a new data.frame with the outliers removed
gogglesNoOutliers = gogglesData[abs(gogglesData$sresiduals)<1.96,]  

################################
#
#...RE RUN THE MODEL WITH THE OUTLIERS REMOVED.
#
################################

#...Note:  this requires going through all of the steps again.

#...GET THE DESCRIPTIVE STATISTICS

#...main effect of alcohol
describeBy(gogglesNoOutliers$attractiveness,gogglesNoOutliers$alcohol,mat= TRUE,digits = 1)

#...main effect of gender
describeBy(gogglesNoOutliers$attractiveness,gogglesNoOutliers$gender,mat= TRUE,digits = 1)
#...interaction of gender and alcohol
describeBy(gogglesNoOutliers$attractiveness,
           list(gogglesNoOutliers$gender,gogglesNoOutliers$alcohol),
           mat= TRUE,
           digits = 1)

noOutlierModel<-aov (attractiveness ~ gender * alcohol,data = gogglesNoOutliers  )
Anova(noOutlierModel, type = "III")
#...the effects are that much larger


#...TEST ASSUMPTIONS...AGAIN

#...get the predicted, residuals and standardized residuals (AGAIN)
gogglesNoOutliers$predicted = predict(noOutlierModel)
gogglesNoOutliers$residuals = residuals(noOutlierModel)
gogglesNoOutliers$sresiduals = rstandard(noOutlierModel)



#...Homogeneity of variance (AGAIN)
leveneTest(gogglesNoOutliers$residuals,
           interaction(gogglesNoOutliers$alcohol,gogglesNoOutliers$gender),
           center = median)

#...Normality distributed error (AGAIN)
shapiro.test(gogglesNoOutliers$residuals)

#...No outliers (AGAIN)  
#...we will only remove outliers once.  Though this could
#...go on forever.
gogglesNoOutliers[abs(gogglesNoOutliers$sresiduals)>1.96,]  

#Graphs####

#...PLOT THE MAIN EFFECT OF GENDER
library(ggplot2)
bar <- ggplot(gogglesNoOutliers, aes(gender, attractiveness))
bar + 
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               colour = "Black") + 
  stat_summary(aes(gender, attractiveness),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Gender", 
       y = "Attractiveness") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE MAIN EFFECT OF ALCOHOL
bar <- ggplot(gogglesNoOutliers, aes(alcohol, attractiveness))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(alcohol, attractiveness),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Alcohol", 
       y = "Attractiveness") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE INTERACTION OF GENDER AND ALCOHOL
bar <- ggplot(gogglesNoOutliers, aes(alcohol, attractiveness, linetype=gender))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=gender), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(alcohol, attractiveness),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width =.2, 
               position=position_dodge(width=.1)) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Alcohol", 
       y = "Attractivness") +
  coord_cartesian(xlim = NULL, ylim = c(0,100), expand = TRUE)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

##################################################
### Simple Effects####
##################################################
#Split based on the variable
View(gogglesNoOutliers)

####How you split your data will depend on your hypothesis#######
#####Remember you are doing an a prior test###############

#...Don't forget [Rows, Columns]
noneData=gogglesNoOutliers[gogglesNoOutliers$alcohol=="None",]
twoData=gogglesNoOutliers[gogglesNoOutliers$alcohol=="2 Pints",]
fourData=gogglesNoOutliers[gogglesNoOutliers$alcohol=="4 Pints",]
View(twoData)
View(noneData)

#Run oneway ANOVAs for each of the new dataframes
#Gender at each level of Alcohol
noneModel=aov(attractiveness~gender,data=noneData)
summary(noneModel)
twoModel=aov(attractiveness~gender,data=twoData)
summary(twoModel)
fourModel=aov(attractiveness~gender,data=fourData)
summary(fourModel)


#####            OR          #######

# simple effect of alcohol @ gender
#Alcohol at eachlevel of Gender 
#..Don't forget [rows,columns]
maleData=gogglesNoOutliers[gogglesNoOutliers$gender== "Male",]
View(maleData)
femaleData=gogglesNoOutliers[gogglesNoOutliers$gender== "Female",]
View(femaleData)


maleModel=aov(attractiveness~alcohol,data=maleData)
summary(noneModel)
#...there was an effect of alcohol for males

femaleModel=aov(attractiveness~alcohol,data=femaleData)
summary(femaleModel)
#...there was no evidence of an effect of alcohol for females


##################################
#...if the simple effect has more than two levels
#...then you should follow up a significant anova with
#...additional tests.
#Therefore we need a follow up test on the maleModel
######################################################
# Pairwise Comparisons for the levels of Alcohol(3 leves) for male  
# Remeber the oneway ANova was significant F(1,13)= 6.136, p=.0278
###  Bonferroni
pairwise.t.test(maleData$attractiveness,maleData$alcohol,p.adjust.method = "bonferroni",pooled.sd=TRUE)


