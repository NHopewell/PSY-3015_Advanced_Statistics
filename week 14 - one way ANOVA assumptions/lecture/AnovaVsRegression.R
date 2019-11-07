#...REgression vs. ANOVA Example
source()
#...CREATE OUR DATA
#...Remember, this creates different data everytime it is run
sjNum =paste(rep("sj",15),seq(1,15,1),sep="") 
micturationTime <-c(rnorm(n=5, mean=30, sd=10),
                    rnorm(n=5,mean=40,sd=10),
                    rnorm(n=5,mean=50,sd=10))
micturationTime=c(29,31,30,38,5,36,41,30,47,45,46,53,41,55,47)
distance <-c(rep("Absent",5),
             rep("1 Urinal",5), 
             rep("Adjacent",5))

#...add the factors used by regression
x1<-c(rep(0,5),
      rep(1,5),
      rep(0,5))  
x2<-c(rep(0,5),
      rep(0,5),
      rep(1,5))


#...combine the variables into a data.frame
micturationData <- data.frame(sjNum,
                              distance,
                              x1,
                              x2,
                              micturationTime=round(micturationTime,0))


#...view the data.frame to make sure everything is there
head(micturationData, n=15)

#...check the structure of the data.frame to make sure that our  
#...variables are coded correctly
str(micturationData)

#...here we predict that the closer someone is the longer it should  
#...take to start peeing.  Therefore, we will re-order the levels 
#...of distance to match this prediction
micturationData$distance = factor(micturationData$distance,
                                  levels=c("Absent",
                                           "1 Urinal",
                                           "Adjacent"))

#...check the structure again
str(micturationData)

#...RUN THE ANOVA
aovOutput <-aov(micturationTime~distance,
                data=micturationData)

summary(aovOutput)

#...RUN THE REGRESSION
regOutput <- lm(micturationTime~x1+x2,
                data=micturationData)

summary(regOutput)




library(psych)
describeBy(micturationData$micturationTime,
           micturationData$distance, 
           mat=TRUE,
           digits=1)

#...EXAMINE THE ANOVA AS A REGRESSION
summary.lm(aovOutput)

print(model.tables(aovOutput,"means"),digts=3)



#...BECAUSE ANOVA IS LIKE REGRESSION WE CAN GET OUR PREDICTED AND RESIDUAL VALUES
micturationData$predicted = predict(aovOutput)
micturationData$residuals = resid(aovOutput)
micturationData$sresiduals = rstandard(aovOutput)
head(micturationData,n=15)



####CHECKING ASSUMPTIONS####
#...Use the data from last week

#...enter the data
sjNum = c(1,2,3,4,5,6,7,8,9,10)
major = c("english","english","english","english","english",
          "math","math","math","math","math")
score = c(95,93,95,96,98,1,8,6,17,1)

#...combine the variables into a data.frame
anxietyData = data.frame(sjNum,major,score)

#...view the data.frame
head(anxietyData,n=10)


#...convert sjNum into a factor
anxietyData$sjNum = factor(anxietyData$sjNum)

#...run the ANOVA
anxietyModel = aov(score~major,data=anxietyData)
summary(anxietyModel)

#...CHECK ASSUMPTIONS
library(pastecs)
by(anxietyData,anxietyData$major, function(x)
  {
  stat.desc(x$score,basic=FALSE,norm=TRUE)
})

#...GET PREDICTED AND RESIDUAL VALUES
anxietyData$predicted = predict(anxietyModel)
anxietyData$residuals = resid(anxietyModel)
anxietyData$sresiduals = rstandard(anxietyModel)

#...view the data
head(anxietyData,n=10)

#...set the number of decimals before scientific notation is used
options(scipen=10)

#get the descriptive statistics 
round(stat.desc(anxietyData$residuals,basic=FALSE,norm=TRUE),3)


#...HOMOGENEITY OF VARIANCE

#...run on the raw scores as if a t-test
library(car)
leveneTest(score~major, data=anxietyData, center=median)

#...run on the residuals as if a regression
leveneTest(residuals~major, data=anxietyData, center=median)

###NICE