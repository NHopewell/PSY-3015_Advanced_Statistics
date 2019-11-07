

#...Math anxiety example
#sjNum = seq(1,10,1)
#major = c(rep("english",5),rep("math",5))
#score = c(rnorm(n=5, mean = 93, sd = 5),rnorm(n=5,mean = 12, sd=5))

#...Set the working directory
setwd("~/Documents/Teaching/2015-2016/Statistics/mikeDataExamples")


#...enter the data
sjNum = c(1,2,3,4,5,6,7,8,9,10)
major = c("english","english","english","english","english",
          "math","math","math","math","math")
score = c(95,93,95,96,98,1,8,6,17,1)

#...combine the variables into a data.frame
anxietyData = data.frame(sjNum,major,score)

#...view the data.frame
head(anxietyData,n=10)

#...check the coding of variables
str(anxietyData)

#...convert sjNum into a factor
anxietyData$sjNum = factor(anxietyData$sjNum)

#...run the ANOVA
anxietyModel = aov(score~major,data=anxietyData)
summary(anxietyModel)

#...get the means
print(model.tables(anxietyModel,"means"),digts=3)


#...Make an APA table
library(apaTables)
apa.1way.table(iv=major,dv=score,data = anxietyData)



#...You can save the APA table into a word document

#...Set the working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture13.betweenANOVA/")

apa.1way.table(iv=major,
               dv=score,
               data = anxietyData, 
               show.conf.interval = TRUE,
               filename = "onewayTable.doc",
               landscape = FALSE)
#...can also use the describeBy command
library(psych)
describeBy(anxietyData$score,anxietyData$major,mat=TRUE,digits=1)



#...calculate predicted values and add them to the data.frame
#...notice that they match our Model predictions
anxietyData$predicted = predict(anxietyModel)
head(anxietyData,n=10)

#...calculate the residual values and add them to the data.frame
#...notice that match our hand calculations
anxietyData$residuals = resid(anxietyModel)
head(anxietyData,n=10)


#...EXAMPLE 2....maybe for next weeks class
setwd("~/Documents/Teaching/2015-2016/Statistics/mikeDataExamples")
y <-c(runif(3, 10, 20),runif(3,15,25),runif(3,20,30))
group <-c(rep("group1",3),rep("group2",3), rep("group3",3))
x1<-c(rep(0,3),rep(1,3),rep(0,3))
x2<-c(rep(0,3),rep(0,3),rep(1,3))


tempData <- data.frame(group,x1,x2,y=round(y,0))
head(tempData)

regOutput <- lm(y~x1+x2, data = tempData)
summary(regOutput)

aovOutput <-aov(y~group,data=tempData)
summary(aovOutput)
summary.lm(aovOutput)
#tempData$group = factor(tempData$group)

library(psych)
describeBy(tempData$y,tempData$group, mat=TRUE)
aovOutput$coefficients
regOutput$coefficients

print(model.tables(aovOutput,"means"),digts=3)

write.table(tempData, "anovaExample01.dat", sep="\t", row.names = FALSE)

tempData$group2 = tempData$group

#create polynomial (trend analysis contrasts)
contrasts(tempData$group2) <- contr.poly(3)
head(tempData)
contrasts(tempData$group2)
tempData$groupDummy<-NULL
tempData$group2<-NULL



libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose<-gl(3,5, labels=c("Placebo","Low Dose","High Dose"))
viagraData <- data.frame(dose,libido)
head(viagraData)

by(viagraData, viagraData$dose, function(x) hist(x$libido, main=as.character(unique(x$dose))))
par(mfrow=c(1,3))

library(pastecs)
by(viagraData, viagraData$dose, function(x) stat.desc(x$libido, basic=FALSE))
