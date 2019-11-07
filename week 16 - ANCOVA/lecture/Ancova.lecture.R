setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")




#...EXAMPLE #1
#...MINIMAL VIOLATION OF ASSUMTPIONS

ID = c( "sj1" , "sj2", "sj3", "sj4", "sj5", "sj6", "sj7", "sj8", "sj9", 
        "sj10" ,"sj11","sj12","sj13","sj14","sj15","sj16","sj17","sj18",
        "sj19" ,"sj20","sj21","sj22","sj23","sj24","sj25","sj26","sj27",
        "sj28", "sj29","sj30")

section = c("part01", "part01", "part01", "part01", "part01", "part01", "part01",
            "part01", "part01", "part01", "part02", "part02", "part02", "part02",
            "part02", "part02", "part02", "part02", "part02", "part02", "part03",
            "part03", "part03", "part03", "part03", "part03", "part03", "part03",
            "part03", "part03")
testGrade = c(82, 49, 55, 41, 64, 68, 75, 61, 63, 67, 67,
              62, 69, 72, 76, 79, 84, 82, 62, 85, 43, 48,
              82, 90, 81, 55, 45, 55, 59, 36)
assignGrade = c(100, 69,  87,  61,  86,  90,  98,  82,  88,  88, 
                75,  66,  74,  88,  82,  86,  95,  94,  69,  95, 
                51,  61,  89,  108, 96,  73,  65,  63,  74,  37)

testData = data.frame(ID,section,testGrade,assignGrade)
head(testData,20)


#...EXAMPLE #2
#...ASSUMPTIONS VIOLATED
testData2 = read.delim("testData02.dat",header=TRUE)
head(testData2,20)




#...Check the structure
str(testData)

#...make sure that the course sections are in the right order
testData$section = factor(testData$section, 
                          levels = c("part01","part02","part03"))  

#...RUN THE ANOVA
anovaModel = aov(assignGrade~section,data=testData)
summary(anovaModel)
#...Oh no, there is no effect.
#...Maybe we should run an ANCOVA



#...Get descriptive statistics for the Dependent Variable
library(psych)
describeBy(testData$assignGrade,testData$section, mat=TRUE, digits=2) 


#...G the descriptive statistics for the Covariate (predictor) Variable
#for covariate
#remember we want the covariate to be independent of our
#categorical independent variable 
#hopeully the means and standard deviations are similar to one another

describeBy(testData$testGrade,testData$section, mat=TRUE, digits=2) 




#graph the data
#we haven't done blox plots in a while!!!

par(mfrow=c(1,2))
#again we want to see independence for the covariate
boxplot(testGrade~section, 
        data=testData, 
        ylim=c(01,120),
        xlab="Course Section", 
        ylab="Test Grade %", 
        main = "Covariate")

#outcome variable
boxplot(assignGrade~section, 
        data=testData, 
        ylim=c(01,120),
        xlab="Course Section", 
        ylab="Assignment Grade %", 
        main = "Dependent Variable")

#...test normaltity of variance for Dependent Variable
par(mfrow=c(3,2))
by(testData,testData$section,
   function(x) {
     print(shapiro.test(x$assignGrade))
     hist(x$assignGrade, main = paste("Section =",unique(as.character(x$section))))
     qqnorm(x$assignGrade, main = paste("Section =",unique(as.character(x$section))))
     qqline(x$assignGrade)
     })


#...homogeneity of variance of Dependent Variable
#...uses car package
library(car)
leveneTest(testData$assignGrade~ testData$section, center=median)


#...independence of the covariate and the IV on the outcome variable
#...if they are independent, then there should be no effect
independenceAssumption = aov(testGrade~section, data=testData)
summary(independenceAssumption)

#...if they are not independent, then we need to ask if they can predict one another

#...plot homogeneity of regression slopes
library(ggplot2)  

#SCATTER PLOT SHOWING SLOPE OF COVARIATE AS A FUNCTION OF IV
scatter <- ggplot(testData, aes(testGrade, assignGrade, colour = section))  
scatter + geom_point() + 
  geom_smooth(method = "lm", aes(fill = section), alpha = 0.2) + 
  #  geom_smooth(method = "lm", aes(testGrade,assignGrade), colour="Black",alpha = 0.2) + 
  labs(x = "Test Grade %", y = "Assignment Grade %", colour = "Section") 






#RUN THE ANCOVA
# use orthogonal contrasts
contrasts(testData$section)<-contr.poly(length(unique(testData$section)))
contrasts(testData$section)

# use type III sums of squares

ancovaModel = aov(assignGrade~testGrade+section, data=testData)
summary(ancovaModel)
Anova(ancovaModel, type="III")


# get the outcomes from our planned contrasts
summary.lm(ancovaModel)

# get the adjusted means
library(effects)
adjustedMeans <-effect("section", ancovaModel, se=TRUE)
summary(adjustedMeans)


#if the covariate is indpendent, the absolute value of the means should
#change but the relative differences should stay constant

adjustedMeans$se
#again the SE should be reduced similarly for all of the levels

library(multcomp)
postHocs<-glht(ancovaModel, linfct = mcp(section = "Tukey"))
summary(postHocs)
confint(postHocs)

#EXAMINING ASSUMPTIONS OF THE ANCOVA
par(mfrow=c(2,2))
plot(ancovaModel)
par(mfrow=c(1,1))
#examine homoscedasticity and normality of the residuals

testData$predicted =predict(ancovaModel)

testData$residuals =resid(ancovaModel)

#...test normality of residuals
shapiro.test(testData$residuals)


#...test homogeneity of variane
leveneTest(residuals~section,data=testData)


#...FULL ANCOVA TO TEST THE HOMOGENEITY OF THE REGRESSION SLOPES
fullAncovaModel = aov(assignGrade~testGrade+section+testGrade:section, data=testData)
fullAncovaModel = aov(assignGrade~testGrade*section, data=testData)
Anova(fullAncovaModel, type="III")
summary(lm(fullAncovaModel))



library(sjstats)
eta_sq(ancovaModel,partial=TRUE)
eta_sq(ancovaModel,partial=FALSE)



#...WE NEED TO  MAKE OUR OWN TABLE BECAUSE THERE IS NO SPECIAL FUNCTION 
#...TO DO THIS FOR US.

#...MEAN AND SE FOR THE Covariate Variable
library(psych)
describeData = describeBy(testData$testGrade,
                          testData$section, 
                          mat=TRUE, 
                          digits=2) 
newTable = data.frame(describeData[,c("group1","mean","se")])
names(newTable) <-c("Section"," Test Grade","se")



describeData = describeBy(testData$assignGrade,
                          testData$section, 
                          mat=TRUE, 
                          digits=2) 

newTable = cbind(newTable,describeData[,c("mean","se")])
newTable

names(newTable) <-c("Section","Test Grade","se", "Assignment Grade","se")
newTable

#add adjusted means
adjustedMeans <-effect("section", ancovaModel, se=TRUE)
adjMeans=adjustedMeans$fit
adjLB=adjustedMeans$lower
adjHB=adjustedMeans$upper
adjSE = adjustedMeans$se

newTable = cbind(newTable,adjMeans,adjSE,adjLB,adjHB)
newTable
write.table(newTable,"newTable.txt",row.names = FALSE,sep = "\t")
