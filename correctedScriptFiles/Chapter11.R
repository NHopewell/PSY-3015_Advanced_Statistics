

section = c(rep("part01",10),
            rep("part02",10),
            rep("part03",10))
testGrade = c(rnorm(10,mean=68,sd=10),
              rnorm(10, mean=53, sd= 10),
              rnorm(10,mean=62, sd=10))
assignGrade=c(testGrade[1:10]+rnorm(10,mean=-2,sd=10),
              testGrade[11:20]+rnorm(10,mean=3,sd=10),
              testGrade[21:20]+rnorm(10,mean=5,sd=10))
x1<-c(rep(0,10),rep(1,10),rep(0,10))
x2<-c(rep(0,10),rep(0,10),rep(1,10))
ID = c(seq(1,10,1),seq(1,10,1),seq(1,10,1))


testData = data.frame(ID,section,x1,x2,testGrade,assignGrade)
testData$ID = factor(testData$ID)
testData$section = factor(testData$section, levels = c("part01","part02","part03"))  #need to override default order
View(testData)
str(testData)
cor(testData$testGrade,testData$assignGrade)

#WRITE THE FILE
#write.table(testData, "testData02.dat", sep="\t")


#OPEN THE DATA
testData = read.delim("testData02.dat", header=TRUE, sep="\t")
testData$ID = factor(testData$ID)
testData$section = factor(testData$section, levels = c("part01","part02","part03"))  #need to override default order
View(testData)
str(testData)
cor(testData$testGrade,testData$assignGrade)


#View the data file
View(tempData)


#get descriptive statistics
library(pastecs)
by(testData, testData$section, function(x) stat.desc(x$testGrade, basic=FALSE, norm=TRUE))

library(psych)
describeBy(testData$testGrade,testData$section, mat=TRUE)  #for covariate
describeBy(testData$assignGrade,testData$section, mat=TRUE) #for outcome variable

#graph the data

par(mfrow=c(1,2))
boxplot(testGrade~section, data=testData, xlab="Course Section", ylab="Test Grade %")
boxplot(assignGrade~section, data=testData, xlab="Course Section", ylab="Assignment Grade %")

#homogeneity of variance
#uses car package
library(car)
leveneTest(testData$assignGrade, testData$section, center=median)


#independence of the predictor and the outcome variable
independenceAssumption = aov(testGrade~section, data=testData)
summary(independenceAssumption)



#RUN THE ANCOVA
# use orthogonal contrasts
# use type III sums of squares
contrasts(testData$section)<-cbind(c(-2,1,1),c(0,-1,1))
#contrasts(testData$section)<-contr.helmert(2)
#contrasts(testData$section)<-NULL
ancovaModel = aov(assignGrade~testGrade+section, data=testData)
summary(ancovaModel)
Anova(ancovaModel, type="III")


# get the outcomes from our planned contrasts
summary.lm(ancovaModel)

# get the adjusted means
library(effects)
adjustedMeans <-effect("section", ancovaModel, se=TRUE)
summary(adjustedMeans)

adjustedMeans$se


library(multcomp)
postHocs<-glht(ancovaModel, linfct = mcp(section = "Tukey"))
summary(postHocs)
confint(postHocs)

#EXAMINING ASSUMPTIONS OF THE ANCOVA
plot(ancovaModel)

library(ggplot2)  

#SCATTER PLOT SHOWING SLOPE OF COVARIATE AS A FUNCTION OF IV
scatter <- ggplot(testData, aes(testGrade, assignGrade, colour = section))  #NOTE COLOUR = GENDER
scatter + geom_point() + 
    geom_smooth(method = "lm", aes(fill = section), alpha = 0.1) + 
    labs(x = "Test Grade %", y = "Assignment Grade %", colour = "Section") 


#FULL ANCOVA
ancovaModel = aov(assignGrade~testGrade+section+testGrade:section, data=testData)
Anova(ancovaModel, type="III")

regModel = lm(assignGrade~testGrade, data=testData)
summary(regModel)
testData$predictedY = 25.7654 + (.5796*testData$testGrade)
testData$adjustY1 = testData$assignGrade-testData$predictedY
testData$adjustY2 = testData$assignGrade-testData$adjustY1
View(testData)
describeBy(testData$predictedY,testData$section, mat=TRUE) #for outcome variable

describeBy(testData$assignGrade,testData$section, mat=TRUE) #for outcome variable
