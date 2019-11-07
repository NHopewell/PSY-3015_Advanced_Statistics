setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture08.MultipleRegression")
# sj = seq(1,10,1)
# x = c(8,6,7,10,10,5,3,2,6,9)
# y = c(9,7,6,5,6,7,5,5,4,7)
# w = rnorm(n=10, mean = 3, sd =1)*.3*y
# z = rnorm(n=10, mean = 3, sd = 1*.3*y)
# x = x1
# w = x2
# z = x3
#tempData = data.frame(sj,x,w =round(w,0),z =round(z,0),y)
#write.table(tempData, "lectureExample.dat",row.names = FALSE)


tempData = read.delim("lectureExample.dat",header=TRUE, sep="")
#View(tempData)


#view the relationship between X and Y
plot(tempData$x,tempData$y,
     main="simple scatter plot") #plot

plot(tempData$x,tempData$y,
     main = "best fit line") #plot
abline(lm(y~x,data=tempData)) #add regression

#options(echo=TRUE)
#old model
tempModel = lm(y~x+w, data=tempData)
summary(tempModel)

library(scatterplot3d)
newPlot = scatterplot3d(x=tempData$x,y=tempData$w,z=tempData$y,
                        main = "3D scatter plot")
newPlot = scatterplot3d(x=tempData$x,y=tempData$w,z=tempData$y, col=tempData$z,
                        main = "3D best fit line")
planeInfo = lm(y~x+w, data=tempData)
newPlot$plane3d(planeInfo)
#multiple Regression Model




#FORCED (STANDARD REGRESSION)
setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture08.MultipleRegression")

# gun of statistics that may be related to gun violence

gunData = read.delim("guns.txt",header=TRUE,sep="\t")
View(gunData)

str(gunData)
gunData$stateID = factor(gunData$stateID)


#FORCED ENTRY
forcedRegression = lm(deaths~ population+medianIncome, data=gunData)
summary(forcedRegression)


#HIERARCHICAL REGRESSION
baseModel = lm(deaths~ population, data=gunData)
summary(baseModel)

newModel = lm(deaths~ population+medianIncome, data=gunData)
summary(newModel)

#compare the two models
anova(baseModel, newModel)

#hierarchical
baseModel = lm(y~x, data=tempData)
summary(baseModel)

newModel = lm(y~x+w, data=tempData)
summary(newModel)

spcor(gunData[,c("deaths","population","medianIncome")])$estimate^2
beta.lm(forcedRegression)

pcor.test(x = gunData$population, y=gunData$deaths, z=gunData$medianIncome)
pcor.test(x = gunData$medianIncome, y=gunData$deaths, z=gunData$population)
apa.reg.table(forcedRegression)


#get slopes
library(spcor)
spcor(tempData[,c("x","w","z","y")])
describe(tempData)

partialCorrelations = pcor(tempData[,c("x","w","z","y")])

#partialCorrelations$estimate["y","x"]
c(
paste("pr(x,y) = ", round(partialCorrelations$estimate["y","x"],3), 
      ", t = ", round(partialCorrelations$statistic["y","x"],3),
      ", p = ",round(partialCorrelations$p.value["y","x"],3)),
paste("pr(w,y) = ", round(partialCorrelations$estimate["y","w"],3), 
      ", t = ", round(partialCorrelations$statistic["y","w"],3),
      ", p = ",round(partialCorrelations$p.value["y","w"],3)),  
paste("pr(z,y) = ", round(partialCorrelations$estimate["y","z"],3), 
      ", t = ", round(partialCorrelations$statistic["y","z"],3),
      ", p = ",round(partialCorrelations$p.value["y","z"],3))  
)

semiPartialCorrelations = spcor(tempData[,c("x","w","z","y")])
c(
  paste("spr(x,y) = ", round(semiPartialCorrelations$estimate["y","x"],3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","x"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","x"],3)),
paste("spr(w,y) = ", round(semiPartialCorrelations$estimate["y","w"],3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","w"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","w"],3)),
paste("spr(z,y) = ", round(semiPartialCorrelations$estimate["y","z"],3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","z"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","z"],3))
)
c(
  paste("spr(x,y) = ", round(semiPartialCorrelations$estimate["y","x"]^2,3), 
        ", t = ", round(semiPartialCorrelations$statistic["y","x"],3),
        ", p = ",round(semiPartialCorrelations$p.value["y","x"],3)),
  paste("spr(w,y) = ", round(semiPartialCorrelations$estimate["y","w"]^2,3), 
        ", t = ", round(semiPartialCorrelations$statistic["y","w"],3),
        ", p = ",round(semiPartialCorrelations$p.value["y","w"],3)),
  paste("spr(z,y) = ", round(semiPartialCorrelations$estimate["y","z"]^2,3), 
        ", t = ", round(semiPartialCorrelations$statistic["y","z"],3),
        ", p = ",round(semiPartialCorrelations$p.value["y","z"],3))
)

.849*(1-.0519)+ (.0519*(1-.849))
#approximate slopes
#not the real formula
sdx = sd(tempData$x)
sdy = sd(tempData$y)
sdw = sd(tempData$w)
sdz = sd(tempData$z)

semiPartialCorrelations = spcor(tempData[,c("x","w","z","y")])
c(
paste("beta(x,y) = ", round(semiPartialCorrelations$estimate["y","x"]*(sdy/sdx),3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","x"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","x"],3)),
paste("beta(w,y) = ", round(semiPartialCorrelations$estimate["y","w"]*(sdy/sdw),3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","w"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","w"],3)),
paste("beta(z,y) = ", round(semiPartialCorrelations$estimate["y","z"]*(sdy/sdz),3), 
      ", t = ", round(semiPartialCorrelations$statistic["y","z"],3),
      ", p = ",round(semiPartialCorrelations$p.value["y","z"],3))  
)
tempModel$coefficients


tempModel = lm(y~x+w+z, data=tempData)
summary(tempModel)
tempModel$coefficients
dlibrary(apaTables)

apa.reg.table(tempModel)


#4d plot
sampleSize = 1000
 sj = seq(1,sampleSize,1)
 y = rnorm(n=sampleSize, mean = 3, sd =1)
 x = rnorm(n=sampleSize, mean = 3, sd =1)*.3*y
 w = rnorm(n=sampleSize, mean = 3, sd =1)*.7*x
 z = rnorm(n=sampleSize, mean = 3, sd = 1)*.3*y
 zColour = rainbow(z)

tempData2 = data.frame(sj,x,w,z,y,zColour)

newPlot = scatterplot3d(x=tempData2$x,y=tempData2$w,z=tempData2$y, color=rainbow(tempData2$z),
                        main = "3D best fit line")
tempModel = lm(y~w, data=tempData)
tempModel = lm(y~x+z, data=tempData)
tempModel = lm(y~x+w+z, data=tempData)
tempModel = lm(y~z, data=tempData)
summary(tempModel)

library(psych)
library(car)
cor(tempData)
library(yhat)
regr(tempModel)
#plot(x,y,z)

#library(psych)
#corr.test(tempData)
