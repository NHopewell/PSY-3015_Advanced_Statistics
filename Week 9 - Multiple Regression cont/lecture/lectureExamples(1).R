id = c(1,2,3,4,5,6,7,8,9,10)

x = c(14,0,5,9,8,5,1,2,3,10)
w = c(6,0,1,6,4,1,3,5,2,3)
y = c(9,1,4,8,7,6,5,3,2,7)
tempData = data.frame(id,x,w,y)

library(ppcor)
pcor(tempData[,c("x","w","y")])
cor(tempData[,c("x","w","y")])

tempModel = lm(y~x+w,data=tempData)
summary(tempModel)
anova(tempModel)
library(lm.beta)
lm.beta(tempModel)

library(psych)
describe(tempData)
.85*(2.66/4.47)
.567*(2.66/2.13)

.2461/(2.66/2.13)
sqrt(1.765)
1.765/()

######################################
###   EXAMPLE 2
######################################
id = c(1,2,3,4,5)
v= c(6,4,5,3,1)
w = c(3,1,3,6,5)
x = c(4,2,5,6,3)
y = c(9,1,4,8,7)
tempData = data.frame(id,v,w,x,y)
library(psych)
describe(tempData)
library(ppcor)

#LET'S GET OUR CORRELATIONS!
interceptModel = lm(y~1, data=tempData)
summary(interceptModel)
anova(interceptModel)

step1Model = lm(y~w,data=tempData)
anova(step1Model)




library(MASS)


# 1.  #start with a model only contains the intercept
startModel<-lm(y ~ 1, data=tempData) 

# 2.  Run the stepwise function on the model
# you need to set the lower and upper bounds of the regression model.  
forwardModel <- stepAIC(# the function
  startModel, # the initial statistical model with only the intercept
  scope=list(lower=~1, #set the lower bound as the intercept
             upper = ~ v + w + x), 
  #set the upper bound as the complete model
  direction="forward") #set the direction








cor(tempData[,c("x","w","y")])

cor.test(tempData$w,tempData$y)



#LET'S GET OUR PARTIAL CORRELATIONS!
pcor(tempData[,c("x","w","y")])$estimate

#LET'S GET OUR SEMI PARTIAL CORRELATIONS!
spcor.test(tempData$x, tempData$y, tempData$w)
spcor.test(tempData$v,tempData$y, tempData$w)

tempModel = lm(y~v+w,data=tempData)
summary(tempModel)
anova(tempModel)
library(lm.beta)
lm.beta(tempModel)

library(apaTables)
apa.reg.table(tempModel)
library(psych)
describe(tempData)
.85*(2.66/4.47)
.567*(2.66/2.13)

.2461/(2.66/2.13)
sqrt(1.765)
1.765/()