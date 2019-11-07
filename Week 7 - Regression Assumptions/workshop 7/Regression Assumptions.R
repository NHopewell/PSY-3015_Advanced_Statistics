#Initiate packages
library(ppcor)
library(ggplot2)
library(polycor)
library(psych)
library(car)
setwd("C:\\Users\\nicho\\Desktop\\Courses\\advanced stats 3015\\Data files")

#OPEN THE DATA FILE
aSaleData = read.delim("Album Sales 1.dat",  header = TRUE)

#INSPECT THE DATAFRAME
View(aSaleData)

#check structure
str(aSaleData)
#Descriptive statistics
describe(aSaleData)




options(scipen =15)

#linear model with regression line slope, intercept 
albumModel= lm(sales~adverts, data=aSaleData)
summary(albumModel)

#make nice tables!!!
library("apaStyle")
library ("apaTable")

apa.reg.table(albumModel,filename = "Album Regression Table.doc")



# ASSUMPTION #1:  APPRROPRIATE PREDICTOR VARIABLE TYPES
#PREDICTOR VARIABLE(S) INTERVAL OR CATEGORICAL WITH TWO LEVELS
#USE THE str() COMMAND
str(aSaleData)

# ASSUMPTION #2 OUTCOME VARIABLE INTERVAL AND UNBOUND
#USE THE str() COMMAND TO ASSESS SCALE TYPE

str(aSaleData)

# ASSUMPTION #3 UNBOUNDNESS
#REQUIRES KNOWLEDGE OF YOUR VARIABLE
# A HISTOGRAM MAY BE ONE OF THE BEST WAYS TO SEE
# IF YOU ARE SUFFERING SOME OF THE CONSEQUENCES OF 
# UNBOUNDNESS -CLUSTERING AT ONE OF YOUR EXTREME VALUES
#some variable types will never meet the unbound assumption!!
hist(aSaleData$adverts)
hist(aSaleData$sales)


#CALCULATE DESCRIPTIVE STATISTICS 
#INSIGHT INTO: 
#------ASSUMPTION # 4 -RESTRICTED RANGE
#------ASSUMPTION # 5 - Non-zero variance,
#USE describe() COMMAND WHICH GIVES MIN AND MAX
library(psych)
describe(aSaleData)


#OR calculate more advanced descriptives using stat.desc
library(pastecs)
stat.desc(aSaleData$sales, basic = FALSE, norm = TRUE)
stat.desc(aSaleData$adverts, basic = FALSE, norm = TRUE)



#ASSUMPTION # 6 - HOMOSCEDASTICITY

#APPROACH # 1
#PLOT THE RESIDUALS AS A FUNCTION OF THE PREDICTED VALUES
# THE PREDICTED AND RESIDUAL VALUES CAN EASILY BE ADDED TO YOUR DATA SET

#----GET THE PREDICTED VALUES
#----ADD THEM TO YOUR DATAFRAME
aSaleData$predicted = round(predict(albumModel),3)

#----GET THE RESIDUAL VALUES
#----ADD THEM TO YOUR DATAFRAME
aSaleData$residuals = round(resid(albumModel),3)
View(aSaleData)


###USE A SCATTER PLOT TO SEE WHETHER THE VARIANCE IS HOMOGENEOUS
#MEANING THAT THE VARIABILITY OF THE ERROR IS EQUAL ACROSS
#THE REGRESSION LINE
library(ggplot2)
scatter <- ggplot(aSaleData, aes(predicted, residuals))
scatter + geom_point() + 
  #geom_smooth()+
  geom_smooth(method = "lm", colour = "Red", se = T) + 
  labs(x = "PREDICTED VALUES (FITTED)", y = "RESIDUALS") 

#METHOD # 2
#can also use the Breusch-Pagan test
# nul-hypothesis = no difference, vriances are equal therefore we have homoscedasticity
# alternative = difference, hetero
library(car)
ncvTest(albumModel)





#ASSUMPTION #7 - NORMALLY DISTRIBUTED ERRORS
#METHOD # 1 - histogram of residuals
aSaleData.errors <- ggplot(aSaleData, aes(residuals)) + 
  labs(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(aSaleData$residuals, na.rm = TRUE), 
                            sd = sd(aSaleData$residuals, na.rm = TRUE)), 
                colour = "black", size = 1) +
  labs(x="MODEL RESIDUALS", y = "Density")

aSaleData.errors

#METHOD # 2 CAN  USE QQPLOTS
#QQPlot of residuals
qqplot.residuals <- qqnorm(aSaleData$residuals,
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles RESIDUALS")
qqline(aSaleData$residuals)

#METHOD NUMBER #3 USE SHAPIRO-WILKS TEST
shapiro.test(aSaleData$residuals)

#METHOD #4 USE SKEW AND KURTOSIS
stat.desc(aSaleData$residuals, basic = FALSE, norm = TRUE)





# ASSUMPTION # 8 -LINEARITY
# SCATTER TO SEE IF THE RELATIONSHIP IS LINEAR
library(ggplot2)
scatter <- ggplot(aSaleData, aes(sales, adverts))
scatter + geom_point() + 
  geom_smooth()+
  geom_smooth(method = "lm", colour = "Red", se = T) + 
  labs(x = "Predictor (xvar)", y = "Outcome (yvar)") 





#YOU CAN ALSO GET THE STANDARDIZED RESIDUALS
#ADD THEM TO YOUR DATAFRAME
aSaleData$rstandard = round(rstandard(albumModel),3)

#GET THE STUDENTIZED RESIDUALS
#ADD THEM TO YOUR DATAFRAME
aSaleData$rstudent = round(rstudent(albumModel),3)
View(aSaleData)