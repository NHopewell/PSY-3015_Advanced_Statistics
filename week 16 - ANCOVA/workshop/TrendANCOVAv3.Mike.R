
setwd("F:/Workshop Psyc3015 2017 2018/Data files/Data files")

#...CONTINUE EXAMPLE FROM LAST WEEK TO EXAMINE TREND ANALYSES
ViagraData= read.delim("Viagra.dat", header = TRUE)

head(ViagraData, n=20)

#...check the coding of variables
str(ViagraData)

#...convert sjNum and dose into a factor
ViagraData$person = factor(ViagraData$person)
ViagraData$dose=factor(ViagraData$dose,labels=c("Placebo","Low Dose","High Dose"))
str(ViagraData)

#...describe your data

library(psych)

describe(ViagraData)
summary(ViagraData)

#...examine your DV 
#...remember that it is more critical to examine it as a function
#...of your independent variable
hist(ViagraData$libido)

describeBy(ViagraData$libido,ViagraData$dose,mat= TRUE,digits = 1)


#...TEST ASSUMPTIONS BEFORE RUNNING THE MODEL

#...Homogeneity of Variance
library(car)
#Levene's test for comparison of variances of libido scores by dose.
#leveneTest(outcomevariable, group variable, center =median/mean)
leveneTest(ViagraData$libido, ViagraData$dose)  #DEFAULT USING THE MEDIAN AS THE CENTER

#...Normality
by(ViagraData$libido,ViagraData$dose, shapiro.test)

#...RUN THE ANOVA USING A TREND ANALYSIS

#...TREND ANALYSES
#...use the contr.poly() function
#...the value entered into the contr.poly function is the number 
#...of levels in your IV

contrasts(ViagraData$dose) <- contr.poly(3)

contrasts(ViagraData$dose)

#...rerun the anova, it will now run using the contrastsx
trendModel = aov(libido~dose, data=ViagraData)
summary(trendModel)

#...examine the contrasts
summary.lm(trendModel)

#...EXAMINE THE ASSUMPTIONS AFTER RUNNING THE MODEL
#...should lead to the same conclusions as analyzing the
#...assumptions before hand.  Your choice which one you
#...want to do

#...calculate predicted and residual values and 
#...add them to the data.frame
ViagraData$predicted = predict(trendModel)
ViagraData$residuals = resid(trendModel)


#...Normally Distributed Errors assumption
shapiro.test(ViagraData$residuals)

#...Homoscedasticity (homogeneity of variance)

leveneTest(residuals~dose, data=ViagraData)

###########################
#...        ANCOVA    ...##
###########################
#...the viagra data, but with an additional variable
#...partnerLibido can act as a covariate

viagracoData= read.delim("ViagraCovariate.dat", header = TRUE)
head(viagracoData,n=20)

#...check the coding of variables
str(viagracoData)

#...convert  dose into a factor
viagracoData$dose=factor(viagracoData$dose,labels=c("Placebo","Low Dose","High Dose"))
str(viagracoData)

library(psych)

summary(viagracoData)  #this is probably better for an overall inspection


#...examine your DV 

par(mfrow=c(1,3))
by(viagracoData$libido,viagracoData$dose,hist)
describeBy(viagracoData$libido,viagracoData$dose,mat= TRUE,digits = 1)

#...examine your Covariate 
hist(viagracoData$partnerLibido) 
par(mfrow=c(1,3))
by(viagracoData$partnerLibido,viagracoData$dose,hist)
describeBy(viagracoData$partnerLibido,viagracoData$dose,mat= TRUE,digits = 1)



### Test Assumption - Are predictor variables and covariate independent of each other?##
#run the ANOVA with DV = partnerLibdio and IV =dose
indpendenceModel = aov(partnerLibido~dose, data=viagracoData)
summary(indpendenceModel)
#...not a significant effect of the level of viagra on the partner's libido ####

#...Testing for homogeneity of regression slopes 
homogeneousSlopesModel<- aov(libido ~ partnerLibido + dose + dose:partnerLibido, data = viagracoData)
Anova(homogeneousSlopesModel, type = "III")
#...looks like there might be a problem


#...RUN THE ANCOVA MODEL

#...specify orthogonal contrasts
contrasts(viagracoData$dose) <-contr.poly(3)
viagracoModel<-aov (libido ~ partnerLibido + dose,data = viagracoData)
Anova(viagracoModel, type = "III")


#...CHECK THE ANOVA ASSUMPTIONS
viagracoData$predicted = predict(viagracoModel)
viagracoData$residuals = resid(viagracoModel)
viagracoData$sresiduals = rstandard(viagracoModel)

#...Normally distribute residuals
shapiro.test(viagracoData$residuals)
hist(viagracoData$residuals)

#...Homogeneity of variance
library(car)
leveneTest(residuals~dose,viagracoData)



#...get the adjusted values
library(effects)
adjustedMeans<-effect("dose",viagracoModel)

#..get the adjusted means
summary(adjustedMeans)

#..get the adjusted standard errors
adjustedMeans$se

#...conduct follow-up tests
#...use Tukey HSD test
library(multcomp)
postHocs<-glht(viagracoModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)
