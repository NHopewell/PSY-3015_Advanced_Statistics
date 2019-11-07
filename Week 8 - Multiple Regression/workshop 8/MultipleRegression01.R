##########################################
#----Set the working directory------
##########################################

setwd("~/Documents/Teaching/2017-2018/Statistics/Data files/")
options(scipen=15)


######################################
#OPEN THE DATA FILE
######################################

albumData<-read.delim("Album Sales 2.dat", header = TRUE)
View(albumData)

str(albumData)

summary(albumData)

######################################
#RUN A SIMPLE REGRESSION
######################################
#regression with 1 variable
simpleModel<-lm(sales ~ adverts, data = albumData)
summary(simpleModel)

library(apaTables)
apa.reg.table(simpleModel,filename = "..//simpleModel.doc")
######################################
#forced, enters everything at once
######################################


#Multiple Regression
albumForced<-lm(sales ~ adverts + airplay + attract, data = albumData)
summary(albumForced)


# What would we infer from this model?
#It has good fit (explains significant amount of variance)
#Each of the predictors is significant (contributes to the model)


library(apaTables)

apa.reg.table(albumForced)
apa.reg.table(albumForced,filename = "..//albumForced.doc")
#can use beta to understand the relative contributions of the different predictors
#should notice that beta and semipartial are closely linked.


######################################
#hierarchical
######################################

#the model based on past data
baseModel<-lm(sales ~ adverts + airplay, data = albumData)  
summary(baseModel)  # examine its fit

#add new variables based on theoretical importance 
#note you could do this more than once
newModel<-lm(sales ~ adverts + airplay + attract, data = albumData)  
summary(newModel)


# see if the newmodel is better than the old model
anova(baseModel,newModel)  


#get R2 change
apa.reg.table(baseModel,newModel, filename = "..//hierarchical.doc")  


#should point out that it is possible that all of the predictors are
#now significant, but it is not necessarily the case that the model will be
#any better

########################
#stepwise forward
########################

# Requires the MASS library,  this should already be installed and active.  
# If not, install it and activate it.
library(MASS)


# 1.  #start with a model only contains the intercept
startModel<-lm(sales ~ 1, data=albumData) 

# 2.  Run the stepwise function on the model
# you need to set the lower and upper bounds of the regression model.  
forwardModel <- stepAIC(# the function
  startModel, # the initial statistical model with only the intercept
  scope=list(lower=~1, #set the lower bound as the intercept
             upper = ~ adverts + airplay + attract), 
            #set the upper bound as the complete model
  direction="forward") #set the direction


# 3.  display results
# will provide a history of the interation of the model.  
forwardModel$anova

#STEP = the model
#df the numerator df
#deviance = SS predictor  - how much new variance is explained at each step
#resid. Df = the denominator (residual) df  
#resid. Dev = the residual variance at each step
#AIC = quality indicator (goodness of fit / complexity)

#you can use these values to get the R^2 change
#R = variance explained / TOTAL VARIANCE
forwardModel$anova[2,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  
forwardModel$anova[3,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  
forwardModel$anova[4,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  


#get the parameters of the final model
summary(forwardModel)   

#create a table of the final model
#I'm not sure how to create a table of the multiple models tested
apa.reg.table(forwardModel)
apa.reg.table(forwardModel, filename = "..//forwardModel.doc")  


########################
#stepwise backward
########################
# 1.  build a model with all of the relevant variables
completeModel<-lm(sales ~ adverts + airplay + attract, data = albumData)  #add new variables based on theoretical importance #note you could do this more than once

# 2.  Run the stepwise function on the complete model
#see the forward model to have a better idea of what the values are
backwardModel <- stepAIC(completeModel, 
                         scope=list(lower=~1, 
                                    upper = ~ adverts + airplay + attract),
                         direction="backward")

# 3.  display results
backwardModel$anova # will provide a history of the interation of the model.  Our example only does one interation
summary(backwardModel)  

apa.reg.table(backwardModel)
apa.reg.table(backwardModel, filename = "..//backwardModel.doc")
########################
#stepwise both
########################
# 1.  build a model with all of the relevant variables
completeModel<-lm(sales ~ adverts + airplay + attract, data = albumData)  #add new variables based on theoretical importance #note you could do this more than once

# 2.  Run the stepwise function on the complete model
bothModel <- stepAIC(completeModel, 
                     scope=list(lower=~1, 
                                upper = ~ adverts + airplay + attract),
                     direction="both")

# 3.  display results
bothModel$anova # will provide a history of the interation of the model.  Our example only does one interation
summary(bothModel)  

apa.reg.table(bothModel)
apa.reg.table(bothModel,filename = "..//bothModel.doc")
              