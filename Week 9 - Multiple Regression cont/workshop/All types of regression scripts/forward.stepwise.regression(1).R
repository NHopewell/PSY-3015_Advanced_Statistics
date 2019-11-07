#FORWARD STEPWISE REGRESSION

#SEE FORCED ENTRY REGRESSION FOR INFO ABOUT EXAMINING RESIDUALS

# Requires the MASS library,  this should already be installed and active.  
# If not, install it and activate it.
library(MASS)


# 1.  #start with a model only contains the intercept
startModel<-lm(outcomeVariable ~ 1, data=dataframe) 

# 2.  Run the stepwise function on the model
# you need to set the lower and upper bounds of the regression model.  
forwardModel <- stepAIC(# the function
  startModel, # the initial statistical model with only the intercept
  scope=list(lower=~1, #set the lower bound as the intercept
             upper = ~ predictor1 + predictor2 + predictor3), 
  #set the upper bound as the complete model (all the IVs you are interested in)
  direction="forward") #set the direction


# 3.  display results
# will provide a history of the interation of the model.  
forwardModel$anova

#INFO ABOUT THE OUTPUT 
#STEP = the model
#df the numerator df
#deviance = SS predictor  - how much new variance is explained at each step
#resid. Df = the denominator (residual) df  
#resid. Dev = the residual variance at each step
#AIC = quality indicator (goodness of fit / complexity)

#you can use these values to get the R^2 change
#R = variance explained / TOTAL VARIANCE
# THE NUMBERS 2,3,4 ARE THE STEP NUMBERS... IF YOU HAD 5 STEPS YOU WOULD
# NEED ONE MORE COMMAND BELOW
forwardModel$anova[2,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  
forwardModel$anova[3,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  
forwardModel$anova[4,"Deviance"]/ forwardModel$anova[1,"Resid. Dev"]  


#get the parameters of the final model
summary(forwardModel)   

#create a table of the final model
#I'm not sure how to create a table of the multiple models tested
apa.reg.table(forwardModel)
apa.reg.table(forwardModel, filename = "..//forwardModel.doc")  
