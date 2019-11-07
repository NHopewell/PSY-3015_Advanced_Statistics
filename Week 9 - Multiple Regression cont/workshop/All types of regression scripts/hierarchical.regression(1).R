#HIERARCHICAL REGRESSION

#THIS SCRIPT CAPTURES HOW HIERARCHICAL REGRESSION 
#DIFFERS FROM FORCED ENTRY AKA STANDARD REGRESSION
#SEE FORCED ENTRY REGRESSION FOR INFO ABOUT EXAMINING RESIDUALS

#run the regression analysis of the BASE model
#you can add more predictors if necessary 
baseModelName = lm(outcomeVariable ~ predictor1 + predictor2, data=dataframe)

#get the regression summary
summary(baseModelName)

#run the regression analysis of the NEW model
#you can add more predictors if necessary 
newModelName = lm(outcomeVariable ~ predictor1 + predictor2, data=dataframe)

#get the regression summary
summary(newModelName)

#compare the two models
anova(baseModelName,newModelName)



