#FORCED ENTRY AKA STANDARD REGRESSION

#run the regression analysis
#you can add more predictors if necessary 
regressionModelName = lm(outcomeVariable ~ predictor1 + predictor2, data=dataframe)

#get the regression summary
summary(regressionModelName)

#plot the regression model to examine assumptions
plot(regressionModelName)

#extract predicted and residuals to examine assumptions
dataframe$predicted = predict(regressionModelName)
dataframe$residuals = resid(regressionModelName)


