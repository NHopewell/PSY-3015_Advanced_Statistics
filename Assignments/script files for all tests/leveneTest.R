#levene's test


##########################################
##########################################
#Levene's test for comparison of variances of day 1 scores by gender.
#SECTION 5.7.1.1
##########################################
##########################################
#leveneTest(outcomevariable, group variable, center =median/mean)
leveneTest(dataframe$dv, dataframe$iv)  #DEFAULT USING THE MEDIAN AS THE CENTER
leveneTest(dataframe$dv, dataframe$iv, center = mean)  #CHANGE THE CENTER TO MEAN

# REMEMBER THE LEVENE TEST IS JUST LIKE ANY OTHER IF THE P<.05 THEN THERE IS A DIFFERENCE
