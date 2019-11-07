#STEP #6: EXAMINE THE SUMMARY INFORMATION ABOUT OUR VARIABLES
#check variable for errors

#DESCRIBE
library(psych)
describe(dataframe)  #returns basic summary statistics include min and max

#if you need to split by an independent variable
describeBy(dataframe$dv, #your dependent variable
           group=dataframe$iv, #your independent variable
           mat=TRUE, #return it as a matrix - you can then "View" it
           digits=1)  #the number of decimals

#for multiple independet variables
describeBy(dataframe$dv, 
           group=list(dataframe$iv1,dataframe$iv2), #the list function allows you to split by more than one variable
           mat=TRUE, 
           digits=1)

