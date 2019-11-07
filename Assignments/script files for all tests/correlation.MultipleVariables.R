

##COMPARE MORE THAN TWO VARIABLES
##--- corr.test(data, use, method, adjust, alpha, ci)
##---THE PROBLEM WITH THIS FUNCTION IS THAT IT NEEDS A DATAFRAME, 
##---BUT THE DATAFRAME CANNOT HAVE ANY FACTORS OR TEXT IN IT...
##---ONLY NUMBERS!!!


##--- corr.test(data, use, method, adjust, alpha, ci)
##--------- In the library(psych)
##--------- data is a matrix or dataframe (only numerical data)
##--------- use either "pair-wise" deletion or "complete" cases
##--------- method can be "pearson", method = "kendall", method = "spearman"
##--------- adjust how to adjust the p values for multiple comparisons (default is Holm)
##--------- alpha is the p value for the confidence intervals (.05 is the default)
##--------- ci is either TRUE or FALSE


corr.test(dataframe[, c("variable1", "variable2", "variable3")],  #USE ONLY THE VARIABLES YOU WANT TO CORRELATE (NEED TO BE NUMERIC)
          use = "pair-wise", 
          method = 'pearson') #USE 2 OF THE NUMERIC COLUMNS


#can also put it in a table
testResults = corr.test(examData2,  use = "complete", method = "pearson") #USE 2 OF THE NUMERIC COLUMNS

testResults$r   #see the correlations
testResults$p   #see the p values
testResults$n   #see the nvalues