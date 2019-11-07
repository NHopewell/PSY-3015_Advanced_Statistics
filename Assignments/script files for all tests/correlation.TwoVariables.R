
library(psych)
##cor.test(x,y, 
##         alternative = "string", #can be "two.sided", "less", "greater"
##         use="complete.obs",
##         method is "correlation type", #can be "pearson", "kendall", "spearman"
##         conf.level = 0.95) RETURNS THE PVALUES


cor.test(dataframe$v1,dataframe$v2, 
         alternative = "two.sided",
         use = "complete.obs", 
         method = "pearson")  


#can save in a variable to extract values

testResult = cor.test(dataframe$v1,dataframe$v2, 
         alternative = "two.sided",
         use = "complete.obs", 
         method = "pearson")  