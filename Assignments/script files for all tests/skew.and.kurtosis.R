#get skew and kurtosis
library(pastecs)

#GET DESCRIPTIVE STATISTICS FOR A SINGLE VARIABLE
#REMEMBER THIS FUNCTION GIVE USE THE VALUES NECESSARY TO COMPARE SKEW AND KURTOSIS ON THE Z DISTRIBUTION
#ALSO GIVES US THE SHAPIRO-WILK TEST (normtest.W, normtest.p)
# stat.desc(variable, basic, norm)
# basic = TRUE, give only basic statistics
# norm = TRUE, give normality statistics

stat.desc(dataframe$dv, basic = FALSE, norm = TRUE)

#round = round statistics to 3 decimal places
round(stat.desc(dataframe$dv, basic = FALSE, norm = TRUE),3)


#GET DESCRIPTIVE STATISTICS FOR MULTIPLE VARIABLES
#ROUND THE VALUES TO 3 DECIMAL PLACES
#round(values, digits = )
# dlf[,c("day1","day2")]  # says return all of the rows for columns "day1"and "day2"
round(stat.desc(datafame[, c("dv1", "dv2")], basic = FALSE, norm = TRUE), digits = 3)

#Use by() to get descriptives for two variables, split by gender
by(dataframe[, c("dv1", "dv2")], dataframe$iv, stat.desc, basic = FALSE, norm = TRUE)
