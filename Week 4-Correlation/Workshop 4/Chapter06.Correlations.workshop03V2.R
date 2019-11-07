
##---------------------------------------------------------------------------------------------------------
##R Code for Chapter 6 of:
##
##Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

#library(knitr)
#opts_knit$set(root.dir="~/Documents/Teaching/2017-2018/Statistics/Data files")
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")
######Initiate packages

#If you don't have Hmisc installed then use:
install.packages("Hmisc")
install.packages("polycor")
install.packages("ppcor")

#Initiate packages
library(ppcor)
library(ggplot2)
library(polycor)
library(psych)


#OPEN THE DATA FILE
examData = read.delim("Exam Anxiety.dat",  header = TRUE)

#INSPECT THE DATAFRAME
View(examData)

#check structure
str(examData)

#fix sj id variable
examData$Code = factor(examData$Code)

#calculate basic statistics
#requires library(psych)
describe(examData)



#TEST ASSUMPTIONS!!!!
#-----Interval Data
#-----Aproximately Normal Distribution (for p value)
#-----No major outliers
#-----Linearity


##########################################
##----TEST NORMALITY USING HISTOGRAMS
##########################################
hist.Anxiety <- ggplot(examData, aes(Anxiety)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Exam Anxiety", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(examData$Anxiety, na.rm = TRUE), sd = sd(examData$Anxiety, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.Anxiety  # DO THIS TO VIEW THE GRAPH

#MIGHT BE SKEWED

hist.Exam <- ggplot(examData, aes(Exam)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Exam Performance", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(examData$Exam, na.rm = TRUE), sd = sd(examData$Exam, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.Exam  # DO THIS TO VIEW THE GRAPH

library(grid)
library(gridExtra)
grid.arrange(hist.Anxiety, 
             hist.Exam,
             nrow=1, ncol = 2)  #NCOL * NROW MUST BE > THAN THE NUMBER OF GRAPHS

#HMMMM... MIGHT BE A PROBLEM???!!!!

#HOW COULD WE RESOLVE THIS????
# 1. TRANSFORM THE DATA
# 2. USE A NON-PARAMETRIC TEST
# 3. IS OUR SAMPLE IS LARGER THAN 30? (WHAT DO WE KNOW ABOUT THE POPULATION,HERE)


##########################################
##----TEST LINEARITY USING SCATTERPLOT
##########################################
###Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))+
  geom_point() + 
  geom_smooth(colour="Blue") +
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Red") + #SET THE COLOUR OF THE LINE
  labs(x = "Exam Anxiety",    #ADD THE X LABEL
       y = "Exam Performance %") # ADD THE Y LABEL
scatter


#LOOKS OK!


#OUTLIERS - DON'T SEEM TO BE ANY EXTREME SCORES
#NOT REALLY DEFINED WELL.  WE WILL RETURN TO THIS ISSUE WHEN
#WE DISCUSS REGRESSION

########################################
#MAIN FUNCTIONS USED TO CALCULATE CORRELATIONS
########################################

?cor.test

# cor.test
# corr.test

#  SEE ?COR.TEST TO UNDERSTAND THE ALTERNATIVE ARGUMENT

##--------- this method returns the correspinding t value, df, confidence interval, and p-value
##--------- data can be x,y,
##--------- alternative can be "two.sided", "less", "greater"
##--------- method can be "pearson", "kendall", "spearman"
##--------- conf.level is 0.95, 


##--- corr.test(data, use, method, adjust, alpha, ci)
##--------- In the library(psych)
##--------- data is a matrix or dataframe (only numerical data)
##--------- use either "pair-wise" deletion or "complete" cases
##--------- method can be "pearson", method = "kendall", method = "spearman"
##--------- adjust how to adjust the p values for multiple comparisons (default is Holm)
##--------- alpha is the p value for the confidence intervals (.05 is the default)
##--------- ci is either TRUE or FALSE



########################################
#--------Pearson r----------
########################################

##cor.test(x,y, 
##         alternative = "string", 
##         use="complete.obs",
##         method is "correlation type", 
##         conf.level = 0.95) RETURNS THE PVALUES


#COMPARE TWO VARIABLES

cor.test(examData$Exam,examData$Anxiety, 
         alternative = "two.sided",
         use = "complete.obs", 
         method = "pearson")  

#TRY IT ON YOUR OWN BETWEEN REVISE AND ANXIETY

cor.test(examData$Revise,examData$Anxiety, 
         alternative = "two.sided",
         use = "complete.obs", 
         method = "pearson")  


## >>> COMPARE MORE THAN TWO VARIABLES <<< 


##--- corr.test(data, use, method, adjust, alpha, ci)
##---THE PROBLEM WITH THIS FUNCTION IS THAT IT NEEDS A DATAFRAME, 
##---BUT THE DATAFRAME CANNOT HAVE ANY FACTORS OR TEXT IN IT...
##---ONLY NUMBERS!!!

#  corr.test can only use a dataframe containing numeric variables!!
#THE SIMPLEST WAY TO DO THIS IS TO CREATE A NEW DATAFRAME AND USE CORR.TEST ON VARIABLES. 
# corr.test would not work with gender and and ID, we need to make a new dataframe without thiese
# new data frame should have only the variables we want to rub the test on.
#THAT ONLY HAS THE VARIABLES YOU WANT IN IT

examData2 <- examData[, c("Exam", "Anxiety", "Revise")] #CREATE A NEW VARIABLE WITH ONLY THE COLUMNS OF INTEREST
corr.test(examData2,  use = "complete", method = 'pearson') #USE 2 OF THE NUMERIC COLUMNS

#the output from both functions (cor.text and corr.test) can be put into a variable
#this may be useful for corr.test
corOutput =cor.test(examData$Exam,examData$Anxiety, alternative = "two.sided",use = "complete.obs", method = "pearson")  # WILL NOT RUN BECAUSE SOME COLUMNS ARE NOT NUMERIC
#  Notice, the cor.test here will be saved as a list
str(corOutput)


#to access the correlation coefficient use
corOutput$estimate

#to get the R^2
corOutput$estimate^2


# We could save it as a matrix with the corr.test and saving it in an object
corrMatrix = corr.test(examData2,  use = "complete", method = "pearson") #USE 2 OF THE NUMERIC COLUMNS

corrMatrix$r   #see the correlations
corrMatrix$p   #see the p values
corrMatrix$n   #see the nvalues

#to get R^2
corrMatrix$r^2


#NONPARAMETRIC TESTS
########################################
#--------Spearmans Rho----------
# Section 6.5.5, p. 223
# set specific predictions about the direction of the correlation
# compare different types of correlations
########################################
liarData = read.delim("The Biggest Liar.dat",  header = TRUE)
View(liarData)
str(liarData)
describe(liarData)
#EXAMINE ASSUMPTIONS
#one of the variables is ORDINAL!!!!  Violates assumption of Pearsons correlation
#THEREFORE WE NEED TO USE A NON-PARAMETRIC TEST - SPEARMAN'S RHO

# think about the alternative = "less"
#  We are saying if they have higher scores they are going to place lower, the lower rankd
#  around 1 and 2 denote better liars. 
#  THIS IS VERY IMPORTANT TO UNDERSTAND WHEN RANKING DATA for a spearman.

cor.test(liarData$Position, liarData$Creativity, alternative = "less", use="complete.obs",method = "spearman") # will return an email because of the tied values
cor.test(liarData$Position, liarData$Creativity, alternative = "less", use="complete.obs",method = "pearson") # will return an email because of the tied values

#RUN ALL OF THE CORRELATIONS AT ONCE
corr.test(liarData, method="spearman")

hist.Liar <- ggplot(liarData, aes(Creativity)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Creativity", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(liarData$Creativity, na.rm = TRUE), sd = sd(liarData$Creativity, na.rm = TRUE)), 
                colour = "black", size = 1)

hist.Liar  # DO THIS TO VIEW THE GRAPH

  
########################################
#--------Kendalls Tau----------
#compare tau and pearson's r
########################################

#KENDALL'S TAU IS MORE ROBUST THAN SPEARMAN'S RHO 
#WHEN YOU HAVE A SMALL SAMPLE AND LOTS OF TIED VALUES
cor.test(liarData$Position, liarData$Creativity, alternative = "less", use="complete.obs",method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", use="complete.obs",method = "pearson")

#RUN ALL OF THE CORRELATIONS AT ONCE
corr.test(liarData, method="kendall")


