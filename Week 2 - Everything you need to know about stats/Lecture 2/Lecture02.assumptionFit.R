###########################################
# CHECK THE ABILITY OF OUR STATISTICAL MODEL 
# TO MAKE PREDICTIONS WHEN ASSUMPTIONS ARE 
#  ****VALID****
###########################################

# set this subject variable to be a sequence of numbers from 1 to 1000, increasing by 1
# then, the outcome using rnorm to sample from a sample with those parameteres
# then, the tempData using the data.frame will put it into a data frame
sj = seq(1,1000,1)
outcome = rnorm(n=1000, mean=100, sd = 15 )
tempData = data.frame(sj,outcome)
View(tempData)

#GET THE MEAN AND THE STANDARD DEVIATION 
mOutcome = mean(tempData$outcome)
sdOutcome = sd(tempData$outcome)

library(ggplot2)
hOutcome <- ggplot(tempData, aes(outcome)) + #CREATE THE HISTOGRAM
  labs(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") + 
  labs(x = "Outcome Values", y = "Density") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(tempData$outcome, na.rm = TRUE), 
                            sd = sd(tempData$outcome, na.rm = TRUE)), 
                colour = "red", size = 1)

hOutcome  #DISPLAY THE HISTOGRAM

# add lines for 2 SDs above and below
hOutcome +
  geom_vline(xintercept = mOutcome, colour="blue")+
  geom_vline(xintercept = mOutcome-(2*sdOutcome), colour="blue")+
  geom_vline(xintercept = mOutcome+(2*sdOutcome), colour="blue")

#add zscores
# create a new variable called zscore and
tempData$zscore = scale(tempData$outcome)

totalObservations = dim(tempData)[1] #1000 because that is how many samples we have
#this will show hte total of scores above and below two standarddeviations
totalAbove2SD = length(tempData$outcome[tempData$zscore>2])
totalBelow2SD = length(tempData$outcome[tempData$zscore<(-2)])

#What is our proportion ABOVE 2SD?  Compare to .025
totalAbove2SD/totalObservations

#turn it into a percent
(totalAbove2SD/totalObservations)*100

#What is our proportion BELOW 2SD?  Compare to .025 - this is what we would expect to see in a 2 tailed test
# 2.5 percent of the data should be above and below
totalBelow2SD/totalObservations

#turn it into a percent
(totalBelow2SD/totalObservations)*100

###########################################
#CHECK THE ABILITY OF OUR STATISTICAL MODEL 
# TO MAKE PREDICTIONS WHEN ASSUMPTIONS ARE 
#  ****INVALID****
###########################################


# notice the different in the outcome variable here

sj = seq(1,1000,1)
outcome = rnorm(n=1000, mean=100, sd = 15 )+rexp(n=1000, rate = .020)
tempData = data.frame(sj,outcome)
#View(tempData)


#GET THE MEAN AND THE STANDARD DEVIATION 
mOutcome = mean(tempData$outcome)
sdOutcome = sd(tempData$outcome)

library(ggplot2)
hOutcome <- ggplot(tempData, aes(outcome)) + #CREATE THE HISTOGRAM
  labs(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") + 
  labs(x = "Outcome Values", y = "Density") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(tempData$outcome, na.rm = TRUE), 
                            sd = sd(tempData$outcome, na.rm = TRUE)), 
                colour = "red", size = 1)
hOutcome  #DISPLAY THE HISTOGRAM

hOutcome +
  geom_vline(xintercept = mOutcome, colour="blue")+
  geom_vline(xintercept = mOutcome-(2*sdOutcome), colour="blue")+
  geom_vline(xintercept = mOutcome+(2*sdOutcome), colour="blue")

#add zscores

tempData$zscore = scale(tempData$outcome)

totalObservations = dim(tempData)[1] #1000 because that is how many samples we have
totalAbove2SD = length(tempData$outcome[tempData$zscore>2])
totalBelow2SD = length(tempData$outcome[tempData$zscore< (-2)])

#What is our proportion ABOVE 2SD?  Compare to .025
totalAbove2SD/totalObservations

#turn it into a percent
(totalAbove2SD/totalObservations)*100

#What is our proportion BELOW 2SD?  Compare to .025
totalBelow2SD/totalObservations

#turn it into a percent
(totalBelow2SD/totalObservations)*100


# Here we see that what we assume is not met, the scores falling above and below 2 SDs
#  are not what we would expect in a normal distribution
#  You see skew and you see where the model over and under predicts the observed data.
