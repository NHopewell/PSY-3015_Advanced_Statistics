
library(ggplot2)

#a simple histogram
#CREATE THE OBJECT - USE THE DATA FROM DAY1 ONLY, NO LEGEND
simpleHistogram <- ggplot(dataframe, aes(variable)) 

#ADD THE DATAPOINTS IN THE FORM OF A HISTOGRAM.  
simpleHistogram +
  geom_histogram() +  #without specifying any characteristics
  labs(x = "Description of X variable", 
       y = "Frequency")


#HISTOGRAM  SPLIT BASED ON INDEPENDENT VARIABLE
simpleHistogram <- ggplot(dataframe, aes(dependentVariable))
simpleHistogram + 
  geom_histogram(aes(fill = independentVariable),alpha = 0.5) + 
  labs(x = "Description of X variable", 
       y = "Frequency")+
  theme(legend.position = "right")  #location of Indpendent variable legend

#DENSITY PLOT WITH A NORMAL CURVE
densityPlot <- ggplot(dataframe, aes(dependentVariable)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="white") +
  stat_function(fun = dnorm, #plot a normal density function
                args = list(mean = mean(dataframe$dependentVariable, na.rm = TRUE),  #the mean of your distribution
                            sd = sd(dataframe$dependentVariable, na.rm = TRUE)),  #the sd of your distribution
                colour = "black", 
                size = 1) +
  labs(x = "Description of X variable", 
       y = "Density")

densityPlot  # DO THIS TO VIEW THE GRAPH
