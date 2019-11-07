library(ggplot2)


boxplot <- ggplot(dataframe, aes(iv, dv))
boxplot + 
  geom_boxplot(outlier.colour = "Blue", #plot data, set the colour of the outliers to blue!
               outlier.size = 3) + #increase the size of the outlers
  labs(x = "Description of IV", 
       y = "Description of DV")


#if you need to add a second independent variable
boxplot <- ggplot(dataframe, aes(iv, dv,colour=iv2))
boxplot + 
  geom_boxplot(outlier.colour = "Blue", #plot data, set the colour of the outliers to blue!
               outlier.size = 3) + #increase the size of the outlers
  labs(x = "Description of IV", 
       y = "Description of DV")