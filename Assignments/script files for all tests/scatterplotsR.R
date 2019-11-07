library(ggplot2)

#simple scatter plot
scatterPlot <- ggplot(dataframe, aes(xvariable, yvariable))
scatterPlot + 
  geom_point() + #ADD THE DATA POINTS
  labs(x = "Description of X Variable", #ADD THE X LABEL
       y = "Description of Y variable") # ADD THE Y LABEL


#Scatter plot with best fit line (non-linear)
scatterPlot <- ggplot(dataframe, aes(xvariable, yvariable))
scatterPlot + 
  geom_point() + #ADD THE DATA POINTS
  geom_smooth(colour="Blue",
              se=TRUE) + #ADD THE BEST FIT LINE AND STANDARD ERROR
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Red", #SET THE COLOUR OF THE LINE
              se = FALSE) +       #NO STANDARD ERROR, TRUE= standard error
  labs(x = "Description of X Variable", #ADD THE X LABEL
       y = "Description of Y variable") # ADD THE Y LABEL

