#qqplots

#if you know how many graphs you are going to create (when your code is running correctly)
#you can split the graph window into multiple parts to see your graphs together
par(mfrow=c(1,3)) # 1 row with  3 graphs


qqplot <- qqnorm(dataframe$dv,
                      xlim = c(-4,4),  #set your x axis to be the same for all graphs
                      ylim = c(-4,4),  #set your y axis to be the same for all of your graphs
                      xlab = "Theoretical Quantiles", 
                      ylab = "Sample Quantiles DAY 1")
qqline(dataframe$dv)

#if you change the graph window, change it back to default using the following
par(mfrow=c(1,1)) 