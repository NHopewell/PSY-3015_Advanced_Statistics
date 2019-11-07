#the shapiro wilks test
library(car)
shapiro.test(dataframe$dv)  # RUN THE SHAPIRO-WILKS TEST ON A DEPENDENT VARIABLE


by(dataframe, #dataframe
   list(dataframe$iv1, dataframe$iv2),
   function(x) shapiro.test(x$dv)) # RUN THE SHAPIRO-WILKS TEST SPLIT BY 1 OR MORE IVS

