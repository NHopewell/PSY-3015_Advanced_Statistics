#...repeated measures example

#...set working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")

#...open the file
dietData = read.delim("Diet.dat", header=TRUE, sep = "\t")

#...visually inspect the data
head(dietData,n=10)

#...add a subject ID so that we can reshape the data
dietData$sj= c(seq(1,10,1))

library(reshape2)
longDietData = melt(dietData, id = c("sj"), 
                       varying=c("start","month1","month2"), 
                       variable.name="condition", 
                       value.name="weight")
head(longDietData, n=10)

#..examine variable coding
str(longDietData)
levels(longDietData$condition)
longDietData$sj= factor(longDietData$sj)

#...explore the data

library(psych)
describeBy(longDietData$weight,  #...DV
           longDietData$condition, #...IV
           mat=TRUE, #...return a matrix
           digits =1)  #...round to 1 decimal

boxplot(weight~condition,  #...formula
        data=longDietData) #...dataframe

#..EXAMINE ASSUMPTIONS

#...normality
by(longDietData$weight,  #dv
   longDietData$condition, #iv
   shapiro.test) #function
#violated in two conditions

#...homogeneity of variance
library(car)
leveneTest(weight~condition,  #...formula
           data = longDietData)  #...dataframe
#...ok!

#...RUN THE ANOVA
contrasts(longDietData$condition) = contr.poly(3)

library(ez)
ezANOVA(data = longDietData,
        dv = "weight", 
        wid = sj, 
        within=condition,
        type = 3)

#...sphericity is violated

#...let's try a non-parametric test
friedman.test(weight~condition|sj, #dv~iv|within ID
              data=longDietData) #..dataframe

