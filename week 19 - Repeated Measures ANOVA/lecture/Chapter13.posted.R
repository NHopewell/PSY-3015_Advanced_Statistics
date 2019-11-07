

sj <- paste("pid",c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),sep="")
food <- c(rep(c("vegetables","fruit","chocolate"),5))
pleasure<-c(7,10,10,4,6,8,5,9,10,3,3,6,1,2,6)

#...create the data.frame
exampleData <- data.frame(sj,food, pleasure)
str(exampleData)

#...reorganize food so that it matches understandable organization
exampleData$food <-factor(exampleData$food, 
                          levels = c("vegetables","fruit","chocolate"))
str(exampleData)

#...view the data
head(exampleData,n=15)

#...even though we are most interested in the differences 
#...distributions it is still worth while to examine the data
library(psych)
describeBy(exampleData$pleasure, 
           group=exampleData$food, 
           mat=TRUE, 
           digits=1)

#...test normality
by(exampleData, #...data.frame
   exampleData$food,  #...IV
   function(x)
     shapiro.test(x$pleasure)
)

#...specify contrasts
contrasts(exampleData$food) = contr.poly(3)

withinModel <- aov(pleasure~food+Error(sj/(food)), 
                   data=exampleData)
library(car)
summary(withinModel)

#...the following doesn't work, 
#...within subjects will be type 3 by default
#Anova(withinModel,Type = "III")  

library(ez)
#...run the repeated measures anova
ezModel <- ezANOVA(exampleData, # data frame
                   dv = .(pleasure), #list of dependent variables 
                   wid=.(sj),  #list of within id 
                   within=.(food), #list of independent variables
                   detailed=TRUE, #gives the sphericy
                   type=3)  #returns type 3 - good habits

#...view the output from the anova
ezModel

#...follow up tests
pairwise.t.test(exampleData$pleasure, 
                exampleData$food, 
                paired=TRUE,
                p.adjust.method = "bonferroni")

#...convert W statistic returned by Mauchley's test into Chi Square
k = 3  #Number of Groups
n = 5  #Number of Subjects per condition
d = 1-((2*((k-1)^2)+(k-1)+2)/(6*(k-1)*n-1))


W = ezModel[[2]][2] # or enter the value by hand
chi = -(n-1)*d*log(W)
df = (k*(k-1)/2)-1

paste("Mauchley's test is X(",df,
      ") = ",
      round(chi,2), 
      ", p = ", 
      round(ezModel[[2]][1,3],4),sep="")
