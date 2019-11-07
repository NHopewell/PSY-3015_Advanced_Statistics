#...dependent sample t-test
#...create data

id = paste("pid",seq(1,10,1),sep="")
sunday= c(16,15,20,15,16,13,14,19,18,18)
wednesday=c(5,6,30,8,9,7,6,17,3,10)

#...create the data.frame()
wideData = data.frame(id,sunday,wednesday)

#...visually inspect the data
head(wideData)

#...check coding of variables
str(wideData)

#...EXAMINE ASSUMPTIONS
#...USE WIDEFORMAT TO EXAMINE THE DISTRIBUTION OF DIFFERENCES

#...calculate the difference scores
wideData$difference = wideData$sunday-wideData$wednesday

#...test normality
shapiro.test(wideData$difference)

#calculate t-test
t.test(sunday,wednesday, 
       data=wideData,
       paired = TRUE,
       var.equal = FALSE)


#...BUT WE VIOLATED NORMALITY
#...let's try and non-parametric test


#...illustrate ranking 
#...note the statistical tests will do this for us
wideData$rank = rank(abs(wideData$difference), 
                     ties.method = "average")
head(wideData)


#...need the data in long format
library(reshape2)
longData = melt(data=wideData,
                id.vars = "id",
                measure.vars = c("sunday","wednesday"),
                variable.name = "condition",
                value.name = "score")
head(longData)

#...run the test
wilcox.test(score ~ condition,
            data = longData,
            exact = FALSE,
            correct = TRUE,
            paired = TRUE)



#...long format
id = rep(paste("pid",seq(1,10,1),sep=""),2)
condition = c(rep("sunday",10),rep("wednesday",10))
score = c(16,15,20,15,16,13,14,19,18,18,5,6,30,8,9,7,6,17,3,10)


#...create the data.frame()
longData = data.frame(id,condition,score)
head(longData)

#...use the longData to explore
#...get descriptives
library(psych)
describeBy(longData$score, longData$condition, mat=TRUE)

#...examine distributions
par(mfrow=c(1,2))
by(longData, longData$condition, function(x)
  hist(x$score, 
       xlim=c(min(longData$score-10),max(longData$score+10)),
       main = unique(x$group)))