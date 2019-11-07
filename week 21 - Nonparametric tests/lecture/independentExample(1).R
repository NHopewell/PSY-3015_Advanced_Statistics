#... create data

group = paste("group0",c(rep(1,8),rep(2,8)),sep="")
score = c(749,901,849,698,816,1076,966,916,
          182,287,859,438,974,924,235,770)

#...create the data.frame()
exampleData = data.frame(group,score)

#...illustrate ranking 
#...note the statistical tests will do this for us
exampleData$rank = rank(exampleData$score, 
                        ties.method = "average")


#...visually inspect the data
head(exampleData)

#...check coding of variables
str(exampleData)

#...get descriptives
library(psych)
describeBy(exampleData$score, exampleData$group, mat=TRUE)
#...examine distributions
par(mfrow=c(1,2))
by(exampleData, exampleData$group, function(x)
  hist(x$score, 
       xlim=c(min(exampleData$score),
              max(exampleData$score+100)),
       main = unique(x$group)))

#...test normality
by(exampleData, 
   exampleData$group, 
   function(x)
     shapiro.test(x$score))

#...test homogeneity of variance
library(car)
leveneTest(score ~ group, #...formula
           data = exampleData,   #...dataframe
           center = median)  #...center method

t.test(score~group, #...formula
       data=exampleData, #...dataframe
       paired = FALSE,   #dependent vs. independent
       var.equal = FALSE)  #do not used pooled var.


wilcox.test(score ~ group, #...formula
            data = exampleData, #...dataframe
            paired =FALSE,  # independent t-test
            exact = FALSE,  # should an exact p value be calcualted
            correct = TRUE)  #...apply continuity correction
