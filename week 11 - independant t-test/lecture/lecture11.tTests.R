
#CREATE DATA TO SHOW THAT WE HAVE SEEN T-TESTS BEFORE
setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture11.independent.t.tests")

x = seq(1,10,1)
r = runif(10,0,1)
y = (r*x)
tempData= data.frame(x,y)
#View(tempData)

#  In correlation!!!
cor.test(x,y)

#  In regression!!!
tempReg <- lm(y~x, data=tempData)
summary(tempReg)

#  create a sample data set
x = c(-3,7,7,4,3)

t.test(x)


#  create a sample data set.
sj = seq(1,10,1)
movie <- c(rep("T1",5),rep("T2",5))
rating<-c(3,6,4,9,4,3,8,2,2,6)

movieData = data.frame(sj,movie,rating)
head(movieData,16)

library(psych)

describeBy(movieData$rating,movieData$movie,mat=TRUE)
# View(movieData)
t.test(rating~movie,data=movieData, 
       alternative = "two.sided",
       mu=0,
       paired = FALSE,
       var.equal=TRUE)


#WILCOXON RANK SUM TEST
#MANN-WHITNEY TEST
group = c(rep(1,8),rep(2,8))
score = c(411,901,849,98,816,976,946,1006,182,387,943,538,974,1024,235,770)
exampleData = data.frame(group,score)
exampleData$group = factor(exampleData$group)
head(exampleData,16)

exampleData$rank = rank(exampleData$score, ties.method = "average")
head(exampleData,16)

wilcox.test(score ~ group,
            data = exampleData,
            exact = FALSE,
            correct = FALSE)








