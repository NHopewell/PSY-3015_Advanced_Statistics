###########################################
# CHECK THE ABILITY OF OUR STATISTICAL MODEL 
# TO MAKE PREDICTIONS WHEN ASSUMPTIONS ARE 
#  ****VALID****
###########################################


# Prove the N => 30 rule of the central limit theorem
# Here we will sample from exponential functions and graph the sampling distribution. 
# We will sample 30 times and plot it, it should be normal despite sampling from an exponential function.
# The exponential function is the furthest from normal.
# This shows that 30 samples taken even from an exponential function will have an underlying normal dist
#  as its sampling distribution. 

# If we changed this to sample 10 times, the sampling distribution wouldnt be normal.
# See notes within the code to change type of distribution...
# An ex gaussian distribution is a normal distribution with an exponential tail..
#   In the code this is shows when he has rnorm()+rexp()


## DO NOT FUCK WITH par() just adjust your plot window

number = 30
count = 0
par(mfrow=c(5,5))
for(i in 1:250){
  #outcome = rnorm(n=number, mean=100, sd = 15 )  #NORMAL DISTRIBUTION
  #outcome = rnorm(n=number, mean=100, sd = 15 )+rexp(n=10, rate = .020)  #NORMAL + EXPONENTIAL(ex Gaussian)
  outcome = rexp(n=number, rate = .020) #EXPONENTIAL
  #outcome = runif(n=number,min=0, max=200)  #UNIFORM DISTRIBUTION
  if(count<1){
    sampleDist=mean(outcome)
    }
  else{
    sampleDist[i]=mean(outcome)
  }
  count=count+1
  hist(outcome)
}

par(mfrow=c(1,1))
hist(sampleDist)


# >>>>> It is very important to assess normality VISUALLY!!! <<<<


#########################################
#########################################
#  USE HISTOGRAMS TO EXAMINE NORMALITY
#########################################
#########################################

# Want to overlay a normal distribution over your historgram and assess normality. 
#  Check fit. 

number = 100#16*3
#par(mfrow=c(round(sqrt(number/2),0),round(sqrt(number/2),0)))
par(mfrow=c(4,4))
for(i in 1:number){
  sj = seq(1,number,1)
  #outcome = rnorm(n=number, mean=100, sd = 15 )
  outcome = rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  outcome = rexp(n=number, rate = .020)
  outcome = runif(n=number,min=0, max=200)
  tempData = data.frame(sj,outcome)
  hist(tempData$outcome, prob=TRUE, ylim=c(0,.025))
  curve(dnorm(x, mean(tempData$outcome), sd(tempData$outcome)), 
        add=TRUE, 
        col="darkblue", lwd=2)
  
}


## >> IF YOU GET ERROR "Error in plot.new() : figure margins too large" change the 'par' values

#########################################
#########################################
#  USE QQPLOTS TO EXAMINE NORMALITY
#########################################
#########################################

# You want your data to fall on the diagnal line if it was perfectly normally distributed.

# See andy field textbook to evaluate. 
# plot different types

par(mfrow=c(3,3))
for(i in 1:9){
  sj = seq(1,number,1)
  outcome = rnorm(n=number, mean=100, sd = 15 )
  outcome = rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  outcome = rexp(n=number, rate = .020)
  outcome = runif(n=number,min=0, max=200)
  tempData = data.frame(sj,outcome)
  qqnorm(tempData$outcome)
  qqline(tempData$outcome,
        col="darkblue", lwd=2)
}

#########################################
#########################################
#  COMPARE HISTOGRAMS AND QQPLOTS TO EXAMINE NORMALITY
#########################################
#########################################

# each row is plotting THE SAME data so you can compare side by side

# You can simply change the code with the distriutions in the comments to plot different dists.
#  Can literally just take out the comments and make the code you dont want a comment. 

par(mfrow=c(3,2))
for(i in 1:3){
  sj = seq(1,number,1)
  #outcome = rnorm(n=number, mean=100, sd = 15 )
  outcome = rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  #outcome = rexp(n=number, rate = .020)
  #outcome = runif(n=number,min=0, max=200)
  tempData = data.frame(sj,outcome)
  qqnorm(tempData$outcome)
  qqline(tempData$outcome,
         col="darkblue", lwd=2)
  hist(tempData$outcome, prob=TRUE, ylim=c(0,.02))
  curve(dnorm(x, mean(tempData$outcome), sd(tempData$outcome)), 
        add=TRUE, 
        col="darkblue", lwd=2)
}

install.packages("pastec")
library(pastecs)
a = c(5,9,3,1,1)
skew(a)
stat.desc(a,norm=TRUE)



#########################################
#########################################
#  COMPARE HISTOGRAMS, QQPLOTS and SHAPIRO-WILKS
#  TO EXAMINE NORMALITY
#########################################
#########################################

#for(i in 1:3) just means that you are making 3 plots.
# Look at the p value for the sharpiro test!  If it is less than
#  .05 then its saying the distribution is significantly different
#  from a normal distribution thus the distribution is not normal.
# But remember to be veryyy cautious about this value, see lecture slide
#  notes that I added as sticky notes. 
#  Remember, its making assumptions about sampling distribution not sample. 


number <- 20
par(mfrow=c(3,2))
for(i in 1:3){
  sj <- seq(1,number,1)
  outcome <- rnorm(n=number, mean=100, sd = 15 )
  #outcome <- rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  #outcome <- rexp(n=number, rate = .020)
  #outcome <- runif(n=number,min=0, max=200)
  tempData <- data.frame(sj,outcome)
  shapiroValu<-shapiro.test(tempData$outcome)
  
  qqnorm(tempData$outcome, 
         main=paste("W = ",
                    as.character(round(shapiroValue[1]$statistic,digits=3)),
                    ",  p = ", 
                    as.character(round(shapiroValue[2]$p.value,digits=3))))
  qqline(tempData$outcome,
         col="darkblue", lwd=2)
  hist(tempData$outcome, 
       prob=TRUE, 
       ylim=c(0,.04),
       main=paste("W = ",
                  as.character(round(shapiroValue[1]$statistic,digits=3)),
                  ",  p = ", 
                  as.character(round(shapiroValue[2]$p.value,digits=3))))
  curve(dnorm(x, mean(tempData$outcome), sd(tempData$outcome)), 
        add=TRUE, 
        col="darkblue", lwd=2)
}

skew(tempData$outcome)


#########################################
#########################################
#  USE HISTOGRAMS AND BOXPLOTS 
#  TO EXAMINE HOMOGENEITY OF VARIANCE
#########################################
#########################################

# NOTICE we are chaning the sd in the code
#  Because we are now talking about homogenity of variance!! not normality
# You are trying to plot the data from two distributions and compare them
# Histograms and boxplots are great for this. Trying to see if the variability
# is even.

# so below we are comparing distributions with different distributions
# can also compare different types of distributions - see comment code

#  SEE the plots done in this weeks workshop!! we get transparency and
#  and can see overlap!


number <- 100
par(mfrow=c(2,2))
for(i in 1:4){
  sj = seq(1,number,1)
  outcome1 <- rnorm(n=number, mean=100, sd = 5 )
  outcome2 <- rnorm(n=number, mean=140, sd = 10 )
  #outcome <- rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  #outcome <- rexp(n=number, rate = .020)
  #outcome <- runif(n=number,min=0, max=200)
  tempData <- data.frame(sj,outcome1,outcome2)
  hist(tempData$outcome1, prob=TRUE,col='blue',xlim=c(50,200),ylim=c(0,.08),alpha=.04)
  curve(dnorm(x, mean(tempData$outcome1), sd(tempData$outcome1)), 
        add=TRUE, 
        col="darkblue", lwd=2)
  hist(tempData$outcome2, prob=TRUE,col='red', add=T)
    
    curve(dnorm(x, mean(tempData$outcome2), sd(tempData$outcome2)), 
          add=TRUE, 
          col="darkred", lwd=2)
}



number <- 100
par(mfrow<- c(2,2))
for(i in 1:4){
  sj <- seq(1,number,1)
  outcome1 <- rnorm(n=number, mean=100, sd = 5 )
  outcome2 <- rnorm(n=number, mean=120, sd = 10 )
  #outcome <- rnorm(n=number, mean=100, sd = 15 )+rexp(n=number, rate = .020)
  #outcome <- rexp(n=number, rate = .020)
  #outcome <- runif(n=number,min=0, max=200)
  tempData <-  data.frame(sj,outcome1,outcome2)
  boxplot(tempData[,c("outcome1","outcome2")])
}

a <- c(0,
      1,
      3,
      4,
      4,
      4,
      5,
      7,
      7,
      8,
      2,
      3,
      4,
      5,
      6,
      10,
      11,
      12,
      12,
      14)

b<- c(rep(1,10),rep(2,10))

leveneTest(a,b)


#  SEE SLIDE for tests - levene test is the best one, most used.
#  For homogeneity of variance,we use these tests a lot more
#  than tests for normality. 

# p value of less than ;05 = variances are not equal, thus, HoV violated
# p value of greater than .05 = variances are equal

# F-test is an easy test to calculate - see slide from this week. 


 