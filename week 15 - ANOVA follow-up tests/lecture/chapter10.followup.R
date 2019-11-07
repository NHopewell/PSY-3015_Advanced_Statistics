
#...CREATE DATA
#...Examine math anxiety as a function of declared major at university
#...Use a random sample with a mean and std devation.
#...Unique ID for each subject


nSize=15   #set N per group
nGroup = 4  #set the number of groups
major = c(rep("history",nSize),  #create IV
         rep("psychology",nSize),
         rep("biology",nSize), 
         rep("mathematics",nSize))
a = rnorm(15, 50, 10)
anxiety = c(a,a+5,a+10,a+15)
anxiety = c(rnorm(nSize,mean=93,sd=15)-rexp(n=nSize,rate=.2), #create dv
          rnorm(nSize, mean=72, sd= 25),
          rnorm(nSize,mean=56, sd=25), 
          rnorm(nSize, mean=5, sd=15)+rexp(n=nSize,rate=.2))
sjNum =paste(rep("sj",nSize*nGroup),  #create the subject ID
             seq(1,nSize*nGroup,1),
             sep="") 

#...create the data.frame 
anxietyData = data.frame(sjNum,
                         major,
                         anxiety =round(anxiety,0))

#...correct impossible scores (not the best way, but it works)
anxietyData$anxiety[anxietyData$anxiety>100] = 100
anxietyData$anxiety[anxietyData$anxiety<1] = 1 

#paste(anxietyData$anxiety, sep=" ", collapse=",")

#...values from class example
anxietyData$anxiety = c(77,48,84,86,91,87,100,98,78,80,100,72,80,100,
            56,63,57,96,46,49,7,45,81,100,100,91,81,100,
            100,1,58,38,38,20,71,59,55,33,43,86,92,1,47,
            87,66,33,1,1,1,19,14,1,2,43,1,10,23,1,25,14)


head(anxietyData,n=20)


#...examine variable coding
str(anxietyData)

#...need to override default order
anxietyData$major = factor(anxietyData$major, 
                           levels = c("history",
                                      "psychology",
                                      "biology",
                                      "mathematics"))  

#...check to make sure recoding worked
str(anxietyData)


#...VISUALLY INSPECT THE DATA
#...create histograms
par(mfrow=c(2,2))
by(anxietyData,list(anxietyData$major), function(y){
  hist(y$anxiety, 
       main = paste(unique(y$major)),
       xlim=c(min(anxietyData$anxiety),max(anxietyData$anxiety)))
})

#...check basic statistics
library(psych)
describeBy(anxietyData$anxiety,list(anxietyData$major),mat=TRUE, digits=2)
summary(anxietyData)

#...RUN THE ANOVA
anovaModel = aov(anxiety~major, data=anxietyData)
summary(anovaModel)


#...examine normality
par(mfrow=c(1,1))
plot(anovaModel)

#...get residuals
anxietyData$predicted = predict(anovaModel)
anxietyData$residuals = resid(anovaModel)
anxietyData$sresiduals = rstandard(anovaModel)

#...examine normality of residuals
shapiro.test(anxietyData$residuals)

#...test for outliers
anxietyData[abs(anxietyData$sresiduals)>2,]



#...examine homogeneity of variance
library(car)
leveneTest(residuals~major, data=anxietyData, center="median")
#oh well, nobody is perfect

#...we can see this in a boxplot
par(mfrow=c(1,1))
boxplot(residuals~major, data=anxietyData)


#...LET'S GRAPH OUR MEANS
#...this is from the workshop 02 script
library(ggplot2)
bar <- ggplot(anxietyData, aes(major, anxiety))
bar + 
  #set the y limits from 0 - 100
  scale_y_continuous(limits = c(0, 100))+  
  
  ### Plot the means in a bar graph
  stat_summary(fun.y = mean, 
               geom = "bar", 
               colour="black",  #set the lines to black
               fill="gray")+ #fill the bars with gray
  
  #gets the summary statistics 95%confidnence interval
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange") + #plots a line with a value for a point
  theme(axis.text=element_text(size=16),  #increase the font size of axis labels
        axis.title=element_text(size=20,face="bold"))+  #increase the font and bold axis titles
  labs(x = "Declared Major", y = "Mean Anxiety") 

#...EXAMINE PLANNED COMPARISONS
#...Below is some code that you can use to calculate
#...the adjusted p.values for the different methods
#...used in lecture.  Make sure that you can calculate
#...the values by hand as well.

alpha = .05  #set your family-wise error
numComp = 3  #the number of comparisons
numDecimals = 4  #the number of decimal places

#Bonferroni Method
bonferroniAlpha = alpha/numComp
print(paste("Bonferroni Alpha is: ", 
            round(bonferroniAlpha,numDecimals)))

#holmAlpha
for(i in 1:numComp){
  holmAlpha = alpha/(numComp+(1-i))
  print(paste("Holm Alpha for comparison ", 
              i, 
              ": is ", 
              round(holmAlpha,numDecimals)))
}

#SidakAlpha
sidakAlpha = 1-(1-alpha)^(1/numComp) 
print(paste("Sidak Alpha is: ", 
            round(sidakAlpha,numDecimals)))

#Bejamini Hochberg Method
for(i in 1:numComp){
  bhAlpha = alpha*(i/numComp)
  print(paste("Alpha for the Bejamini Hochberg comparison ", 
              i, 
              ": is ", 
              round(bhAlpha,numDecimals)))
}


#...LET'S TEST SOME ACTUAL APRIORI HYPOTHESES
#...psychology would not differ from biology!
#...psychology would be lower than history
a=t.test(anxiety~major, 
       data=anxietyData[anxietyData$major =="biology" | anxietyData$major=="psychology",],
       paired = FALSE,
       var.equal=TRUE)

t.test(anxiety~major, 
       data=anxietyData[anxietyData$major =="history" | anxietyData$major=="psychology",],
       paired = FALSE,
       var.equal=TRUE)


#...CREATE YOUR OWN CONTRASTS
#...WE HAVE 4 LEVELS OF MAJOR THEREFORE MAXIMUM 3 CONTRASTS

#...create the 3 contrasts using the concatenate function
contrast1 = c(-1,-1,-1,3)  #math is different from the others
contrast2 = c(2,-1,-1,0)   #History is different than bio sciences
contrast3 = c(0,-1,1,0)    #psych and biology differ

#...add the contrasts to the appropriate variable 
#...in our data frame
contrasts(anxietyData$major) <- cbind(contrast1,
                                      contrast2,
                                      contrast3)

#...look at the variable
contrasts(anxietyData$major)

#...rerun the anova, it will now run it using the contrasts
contrastModel = aov(anxiety~major, data=anxietyData)
summary(contrastModel)

#...examine the contrasts
summary.lm(contrastModel)

#...there are some built in functions for specifying contrasts
#...Not all of them are orthogonal
contr.Helmert(4)#two groups,  independent (orthogonal)
contr.SAS(4)#two groups, not independent (not orthogonal)
contr.sum(4)  #two groups, not independent (not orthogonal)
contr.treatment(4) #two groups, not independent (not orthogonal)


#...TREND ANALYSES
#...use the contr.poly() function
#...the value entered into the contr.poly function is the number 
#...of levels in your IV

contrasts(anxietyData$major) <- contr.poly(4)

contrasts(anxietyData$major)

#...rerun the anova, it will now run it using the contrastsx
trendModel = aov(anxiety~major, data=anxietyData)
summary(trendModel)

#...examine the contrasts
summary.lm(trendModel)

#...POST HOC TESTS USING ALPHA CORRECTION METHODS
#...USE THE pairwise.t.test FUNCTION

#...pairwise.t.test(DV, IV, pool.sd,paired, p.adjust.methd)

#...pool.sd = TRUE  #use the sqrt(2*MSE / n) estimate of error
#...pool.sd = FALSE  #use the relevant sample errors
#...paired = FALSE #indpendent samples
#...paired = TRUE #repeated measures
#...p.adjust.method = "none"  # no correction
#...p.adjust.method = "bonferroni"
#...p.adjust.method = "holm"  
#...p.adjust.method = "BH"  # control the false discovery rate


#no alpha correction
comparisonOutput =pairwise.t.test(anxietyData$anxiety, 
                                  anxietyData$major,
                                  pool.sd = TRUE,
                                  paired=FALSE, 
                                  p.adjust.method="none")
comparisonOutput

#bonferonni alpha correction
comparisonOutput =pairwise.t.test(anxietyData$anxiety, 
                                  anxietyData$major, 
                                  pool.sd = TRUE,
                                  paired=FALSE, 
                                  p.adjust.method="bonferroni")
comparisonOutput

#holm alpha correction
comparisonOutput =pairwise.t.test(anxietyData$anxiety, 
                                  anxietyData$major,
                                  pool.sd = TRUE,
                                  paired=FALSE, 
                                  p.adjust.method="holm")
comparisonOutput



#...TUKEY AND DUNNETT POST HOC TESTS USING

library(multcomp)
#...comparisonOutput = glht(anovaModel, linfct=mcp(independentVariable = "posthoc))

comparisonOutput =glht(anovaModel,linfct=mcp(major="Tukey"))
summary(comparisonOutput)


comparisonOutput =glht(anovaModel,linfct=mcp(major="Dunnett"))
summary(comparisonOutput)


comparisonOutput =glht(anovaModel,linfct=mcp(major="Dunnett"), base="psychology")
summary(comparisonOutput)
TukeyHSD(anovaModel)



#...CREATE A DEMONSTRATION OF THE CHANGING CRITICAL VALUES
#...FOR ALL OF THE METHODS DISCUSSED TODAY!

#...STEP 1 get the MSerror and df from the ANOVA
#...number of independent variables
numIV = 1

modelError = summary(anovaModel)[[1]]$'Mean Sq'[numIV+1]
modelDf = summary(anovaModel)[[1]]$'Df'[numIV+1]

#...STEP 2 set parameters
alpha = .05  #family wise errors
numGroups = 4
numComp = 6  #number of pairwise comparisons factorial(numGroups) /(2*factorial(numGroups-1))
numDecimals = 4
n = (modelDf+numGroups)/numGroups

for(i in 1:numComp){
  if(i==1){
    #Bonferroni Method
    bonferroniAlpha = alpha/numComp
    bonferroniTcrit = abs(qt(bonferroniAlpha/2, df = modelDf))
    bonferroniDiff = abs(qt(bonferroniAlpha/2, df = modelDf)* sqrt(2*modelError/n))
    
    #holmAlpha
    holmAlpha = alpha/(numComp+(1-i))
    holmAlphaTcrit = abs(qt(holmAlpha/2, df = modelDf))
    holmAlphaDiff = abs(qt(holmAlpha /2, df = modelDf)* sqrt(2*modelError/n))
    
    #SidakAlpha
    sidakAlpha = 1-(1-alpha)^(1/numComp) 
    sidakAlphaTcrit = abs(qt(sidakAlpha/2, df = modelDf))
    sidakAlphaDiff = abs(qt(sidakAlpha /2, df = modelDf)* sqrt(2*modelError/n)) 
    
    #Bejamini Hochberg Method
    bhAlpha = alpha*(i/numComp)
    bhAlphaTcrit = abs(qt(bhAlpha/2, df = modelDf))
    bhAlphaDiff = abs(qt(bhAlpha /2, df = modelDf)* sqrt(2*modelError/n))
    
    #Tukey HSD
    tukey.Alpha = .95
    tukey.Qcrit  =qtukey(tukey.Alpha, numComp, df =  modelDf)
    tukey.Diff = tukey.Qcrit* sqrt(modelError/n)
  } 
  else{#Bonferroni Method
    bonferroniAlpha[i] = alpha/numComp
    bonferroniTcrit[i] = abs(qt(bonferroniAlpha[i]/2, df = modelDf))
    bonferroniDiff[i] = abs(qt(bonferroniAlpha[i]/2, df = modelDf)* sqrt(2*modelError/n))
    
    #holmAlpha
    holmAlpha[i] = alpha/(numComp+(1-i))
    holmAlphaTcrit[i] = abs(qt(holmAlpha[i]/2, df = modelDf))
    holmAlphaDiff[i] = abs(qt(holmAlpha[i] /2, df = modelDf)* sqrt(2*modelError/n))
    
    #SidakAlpha
    sidakAlpha[i] = 1-(1-alpha)^(1/numComp) 
    sidakAlphaTcrit[i] = abs(qt(sidakAlpha[i]/2, df = modelDf))
    sidakAlphaDiff[i] = abs(qt(sidakAlpha[i] /2, df = modelDf)* sqrt(2*modelError/n))
    
    #Bejamini Hochberg Method
    bhAlpha[i] = alpha*(i/numComp)
    bhAlphaTcrit[i] = abs(qt(bhAlpha[i]/2, df = modelDf))
    bhAlphaDiff[i] = abs(qt(bhAlpha[i] /2, df = modelDf)* sqrt(2*modelError/n))
    
    #Tukey HSD
    tukey.Alpha[i] = .95
    tukey.Qcrit[i]  =qtukey(tukey.Alpha[i], numComp, df =  modelDf)
    tukey.Diff[i] = tukey.Qcrit[i]* sqrt(modelError/n)
  }
}

#OUTPUT EVERYTHING IN A NICE TABLE
valueTable = data.frame(i = seq(1,numComp,1),
                        bf.Alpha=round(bonferroniAlpha,4),
                        bf.Tcrit=round(bonferroniTcrit,3),
                        bf.Diff=round(bonferroniDiff,3),
                        holm.Alpha=round(holmAlpha,4),
                        holm.Tcrit=round(holmAlphaTcrit,3),
                        holm.Diff=round(holmAlphaDiff,3),
                        sidak.Alpha=round(sidakAlpha,4),
                        sidak.Tcrit=round(sidakAlphaTcrit,3),
                        sidak.Diff =round(sidakAlphaDiff,3),
                        bh.Alpha = round(bhAlpha,4),
                        bh.Tcrit =round(bhAlphaTcrit,3),
                        bh.Diff =round(bhAlphaDiff,3),
                        hsd.Alpha = round(tukey.Alpha,3),
                        hsd.Qcrit = round(tukey.Qcrit,3),
                        hsd.Diff = round(tukey.Diff,3)
)
#View(valueTable)

head(valueTable)
