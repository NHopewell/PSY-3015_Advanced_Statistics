#...create data
sj = paste("p",c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,
                 6,6,6,7,7,7,8,8,8,9,9,9,10,10,10),
           sep = "")
sex = c(rep("male",15),
        rep("female",15))
food =c("vegetables", "fruit",  "chocolate",	
        "vegetables", "fruit",  "chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate",	
        "vegetables",	"fruit",	"chocolate")
pleasure = c(2,4,9,1,5,9,2,7,9,3,5,7,2,4,6,
             4,8,9,5,4,6,4,9,8,10,6,8,7,8,9)

#...combined the data into a data frame
exampleData = data.frame(sj, sex,food, pleasure)
head(exampleData)

#...are the variables coded properly?
str(exampleData)

#...reclassify the variables as needed
#...here we just re-order the levels
exampleData$food <-factor(exampleData$food, 
                          levels = c("vegetables","fruit","chocolate"))
exampleData$sex <-factor(exampleData$sex, 
                         levels = c("female","male"))


#...calculate descriptive statistics
library(psych)
describeBy(exampleData$pleasure, 
           group=list(exampleData$food,exampleData$sex), 
           mat=TRUE, 
           digits=1)

#...create A density plot...fancy histogram
library(ggplot2)
densityPlot <- ggplot(exampleData, aes(pleasure, colour=food))
densityPlot + 
  geom_density(aes(pleasure,fill = food), 
               alpha = 0.5) + 
  labs(x = "pleasure", 
       y = "Density Estimate")+
  facet_grid(~sex)

#...or the normal way
par(mfrow=c(2,3))
by(exampleData, #...data.frame
   list(exampleData$food,exampleData$sex),  #...IV
   function(x)
     hist(x$pleasure, #...hist
          main = paste(unique(x$sex),unique(x$food))) #titles
)


#test homogeneity of variance
library(car)
leveneTest(exampleData$pleasure, #...DV
           interaction(exampleData$sex,exampleData$food), #...IVs
           center=median)  #...center

#...test normality
by(exampleData, #...data frame to split
   list(exampleData$sex,exampleData$food), #IVs
   function(x) 
     shapiro.test(x$pleasure))  #...normality test

#...create orthogonal contrasts
contrasts(exampleData$sex)<-contr.poly(2)
contrasts(exampleData$food)<-contr.poly(3)

#...run the ANOVA using the ezANOVA function
library(ez)
ezModel <- ezANOVA(exampleData, #...dataframe
                   dv = .(pleasure), #.. dependent variable
                   wid=.(sj),  #...subject variable
                   within=.(food),  #...within IV
                   between=.(sex),  #...betweenIV
                   detailed=TRUE,   #...include mauchly test
                   type=3,  #....type 3 sums of squares
                   return_aov = TRUE) #...return aov object for effect size       

ezModel

#...NEED TO ADD EFFECT SIZE
#...same as SPSS
library(sjstats)
eta_sq(ezModel$aov$`sj`,partial=TRUE)
eta_sq(ezModel$aov$`sj:food`,partial=TRUE)


#...PLOT THE MAIN EFFECT OF SEX
bar <- ggplot(exampleData, aes(sex, pleasure))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(sex, pleasure),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + 
  labs(x = "Sex", y = "Pleasure") +
  ylim(0,15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


#...PLOT THE MAIN EFFECT OF FOOD
bar <- ggplot(exampleData, aes(food, pleasure))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "White", 
                   colour = "Black") + 
  stat_summary(aes(food, pleasure),
               fun.data = mean_cl_normal, 
               geom = "pointrange") + 
  labs(x = "Food", y = "Pleasure") +
  ylim(0,15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#...PLOT THE INTERACTION OF FOOD AND SEX
bar <- ggplot(exampleData, aes(food, pleasure, linetype=sex))
bar + stat_summary(fun.y = mean, 
                   geom = "line", 
                   aes(group=sex), 
                   colour = "Black",
                   position=position_dodge(width=.1)) + 
  stat_summary(aes(food, pleasure, group=sex),
               fun.data = mean_cl_normal, 
               geom = "errorbar",
               width =.2, 
               position=position_dodge(width=.1)) + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Food", y = "Pleasure") +
  ylim(0,15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))

#...can plot using the ezPlot function
ezPlot(exampleData, #...dataframe
       dv = .(pleasure), #.. dependent variable
       wid=.(sj),  #...subject variable
       within=.(food),  #...within IV
       between=.(sex),  #...betweenIV
       split=.(sex),    #...split the lines
       x = .(food),     #...put food on the x axis
       do_lines=TRUE,   #...include mauchly test
       type=3)          #....type 3 sums of squares

#...SIMPLE EFFECTS
#...sex at food
vegData = exampleData[exampleData$food=="vegetables",]
fruitData = exampleData[exampleData$food=="fruit",]
chocoData = exampleData[exampleData$food=="chocolate",]


#...sex @ vegetable
vegModel = aov(pleasure~sex, data=vegData)
summary(vegModel)

#...sex @ fruit
fruitModel = aov(pleasure~sex, data=fruitData)
summary(fruitModel)

#...sex @ chocolate
chocoModel = aov(pleasure~sex, data=chocoData)
summary(chocoModel)

#...SIMPLE EFFECTS
#...food at sex
femaleData = exampleData[exampleData$sex=="female",]
maleData = exampleData[exampleData$sex=="male",]

#...food @ female
femaleModel = aov(pleasure~food +Error(sj/(food)), data=femaleData)
summary(femaleModel)

#...food @ male
maleModel = aov(pleasure~food + Error(sj/(food)), data=maleData)
summary(maleModel)


#...post hoc comparisons
pairwise.t.test(exampleData$pleasure, 
                interaction(exampleData$food,exampleData$sex), 
                paired=TRUE, 
                p.adjust.method = "bonferroni")

