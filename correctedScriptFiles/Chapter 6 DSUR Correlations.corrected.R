#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 6 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)
setwd("~/Documents/Teaching/2015-2016/Statistics/Data files")

######Initiate packages

#If you don't have Hmisc installed then use:
install.packages("Hmisc")
install.packages("polycor")
install.packages("ggm")

#Initiate packages
library(Hmisc)
library(ggplot2)
library(boot)
library(polycor)
library(ggm)
library(Rcmdr)


#--------Entering data----------

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
advertData<-data.frame(adverts, packets)


cov(advertData)  #CREATE A VARIANCE / COVARIANCE MATRIX
var(adverts)  #CALCULATE THE VARIANCE
var(packets)  #CALCULATE THE VARIANCE


########################################
#--------Self Help Task----------
########################################
scatter<-ggplot(advertData, aes(adverts, packets))
scatter + geom_point()

scatter + geom_point(size = 3) + 
  labs(x = "Adverts", y = "Packets") + #ADD LABELS
  scale_y_continuous(limits=c(0, 15), breaks=0:15) + #SET THE LIMITS AND BREAK PROPERTIES OF THE Y AXIS
  scale_x_continuous(limits=c(0, 9), breaks=0:9)  #SET THE LIMITS AND BREAK PROPERTIES OF THE X AXIS

####COULD ALSO USE
#scale_x_log10(...)
#scale_y_log10(...)
#scale_x_reverse(...)
#scale_y_reverse(...)
#scale_x_sqrt(...)
#scale_y_sqrt(...)



########################################
#-----Dealing with misisng cases
########################################
adverts<-c(5,4,4,6,8)
packetsNA<-c(8,9,10,NA,15)
age<-c(5, 12, 16, 9, 14)
advertNA<-data.frame(adverts, packetsNA, age)
View(advertNA)

#calculate corelations using the cor() function
#will only work on matricies of numeric data
cor(advertNA, use = "everything",  method = "pearson")  #WILL INDICATE THAT CORRELATIONS WITH NA VALUES COULD NOT BE COMPUTED
cor(advertNA, use = "complete.obs",  method = "kendall")  #WILL RUN THE CORRELATION WITH THE MISSING VALUES REMOVED


########################################
#--------Pearson r----------
########################################
#cor(x,y, use = "everything", method = "correlation type")  #RETURNS THE CORRELATION MATRIX/ VALUES
#cor.test(x,y, alternative = "string", method = "correlation type", conf.level = 0.95) RETURNS THE PVALUES

examData = read.delim("Exam Anxiety.dat",  header = TRUE)
View(examData)

cor(examData, use = "complete.obs", method = "pearson")  # WILL NOT RUN BECAUSE SOME COLUMNS ARE NOT NUMERIC
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson') #USE 2 OF THE NUMERIC COLUMNS

#create the correlation matrix for the relevant variables in two steps
examData2 <- examData[, c("Exam", "Anxiety", "Revise")] #CREATE A NEW VARIABLE WITH ONLY THE COLUMNS OF INTEREST
cor(examData2) #RUN THE ANALYSIS ON THE NEW DATA FRAME OBJECT

#create the correlation matrix for the revevant variables in one step
cor(examData[, c("Exam", "Anxiety", "Revise")]) #

#get the variance explained (r squared) then multiply by 100 to make it a percent
cor(examData2)^2 * 100


examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
Hmisc::rcorr(examMatrix)
Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

#run the correlations again.
#this method returns the correspinding t value, df, confidence interval, and p-value
cor.test(examData$Anxiety, examData$Exam)
cor.test(examData$Revise, examData$Exam)
cor.test(examData$Anxiety, examData$Revise)


########################################
#--------Spearman's Rho----------
# Section 6.5.5, p. 223
########################################
liarData = read.delim("The Biggest Liar.dat",  header = TRUE)
View(liarData)


liarData = read.delim(file.choose(),  header = TRUE)  #if this works, it will bring up a navigation window

cor(liarData$Position, liarData$Creativity, method = "spearman")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman") # will return an email because of the tied values

liarMatrix<-as.matrix(liarData[, c("Position", "Creativity")])
cor(liarMatrix)  #returns
rcorr(liarMatrix)  #returns a 1 for me.
Hmisc::rcorr(liarMatrix)  #specifying the package to use, seems to fix the problem.

########################################
#--------Kendall's Tau----------
########################################
cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "pearson")

cor(advertData$adverts, advertData$packets, method = "pearson")
cor.test(advertData$adverts, advertData$packets, alternative = "greater", method = "kendall")  # will get an error


########################################
#--------Self Test----------
# page 224????
# this doesn't seem to line up with anything
########################################
adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
cor.test(adverts, packets, method="spearman")

########################################
#--------Bootstrapping----------
########################################

bootTau<-function(liarData,i)cor(liarData$Position[i], liarData$Creativity[i], use = "complete.obs", method = "kendall")
boot_kendall<-boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall)
boot.ci(boot_kendall, 0.99)

#--------Self Test----------

bootR<-function(examData2,i) cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)


bootR<-function(examData2,i) cor(examData2$Revise[i], examData2$Anxiety[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)

bootR<-function(examData2,i) cor(examData2$Revise[i], examData2$Exam[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)

bootRho<-function(examData2,i) cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs", method = "spearman")
boot_spearman<-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)


bootRho<-function(examData2,i) cor(examData2$Revise[i], examData2$Anxiety[i], use = "complete.obs", method = "spearman")
boot_spearman <-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)

bootRho<-function(examData2,i) cor(examData2$Revise[i], examData2$Exam[i], use = "complete.obs", method = "spearman")
boot_spearman <-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)


########################################
#-------Point Biserial-----
# section 6.5.8, page 229
########################################
catData = read.csv("pbcorr.csv",  header = TRUE)
View(catData)

#  CALCULATING THE POINT BI-SERIAL
#  USE THE STANDARD PEARSON CORRELATION BUT HAVE YOUR DICHOTOMOUS VARIABLE 
#  CODED AS 1 AND 0 
cor.test(catData$time, catData$gender)  #yields a positive point biserial correlation (by converting to 1s and 0s)
cor.test(catData$time, catData$recode)  #yields a negative correlation
catFrequencies<-table(catData$gender)   #returns a frequency table
prop.table(catFrequencies)  #turns the frequency table (count) into proporitions


polyserial(catData$time, catData$gender)  #calculate the biserial correlation
########################################
#-------Partial-----
#-------Section 6.6
########################################

#uses library(ggm)
#there is another package library(ppcor) that does even more stuff
maleExam<-examData[examData$Gender == "Male", c("Exam", "Anxiety")]  #create two data sets
femaleExam<-examData[examData$Gender == "Female", c("Exam", "Anxiety")]
cor(maleExam)
cor(femaleExam)

# pcor(c("x","y","control1","control2" etc), var(dataframe))
#calculate the partial correlation btwn exam and anxiety while controling for revise
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2)) 

pc #return the correlation
pc^2 #convert into variance explained

# return the p value 
# pcor.test(correlation,number of control variables, sample size)
pcor.test(pc, 1, 103) 
pcor.test(pc,1, dim(examData2)[1])  # dim returns the nRows and nCols.  [1] asks for the rows

########################################
#-------Differences between independent rs-----
########################################

#this is a function that allows you to compare to R values using z score conversion
zdifference<-function(r1, r2, n1, n2)   #create a function called zdifference that takes 4 arguments 
{zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
	p <-1 - pnorm(abs(zd))
	print(paste("Z Difference: ", zd))
	print(paste("One-Tailed P-Value: ", p))
	}
	
zdifference(-0.506, -0.381, 52, 51)

########################################
#-------Differences between dependent rs-----
########################################
#this is a function that allows you to compare to R values using t score conversion
tdifference<-function(rxy, rxz, rzy, n) #create a function called tdifference that takes 4 arguments
{	df<-n-3
	td<-(rxy-rzy)*sqrt((df*(1 + rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
	p <-pt(td, df)
	print(paste("t Difference: ", td))
	print(paste("One-Tailed P-Value: ", p))
	}
	
tdifference(-0.441, -0.709, 0.397, 103)

########################################
#-------labcoat leni-----
########################################
#Load the data:
personalityData = read.delim("Chamorro-Premuzic.dat",  header = TRUE)
View(personalityData)
#Create a matrix from the personalityData dataframe:
personalityMatrix<-as.matrix(personalityData[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")])

#run the correlation analysis:
rcorr(personalityMatrix)

#or convert the dataframe into a matrix and run the correlation analysis in one:
rcorr(as.matrix(personalityData[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")]))

########################################
#-------------------------Smart Alex Task 1
########################################
#Load the data:

essayData = read.delim("EssayMarks.dat",  header = TRUE)

#Create a plot object called scatter:
scatter<-ggplot(essayData, aes(hours, essay))

#Add labels and with to your scatterplot:
scatter + geom_point(size = 3) + labs(x = "Hours Spent on Essay", y = "Essay Mark (%)") 

#Do a Shapiro-Wilks test to see whether the data are normal:

shapiro.test(essayData$essay)
shapiro.test(essayData$hours)

#Because the shapiro-Wilks tests were both non-sig we can use pearsons correltation:
cor.test(essayData$essay, essayData$hours, alternative = "greater", method = "pearson")

#correlation of hours spent on essay and grade:

essayData$grade<-factor(essayData$grade, levels = c("First Class","Upper Second Class", "Lower Second Class", "Third Class"))

cor.test(essayData$hours, as.numeric(essayData$grade), alternative = "less", method = "kendall")
cor.test(essayData$hours, as.numeric(essayData$grade), alternative = "less", method = "spearman")

########################################
#Smart Alex Task 2-------------
########################################
#load the data:

chickFlick = read.delim("ChickFlick.dat", header = TRUE)
View(chickFlick)
#conduct two point-biserial correlations:

cor.test(chickFlick$gender, chickFlick$arousal) # won't work because gender is not numeric

cor.test(chickFlick$film, chickFlick$arousal) # won't work because gender is not numeric

#solution - create a new variable where gender is coded as numbers
chickFlick$genderNumber[chickFlick$gender=="Female"]<-0  #set gender number to 0 when gender == female
chickFlick$genderNumber[chickFlick$gender=="Male"]<-1    #set genderNumber to 1 when gender == male
cor.test(chickFlick$genderNumber, chickFlick$arousal) # won't work because gender is not numeric

########################################
#Smart Alex Task 3--------
########################################
#load in the data

gradesData = read.csv("grades.csv", header = TRUE)

#Conduct a Spearman correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "spearman")

#conduct a Kendall correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "kendall")

########################################
#-------R Souls Tip Writing Functions----
########################################
nameofFunction<-function(inputObject1, inputObject2, etc.)
{
	a set of commands that do things to the input objects
	a set of commands that specify the output of the function
}


meanOfVariable<-function(variable)
{
	mean<-sum(variable)/length(variable)
	cat("Mean = ", mean)
	
}

meanOfVariable<-function(HarryTheHungyHippo)
{
	mean<-sum(HarryTheHungyHippo)/length(HarryTheHungyHippo)
    cat("Mean = ", mean)
}


lecturerFriends = c(1,2,3,3,4)

meanOfVariable(lecturerFriends)



