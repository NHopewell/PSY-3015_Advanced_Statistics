#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#######################################
#SET THE WORKING DIRECTORY
#the working directory is the folder on your computer
#that has the data file that you want to open

setwd("~/Documents/Academic/Data/DSU_R/Chapter 04 (Graphs) R")


#######################################
#INSTALL PACKAGES
# R is like a car in that it has a base model and then you have to 
# add packages to get it to look fancy.
# In R many of the advanced (really good looking functions) in in libraries and packages

# The list of packages that you currently have installed can be seen under the
# the packages tab on the right.

#If you don't have a package installed, we will use the RStudio interface
#to install it.

#
# In order to activate a package we use the "library" command
library(ggplot2)  #activate the ggplot2 library (lets you do fancy graphs)
library(reshape2)  #activate the reshape2 library (lets you reorganze your data)
library(plyr)  #activate the plyr library (apply r)


####################################
#--------Quick Tutorial----------
# SECTION 4.4.8  PUTTING IT ALL TOGETHER: A QUICK TUTORIAL
####################################

#LOAD AND VIEW THE DATAFILE
facebookData <- read.delim("FacebookNarcissism.dat",  header = TRUE)
View(facebookData)

# 1 = ID 
# 2 = NPQC_R_TOTAL - SCORE ON A NARCISISIM QUESTIONNAIRE
# 3 = RATING_TYPE (COOLNESS, GLANOUR, FASHION, ATTRACTIVENESS)
# 4 = RATING  score from 1 to 5

# plot narcism score by rating
graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
graph + geom_point() + 
  labs(title = "geom_point()")  #NOTE THE CHANGE of OPTS to LABS


#CHANGE THE SHAPE OF THE POINTS AND GRAPH TITLE
graph + geom_point(shape = 17) + 
  labs(title = "geom_point(shape = 17)") #change opts to labs


#CHANGE THE POINT SIZE AND THE TITLE
graph + geom_point(size = 6) + 
  labs(title = "geom_point(size = 6)")  #change opts to labs

#Change the colour of the points based on rating type
graph + geom_point(aes(colour = Rating_Type)) + 
  labs(title = "geom_point(aes(colour = Rating_Type))")

#CHANGE THE COLOUR BY RATING TYPE AND SHIFT THE POSITION OF THE POINTS USING JITTER SO THAT THEY ARE VISIBLE 
graph + geom_point(aes(colour = Rating_Type), position = "jitter") + 
  labs(title = "geom_point(aes(colour = Rating_Type), position = jitter)")

#CHANGE THE SHAPE BY RATING TYPE AND SHIFT THE POSITION OF THE POINTS USING JITTER
graph + geom_point(aes(shape = Rating_Type), position = "jitter") + 
  labs(title = "geom_point(aes(shape = Rating_Type), position = jitter)")


####################################
#--------Scatterplots----------
# SECTION 4.5.1  SIMPLE SCATTERPLOT
####################################

#LOAD A NEW DATA FILE
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
View(examData)
names(examData)

# CODE = ID 
# Revise = hours spent revising
# Exam = mark on the exam as a percentage
# Anxiety = the score on the EAQ
# Gender = male / female


#Simple scatter #PAGE 137
# create a ggplot object (scatter) from (examData), Anxiety = x axis, Exam = y axis


scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") #ADD NICE LABELS  #DOESN'T USE THE OPTS COMMAND ANYMORE

# NOTE THAT THIS EXMAPLE WILL NOT WORK RIGHT  COMPARE THIS ONE WITH THE ONE BELOW
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point()  #ADD POINTS TO THE GRAPH
scatter + labs(x = "Exam Anxiety", y = "Exam Performance %") #ADD NICE LABELS  #DOESN'T USE THE OPTS COMMAND ANYMORE

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter = scatter + geom_point()  #ADD POINTS TO THE GRAPH
scatter + labs(x = "Exam Anxiety", y = "Exam Performance %") #ADD NICE LABELS  #DOESN'T USE THE OPTS COMMAND ANYMORE

#HIGHLIGHT THAT THERE ARE  NO OBVIOUS OUTLIERS AND THAT LOW ANXIETY IS ALMOST EXCLUSIVELY ASSOCIATED WITH HIGH EXAM MARKS

####################################
#SECTION 4.5.2 ADDING A FUNKY LINE
####################################


#Simple scatter with smooth

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %") 
#NOTE:  NOT LINEAR
#NOTE:  GREY AREA IS THE 95% CONFIDENCE AREA


###Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %") 
#se=f # NO STANDARD ERROR
#NOTE: LINEAR LINE

#Simple scatter with regression line + CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Exam Anxiety", y = "Exam Performance %") 
#saveInImageDirectory("04 Exam Scatter w Line & CI.png")


#Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %") 
# FILL SETS THE COLOUR OF THE SE
# ALPHA SETS THE TRANSPARENCY OF THE SE



#SECTION 4.5.3 GROUPED SCATTER PLOT WITH REGRESSION LINE AND CI
#GROUP BY GENDER!!!
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))  #NOTE COLOUR = GENDER
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 


####################################
#--------HISTOGRAMS----------
# SECTION 4.6
####################################


##Load the data file into R. This is a tab-delimited file hence use of read.delim
festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
View(festivalData)

#HAVE THEM GO LOOK AT FESTIVAL DATA AND IDENTIFY THE TYPES OF VARIABLES (INDEPENDENT AND DEPENDENT), AND WHETHER THEY ARE CONTINUOUS, OR CATEGORICAL


#CREATE THE OBJECT - USE THE DATA FROM DAY1 ONLY, NO LEGEND
festivalHistogram <- ggplot(festivalData, aes(day1)) + labs(legend.position="none")

#ADD THE DATAPOINTS IN THE FORM OF A HISTOGRAM.  
festivalHistogram + geom_histogram()  #without specifying any characteristics

festivalHistogram + geom_histogram(binwidth = 0.4) + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

#binwidt specifies how wide the bins area
#get them to change the values a couple of times

#STUDENTS SHOULD NOTICE THAT SOMETHING IS WRONG, THAT ONE DATA POINT DOES NOT FIT WITH THE OTHERS


####################################
#Density without outlier
#SECTION 4.8, PAGE 148
#NOTE THIS SECTION IS OUT OF ORDER FROM THE TEXTBOOK
####################################

#DENSITY PLOTS ARE SIMILAR TO HISTOGRAMS BUT THE SMOOTH THE DATA

#I ADDED THESE LINES
festivalDensity <- ggplot(festivalData, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")


#LOAD IN A NEW DATAFILE WITH THE OUTLIER REMOVED
festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#HISTOGRAM WITH THE OUTLIER REMOVED
festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency") + labs(legend.position="none")



#DESNITY PLOT SPLIT BASED ON GENDER
festivalDensity + geom_density(aes(fill = gender), alpha = 0.5) + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")
#saveInImageDirectory("04 Download gender Density.png")


####################################
#--------BOXPLOTS----------
#SECTION 4.7 - AKA BOX AND WHISKER PLOTS
####################################
#MEDIAN
#MIDDLE 50 % OF SCOLES (INTERQUARTILE RANGE)
#WHISKERS SHOW THE TOP AND BOTTOM 25%
festivalBoxplot <- ggplot(festivalData, aes(gender, day1)) #CREATE THE OBJECT
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")  #ADD THE POINTS


#saveInImageDirectory("04 Download Festival Boxplot with Outlier.png")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalBoxplot2 <- ggplot(festivalData2, aes(gender, day1))
festivalBoxplot2 + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
#saveInImageDirectory("04 Download Festival Boxplot.png")


####################################
#SELF TEST ON PAGE 148
# LET THE STUDENTS GIVE THIS A TRY
####################################

#days 2 and 3
festivalBoxplot <- ggplot(festivalData, aes(gender, day2))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 2 of Festival)")
#saveInImageDirectory("04 Download Festival Boxplot day 2.png")

festivalBoxplot <- ggplot(festivalData, aes(gender, day3))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 3 of Festival)")
saveInImageDirectory("04 Download Festival Boxplot day 3.png")


####################################
#--------OUTLIERS----------
# JANE SUPERBRAIN 4.2
# WE WILL NOT DO THIS, BUT THE STUDENTS CAN IF THEY WANT
####################################

# outlierSummary<-function(variable, digits = 2){
# 	
# 	zvariable<-(variable-mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
# 		
# 	outlier95<-abs(zvariable) >= 1.96
# 	outlier99<-abs(zvariable) >= 2.58
# 	outlier999<-abs(zvariable) >= 3.29
# 	
# 	ncases<-length(na.omit(zvariable))
# 	
# 	percent95<-round(100*length(subset(outlier95, outlier95 == TRUE))/ncases, digits)
# 	percent99<-round(100*length(subset(outlier99, outlier99 == TRUE))/ncases, digits)
# 	percent999<-round(100*length(subset(outlier999, outlier999 == TRUE))/ncases, digits)
# 	
# 	cat("Absolute z-score greater than 1.96 = ", percent95, "%", "\n")
# 	cat("Absolute z-score greater than 2.58 = ",  percent99, "%", "\n")
# 	cat("Absolute z-score greater than 3.29 = ",  percent999, "%", "\n")
# }
# 
# outlierSummary(festivalData$day2)
# 
# 

####################################
#--------Bar Charts----------
# SECTION 4.9 - GRAPHING MEANS
####################################

#LOAD DATA
chickFlick = read.delim("ChickFlick.dat",  header = TRUE)
View(chickFlick)
# GET THE STUDENTS TO VIEW THE DATA AND IDENTIFY IVS AND DVS AND THE CHRACTERISTICS OF THE VARIABLES

#CREATE A BAR GRAPH
#fun.y = mean  ### Plot the means
#geom = "bar  ### plot as a bar graph
#fill = "White"  ### fill the bars with white
#coloru = "Black"  ### bar outlier as black
#SEE TABLE 4.4 on page 151 for more information
bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(aes(film, arousal),fun.data = mean_cl_normal, geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Film", y = "Mean Arousal") 



colours = c(Female = "Red", Male = "Green")


#add a third variable (GENDER)
bar <- ggplot(chickFlick)
bar + stat_summary(aes(film, arousal, fill = gender ), fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(aes(film, arousal, fill = gender ), fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")


#NEED TO FIX 'opts' to "labs"
#uses the 'facet_wrap' command
bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender) + labs(x = "Film", y = "Mean Arousal") + labs(legend.position="none")
#saveInImageDirectory("04 Chick Flick Facet Error Bar.png")


####################################
# R'S SOULS' TIP 4.3
# CUSTOM COLOURS
####################################

#NOTE THAT THE WORD "values" was missing from the "scale_fill_manual" instruction in the last line
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2)+
  labs(x = "Film", y = "Mean Arousal", fill = "Gender") + 
  scale_fill_manual("Gender", values = c("Female" = "Blue", "Male" = "Green"))  


#NEED TO ADD "values" in 'scale_fill_manual'
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender") + scale_fill_manual("Gender", values =c("Female" = "#3366FF", "Male" = "#336633"))






####################################
# SELF TEST 
####################################


graph + geom_line() + opts(title = "geom_line()")
saveInImageDirectory("04 Tutorial Line.png")

graph + geom_line(aes(colour = Rating_Type)) + opts(title = "geom_line(aes(colour = Rating_Type))")
saveInImageDirectory("04 Tutorial colour Line.png")

graph + geom_smooth(aes(colour = Rating_Type)) + opts(title = "geom_smooth(aes(colour = Rating_Type))")
saveInImageDirectory("04 Tutorial colour Smooth.png")

graph + geom_smooth(aes(colour = Rating_Type), method = lm) + opts(title = "geom_smooth(aes(colour = Rating_Type), method = lm)")
saveInImageDirectory("04 Tutorial colour lm.png")

graph + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + opts(title = "geom_smooth(aes(colour = Rating_Type), method = lm, se = F)")
saveInImageDirectory("04 Tutorial colour lm sef.png")

graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F)
saveInImageDirectory("04 Tutorial colour lm & point.png")

graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + labs(x = "Narcissism (NPQC)", y = "Facebook Picture Rating", colour = "Rated Attribute")
saveInImageDirectory("04 Tutorial colour labels.png")


graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + labs(x = "Narcissism (NPQC)", y = "Facebook Picture Rating", colour = "Rated Attribute") + scale_x_continuous(limits=c(0, 50))
saveInImageDirectory("04 Tutorial colour axis.png")


bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "Red",  width = 0.2) + labs(x = "Film", y = "Mean Arousal") 
saveInImageDirectory("04 Chick Flick Error Bar Red.png")


bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "Red", width = 0.2) + labs(x = "Film", y = "Mean Arousal") 
saveInImageDirectory("04 Chick Flick Error Boot Red.png")


####################################
# LINE GRAPHS
# SECTION 4.9.2
####################################



hiccupsData <- read.delim("Hiccups.dat",  header = TRUE)
View(hiccupsData)
#data is in wide format!!!!

#CONVERT DATA TO LONG FORMAT
hiccups<-stack(hiccupsData)
View(hiccups)

#need to add variable names
#note that we have lost identifier information (should have used the melt command!)
names(hiccups)<-c("Hiccups","Intervention")
View(hiccups)

#create a new variable that adds the labels
hiccups$Intervention_Factor<-factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])


#create the graph object
line <- ggplot(hiccups,  aes(Intervention_Factor, Hiccups))

#add the data
line + stat_summary(fun.y = mean, geom = "point") +  #ADD THE DATAPOINTS
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD THE ERROR BARS
  labs(x = "Intervention", y = "Mean Number of Hiccups") +   #ADD THE X AND Y LABELS
  stat_summary(fun.y = mean, geom = "line", aes(group=1),colour = "Red", linetype = "dashed") #ADD THE LINES
#saveInImageDirectory("04 Hiccups Line.png")


#LOAD A NEW DATA FILE WITH 2 INDEPENDENT VARIABLES
textData <- read.delim("TextMessages.dat",  header = TRUE)
View(textData)

#THE FILE DOESN'T HAVE A SUBJECT INDENTIFIER, SO LET'S ADD ONE
#THIS COMMOND WILL TAKE THE ROW NUMBER AND PUT IT INTO A NEW VARIABLE CALLED 'id'
textData$id = row(textData[1])
View(textData)



#textMessages = reshape(textData, idvar = c("id", "Group"), varying = c("Baseline", "Six_months"), v.names = "Grammar_Score", timevar = "Time", times = c(0:1), direction = "long")

#THE FILE IS AGAIN IN WIDE FORMAT
#USE THE MELT COMMAND TO TURN IT INTO LONG FORMAT
textMessages<-melt(textData, id = c("id", "Group"), measured = c("Baseline", "Six_months"))
View(textMessages)

#change the names of the columns 
names(textMessages)<-c("id", "Group", "Time", "Grammar_Score")

#convert 'Time' into a factor and add labels
textMessages$Time<-factor(textMessages$Time, labels = c("Baseline", "6 Months"))
View(textMessages)
#print (textMessages)  #DON'T USE PRINT - USE VIEW INSTEAD


#CREATE GRAPH OBJECT
line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))

#ADD DATA TO THE GRAPH
line + stat_summary(fun.y = mean, geom = "point") +   #PLOT THE MEANS AS POINTS
  stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + #CONNECT THE MEANS WITH A LINE, ONE FOR EACH GROUP
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD ERROR BARS USING BOOTSTRAP METHOD
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group") #ADD LABELS

#DO IT AGAIN BUT CHANGE THE Y AXIS
line + stat_summary(fun.y = mean, geom = "point") +   #PLOT THE MEANS AS POINTS
  coord_cartesian(ylim = c(0, 90))+  #SET THE Y RANGE
  stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + #CONNECT THE MEANS WITH A LINE, ONE FOR EACH GROUP
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD ERROR BARS USING BOOTSTRAP METHOD
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group") #ADD LABELS


#self test

line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun.y = mean, geom = "point", aes(shape = Group), size = 4) + stat_summary(fun.y = mean, geom = "line", aes(group= Group, linetype = Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group") 
saveInImageDirectory("04 Text Message Line 2.png")








#notes:
# default theme is theme_grey(), you can change to black and white by adding + theme_bw()
# mean_cl_normal gives normal confidence intervals, mean_cl_boot produces bootstrapped CIs
# define any colour with #RRGGBB, e.g. fill = "#336633"

#-------------------------Smart Alex Task 1

lecturerData = read.delim("Lecturer Data.dat", header = TRUE)

#Tell R that 'job' is a factor:

lecturerData$job<-factor(lecturerData$job, levels=c(1:2), labels = c("lecturer", "student"))

#------An error bar chart showing the mean number of friends for students and lecturers. 

bar<-ggplot(lecturerData, aes(job, friends))
bar + stat_summary(fun.y = "mean", geom = "bar", fill = "white", colour = "black") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", colour = "red", width = 0.2) + labs(x = "Job", y = "Mean Number of Friends")

#-------An error bar chart showing the mean alcohol consumption for students and lecturers. 

bar < - ggplot(lecturerData, aes(job, alcohol))
bar + stat_summary(fun.y = "mean", geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", colour = "Red", width = 0.2) + labs(x = "Job", y = "Mean Alcohol Consumption")

#-----An error line chart showing the mean income for students and lecturers:

line <- ggplot(lecturerData, aes(job, income))
line + stat_summary(fun.y = "mean", geom = "point") + stat_summary(fun.data = "mean_cl_normal", geom= "errorbar", width = 0.2) + labs(x = "Job", y = "Mean Income")+ stat_summary(fun.y = "mean", geom = "line", aes(group=1),colour = "Red", linetype = "dashed")

#An error line chart showing the mean neuroticism for students and lecturers:
line <- ggplot(lecturerData, aes(job, neurotic))
line + stat_summary(fun.y = "mean", geom =  "point") + stat_summary(fun.y = "mean", geom = "line", aes(group=1),colour = "Red", linetype = "dashed")+ stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) + labs(x = "Job", y = "Mean Neuroticism")

#----A scatterplot with regression lines of alcohol consumption and neuroticism grouped by lecturer/student. 

scatter <- ggplot(lecturerData, aes(neurotic, alcohol, colour = job))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = job), alpha = 0.1) + labs(x = "Neuroticism", y = "Alcohol Consumption", colour = "job")

#-------------------------Smart Alex Task 2

infidelityData = read.delim("Infidelity Data.csv", header = TRUE)

#------plot a clustered error bar chart of the mean number of bullets used against the self and the partner for males and females.

infidelityData$id = row(infidelityData[1])
Bullets = reshape(infidelityData, idvar = c("id", "Gender"), varying = c("Partner", "Self"), v.names = "Number_of_Bullets", timevar = "Recipient", times = c(0:1), direction = "long")
Bullets$Recipient<-factor(Bullets$Recipient, labels = c("Partner","Self"))
bar <- ggplot(Bullets, aes(Recipient, Number_of_Bullets, fill = Gender))
bar + stat_summary(fun.y = "mean", geom = "bar", position="dodge")
bar + stat_summary(fun.y = "mean", geom = "bar", position="dodge") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x ="Recipient", y ="Number of Bullets", fill = "Gender")
