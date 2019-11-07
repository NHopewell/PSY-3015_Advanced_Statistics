#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#######################################
#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Documents/Academic/Data/DSU_R/Chapter 04 (Graphs) R")




######Initiate packages


#Initiate ggplot2
library(ggplot2)  
library(reshape2)  #NOTE THE CHANGE####
library(plyr)

####################################
#--------Quick Tutorial----------
# SECTION 4.4.8  PUTTING IT ALL TOGETHER: A QUICK TUTORIAL

##### NOTE THAT OPTS IS CHANGED TO LABS!!!!!!!!#######
####################################


####################################
#--------Scatterplots----------
# SECTION 4.5.1  SIMPLE SCATTERPLOT
####################################

#LOAD A NEW DATA FILE
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
View(examData)


# CODE = ID 
# Revise = hours spent revising
# Exam = mark on the exam as a percentage
# Anxiety = the score on the EAQ
# Gender = male / female


#Simple scatter #PAGE 137


#generic graph process
# 1.  create graph ojbect using ggplot(dataFrame, aes(x-variable,y-variable, other variable))
# 2.  fill the object with data points  (this determines the type of graph)
# 3.  Add labels and stuff


#CREATE A SCATTERPLOT
scatter <- ggplot(examData, aes(Anxiety, Exam))  #CREATE THE GRAPH OBJECT

scatter + #ADD TO THE GRAPH OBJECT
  geom_point() + #ADD THE DATAPOINTS
  labs(x = "Exam Anxiety", y = "Exam Performance %") #ADD NICE LABELS  #DOESN'T USE THE OPTS COMMAND ANYMORE

#HIGHLIGHT THAT THERE ARE  NO OBVIOUS OUTLIERS AND THAT LOW ANXIETY IS ALMOST EXCLUSIVELY ASSOCIATED WITH HIGH EXAM MARKS




####################################
#SECTION 4.5.2 ADDING A FUNKY LINE
####################################


scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + #ADD THE DATA POINTS
  geom_smooth() + #ADD THE BEST FIT LINE AND STANDARD ERROR
  labs(x = "Exam Anxiety", y = "Exam Performance %") #ADD THE LABELS

#NOTE:  NOT LINEAR
#NOTE:  GREY AREA IS THE 95% CONFIDENCE AREA


###Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", se = F) + #ADD BEST FIT LINE (LINEAR MODEL), SET THE COLOUR, NO STANDARD ERROR
  labs(x = "Exam Anxiety", y = "Exam Performance %") # ADD THE LABELS
#se=f # NO STANDARD ERROR
#NOTE: LINEAR LINE


####################################
#--------HISTOGRAMS----------
# SECTION 4.6
####################################


##Load the data file into R. This is a tab-delimited file hence use of read.delim
festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
View(festivalData)

#HAVE THEM GO LOOK AT FESTIVAL DATA AND IDENTIFY THE TYPES OF VARIABLES (INDEPENDENT AND DEPENDENT), AND WHETHER THEY ARE CONTINUOUS, OR CATEGORICAL


#CREATE THE OBJECT - USE THE DATA FROM DAY1 ONLY, NO LEGEND
festivalHistogram <- ggplot(festivalData, aes(day1)) + #create the graph object
    labs(legend.position="none") #add no labels

#ADD THE DATAPOINTS IN THE FORM OF A HISTOGRAM.  
festivalHistogram +
  geom_histogram() +  #without specifying any characteristics
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")



#STUDENTS SHOULD NOTICE THAT SOMETHING IS WRONG, THAT ONE DATA POINT DOES NOT FIT WITH THE OTHERS


#LOAD IN A NEW DATAFILE WITH THE OUTLIER REMOVED
festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)  #OPEN THE DATA FILE

#HISTOGRAM WITH THE OUTLIER REMOVED
festivalHistogram <- ggplot(festivalData2, aes(day1))
festivalHistogram + 
  geom_histogram() + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency") + labs(legend.position="none")



#HISTOGRAM  SPLIT BASED ON GENDER
festivalHistogram <- ggplot(festivalData2, aes(day1))
festivalHistogram + 
  geom_histogram(aes(fill = gender), alpha = 0.5) + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency") + labs(legend.position="none")




####################################
#--------BOXPLOTS----------
#SECTION 4.7 - AKA BOX AND WHISKER PLOTS
####################################
#MEDIAN
#MIDDLE 50 % OF SCOLES (INTERQUARTILE RANGE)
#WHISKERS SHOW THE TOP AND BOTTOM 25%

#USE THE DATA WITH THE OUTLIER
festivalBoxplot <- ggplot(festivalData, aes(gender, day1)) #CREATE THE OBJECT
festivalBoxplot + 
  geom_boxplot() + #USE THE BOXPLOT TO SHOW DATA
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")  #ADD THE POINTS


#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalBoxplot2 <- ggplot(festivalData2, aes(gender, day1))
festivalBoxplot2 + 
  geom_boxplot() + 
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")





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
bar + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + #PLOT THE MEANS AS A BAR GRAPH
  stat_summary(aes(film, arousal),fun.data = mean_cl_normal, geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Film", y = "Mean Arousal") 


#add a third variable (GENDER)
bar <- ggplot(chickFlick)
bar + stat_summary(aes(film, arousal, fill = gender ), fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(aes(film, arousal, fill = gender ), fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  labs(x = "Film", y = "Mean Arousal", fill = "Gender")



####################################
# LINE GRAPHS
# SECTION 4.9.2
####################################

#create the graph object
line <- ggplot(chickFlick, aes(film, arousal))

#add the data
line + stat_summary(fun.y = mean, geom = "point") +  #ADD THE DATAPOINTS
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD THE ERROR BARS
  labs(x = "MOVIE", y = "AROUSAL") +   #ADD THE X AND Y LABELS
  stat_summary(fun.y = mean, geom = "line", aes(group=1),colour = "Red", linetype = "dashed") #ADD THE LINES
#saveInImageDirectory("04 Hiccups Line.png")


#LOAD A NEW DATA FILE WITH 2 INDEPENDENT VARIABLES


#CREATE GRAPH OBJECT
line <- ggplot(chickFlick, aes(film, arousal, colour = gender))

#ADD DATA TO THE GRAPH
line + stat_summary(fun.y = mean, geom = "point") +   #PLOT THE MEANS AS POINTS
  stat_summary(fun.y = mean, geom = "line", aes(group= gender)) + #CONNECT THE MEANS WITH A LINE, ONE FOR EACH GROUP
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD ERROR BARS USING BOOTSTRAP METHOD
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group") #ADD LABELS



#################################################
##  YOUR MISSION  #########
###########################


# CREATE A SCATTERPLOT, HISTOGRAM, BAR CHART, AND LINE GRAPH USING THE LECTURER DATA FROM LAST WEEK

#OPEN THE DATA FILE
lecturerData = read.delim("Lecturer Data.dat", header = TRUE)
lecturerData$job<-factor(lecturerData$job, levels=c(1:2), labels = c("lecturer", "student"))  #Tell R that 'job' is a factor:
View(lecturerData)

#MODIFY THESE GRAPHS TO WORK WITH OUR LECTURER DATA

#SCATTERPLOT
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", se = F) + #ADD BEST FIT LINE (LINEAR MODEL), SET THE COLOUR, NO STANDARD ERROR
  labs(x = "Exam Anxiety", y = "Exam Performance %") # ADD THE LABELS
#se=f # NO STANDARD ERROR
#NOTE: LINEAR LINE

#SCATTERPLOT
scatter <- ggplot(lecturerData, aes(income, alcohol))
scatter + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Red", se = F) + #ADD BEST FIT LINE (LINEAR MODEL), SET THE COLOUR, NO STANDARD ERROR
  labs(x = "Income", y = "Alochol Consumption") # ADD THE LABELS


#HISTOGRAM
festivalHistogram <- ggplot(lecturerData, aes(neurotic))  # just need to pick a variable
festivalHistogram + 
  geom_histogram() + 
  labs(x = "Neuroticism", y = "Frequency") + labs(legend.position="none")  #change the labels



#BOX PLOT
festivalBoxplot <- ggplot(lecturerData, aes(job, alcohol)) #CREATE THE OBJECT
festivalBoxplot + 
  geom_boxplot() + #USE THE BOXPLOT TO SHOW DATA
  labs(x = "Job", y = "Alcohol")  #ADD THE POINTS


#BAR GRAPH GRAPH 
bar <- ggplot(lecturerData, aes(job, alcohol))
bar + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + #PLOT THE MEANS AS A BAR GRAPH
  stat_summary(aes(job, alcohol),fun.data = mean_cl_normal, geom = "pointrange") + ###NOTE THAT MINE IS DIFFERENT - I ADDED THE AES TO THIS LINE
  labs(x = "Job", y = "Alcohol") 



#LINE GRAPH object
line <- ggplot(lecturerData, aes(job, alcohol))

line + stat_summary(fun.y = mean, geom = "point") +  #ADD THE DATAPOINTS
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + #ADD THE ERROR BARS
  labs(x = "JOB", y = "ALCOHOL") +   #ADD THE X AND Y LABELS
  stat_summary(fun.y = mean, geom = "line", aes(group=1),colour = "Red", linetype = "dashed") #ADD THE LINES
