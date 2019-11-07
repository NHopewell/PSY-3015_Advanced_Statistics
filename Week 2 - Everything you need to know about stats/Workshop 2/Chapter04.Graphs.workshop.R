#---------------------------------------------------------------------------------------------------------
# FROM Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). 
#Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. 
#London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#
#-----------------------------------------------------------------------------------------------------------

####################################
#
#
# SECTION 4.4.8  PUTTING IT ALL TOGETHER: A QUICK TUTORIAL
#
##### NOTE THAT "OPTS" IS CHANGED TO "LABS"!!!!!!!!#######
#
####################################



#######################################
#     
#     STEP #1:  Set the working directory 
#     You will need to edit this to be the directory 
#     where you have stored the data files for this Chapter
########################################

#setwd("~/Documents/Teaching/2017-2018/Statistics/mikeScriptFiles/Data files")

install.packages("ggplot2")


library(knitr)
opts_knit$set(root.dir="~/Documents/Teaching/2017-2018/Statistics/mikeScriptFiles/Data files")




######Initiate packages

########################################
#  STEP #2: INITIALIZE PACKAGES
#  NOTE*** AS ANALYSES GET MORE COMPLICATED
#         I LIKE TO INITIALIZE PACKAGES ON A 
#         WHEN NEEDED BASIS
########################################


#Initiate ggplot2
library(ggplot2) 
library(psych) #THIS IS CHANGED FROM THE TEXTBOOK
#library(plyr)




####################################
#--------Scatterplots----------
# SECTION 4.5.1  SIMPLE SCATTERPLOT
####################################



#  STEP #3: OPEN THE DATA FILE
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)


#STEP #4: EXAMINE THE DATAFILE TO MAKE SURE IT OPENNED CORRECTLY
View(examData)

#FROM THE TEXTBOOK A DESCRIPTION OF THE VARIABLES.  THIS CAN BE
#USEFUL TO ADD TO YOUR SCRIPT FILE SO THAT YOU CAN REMEMBER WHAT
#THE VARIABLES ARE

# CODE = ID 
# Revise = hours spent revising
# Exam = mark on the exam as a percentage
# Anxiety = the score on the EAQ
# Gender = male / female

#STEP #5a: examine how R has classified our variables
str(examData) 

#STEP #5b: RECODE VARIABLES AS NECESSARY
examData$Code = factor(examData$Code)  #convert $Code into a factor

#STEP #6: EXAMINE THE SUMMARY INFORMATION ABOUT OUR VARIABLES
describe(examData)  #describe() is in the psych package)

######################################
#   STEP #7 VISUALLY EXAMINE THE DATA
#
#     generic graph process
#       A.  create graph object 
#       B.  fill the object with data points  
#           (this determines the type of graph)
#       C.  Add labels and stuff
#
######################################

#STEP #7A:  CREATE A GRAPH OBJECT

# use ggplot() to greate a graph object
# ggplot() takes the following form
# graphObject <- ggplot(dataFrame, aes(x-variable,y-variable, other variables))


#is anxiety related to exam
#use a scatterplot

#CREATE THE GRAPH OBJECT
scatter <- ggplot(examData, aes(Anxiety, Exam))  


scatter + #ADD TO THE GRAPH OBJECT
  geom_point() + #STEP #7B:  ADD THE DATAPOINTS
  labs(x = "Exam Anxiety",  #STEP #7C: ADD THE LABELS
       y = "Exam Performance %") #ADD Y LABEL

#ARE THERE ANY WEIRD DATA POINTS??
#HIGHLIGHT THAT THERE ARE NO OBVIOUS OUTLIERS 
#AND THAT LOW ANXIETY IS ALMOST EXCLUSIVELY 
#ASSOCIATED WITH HIGH EXAM MARKS




####################################
#SECTION 4.5.2 ADDING A FUNKY LINE
####################################


scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + #ADD THE DATA POINTS
  geom_smooth() + #ADD THE BEST FIT LINE AND STANDARD ERROR
  labs(x = "Exam Anxiety", #ADD THE X LABEL
       y = "Exam Performance %") # ADD THE Y LABEL

#NOTE:  NOT LINEAR
#NOTE:  GREY AREA IS THE 95% CONFIDENCE AREA


###Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + 
  geom_point() + 
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Red", #SET THE COLOUR OF THE LINE
              se = FALSE) +       #NO STANDARD ERROR
  labs(x = "Exam Anxiety",    #ADD THE X LABEL
       y = "Exam Performance %")# ADD THE Y LABEL






####################################
#--------HISTOGRAMS----------
# SECTION 4.6
####################################

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)

View(festivalData)

str(festivalData)
#HAVE THEM GO LOOK AT FESTIVAL DATA AND IDENTIFY THE TYPES OF VARIABLES (INDEPENDENT AND DEPENDENT), AND WHETHER THEY ARE CONTINUOUS, OR CATEGORICAL
#Ticknumb= ID 
#Gender=  male or female 
#Day 1 = score on a scale (0=smell like corpse to 4 =smell like roses) 
#Day 2 = score on a scale (0=smell like corpse to 4 =smell like roses)
#Day 3 = score on a scale (0=smell like corpse to 4 =smell like roses)

#DO ANY VARIABLES NEED TO BE CHANGED?
festivalData$ticknumb = factor(festivalData$ticknumb) 
str(festivalData)

describe(festivalData)

#ANYTHING THAT THEY SHOULD BE WORRIED ABOUT?

#CREATE THE OBJECT - USE THE DATA FROM DAY1 ONLY, NO LEGEND
festivalHistogram <- ggplot(festivalData, aes(day1)) 
    

#ADD THE DATAPOINTS IN THE FORM OF A HISTOGRAM.  
festivalHistogram +
  geom_histogram() +  #without specifying any characteristics
  labs(x = "Hygiene (Day 1 of Festival)", 
       y = "Frequency")



#STUDENTS SHOULD NOTICE THAT SOMETHING IS WRONG, 
#THAT ONE DATA POINT DOES NOT FIT WITH THE OTHERS


#LOAD IN A NEW DATAFILE WITH THE OUTLIER REMOVED
festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)  #OPEN THE DATA FILE
View(festivalData2)
str(festivalData2)
describe(festivalData2)

#HISTOGRAM WITH THE OUTLIER REMOVED
festivalHistogram <- ggplot(festivalData2, aes(day1))
festivalHistogram + 
  geom_histogram() + 
  labs(x = "Hygiene (Day 1 of Festival)", 
       y = "Frequency") + 
      theme(legend.position="none") #where to place the legend ("none", "left", "right", "bottom", "top")



#HISTOGRAM  SPLIT BASED ON GENDER
festivalHistogram <- ggplot(festivalData2, aes(day1))
festivalHistogram + 
  geom_histogram(aes(fill = gender),alpha = 0.5) + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")+theme(legend.position = "right")

# How can you dO a HISTOGRAM using festivalData2 but plot for Day#2?



####################################
#--------BOXPLOTS----------
#SECTION 4.7 - AKA BOX AND WHISKER PLOTS
####################################

#REMIND THEM WHAT THE SOLID LINE, BOX AND TAILS REPRESENT!
#MEDIAN
#MIDDLE 50 % OF SCORES (INTERQUARTILE RANGE)
#WHISKERS SHOW THE TOP AND BOTTOM 25%
#DOTS ARE OUTLIERS

#USE THE DATA WITH THE OUTLIER
festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
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
# OFTEN WHEN WE ARE REPORTING DATA IN A MANUSCRIPT
# WE ARE REPORTING SUMMARY INFORMATION, NOT RAW DATA
# THEREFORE THESE TYPES OF GRAPHS ARE MORE COMMON.
####################################

#LOAD DATA
chickFlick = read.delim("ChickFlick.dat",  header = TRUE)
View(chickFlick)

str(chickFlick)
# GET THE STUDENTS TO VIEW THE DATA AND IDENTIFY IVS AND DVS AND THE CHRACTERISTICS OF THE VARIABLES
describe(chickFlick)

#CREATE A BAR GRAPH

#SEE TABLE 4.4 on page 151 for properties of the stat_summary 
bar <- ggplot(chickFlick, aes(film, arousal))
bar + 
  stat_summary(fun.y = mean, ### Plot the means
               geom = "bar")+ ### plot as a bar graph
  labs(x = "Film", y = "Mean Arousal") 


# add the confidence intervals
bar <- ggplot(chickFlick, aes(film, arousal))
bar + 
  stat_summary(fun.y = mean, ### Plot the means
               geom = "bar")+ ### plot as a bar graph
  stat_summary(fun.data = mean_cl_normal, #gets the summary statistics 95%confidnence interval
               geom = "pointrange") + #plots a line with a value for a point
  labs(x = "Film", y = "Mean Arousal") 


#change the appearance of the bars
bar <- ggplot(chickFlick, aes(film, arousal))
bar + 
  stat_summary(fun.y = mean, ### Plot the means
               geom = "bar", ### plot as a bar graph
               fill = "White", ### fill the bars with white
               colour = "Black") + ### bar outline as black
  stat_summary(fun.data = mean_cl_normal, #gets the summary statistics 95%confidnence interval
               geom = "pointrange") + #plots a line with a value for a point
  labs(x = "Film", y = "Mean Arousal") 

#add a third variable (GENDER)
bar <- ggplot(chickFlick,aes(film, arousal, fill=gender))
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, #confidnce interval info
               geom = "errorbar", #plot error bars
               position=position_dodge(width=0.90), #used to shift the position of the errorbar
               width = 0.2) + #the width of bar at the top and bottom
  labs(x = "Film", 
       y = "Mean Arousal", 
       fill = "Gender")#add a legend for gender



####################################
# LINE GRAPHS
# SECTION 4.9.2
####################################

#create the graph object
line <- ggplot(chickFlick, aes(film, arousal))


#add the data
line + 
  stat_summary(fun.y = mean, #the summary you want to plot
               geom = "line", #the type of geom
               aes(group=1), #specifies that you want a single line connecting all of the points
               colour = "Red", #the colour of the line
               linetype = "dashed") + #the type of line
  labs(x = "MOVIE", y = "AROUSAL")   #add the x and y labels

# THE aes(group=1) command is unusual and only pops up 
# when making simple graphs with one independent variable

#add the data
line + 
  stat_summary(fun.y = mean, 
               geom = "point") +  #ADD THE DATAPOINTS
  stat_summary(fun.y = mean, #the summary you want to plot
               geom = "line", #the type of geom
               aes(group=1), #specifies that you want a single line connecting all of the points
               colour = "Red", #the colour of the line
               linetype = "dashed") + #the type of line
  labs(x = "MOVIE", y = "AROUSAL")  #add the x and y labels

#add the data
line + 
  stat_summary(fun.y = mean, 
               geom = "point") +  #ADD THE DATAPOINTS
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = 0.2) + #ADD THE ERROR BARS
  stat_summary(fun.y = mean, #the summary you want to plot
               geom = "line", #the type of geom
               aes(group=1), #specifies that you want a single line connecting all of the points
               colour = "Red", #the colour of the line
               linetype = "dashed") + #the type of line
  labs(x = "MOVIE", y = "AROUSAL")   #add the x and y labels



#LOAD A NEW DATA FILE WITH 2 INDEPENDENT VARIABLES


#CREATE GRAPH OBJECT
line <- ggplot(chickFlick, aes(film, arousal, colour = gender))

#ADD DATA TO THE GRAPH
line + 
  stat_summary(fun.y = mean, 
               aes(group= gender), 
               geom = "point") +   #PLOT THE MEANS AS POINTS
  stat_summary(fun.y = mean, 
               geom = "line", 
               aes(group= gender)) + #when there is more than one IV use the second IV to group
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = 0.2,
               aes(group= gender)) + #ADD ERROR BARS USING BOOTSTRAP METHOD
  labs(x = "Film", 
       y = "Mean Arousal", 
       colour = "Group") #ADD LABELS

#WHAT IS POORLY DONE IN THIS GRAPH???
#CAN YOU THINK OF A WAY TO FIX IT?


line + 
  stat_summary(fun.y = mean, 
               aes(group= gender), 
               geom = "point",
               position=position_dodge(width=0.05)) +   #PLOT THE MEANS AS POINTS
  stat_summary(fun.y = mean, 
               geom = "line", 
               aes(group= gender),
               position=position_dodge(width=0.05)) + #when there is more than one IV use the second IV to group
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = 0.2,
               aes(group= gender),
               position=position_dodge(width=0.05)) + #ADD ERROR BARS USING BOOTSTRAP METHOD
  labs(x = "Film", 
       y = "Mean Arousal", 
       colour = "Group") #ADD LABELS



###########################
##  YOUR MISSION  #########
###########################


# CREATE A SCATTERPLOT, HISTOGRAM, BAR CHART, AND LINE GRAPH 
# USE THE LECTURER DATA

#FOLLOW OUR STEPS

#Name of Participant 
#Birth Date - day/ month/year
#Job - either Lecturer = 1 or Student = 2
#No. of Friends - the number of friends they have
#Alcohol - their weekly alcohol consumption (in units)
#Income - yearly income (in dollars)
#Neurotic - score on a neuroticism scale (the higher the score the more neurotic)
