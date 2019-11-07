#SET YOUR WORKING DIRECTORY
setwd("~/Documents/Teaching/2017-2018/Statistics/Data files")


#INITIATE YOUR LIBRARIES
library(nameOfTheLibraryYouWant)

#OPEN YOUR DATA FILE AND PUT IT IN A DATAFRAME

#READ IN THE DOWNLOAD FESTIVAL DATA AGAIN:
dataframe <- read.delim("file name", 
                        header=TRUE,#can be TRUE or FALSE
                        sep = "")  #delimiter can be ",", "\t", " "
                          
#use the help window to find out more options

#VIEW THE DATAFRAME
View(dataframe)