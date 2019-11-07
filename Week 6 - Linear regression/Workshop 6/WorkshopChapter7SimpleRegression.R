#Initiate packages
library(ppcor)
library(ggplot2)
library(polycor)
library(psych)
#OPEN THE DATA FILE
asaledata = read.delim("Album Sales 1.dat",  header = TRUE)

#INSPECT THE DATAFRAME
View(asaledata)

#check structure
str(asaledata)
#Descriptive statistics
describe(asaledata)


#COMPARE TWO VARIABLES

cor.test(asaledata$adverts,asaledata$sales, 
         use = "complete.obs", 
         alternative = "two.sided",
         method = "pearson")


options(scipen =999)
#linear model with regression line slope, intercept 
albummodel= lm(sales~adverts, data=asaledata)
summary(albummodel)


library("apaStyle")
library ("apaTable")
apa.reg.table(albummodel)
apa.reg.table(albummodel,filename = "Album Regression Table.doc")
