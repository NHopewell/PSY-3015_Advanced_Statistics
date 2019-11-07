
#a simple command returns an 8 to the Console Screen
4 + 4


#a simple command returns an 8 into the variable "a"
a = 4+4


#type a to return the value of "a" to the console
a

#find out what the current working directory is
getwd()


#use the RSTudio Interface to set the working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/mikeScriptFiles/Data files")

satisfactionData = read.delim("Honeymoon Period.dat", header=TRUE)
View(satisfactionData)

Infid= read.delim("Infidelity.csv",header=TRUE, sep =",")
View(Infid)

write.table(satisfactionData,"Honey.dat")

#get the value in row 3, column 2
satisfactionData[3,2]

#get the 3rd row values for all of the columns
satisfactionData[3,]


#get the row 1 to 8 values for column 5
satisfactionData[1:8,5]

#get all of the values in the Person column
satisfactionData$Person

#put all of the values in the Person column into a new variable
personData = satisfactionData$Person

#get the value in the 5th row of the Person column
satisfactionData$Person[5]

#set maleData as satisfactionData where the gender column in satisfaction data == 0
#set maleData as satisfactionData where the gender column in satisfaction data == 0
#maleData = ###set maleData to
#  satisfactionData[  ###satisfactionData, where
#   satisfactionData$Gender == 0   ####the row values the gender column of satisfactionData ==0
#   , ] ####give me all the rows!!

maleData = satisfactionData[satisfactionData$Gender == 0, ]
View(maleData)

#Change the column gender 
#so that when the row value of gender = 0, 
#it should now equal "male
satisfactionData$Gender[satisfactionData$Gender == 0]="male"
View(satisfactionData)


#Change the column gender 
#so that when the row value of gender = 1, 
#it should now equal "female
satisfactionData$Gender[satisfactionData$Gender == 1]="female"
View(satisfactionData)


#get the structure of our satisfaction Data.
str(satisfactionData)

#need to change Person from int to factor
satisfactionData$Person = factor(satisfactionData$Person)
str(satisfactionData)

satisfactionData$Gender= factor(satisfactionData$Gender)
str(satisfactionData)

#if the Gender column was still coded as 0 and 1 we could do it as follows

satisfactionData = read.delim("Honeymoon Period.dat", header=TRUE)
str(satisfactionData)

satisfactionData$Gender<-factor(satisfactionData$Gender, labels = c("male", "female"),levels=c(0,1))
str(satisfactionData)

satisfactionData$Gender<-factor(satisfactionData$Gender, labels = c("male", "female"))
