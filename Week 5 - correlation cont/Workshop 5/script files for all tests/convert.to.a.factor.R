
#convert a continuous variable coded as 1,2,3 into label1,label2,label3
dataframe$variable<-factor(dataframe$variable, labels = c("label1", "label2","labe3"),levels=c(1,2,3))

#convert a continuous variable coded with 2 values into label1,label2
dataframe$variable<-factor(dataframe$variable, labels = c("label1", "label2"))

#recode an ordinal variable into the order that you want
#it should already have the labels you want, so you just change the labels
essayData$grade<-factor(essayData$grade, levels = c("First Class","Upper Second Class", "Lower Second Class", "Third Class"))



