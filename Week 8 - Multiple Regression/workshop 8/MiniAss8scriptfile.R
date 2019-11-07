##########################################
#----Set the working directory------
##########################################


options(scipen=15)


######################################
#OPEN THE DATA FILE
######################################

albumData<-read.delim("Album Sales 2.dat", header = TRUE)
View(albumData) 

str(albumData)
describe(albumData)
summary(albumData))

######################################
#RUN A SIMPLE REGRESSION
######################################
#regression with 1 variable
simpleModel<-lm(sales ~ adverts, data = albumData)
summary(simpleModel)

library(apaTables)
apa.reg.table(simpleModel,filename = "..//simpleModel.doc")
######################################
#forced, enters everything at once
######################################


#Multiple Regression
albumForced<-lm(sales ~ adverts + airplay + attract, data = albumData)
summary(albumForced)


# What would we infer from this model?
#It has good fit (explains significant amount of variance)
#Each of the predictors is significant (contributes to the model)


library(apaTables)

apa.reg.table(albumForced)
apa.reg.table(albumForced,filename = "..//albumForced.doc")
#can use beta to understand the relative contributions of the different predictors
#should notice that beta and semipartial are closely linked.


######################################
#hierarchical
######################################

#the model based on past data
baseModel<-lm(sales ~ adverts + airplay, data = albumData)  
summary(baseModel)  # examine its fit

#add new variables based on theoretical importance 
#note you could do this more than once
newModel<-lm(sales ~ adverts + airplay + attract, data = albumData)  
summary(newmodel)


# see if the newmodel is better than the old model
anova(baseModel,newModel)  


#get R2 change
apa.reg.table(baseModel,newModel, filename = "..//hierarchical.doc")  


#should point out that it is possible that all of the predictors are
#now significant, but it is not necessarily the case that the model will be
#any better
