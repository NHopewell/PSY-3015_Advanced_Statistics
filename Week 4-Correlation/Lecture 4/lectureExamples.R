setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture04.correlation")

pid = seq(101,150,1)
pid = paste("a",pid,sep="")

x = rnorm(n=50,mean=5,sd=1)
y = x-5+rnorm(n=50,mean=5, sd=1.5)
y2 = y-5+rnorm(n=50,mean=5, sd=1.5)
cor(x,y2)
classData = data.frame(pid,x,y,y2)

write.table(classData,"lecture04classData.txt",row.names = FALSE)


#Set working directory
setwd("~/Documents/Teaching/2017-2018/Statistics/Lectures/lecture04.correlation")

#open Data file

classData = read.delim("lecture04classData.txt",header=TRUE,sep="")

#check that the data file opened correctly
View(classData)

#check the structure
str(classData)

#check variables for errors
describe(classData)


#TEST ASSUMPTIONS!!!!
#-----Interval Data
#-----Aproximately Normal Distribution (for p value)
#-----No major outliers
#-----Linearity

#NORMALITY, OUTLIERS, LINEARITY

par(mfrow=c(2,3))
hist(classData$x)
hist(classData$y)
hist(classData$y2)

#OUTLIERS
boxplot(classData$x,ylim=c(0,10))
boxplot(classData$y,ylim=c(0,10))
boxplot(classData$y2,ylim=c(0,10))
plot(classData$x,classData$y)
plot(classData$x,classData$y2)

library(ggplot2)


###WEAK RELATIONSHIP
scatter <- ggplot(classData, aes(x, y2))
scatter + 
  geom_point(size=2) + 
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Blue", #SET THE COLOUR OF THE LINE
              se = TRUE) +       #NO STANDARD ERROR
  
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))+
  annotate("text",x=3,y=7.5,size=10,label= paste("r = ", round(cor(classData$x,classData$y2),2)))+
  labs(x = "X Variable",size=3,    #ADD THE X LABEL
       y = "Y Variable") # ADD THE Y LABEL


###POSITIVE RELATIONSHIP
scatter <- ggplot(classData, aes(x, y))
scatter + 
  geom_point(size=2) + 
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Blue", #SET THE COLOUR OF THE LINE
              se = TRUE) +       #NO STANDARD ERROR
  
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))+
  annotate("text",x=3,y=7.5,size=10,label= paste("r = ", round(cor(classData$x,classData$y),2)))+
  labs(x = "X Variable",size=3,    #ADD THE X LABEL
       y = "Y Variable") # ADD THE Y LABEL


###NEGATIVE RELATIONSHIP
scatter <- ggplot(classData, aes(x, 10-y))
scatter + 
  geom_point(size=2) + 
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Blue", #SET THE COLOUR OF THE LINE
              se = TRUE) +       #NO STANDARD ERROR
  
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))+
  annotate("text",x=3,y=7.5,size=10,label= paste("r = ", round(cor(classData$x,-classData$y),2)))+
labs(x = "X Variable",size=3,    #ADD THE X LABEL
     y = "Y Variable") # ADD THE Y LABEL

###Simple scatter with regression line
scatter <- ggplot(classData, aes(x, y2))
scatter + 
  geom_point(size=2) + 
  geom_smooth(method = "lm",  #ADD BEST FIT LINE (LINEAR MODEL)
              colour = "Blue", #SET THE COLOUR OF THE LINE
              se = TRUE) +       #NO STANDARD ERROR
  
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))+
  annotate("text",x=3,y=7.5,size=10,label= paste("r = ", round(cor(classData$x,classData$y2),2)))
  labs(x = "X Variable",size=3,    #ADD THE X LABEL
       y = "Y Variable") # ADD THE Y LABEL

  
  pid = seq(1,5,1)
  pid = paste("a",pid,sep="")
  cookies = round(rnorm(n=5,mean=3,sd=1.5),digits=2)
  milk = round(250 + (rnorm(n=5,mean=1,sd=1)*10*cookies),digits=2)
  
  milkData = data.frame(pid,cookies,milk)
  
  cor(milkData$cookies,milkData$milk)
  
  describe(milkData)

  write.table(milkData,"milkData.dat",row.names=FALSE)  
  
  milkData= read.delim("milkData.dat",header=TRUE,sep="")
  View(milkData)
  
  milkData$cookieDev = round(mean(milkData$cookies)-milkData$cookies,2)
  View(milkData)
  
  milkData$milkDev = round(mean(milkData$milk)-milkData$milk,2)
  View(milkData)
  
  #par(mfrow=c(2,1))
  
  ###SHOW ERROR FOR ONE VARIABLE
  scatter <- ggplot(milkData, aes(pid, cookies))
  scatter + 
    geom_point(size=3) + 
    xlim(0,6)+
    geom_hline(yintercept=mean(milkData$cookies))+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=20,face="bold"))+
    geom_text(milkData,mapping=aes(pid,cookies,label=cookieDev),size=5,hjust = -1,vjust=(cookies-mean(milkData$cookies))*5)+
    geom_segment(aes(xend=pid,yend=mean(milkData$cookies)),linetype="dashed")+
    labs(x = "Subject Number",size=3,    #ADD THE X LABEL
       y = "Cookies Consumed") # ADD THE Y LABEL
  
  ###SHOW ERROR FOR ONE VARIABLE
  scatter <- ggplot(milkData, aes(pid, milk))
  scatter + 
    geom_point(size=3) + 
    xlim(0,6)+
    geom_hline(yintercept=mean(milkData$milk))+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=20,face="bold"))+
    geom_text(milkData,mapping=aes(pid,milk,label=milkDev),size=5,hjust = -1,vjust=(milkData$milkDev/200)+.5)+
    geom_segment(aes(xend=pid,yend=mean(milkData$milk)),linetype="dashed")+
    labs(x = "Subject Number",size=3,    #ADD THE X LABEL
         y = "Milk Consumed (ml)") # ADD THE Y LABEL 
  