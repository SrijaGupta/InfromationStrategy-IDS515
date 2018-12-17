setwd( "/Users/srijagupta/Desktop/UIC_3/IDS515/Airbnb")
ls()
View(Rev)
d<-read.csv("Analysis.csv")



str(d)
d$monthfee<-as.factor(d$monthfee)
d$weekfee<-as.factor(d$weekfee)
d$secdep<-as.factor(d$secdep)
d$cleanfee <-as.factor(d$cleanfee)
d<- d[2:21]
str(d)
d$extpeop <-as.factor(d$extpeop)

#review and property's ability to get rented
model1<-lm (sentiment~savwish+reviews, data=d)
summary(model1)


Miami$Esimated.Revenue <- NULL
Miami$Esimated.Revenue <- NULL
Miami$Esimated.Revenue <- NULL
Miami<- subset(d,d$City=='Miami')
str(Miami)
View(Miami)
Miami<- Miami[1:18]
linearModel.c <- lm (reviews ~ rating+price+accommodates+extpeop+min_stay+Conversion.+sentiment+secdep+cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data = Miami)
summary(linearModel.c)

table(Miami$City)

Paris<- subset(d,d$City=='Paris')
str(Paris)
Paris$Esimated.Revenue<- NULL
Paris<-Paris[1:18]
linearModel.c <- lm (reviews ~ rating+price+accommodates+extpeop+min_stay+Conversion.+sentiment+secdep+cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data = Paris)
summary(linearModel.c)
