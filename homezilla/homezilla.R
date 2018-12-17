setwd("/Users/srijagupta/Desktop/UIC_3/IDS515/S1")
t<-read.csv("Homezilla Dataset.xlsx", sheetName = "Browsing Data")
h1<-read.csv("h1.csv")
str(h1)
h1<-h1[,1:10]
h2<-read.csv("h2.csv")
str(h2)
#merging dataset
final<-merge(h1,h2,by="Web.ID")
str(final)

#visualization
#direction and time viewed
par(mfrow = c(1,2))
hist(log(final$Time_View), main = "Distribution of VARNAME", xlab = "VARNAME", col = "steelblue", las = 2, freq = F)
rug(jitter(log(final2$Time.Viewed), col = "red")
lines(density(log(final2$Time.Viewed), col = "green", lwd = 2)
boxplot(NUM_VAR_NAME, main="Distribution of VARNAME", col=c("orange"), xlab = "VARNAME")
summary(final$Time.Viewed)
table(final1$Time.Viewed)
final<- final[final$Time.Viewed<="600",]      
final$time_view[final$Time.Viewed >=3] <- 1
final$time_view[final$Time.Viewed <3] <- 0
final$Direction <- as.character(final$Direction)
final$Direction[is.na(final$Direction)] <- "first"
table(final$time_view,final$Direction)
(tab<-table(final$time_view,final$Direction))
prop.table(tab)*100
prop.table(tab,margin = 2)*100
str(final)
View(final)
dev.off()
#phototag1 and time viewed- factor vs integer
boxplot(final$Time.Viewed ~ final$Photo.Tag.1, data = final, main="Distribution of NUM_VAR_NAME for FACTOR_VAR_NAME", 
        xlab = "FACTOR_VAR_NAME Categories (levels)", ylab = "MPG", col=c("orange", "lightblue4"))
plotmeans(final$Time.Viewed ~ final$Photo.Tag.1, lwd=3, col="red", p=0.99)

testAOV <- aov(final$Time.Viewed ~ final$Photo.Tag.1, data = final)
summary(testAOV) 
#################################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
str(final)
final$X<- NULL
final$X.1<-NULL
final$User.Agent<-NULL
final$last.update<-NULL
final$Timestamp<-NULL
final$status<-NULL
#Assuming that the time.Viewed has outliers and the maximum time a person takes viewing a photo is 60 seconds
final1<-final[final$Time.Viewed<=60,]
str(final1)
View(final1)
final1$time_view <-as.factor(final1$time_view)

#Ques-1: Was the Sequence of photos important?
#Factor and Factor: Direction & time_view
#same as third for every.
t<-(table(final1$Direction,final1$time_view)) 
prop.table(t, margin = 1)

mylogit1 <- glm(time_view ~ Direction, data = final1, family = "binomial"(link="logit"))
summary(mylogit1)
predicted <- plogis(predict(mylogit1, final1))
library(InformationValue)
optCutOff <- optimalCutoff(final1$time_view, predicted)[1] 


#Ques2:Was the type of photo important?

final1$bedrooms<-as.factor(final1$bedrooms)
final1$bathrooms<-as.factor(final1$bathrooms)
final1$half.baths<-as.factor(final1$half.baths)
set.seed(900)
index=sample(2,nrow(final1), replace=TRUE, prob = c(0.70,0.30))
trainData <-final1[index==1,]
#trainData
testData <-final1[index==2,]

mylogit <- glm(time_view ~ type +subtype + sqfoot+bedrooms 
               +bathrooms+ price , data = final1, family = "binomial")

summary(mylogit)

pred1 <- predict(mylogit, newdata = testData)
pred2 <- predict(mylogit, newdata =final1)
library(InformationValue)
optCutOff <- optimalCutoff(final1, pred2)[1]
result<- ifelse(pred2>-0.6105754 ,1,0)
table(final1$time_view,result)

str(final1)
sum(is.na(final1$subtype))
table(final1$subtype)
apply(final1,2,function(x) sum(x==''))
final1<- final1[final1$subtype!='',]

write.csv(final1,file='f.csv')

#Ques3: What photos were most attractive?

head(sort(table(final1$'Photo.Tag.1'[final1$time_view=="1" & final1$'Photo.Tag.1'!='']),decreasing=T,na.last =NA),3)
head(sort(table(final1$'Photo.Tag.2'[final1$time_view=="1" & final1$'Photo.Tag.2'!='']),decreasing=T,na.last =NA),3)
head(sort(table(final1$'Photo.Tag.3'[final1$time_view=="1" & final1$'Photo.Tag.3'!='']),decreasing=T,na.last =NA),5)
head(sort(table(final1$'Photo.Tag.4'[final1$time_view=="1" & final1$'Photo.Tag.4'!='']),decreasing=T,na.last =NA),5)
head(sort(table(final1$'Photo.Tag.5'[final1$time_view=="1" & final1$'Photo.Tag.5'!='']),decreasing=T,na.last =NA),5)
head(sort(table(final1$'Photo.Tag.6'[final1$time_view=="1" & final1$'Photo.Tag.6'!='']),decreasing=T,na.last =NA),5)
head(sort(table(final1$'Photo.Tag.7'[final1$time_view=="1" & final1$'Photo.Tag.7'!='']),decreasing=T,na.last =NA),5)
head(sort(table(final1$'Photo.Tag.8'[final1$time_view=="1" & final1$'Photo.Tag.8'!='']),decreasing=T,na.last =NA),5)







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#logistic testing

c<- glm(formula = final$time_view ~ final$Photo.Tag.1,data = final)
summary(c)
write.csv(f,"final.csv")
