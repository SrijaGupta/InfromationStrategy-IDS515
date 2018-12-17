library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

setwd( "/Users/srijagupta/Desktop/UIC_3/IDS515/Airbnb")
require(xlsx)
Miami<-read.csv("M-0897X1 miami.csv")
m<-Miami
str(m)
m$X<-as.integer(m$X)
m$cleanfee<-as.factor(m$cleanfee)
m$weekfee<-as.factor(m$weekfee)
m$monthfee<-as.factor(m$monthfee)
m$secdep<-as.factor(m$secdep)
m$extpeop<-as.factor(m$extpeop)
m1<-m[2:16]
linearModel.c <- lm (price ~., data = m1)
summary(linearModel.c)
options(scipen = 999)

linearModel.c1 <- lm (price~ accommodates+weekfee+monthfee+savwish+cleanfee+beds+bathroom+bedroom+min_stay+extpeop, data = m1)
summary(linearModel.c1)
options(scipen = 999)

linearModel.c <- lm (price ~bathroom+bedroom+beds, data = m1)
summary(linearModel.c)
options(scipen = 999)
