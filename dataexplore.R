library(ggplot2)
library(stringr)
library(dplyr)
setwd("~/Downloads")
airdata = read.csv("pointfive.csv")
attach(airdata)
#Seperate the city names from states
origincity = str_split_fixed((ORIGIN_CITY_NAME),",",2)[,1]
destcity = str_split_fixed((DEST_CITY_NAME),",",2)[,1]
airdataclean = cbind(airdata,origincity,destcity)

#On-time performance by airlines
otperformal = aggregate(ARR_DEL15,list(CARRIER),mean,na.rm=T)
otperformal$ontimerate = 1 - otperformal$x
#Finding the no. of observation of each airlines
airlineobs = sapply(1:22,function(i){
  x = subset(airdata,UNIQUE_CARRIER == levels(UNIQUE_CARRIER)[i])
  nrow(x)
})
airlineobs = cbind(levels(UNIQUE_CARRIER),airlineobs)
as.data.frame(airlineobs)

otperformloc = aggregate(ARR_DEL15,list(origincity,destcity),mean,na.rm=T)
otperformloc$ontimerate = 1 - otperformloc$x
otperformori = aggregate(ARR_DEL15,list(origincity),mean,na.rm=T)
otperformori$ontimerate = 1 - otperformori$x

#Ploting the on-time performance by airline
background = ggplot(otperformal,aes(as.factor(otperformal$Group.1),otperformal$ontimerate))
background2 = background + geom_bar(stat="identity") + ggtitle("On-time Performance by Airlines")

#Determining the hubs
airport = levels(airdataclean$origincity)
freq = sapply(1:337,function(i){
  x = subset(airdataclean,airdataclean$origincity == airport[i])
  nrow(x)
})
freq = as.numeric(freq)
airportfreq = cbind(airport,freq)
airportfreq = as.data.frame(airportfreq)
airportfreq$freq = as.numeric(as.character(airportfreq$freq))
airportfreq = airportfreq[order(airportfreq$freq,decreasing = T),]
rownames(airportfreq) = 1:337
top20airport = head(airportfreq,50)
#Plot the top 20 frequent departing airport
ggplot(top20airport,aes(top50airport$airport,top20airport$freq)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_bar(stat="identity") + ggtitle("Frequency of Flights by Airport")
