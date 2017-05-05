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
airportori = levels(airdataclean$origincity)
airportdest = levels(airdataclean$destcity)
airport = unique(airportori,airportdest)
freqori = sapply(1:337,function(i){
  x = subset(airdataclean,airdataclean$origincity == airportori[i])
  nrow(x)
})
freqdest = sapply(1:335,function(i){
  y = subset(airdataclean,airdataclean$destcity == airportdest[i])
  nrow(y)
})
freqori = cbind(airportori,freqori)
freqdest = cbind(airportdest,freqdest)
freq = merge(freqori,freqdest,by.x = c("airportori"),by.y = c("airportdest"))
freq$freqori = as.numeric(freq$freqori)
freq$freqdest = as.numeric(freq$freqdest)
freq$total = freq$freqori + freq$freqdest
freq = freq[order(freq$total,decreasing = T),]
rownames(freq) = 1:333

top20airport = head(freq,20)
#Plot the top 20 frequent departing airport
ggplot(top20airport,aes(top20airport$airport,top20airport$total)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_bar(stat="identity") + ggtitle("Frequency of Flights by Airport")

#Unique air schedule
airschedule = unique(airdataclean[,c("origincity","destcity")])
