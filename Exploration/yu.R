library(ggplot2)
library(stringr)
library(randomForest)
airdata = read.csv("pointfive.csv")
attach(airdata)
#Seperate the city names from states
origincity = str_split_fixed((ORIGIN_CITY_NAME),",",2)[,1]
destcity = str_split_fixed((DEST_CITY_NAME),",",2)[,1]
airdataclean = cbind(airdata,origincity,destcity)

#remove the data that doesn't have the indicator of delay.
airdataclean = airdataclean[complete.cases(airdataclean[,28]),]
#Check to see if they are being removed or not.
sapply(airdataclean, function(x) sum(is.na(x)))

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

otperformloc = aggregate(ARR_DEL15,list(ORIGIN,DEST),mean,na.rm=T)
otperformloc$ontimerate = 1 - otperformloc$x
otperformori = aggregate(ARR_DEL15,list(ORIGIN),mean,na.rm=T)
otperformori$ontimerate = 1 - otperformori$x

#Ploting the on-time performance by airline
background = ggplot(otperformal,aes(as.factor(otperformal$Group.1),otperformal$ontimerate))
background2 = background + geom_bar(stat="identity") + ggtitle("On-time Performance by Airlines")

#Determining the hubs
airportori = levels(airdataclean$ORIGIN)
airportdest = levels(airdataclean$DEST)
airport = unique(airportori,airportdest)
freqori = sapply(1:352,function(i){
  x = subset(airdataclean,airdataclean$ORIGIN == airportori[i])
  nrow(x)
})
freqdest = sapply(1:350,function(i){
  y = subset(airdataclean,airdataclean$DEST == airportdest[i])
  nrow(y)
})
freqori = cbind(airportori,freqori)
freqdest = cbind(airportdest,freqdest)
freq = merge(freqori,freqdest,by.x = c("airportori"),by.y = c("airportdest"))
freq$freqori = as.numeric(as.character(freq$freqori))
freq$freqdest = as.numeric(as.character(freq$freqdest))
freq$total = freq$freqori + freq$freqdest
freq = freq[order(freq$total,decreasing = T),]
rownames(freq) = 1:347

top20airport = head(freq,20)
#Plot the top 20 frequent departing airport
ggplot(top20airport,aes(top20airport$airport,top20airport$total)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_bar(stat="identity") + ggtitle("Frequency of Flights by Airport")

#Unique air schedule
airschedule = unique(airdataclean[,c("ORIGIN","DEST")])

#I am going to subset the data base on freq of flights.
#I am going to cut off at 20 flights leaving or landing per day.
#Which will be on average having a freq of 365 in this sample.
freqsub = subset(freq,freq$total >= 365)
# I am also going to subset the data for airdataclean.
# Since flights from a non-popular airport always links to a popular airport, I exclude them from both ORIGIN and DEST.
airdatasub = airdataclean[airdataclean$DEST %in% freqsub$airportori,]
airdatasub = airdatasub[airdatasub$ORIGIN %in% freqsub$airportori,]

#Logistic Model
set.seed(123)
index = sample(1:nrow(airdataclean),size = 0.8*(nrow(airdataclean)))
train = airdatasub[index,]
test = airdatasub[-index,]
fit = glm(ARR_DEL15~MONTH+UNIQUE_CARRIER+ORIGIN+DEST+DISTANCE,family = binomial(link = "logit"),data = train)
predict = predict(fit,test,type = "response")
test$prediction = predict

#Random Forrest
attach(airdatasub)
featureair = data.frame(ARR_DEL15,CARRIER,MONTH)
set.seed(123)
index = sample(1:nrow(airdataclean),size = 0.8*(nrow(airdataclean)))
trainrf = featureair[index,]
trainrf = trainrf[complete.cases(trainrf),]
testrf = featureair[-index,]
rf = randomForest(trainrf,as.factor(trainrf$ARR_DEL15),ntree = 100, importance = T)
predictrf = predict(rf,testrf)
testrf$predict = predictrf
#Writing a function to see how much % of right prediction that we are getting.
testrf$match = sapply(1:nrow(testrf),function(i){
  ifelse(testrf$ARR_DEL15[i] == testrf$predict[i],1,0)
})
sum(testrf$match)

#How accurate is the logistic model
mean(test$prediction)
qt80 = quantile(test$prediction,0.8)
test$bpredict = sapply(1:nrow(test),function(i){
  ifelse(test$prediction[i] >= 0.2404393,1,0)
})
test$match = sapply(1:nrow(test),function(i){
  ifelse(test$ARR_DEL15[i] == test$bpredict[i],1,0)
})
mean(subtest$match)
test$zero = rep(0,nrow(test))
test$match = sapply(1:nrow(test),function(i){
  ifelse(test$ARR_DEL15[i] == test$zero[i],1,0)
})
subtest = subset(test,test$ARR_DEL15 == 1)
subtest$match = sapply(1:nrow(subtest),function(i){
  ifelse(subtest$ARR_DEL15[i] == subtest$bpredict[i],1,0)
})
