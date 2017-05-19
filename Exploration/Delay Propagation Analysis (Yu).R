library(ggplot2)
library(ggmap)
library(maps)
library(animation)
library(gganimate)
airdata1612 = read.csv("On_Time_On_Time_Performance_2016_12.csv")

attach(airdata1612)
#Subset the data with top airports in terms of frequencys
freq_fight = aggregate(airdata1612$ArrDelay,list(airdata1612$Origin),length)
freq_fight = freq_fight[order(-freq_fight$x),]
rownames(freq_fight) = 1:nrow(freq_fight)
topfreq_fight = head(freq_fight,100)
airdata1612sub = airdata1612[airdata1612$Origin %in% topfreq_fight$Group.1,]
attach(airdata1612sub)
#Top and low average delay date
meandelay = aggregate(airdata1612$ArrDelay,list(airdata1612$FlightDate),mean,na.rm = T)
meandelayminute = aggregate(airdata1612$ArrDelayMinutes,list(airdata1612$FlightDate),mean,na.rm = T)
meandelay = meandelay[order(-meandelay$x),]

# Frequency of Flights by Departure Time
flightbytime = aggregate(airdata1612$DepTimeBlk,list(airdata1612$DepTimeBlk),length)
ggplot(flightbytime,aes(flightbytime$Group.1,flightbytime$x)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_bar(stat="identity") + ggtitle("Frequency of Flights by Departure Time")

delaybytime = aggregate(airdata1612$ArrDel15,list(airdata1612$DepTimeBlk),mean,na.rm = T)
ggplot(delaybytime,aes(delaybytime$Group.1,delaybytime$x,group = 1)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_line() + geom_point() + ggtitle("Delay Rate of Flights by Departure Time")

#Locating the congested airports on DEC-17.
airdatadec17 = subset(airdata1612,airdata1612$DayofMonth == "17")
delaytime_airport = aggregate(airdatadec17$ArrDelayMinutes,list(airdatadec17$Origin),mean,na.rm = T)
delaytime_airport$congested = sapply(1:nrow(delaytime_airport),function(i){
  ifelse(delaytime_airport$x[i] >= 30,1,0)
})
#Read the airport location
airportloc = read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = F)
airportloc = airportloc[,c(5,7,8)]
delaytime_airport = merge(delaytime_airport,airportloc,by.x = c("Group.1"),by.y = c("V5"))
#Draw the airports on US map
map_data("usa")
ggplot() + geom_polygon( data=map_data("usa"), aes(x=long, y=lat, group = group),colour="black",fill = "white") +
  geom_point(data = delaytime_airport,aes(x = V8,y = V7,colour = factor(congested))) + coord_cartesian(xlim = c(-125,-70),ylim=c(20,50))
  
#Draw the animation plot based departure time block
animatedplot1 = saveGIF({for(i in 1:19){
    datasub = subset(airdata1612,airdata1612$DepTimeBlk == levels(DepTimeBlk)[i])
    delaytime = aggregate(datasub$ArrDelayMinutes,list(datasub$Origin),mean,na.rm = T)
    delaytime$congested = sapply(1:nrow(delaytime),function(i){
      ifelse(delaytime_airport$x[i] >= 30,1,0)
    })
    delaytime = merge(delaytime,airportloc,by.x = c("Group.1"),by.y = c("V5"))
    print(ggplot() + geom_polygon( data=map_data("usa"), aes(x=long, y=lat, group = group),colour="black",fill = "white") +
      geom_point(data = delaytime,aes(x = V8,y = V7,colour = factor(congested))) + 
        coord_cartesian(xlim = c(-125,-70),ylim=c(20,50)) + 
          ggtitle(paste("Time: ", levels(DepTimeBlk)[i],sep = "")))
  }
})
