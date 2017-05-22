library(ggplot2)
library(ggmap)
library(maps)
library(animation)
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
attach(airdatadec17)
delaytime_airport = aggregate(airdatadec17$ArrDelayMinutes,list(airdatadec17$Origin),mean,na.rm = T)
delaytime_airport$congested = sapply(1:nrow(delaytime_airport),function(i){
  ifelse(delaytime_airport$x[i] >= 30,1,0)
})
#Read the airport location
airportloc = read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = F)
airportloc = airportloc[,c(5,7,8)]
delaytime_airport = merge(delaytime_airport,airportloc,by.x = c("Group.1"),by.y = c("V5"))
#Draw the airports on US map
ggplot() + geom_polygon( data=map_data("usa"), aes(x=long, y=lat, group = group),colour="black",fill = "white") +
  geom_point(data = delaytime_airport,aes(x = V8,y = V7,colour = factor(congested))) + coord_cartesian(xlim = c(-125,-70),ylim=c(20,50))
  
#Draw the animation plot based departure time block
animatedplot1 = saveGIF({for(i in 1:19){
    datasub = subset(airdatadec17,airdatadec17$DepTimeBlk == levels(DepTimeBlk)[i])
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

#Let pick a day that is not that congest.
airdatadec17 = subset(airdata1612,airdata1612$DayofMonth == "3")
attach(airdatadec17)
delaytime_airport = aggregate(airdatadec17$ArrDelayMinutes,list(airdatadec17$Origin),mean,na.rm = T)
delaytime_airport$congested = sapply(1:nrow(delaytime_airport),function(i){
  ifelse(delaytime_airport$x[i] >= 30,1,0)
})
delaytime_airport = merge(delaytime_airport,airportloc,by.x = c("Group.1"),by.y = c("V5"))

animatedplot1 = saveGIF({for(i in 1:19){
  datasub = subset(airdatadec17,airdatadec17$DepTimeBlk == levels(DepTimeBlk)[i])
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


#Import data for route and location of the airports
library(plotly)
library(dplyr)
routes = read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
air = merge(delaytime_airport,air,by.x = c("Group.1"),by.y = c("iata"))
air = air[c(1,2,3,6,7,8,10,11,12)]
aircg = air[c(1,3)]
routes = merge(routes,aircg,by.x = c("airport1"),by.y = c("Group.1"))
routes = merge(routes,aircg,by.x = c("airport2"),by.y = c("Group.1"))
routes$congested = sapply(1:nrow(routes),function(i){
  ifelse(routes$congested.x[i] == "1" && routes$congested.y[i] == "1",1,0)
})


#Determine clusters if congested airports have direct routes connecting together.
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95")
)
plot_geo(locationmode = "USA-states") %>%
  add_markers(
    data = air, x = ~long, y = ~lat, text = ~airport,
    size = ~cnt, hoverinfo = "text", alpha = 0.5, color = ~congested, colors = c("green","red")
  ) %>%
  add_segments(
    data = group_by(routes, id),
    x = ~start_lon, xend = ~end_lon,
    y = ~start_lat, yend = ~end_lat,
    alpha = 0.3, size = I(1), color = factor(routes$congested)
  ) %>%
  layout(
    title = "Congested Routes", geo = geo, showlegend = FALSE
  )

#Let make create a GIF with this version of maps.
saveGIF({for(i in 1:19){
  datasub = subset(airdatadec17,airdatadec17$DepTimeBlk == levels(DepTimeBlk)[i])
  delaytime = aggregate(datasub$ArrDelayMinutes,list(datasub$Origin),mean,na.rm = T)
  delaytime$congested = sapply(1:nrow(delaytime),function(i){
    ifelse(delaytime_airport$x[i] >= 30,1,0)
  })
  delaytime = merge(delaytime,airportloc,by.x = c("Group.1"),by.y = c("V5"))
  air1 = merge(delaytime,air,by.x = c("Group.1"),by.y = c("iata"))
  air1 = air1[c(1,2,3,6,7,8,10,11,12)]
  aircg = air1[c(1,3)]
  routes1 = merge(routes,aircg,by.x = c("airport1"),by.y = c("Group.1"))
  routes1 = merge(routes1,aircg,by.x = c("airport2"),by.y = c("Group.1"))
  routes1$congested = sapply(1:nrow(routes1),function(i){
    ifelse(routes1$congested.x[i] == "1" && routes1$congested.y[i] == "1",1,0)
  })
  routes1$id = seq_len(nrow(routes1))
  geo <- list(
    scope = 'north america',
    projection = list(type = 'azimuthal equal area'),
    showland = TRUE,
    landcolor = toRGB("gray95")
  )
  print(
  plot_geo(locationmode = "USA-states") %>%
    add_markers(
      data = air1, x = ~long, y = ~lat, text = ~airport,
      size = ~cnt, hoverinfo = "text", alpha = 0.5, color = ~congested, colors = c("green","red")
    ) %>%
    add_segments(
      data = group_by(routes1, id),
      x = ~start_lon, xend = ~end_lon,
      y = ~start_lat, yend = ~end_lat,
      alpha = 0.3, size = I(1), color = factor(routes1$congested)
    ) %>%
    layout(
      title = paste("Congested Routes Time: ", levels(DepTimeBlk)[i],sep = ""), geo = geo, showlegend = FALSE
    ))
}
})

#####
datasub = airdatadec17
delaytime = aggregate(datasub$ArrDelayMinutes,list(datasub$Origin,datasub$DepTimeBlk),mean,na.rm = T)
delaytime$congested = sapply(1:nrow(delaytime),function(i){
  ifelse(delaytime$x[i] >= 30,1,0)
})
delaytime = merge(delaytime,airportloc,by.x = c("Group.1"),by.y = c("V5"))
air1 = merge(delaytime,air,by.x = c("Group.1"),by.y = c("iata"))
routes1 = merge(routes,air1,by.x = c("airport1"),by.y = c("Group.1"))
routes1 = merge(routes1,air1,by.x = c("airport2"),by.y = c("Group.1"))
routes1$congested = sapply(1:nrow(routes1),function(i){
  ifelse(routes1$congested.x[i] == "1" && routes1$congested.y[i] == "1",1,0)
})
routes1$id = seq_len(nrow(routes1))
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95")
)

  plot_geo(locationmode = "USA-states") %>%
    add_markers(
      data = air1, x = ~long, y = ~lat, text = ~airport,
      size = ~cnt, hoverinfo = "text", alpha = 0.5, color = ~congested, colors = c("green","red")
    ) %>%
    add_segments(
      data = group_by(routes1, id),
      x = ~start_lon, xend = ~end_lon,
      y = ~start_lat, yend = ~end_lat,
      alpha = 0.3, size = I(1), color = factor(routes1$congested)
    ) %>%
    layout(
      title = paste("Congested Routes"), geo = geo, showlegend = FALSE
    )
