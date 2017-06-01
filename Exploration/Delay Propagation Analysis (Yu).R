library(ggplot2)
library(ggmap)
library(maps)
library(animation)
library(plotly)
library(dplyr)

airdata1612 = read.csv("On_Time_On_Time_Performance_2016_12.csv")
airports = read.csv("airport.csv")

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
airdatadec17 = subset(airdata1612,airdata1612$DayofMonth == "15")
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

#Locating the congested airports on DEC-17.
datasub = subset(airdata1612,airdata1612$DayofMonth == "17")
delaytime = aggregate(datasub$ArrDelayMinutes,list(datasub$Origin,datasub$DepTimeBlk),mean,na.rm = T)
delaytime$congested = sapply(1:nrow(delaytime),function(i){
  ifelse(delaytime$x[i] >= 30,1,0)
})
#Determine clusters if congested airports have direct routes connecting together.
#Animation of the Congested Routes
delay_airport = lapply(1:19,function(i){
  subset(delaytime,delaytime$Group.2 == levels(delaytime$Group.2)[i])
})
routes = unique(airdatadec17[,c("Origin","Dest","DepTimeBlk")])
rownames(routes) = 1:nrow(routes)
routes = lapply(1:19,function(i){
  subset(routes,routes$DepTimeBlk == levels(routes$DepTimeBlk)[i])
})
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
airinfo = lapply(1:19,function(i){
  merge(airports,delay_airport[[i]],by.x = c("iata_code"),by.y = c("Group.1"))
})
route1 = lapply(1:19,function(i){
  merge(routes[[i]],airinfo[[i]],by.x = c("Origin","DepTimeBlk"),by.y = c("iata_code","Group.2"))
})
route2 = lapply(1:19,function(i){
  merge(route1[[i]],airinfo[[i]],by.x = c("Dest","DepTimeBlk"),by.y = c("iata_code","Group.2"))
})
airinfo = airinfo %>% bind_rows
airinfo = merge(airinfo,freq_fight,by.x = c("iata_code"),by.y = c("Group.1"))
route2 = route2 %>% bind_rows
route2$congested = sapply(1:nrow(route2),function(i){
  ifelse(route2$congested.x[i] == "1" && route2$congested.y[i] == "1",1,0)
})
route2$id = seq_len(nrow(route2))


plot_geo(locationmode = "USA-states") %>%
  add_markers(
    data = airinfo, x = ~longitude_deg, y = ~latitude_deg, text = ~name, frame = ~airinfo$Group.2,
    size = ~airinfo$x.y, hoverinfo = "text", alpha = 0.5, color = ~congested, colors = c("green","red")
  ) %>% 
  layout(
    title = paste("Congested Routes"), geo = geo, showlegend = FALSE
  ) %>%
  add_segments(
    data = group_by(route2,id), frame = ~DepTimeBlk,
    x = ~longitude_deg.x, xend = ~longitude_deg.y,
    y = ~latitude_deg.x, yend = ~latitude_deg.y,
    alpha = 0.3, size = I(1), color = factor(route2$congested)
  ) %>%
  animation_opts(1000,easing = "elastic",redraw = T
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Time Block ", font = list(color="Black"))
  )





#Finding clusters
#First subset the data.
datasub = subset(airdata1612,airdata1612$DayofMonth == "17")
delaytime = aggregate(datasub$ArrDelayMinutes,list(datasub$Origin,datasub$DepTimeBlk),mean,na.rm = T)
delaytime$congested = sapply(1:nrow(delaytime),function(i){
  ifelse(delaytime$x[i] >= 30,1,0)
})
congested_airport = subset(delaytime,delaytime$congested == 1)
congestedcounted = aggregate(congested_airport$congested,list(congested_airport$Group.2),length)

#Let me try igraph
library(igraph)
delay_airport = lapply(1:19,function(i){
  subset(delaytime,delaytime$Group.2 == levels(delaytime$Group.2)[i])
})
delay_airport[[1]]$Group.1 = factor(delay_airport[[1]]$Group.1)
routes = unique(airdatadec17[,c("Origin","Dest","DepTimeBlk")])
rownames(routes) = 1:nrow(routes)
routes = lapply(1:19,function(i){
  subset(routes,routes$DepTimeBlk == levels(routes$DepTimeBlk)[i])
})

cluster = sapply(1:19,function(i){
  routes[[i]]$Origin = factor(routes[[i]]$Origin)
  routes[[i]]$Dest = factor(routes[[i]]$Dest)
  airports = data.frame(name = delay_airport[[i]]$Group.1,congested = delay_airport[[i]]$congested)
  paths = data.frame(from = routes[[i]]$Origin, to = routes[[i]]$Dest)
  paths = paths[which(paths[,2] %in% airports$name),]
  paths = paths[which(paths[,1] %in% airports$name),]
  g = graph_from_data_frame(paths,vertices = airports)
  V(g)$color = ifelse(airports$congested == 1,"red","green")
  g = induced.subgraph(g,V(g)[V(g)$color %in% c("red")])
  plot(g)
  max(clusters(g)$csize)
})

#######
routes[[1]]$Origin = factor(routes[[1]]$Origin)
routes[[1]]$Dest = factor(routes[[1]]$Dest)
airports = data.frame(name = delay_airport[[1]]$Group.1,congested = delay_airport[[1]]$congested)
paths = data.frame(from = routes[[1]]$Origin, to = routes[[1]]$Dest)
paths = paths[which(paths[,2] %in% airports$name),]
paths = paths[which(paths[,1] %in% airports$name),]
g = graph_from_data_frame(paths,vertices = airports)
V(g)$color = ifelse(airports$congested == 1,"red","green")
g = induced.subgraph(g,V(g)[V(g)$color %in% c("red")])
plot(g)
max(clusters(g)$csize)





