AtlanticHurricanes = read.csv("/media/sf_Windows/AtlanticHurricanes.csv", stringsAsFactors = FALSE)
recentHurricanes = AtlanticHurricanes[AtlanticHurricanes$Season >= 2005,]

#Map of regions affected by Atlantic Hurricanes since 2005
library(ggmap)
require(gridExtra)
state.map <- get_map("States", zoom = 3,source = "stamen", maptype = "toner")
plotData = recentHurricanes[recentHurricanes$Name %in% c("KATRINA","SANDY","IKE"),]
p1 = ggmap(state.map) + geom_point(data=plotData, mapping=aes(x=Longitude, y=Latitude, group = Name, color= Name), 
                                   size=2, alpha=0.6) + ggtitle("Storm Track")
p2 = ggmap(state.map) + 
  stat_density2d(data=recentHurricanes, mapping=aes(x=Longitude, y=Latitude, fill=..level..), geom="polygon", alpha=0.2) +
  scale_fill_gradient(low = "green", high = "red") + ggtitle("Hurricanes From 2005-2015")
par(mar=c(15,15,4,2)+0.1)#sets margins of plotting area
grid.arrange(p2, p1, ncol=2)

#Number of storms by year since 2005
tapply(recentHurricanes$Wind.WMO.,recentHurricanes$Season, function(storm) {
  max(storm)
})

#Months storm occur between  august to october.
dateTime = as.POSIXct(recentHurricanes$ISO_time, tz = "UTC")
library(lubridate)
recentHurricanes$month = month(dateTime)
tapply(recentHurricanes$Pres.WMO.,recentHurricanes$month, function(storm) {
 min(storm)
})

#Windspeed and atmospheric pressure
cor(recentHurricanes$Wind.WMO., recentHurricanes$Pres.WMO.)

#Katrina, Ike, Sandy
DT = as.POSIXct(plotData$ISO_time)
library(lubridate)
table(month(DT))
unique(date(DT))
#9 days for katrina
#15 days for IKE
#11 days for Sandy
setwd("/home/kavi/Desktop/Courses/STA 160/")
load("KISMerged.rda")

library(plotly)
#Aggregate % delay for each day.
#Subset to Central and Eastern US
KISSubset = KISMerged[KISMerged$`Origin Timezone` %in% c("US/Eastern","US/Central"),]
KISSubset = KISSubset[KISSubset$Origin %in% allAirports & 
           KISSubset$Dest %in% allAirports,]
table(KISSubset$Wind.WMO.)
KISSubset$DepDelay
#Split by storm and calculate cancellation proportion for each region
result = lapply(split(KISSubset[,c("DepDelay","Origin Timezone","UTCDayofMonth","Cancelled","DepDistanceToStorm","Origin")], 
                      list(KISSubset$Name,KISSubset$UTCDayofMonth)), function(day){
  flightsInOperationDay = length(day$Cancelled)
  total = foreach(airport = split(day, day$Origin), .combine = rbind) %do% {
    averageDistanceToStorm = mean(airport$DepDistanceToStorm, na.rm = TRUE)
    cancelledProp = sum(airport$Cancelled)/length(airport$Cancelled)*100
    averageDepDelay = mean(airport$DepDelay, na.rm = TRUE)
    data.frame(averageDistanceToStorm, cancelledProp,  'ORIGIN' = unique(airport$Origin), 
               'Timezone' = unique(airport$'Origin Timezone'),
               averageDepDelay,
               proportionFlights = length(airport$Cancelled))
  }
  total
})
library(reshape2)
result = result[!sapply(result, is.null)]
result = melt(result, id = c("averageDistanceToStorm","cancelledProp","ORIGIN", "Timezone", "proportionFlights", "averageDepDelay"))
stormName = foreach(word = strsplit(result$L1,'\\.'), .combine = rbind) %do% {
  data.frame("Name" = word[[1]], "DayofMonth" = word[[2]])
}
result$L1 = NULL 
result = cbind(result, stormName)
result$DayofMonthLabel = paste("Day",result$DayofMonth)
class(result$DayofMonth[result$Name == "KATRINA"])

max(result[result$Name == "KATRINA",]$cancelledProp)

p1 <- plot_ly(result[result$Name == "KATRINA",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
             type = 'scatter', mode = 'markers', color = ~DayofMonth,
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'))
p2 <- plot_ly(result[result$Name == "IKE",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
                 type = 'scatter', mode = 'markers', color = ~DayofMonth,
                 sizes = c(10, 50),
                 marker = list(opacity = 0.5, sizemode = 'diameter'))
p3 <- plot_ly(result[result$Name == "SANDY",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
              type = 'scatter', mode = 'markers', color = ~DayofMonth,
              sizes = c(10, 50),
              marker = list(opacity = 0.5, sizemode = 'diameter'))
p1 <- plot_ly(result[result$Name == "KATRINA",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
              type = 'scatter', mode = 'markers', size = ~averageDepDelay , color = ~DayofMonthLabel,
              sizes = c(10, 50),
              marker = list(opacity = 0.3, sizemode = 'diameter'),
              colors= "RdYlBu",
              hoverinfo = 'text',
              text = ~paste('Airport:', ORIGIN,
                            '<br> Day of Month:', DayofMonth,
                            '<br>%of Cancelled Flights:', cancelledProp,
                            '<br>Flights Scheduled for Airport:', proportionFlights,
                            '<br>Average Departure Delay:', averageDepDelay))  %>% 
  layout(title = 'Hurricane Katrina')
p2 <- plot_ly(result[result$Name == "SANDY",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
              type = 'scatter', mode = 'markers', size = ~averageDepDelay , color = ~DayofMonthLabel,
              sizes = c(10, 50),
              marker = list(opacity = 0.3, sizemode = 'diameter'),
              colors= "RdYlBu",
              hoverinfo = 'text',
              text = ~paste('Airport:', ORIGIN,
                            '<br>Day of Month:', DayofMonth,
                            '<br>% of Cancelled Flights:', cancelledProp,
                            '<br>Flights Scheduled for Airport:', proportionFlights,
                            '<br>Average Departure Delay:', averageDepDelay))  %>% 
  layout(title = 'Hurricane Sandy')
result$DayofMonthLabel = factor(result$DayofMonthLabel)
result$DayofMonthLabel =factor(result$DayofMonthLabel,levels(result$DayofMonthLabel)[c(1,8,18,21:26,2:7,9:17,19:20)])
p3 <- plot_ly(result[result$Name == "IKE",], x = ~averageDistanceToStorm, y = ~cancelledProp, 
              type = 'scatter', mode = 'markers', size = ~averageDepDelay , color = ~DayofMonthLabel,
              sizes = c(10, 50), colors= "PuBu",
              marker = list(opacity = 0.4, sizemode = 'diameter'),
              hoverinfo = 'text',
              text = ~paste('Airport:', ORIGIN,
                            '<br>Day of Month:', DayofMonth,
                            '<br>Percentage of Cancelled Flights:', cancelledProp,
                            '<br>Flights Scheduled for Airport:', proportionFlights,
                            '<br>Average Departure Delay:', averageDepDelay))  %>% 
  layout(title = 'Hurricane Ike')
p1
p2
p3
result[result$cancelledProp == 100 & result$Name == 'SANDY',]
