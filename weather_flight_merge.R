flightData = read.csv("/home/kavi/Desktop/Windows/FlightData/FD201301.csv")
index = sample(1:nrow(flightData),round(nrow(flightData)*.05), replace = FALSE)
flightData = flightData[index,]

#How to treat data with no departure delay information
flightData = flightData[!is.na(flightData$DEP_DEL15) & !is.na(flightData$ARR_DEL15),]
#Get hour info
flightData$DEP_HOUR = sapply(flightData$DEP_TIME, function(t) substr(t, 1, nchar(t)-2))
flightData$DEP_HOUR[flightData$DEP_HOUR == ""] = 0
flightData$DEP_HOUR[flightData$DEP_HOUR == 24] = 0

flightData$ARR_HOUR = sapply(flightData$ARR_TIME, function(t) substr(t, 1, nchar(t)-2))
flightData$ARR_HOUR[flightData$ARR_HOUR == ""] = 0
flightData$ARR_HOUR[flightData$ARR_HOUR == 24] = 0


#Get top 20
flightHubs = intersect(names(sort(table(flightData$ORIGIN)/nrow(flightData),decreasing = T)[1:20]),
names(sort(table(flightData$DEST)/nrow(flightData),decreasing = T)[1:20]))
flightDataSub = flightData[flightData$ORIGIN %in% flightHubs & flightData$DEST %in% flightHubs, ]


library(readr)
setwd("/home/kavi/Desktop/Courses/STA 160/weatherData")
file = list.files()
library(foreach)
result = foreach(f = file, .combine = rbind) %do% {
  data = read.table(f,
                   header = FALSE, 
                   col.names = c("Year","Month","Day","Hour",
                                 "Temp","DewPoint","SeaLevelPres",
                                 "WindDir","WindSpeed","SkyCond","LiquidPrecip1Hr","LiquidPrecip6Hr"))
  
  data$location = f
  data
}

library(sqldf)
finaldf  = sqldf("
  SELECT fd.YEAR, fd.MONTH, fd.DAY_OF_MONTH, fd.DAY_OF_WEEK,fd.CARRIER,fd.ORIGIN, fd.ORIGIN_CITY_NAME,
  fd.ORIGIN_STATE_ABR, fd.DEST, fd.DEST_CITY_NAME, fd.DEST_STATE_ABR, fd.CRS_DEP_TIME, fd.DEP_TIME, fd.DEP_DELAY,
  fd.DEP_DEL15, fd.CRS_ARR_TIME, fd.ARR_TIME, fd.ARR_DELAY, fd.ARR_DEL15, fd.CANCELLED, 
  fd.DIVERTED, fd.CRS_ELAPSED_TIME, fd.ACTUAL_ELAPSED_TIME, fd.AIR_TIME, fd.DISTANCE, fd.CARRIER_DELAY,
  fd.WEATHER_DELAY,fd.NAS_DELAY, fd.SECURITY_DELAY, fd.LATE_AIRCRAFT_DELAY, fd.DEP_HOUR, fd.ARR_HOUR,
  dw.DewPoint AS dep_DewPoint, dw.SeaLevelPres AS dep_SeaLevelPres, dw.WindSpeed AS dep_WindSpeed,
  dw.SkyCond AS dep_SkyCond,  aw.DewPoint AS arr_DewPoint, aw.SeaLevelPres AS arr_SeaLevelPres, aw.WindSpeed AS arr_WindSpeed,
  aw.SkyCond AS arr_SkyCond 
  FROM flightDataSub fd LEFT JOIN result dw ON fd.MONTH = dw.Month AND fd.DAY_OF_MONTH = dw.Day 
          AND fd.DEP_HOUR = dw.HOUR AND fd.ORIGIN = dw.location LEFT JOIN result aw ON fd.MONTH = aw.Month AND fd.DAY_OF_MONTH = aw.Day 
          AND fd.DEP_HOUR = aw.HOUR AND fd.DEST = aw.location WHERE fd.DEST != fd.ORIGIN;")

names(finaldf)
