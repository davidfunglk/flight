load("/home/kavi/Desktop/Courses/STA 160/flightData")

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
  
  data$location = substr(f,1,3)
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
  FROM flightDataSub fd LEFT JOIN result dw ON fd.YEAR = dw.Year AND fd.MONTH = dw.Month AND fd.DAY_OF_MONTH = dw.Day 
          AND fd.DEP_HOUR = dw.HOUR AND fd.ORIGIN = dw.location LEFT JOIN result aw ON fd.YEAR = aw.Year AND fd.MONTH = aw.Month AND fd.DAY_OF_MONTH = aw.Day 
          AND fd.ARR_HOUR = aw.HOUR AND fd.DEST = aw.location WHERE fd.DEST != fd.ORIGIN;")
