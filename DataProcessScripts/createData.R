#Airport hubs
large_hubs = c('ATL','LAX','ORD','DFW','JFK','DEN','SFO','CLT','LAS','PHX','MIA',
               'IAH','SEA','MCO', 'EWR','MSP','BOS','DTW','PHL','LGA','FLL','BWI',
               'DCA','MDW','SLC','IAD','SAN','HNL','TPA','PDX')
medium_hubs = c('DAL', 'STL', 'HOU', 'AUS', 'BNA', 'OAK', 'MSY', 'MCI',
                'RDU','SNA','SJC','SMF','SJU','RSW','SAT','CLE',
                'PIT', 'IND', 'CMH', 'MKE', 'OGG','PBI', 'CVG','BDL','JAX','ANC',
                'BUF','ABQ','ONT','OMA')

allAirports = c(large_hubs, medium_hubs)

read_data_from_BTS = function(datadir){
  data = read.csv(datadir, stringsAsFactors = FALSE)
  colWanted = c("Year","Quarter","Month","DayofMonth","DayOfWeek","FlightDate",
                "UniqueCarrier","TailNum", "Origin","OriginCityName","OriginState",
                "Dest","DestCityName","DestState","DepTime","CRSDepTime","DepDelay",
                "DepDelayMinutes","DepDel15","DepTimeBlk","TaxiOut","WheelsOff","WheelsOn",
                "TaxiIn","CRSArrTime","ArrTime","ArrDelay","ArrDelayMinutes","ArrDel15",
                "ArrTimeBlk","CRSElapsedTime","ActualElapsedTime","Distance","DistanceGroup","Cancelled","CancellationCode",
                "CarrierDelay","WeatherDelay","NASDelay","SecurityDelay","LateAircraftDelay","Diverted")
  data = data[,colWanted]
  
  data = data[data$Origin %in% allAirports & 
            data$Dest %in% allAirports,]
  data
}

#Identify the dates of the storm 
df201210 = read_data_from_BTS("/home/kavi/Desktop/Windows/FlightData/FD201210.csv")
df201211 = read_data_from_BTS("/home/kavi/Desktop/Windows/FlightData/FD201211.csv")
hurricanedf = rbind(df201210[df201210$DayofMonth %in% c(21:27,28:31),], df201211[df201211$DayofMonth %in% 1:3,])

#Use Python Code to get weather information for prespecified airline hubs. 
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
#Merge weather status 
weatherdf  = sqldf("
                   SELECT fd.YEAR, fd.MONTH, fd.DayofMonth, fd.DayOfWeek, fd.FlightDate, fd.UniqueCarrier,fd.ORIGIN, fd.OriginCityName,
                   fd.OriginState, fd.DEST, fd.DestCityName, fd.DestState, fd.CRSDepTime, fd.DepTime, fd.DepDelay, fd.DepTimeBlk, fd.ArrTimeBlk,
                   fd.DEPDEL15, fd.CRSArrTime, fd.ArrTime, fd.ArrDelay, fd.ARRDEL15, fd.CANCELLED, 
                   fd.DIVERTED, fd.CRSELAPSEDTIME, fd.ACTUALELAPSEDTIME, fd.DISTANCE, fd.CARRIERDELAY,
                   fd.WEATHERDELAY,fd.NASDELAY, fd.SECURITYDELAY, fd.LATEAIRCRAFTDELAY, fd.DEP_HOUR, fd.ARR_HOUR,
                   dw.DewPoint AS dep_DewPoint, dw.SeaLevelPres AS dep_SeaLevelPres, dw.WindSpeed AS dep_WindSpeed,
                   dw.SkyCond AS dep_SkyCond,  aw.DewPoint AS arr_DewPoint, aw.SeaLevelPres AS arr_SeaLevelPres, aw.WindSpeed AS arr_WindSpeed,
                   aw.SkyCond AS arr_SkyCond 
                   FROM hurricanedf fd LEFT JOIN result dw ON fd.YEAR = dw.Year AND fd.MONTH = dw.Month AND fd.DayofMonth = dw.Day 
                   AND fd.DEP_HOUR = dw.HOUR AND fd.ORIGIN = dw.location LEFT JOIN result aw ON fd.YEAR = aw.Year AND fd.MONTH = aw.Month AND fd.DayofMonth= aw.Day 
                   AND fd.ARR_HOUR = aw.HOUR AND fd.DEST = aw.location WHERE fd.DEST != fd.ORIGIN;")

#Merge GPS Location
airports = read.csv("/media/sf_Windows/FlightData/airports.dat",header = FALSE,
                    col.names = c("ID","Name","City","Country","IATA","ICAO",
                                  "Lat","Lon","Altitude","Timezone","DST","Tz","Type","Source"))
USairports = airports[airports$Country == 'United States',c("IATA", "Lat","Lon","Timezone")]
finaldf = merge(weatherdf, USairports, by.x = 'Origin', by.y = 'IATA', all.x = TRUE)
names(finaldf)[names(finaldf) %in% c('Lat','Lon','Timezone')] = paste("Origin",c('Lat','Lon','Timezone'))
finaldf = merge(finaldf, USairports, by.x = 'Dest', by.y = 'IATA', all.x = TRUE)
names(finaldf)[names(finaldf) %in% c('Lat','Lon','Timezone')] = paste("Dest",c('Lat','Lon','Timezone'))

#Fix timezone. Convert UTC offset to US timezone.
finaldf$`Dest Timezone` = factor(finaldf$`Dest Timezone`)
levels(finaldf$`Dest Timezone`) = c("US/Hawaii","US/Eastern","US/Central","US/Mountain","US/Pacific","US/Alaska")
finaldf$`Origin Timezone` = factor(finaldf$`Origin Timezone`)
levels(finaldf$`Origin Timezone`) = c("US/Hawaii","US/Eastern","US/Central","US/Mountain","US/Pacific","US/Alaska")

convertToEastern = function(row){
  if(substring(row[2],1,1) == " "){
    substring(row[2],1,1) = "0"
  }
  date = paste(row[1:2],collapse = " ")
  date = strptime(date, format="%Y-%m-%d %H%M", tz = as.character(row[3]))
  date = with_tz(date, tzone = 'US/Eastern')
  res = data.frame(Year = year(date),Month = month(date), Day = day(date), 
             DayOfWeek = weekdays(date), Hour = hour(date), Time = strftime(date, format="%H:%M"))
  return(res)
}

departure_date_standardize = apply(finaldf[,c("FlightDate","CRSDepTime", "Origin Timezone")] , 1, convertToEastern)
departure_date_standardize = do.call(rbind,departure_date_standardize)
finaldf[,c("Year", "Month","DayofMonth","DayOfWeek","DepHour","CRSDepTime")] = departure_date_standardize

arrival_date_standardize = apply(finaldf[,c("FlightDate","CRSArrTime","Dest Timezone")] , 1 ,convertToEastern)
arrival_date_standardize = do.call(rbind,arrival_date_standardize)
finaldf[,c("ArrHour","CRSArrTime")] = departure_date_standardize[,c("Hour","Time")]
