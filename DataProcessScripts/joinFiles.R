setwd("/home/kavi/Desktop/Windows/FlightData/")
set.seed(1234)
library(foreach)
directories = list.files()
df = foreach(dir = directories, .combine = rbind) %do% {
  setwd(paste0("/home/kavi/Desktop/Windows/FlightData/",dir))
  files = list.files()
  data = foreach(f = files, .combine = rbind) %do% {
    flightData = read.csv(f)
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
    print(paste("file", f, "completed"))
    flightData
  }
  data
}
#Get top N hubs
flightHubs = intersect(names(sort(table(df$ORIGIN)/nrow(df),decreasing = T)[1:20]),
                       names(sort(table(df$DEST)/nrow(df),decreasing = T)[1:20]))
flightDataSub = df[df$ORIGIN %in% flightHubs & df$DEST %in% flightHubs, ]
save(df, file = "flightData1516")
save(flightDataSub, file = 'flightData')
