## We complete the data set with expliciting time information

require(lubridate)

completeTime_v2_transform <- function(bikeData){
  bikeData$datetime <- ymd_hms(bikeData$datetime)
  bikeData$date <- as.Date(bikeData$datetime)
  bikeData$year <- year(bikeData$datetime)
  bikeData$month <- month(bikeData$datetime)
  bikeData$day <- day(bikeData$datetime)
  bikeData$hour <- hour(bikeData$datetime)
  bikeData$weekDay <- wday(bikeData$datetime)
  
  return(bikeData)
}
