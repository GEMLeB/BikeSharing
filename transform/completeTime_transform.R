## We complete the data set with expliciting time information

completeTime_transform <- function(bikeData){
  bikeData$datetime <- ymd_hms(bikeData$datetime)
  bikeData$day <- as.Date(bikeData$datetime)
  bikeData$hour <- hour(bikeData$datetime)
  bikeData$month <- month(bikeData$datetime)
  bikeData$weekDay <- wday(bikeData$datetime)
  bikeData$year <- year(bikeData$datetime)
  
  return(bikeData)
}
