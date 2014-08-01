## We complete the data set with expliciting time information
require(zoo)
require(lubridate)
require(dplyr)

completeTime_v3_transform <- function(bikeData){
  bikeData$datetime <- ymd_hms(bikeData$datetime)
  bikeData$date  <- as.Date(bikeData$datetime)
  bikeData$year  <- year(bikeData$datetime)
  bikeData$month <- month(bikeData$datetime)
  bikeData$day   <- day(bikeData$datetime)
  bikeData$hour  <- hour(bikeData$datetime)
  bikeData$weekDay <- wday(bikeData$datetime)
  
  bikeData <- bikeData %.% arrange(datetime)
  bikeData$count_lag_1  <- lag(bikeData$count, 1)
  bikeData$count_lag_24 <- lag(bikeData$count, 24)
  
  return(bikeData)
}
