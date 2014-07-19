## We complete the data set with expliciting time information

require(lubridate)

loadData_transform <- function(dataFolder = './data/raw/'){
  datosAll <<- read.csv(paste0(dataFolder, 'hour.csv'), sep = ",", header=TRUE, stringsAsFactors=FALSE)
  datosTrain <<- read.csv(paste0(dataFolder, 'train.csv'), sep = ",", header=TRUE, stringsAsFactors=FALSE)
  datosTest <<- read.csv(paste0(dataFolder, 'test.csv'), sep = ",", header=TRUE, stringsAsFactors=FALSE)
  names(datosAll) <<- c("instant", "dateday", "season", "year", "month", "hour", "holiday", "weekday", "workingday", "weather", "temp", "atemp", "humidity", "windspeed", "casual", "registered", "count")
  datosAll$datetime <<- ymd_hms(paste0(datosAll$dateday, ' ', sprintf("%02.0f", datosAll$hour), ':00:00 UTC'))
  datosAll$temp <<- 41*datosAll$temp
  datosAll$atemp <<- 50*datosAll$atemp
  datosAll$humidity <<- 100*datosAll$humidity
  datosAll$windspeed <<- 67*datosAll$windspeed
  datosAll <<- datosAll[,c("datetime", "season", "holiday", "workingday", "weather", "temp", "atemp", "humidity", "windspeed", "casual", "registered", "count")]
}
