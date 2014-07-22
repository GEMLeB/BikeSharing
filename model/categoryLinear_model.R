### This model splits by categories in variables and computes a linear model
## Needs addDaysFromBeginning and completeTime transformation 

require(plyr)

categoryLinear_model <- function(historicBikeData, optL = list()){
  categoryLinear <- dlply(historicBikeData, .(season, hour, workingday), function(splitDataSet){
#   bikeFormula <- count~ temp + windspeed + daysFromBeginning  

    if( optL$lagsTotal > 0 ){
      lagsTxt <- paste('lag', 1:optL$lagsTotal, collapse = '+', sep ="_")
      bikeFormula <- formula(paste('count~daysFromBeginning + ', lagsTxt, sep = ""))
    }else{
      bikeFormula <- count~ daysFromBeginning  
    }
    
    lm( bikeFormula, data = splitDataSet )
#     mean(splitDataSet$count)
  })
  
  return(categoryLinear)
}

categoryLinear_predict <- function(toPredictionBikeData, historicModel){
  toPredictionBikeData$label <- 1:nrow(toPredictionBikeData)
  predictionValue <- dlply(toPredictionBikeData, .(label), function(rowData){
    obsSeason <- rowData['season']
#     obsMonth <- rowData['season']
    obsHour <- as.character(as.numeric(rowData['hour']))
    obsWorkingDay <- rowData['workingday']
#     obsYear <- rowData['year']
    
#     selectIndex <- paste(obsSeason, obsHour, obsWorkingDay, obsYear, sep =".")
    selectIndex <- paste(obsSeason, obsHour, obsWorkingDay, sep =".")
    foundCategory <- ( !is.null(historicModel[[selectIndex]]) )
    if( foundCategory ){
      linearValue <- predict(historicModel[[selectIndex]], rowData)
      linearValue <- max(linearValue, 0)
#       linearValue <- historicModel[[selectIndex]]
    }else{
      linearValue <- NA
    }
  })

  predictionValue <- do.call(rbind, predictionValue)

  return(predictionValue)
}