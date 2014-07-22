### This model splits by categories in variables and computes the mean

require(plyr)

categoryLogMeans_model <- function(historicBikeData, optL = list()){
  categoryModel <- ddply(historicBikeData, .(season, hour, workingday, year), function(splitDataSet){
    logValues <- log(splitDataSet$count + 1)
    logMean <- mean(logValues)
    exp(logMean) - 1
  })
  lastCol <- ncol(categoryModel)
  colnames(categoryModel)[lastCol] <- "mean"
  
  return(categoryModel)
}

categoryLogMeans_predict <- function(toPredictionBikeData, historicModel){
  apply(toPredictionBikeData, 1, function(rowData){
    obsSeason <- as.numeric(rowData['season'])
    obsHour <- as.numeric(rowData['hour'])
    obsWorkingDay <- as.numeric(rowData['workingday'])
        obsYear <- as.numeric(rowData['year'])
    
    selecIndex <- historicModel$season == obsSeason & historicModel$hour == obsHour & historicModel$workingday == obsWorkingDay & historicModel$year == obsYear
    foundCategory <- ( sum(selecIndex) > 0 )
    if( foundCategory ){
      predictionValue <- historicModel$mean[selecIndex]
    }else{
      predictionValue <- NA
    }
  })
}