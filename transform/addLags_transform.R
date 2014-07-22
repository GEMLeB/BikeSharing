## This function adds time lags as columns

addLags_transform <- function(bikeData, optL = list(lagsTotal = 1)){
  if( optL$lagsTotal > 0 ){
    firstTime <- min(bikeData$datetime)
    lastTime <- max(bikeData$datetime)
    
    dateSeq <- seq(firstTime, lastTime, by = '1 hour')
    dateSeq <- data.frame(datetime = dateSeq)
    
    completeDataTb <- merge(dateSeq, bikeData, all.x = TRUE)
    lengthData <- nrow(completeDataTb)
    dataLags <- sapply(1:optL$lagsTotal, function(lagN){
      remRows <- lengthData:(lengthData - lagN + 1)
      c(rep(NA, lagN), completeDataTb$count[-remRows])
    })
    colnames(dataLags) <- paste("lag", 1:optL$lagsTotal, sep ="_")
    
    completeDataTb <- cbind(completeDataTb, dataLags)
    return(na.omit(completeDataTb))
  }else{
    return(bikeData)
  }
}