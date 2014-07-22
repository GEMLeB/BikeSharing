## This cross validation rolles over months: 9 months training, 3 testing
require(lubridate)

executeTransformation <- function(do_transform, dataSet, optL){
  if( class( do_transform ) == 'list' ){
    for( i in 1:length(do_transform)){
      dataSet <- do_transform[[i]](dataSet, optL)
    }
    return(dataSet)
  }else{
    return( do_transform(dataSet) )
  }
}


extendMonths_transform <- function(dataSet){
  datetimeCol <- ymd_hms(dataSet$datetime)
  yearCol <- year(dataSet$datetime)
  monthCol <- month(dataSet$datetime)
  dataSet$extMonth <- monthCol + 12*(as.numeric(yearCol) - 2011)
  return(dataSet)
}

rolling_cv <- function(do_model, do_prediction, do_transform, bikeData, 
                       optL = list()){
 
  trainMonths = 12
  testMonths = 3
  loopMonths = 3
  baseTotal = 4

  bikeData <- extendMonths_transform( bikeData )
  
  sapply(1:baseTotal, function(cvIter){
    indTrain <- sapply(bikeData$extMonth, function(obsMonth){
      as.numeric(obsMonth) %in% ((cvIter-1)*3 + 1:trainMonths)
    })
    indTest <-  sapply(bikeData$extMonth, function(obsMonth){
      as.numeric(obsMonth) %in% ((cvIter-1)*3 + trainMonths + 1:testMonths)
    })
    
    trainSet <- bikeData[ indTrain, ]
    testSet <- bikeData[ indTest, ]
    
    trainSet <- executeTransformation( do_transform, trainSet, optL ) 
    newModel <- do_model( trainSet, optL )
    
    testSet <- executeTransformation( do_transform, testSet, optL )
    newPredictions <- do_prediction( testSet, newModel )
    realValues <- testSet$count
    
    return( kaggle_loss( realValues, newPredictions ) )    
  })
  
}
  
  