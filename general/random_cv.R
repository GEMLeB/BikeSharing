## This cross validation splits the data set randomly
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

random_cv <- function(do_model, do_prediction, do_transform, bikeData, optL = list(kFold = 5)){
  
  lengthSplit <- floor( nrow(bikeData)/optL$kFold )
  
  sapply(1:optL$kFold, function(cvIter){
    trainIndex <- sample(1:nrow(bikeData), size = lengthSplit, replace = FALSE)
    trainSet <- bikeData[ trainIndex, ]
    testSet <- bikeData[ -trainIndex, ]
    
    trainSet <- executeTransformation( do_transform, trainSet, optL ) 
    ## this is important to be done here instead out of the loop to ensure the good working of the cross valdation (not using information of the test set)
    newModel <- do_model( trainSet, optL )
    
    testSet <- executeTransformation( do_transform, testSet, optL )
    newPredictions <- do_prediction( testSet, newModel )
    realValues <- testSet$count
    
    return( kaggle_loss( realValues, newPredictions ) )
  })
}