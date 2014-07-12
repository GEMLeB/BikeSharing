## This cross validation splits the data set randomly
random_cv <- function(do_model, do_prediction, do_transform, bikeData, optL = list(kFold = 5)){
  
  lengthSplit <- floor( nrow(bikeData)/optL$kFold )
  
  sapply(1:optL$kFold, function(cvIter){
    trainIndex <- sample(1:nrow(bikeData), size = lengthSplit, replace = FALSE)
    trainSet <- bikeData[ trainIndex, ]
    testSet <- bikeData[ -trainIndex, ]
    
    trainSet <- do_transform( trainSet ) 
    ## this is important to be done here instead out of the loop to ensure the good working of the cross valdation (not using information of the test set)
    newModel <- do_model( trainSet, optL )
    
    testSet <- do_transform( testSet )
    newPredictions <- do_prediction( testSet, newModel )
    realValues <- testSet$count
    
    return( kaggle_loss( realValues, newPredictions ) )
  })
}