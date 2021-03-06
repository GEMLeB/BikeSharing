## Kaggle's competition loss function

kaggle_loss <- function(realValues, predictedValues){
  sqrt( mean( na.omit( ( log( realValues + 1 ) - log( predictedValues + 1 ) )^2 ) ) )
}
