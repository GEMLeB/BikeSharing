

require(plyr)

categoryLM_model <- function(data, optL = list(target = "count", 
                                               catVariables = .(season, hour, workingday, year),
                                               lmVariables = .(count_lag_1, count_lag_24)  )) 
{
        if (is.null(optL$target)) optL$target = "count"
        
        categoryPlusLMModel <- dlply(data, optL$catVariables, function(splitDataSet)
                        {
                                lmformulalist <- paste(optL$lmVariables , sep = " " , collapse = '+')
                
                                bikeFormula <- formula(paste(optL$target ,'~', lmformulalist , sep = ""))
                
                                lm(bikeFormula , splitDataSet)
                        })
        
         categoryPlusLMModel$catVariables<-optL$catVariables

        return(categoryPlusLMModel)
}


categoryLinear_predict <- function(historicModel,toPredictionBikeData)
{
        predictedValues <- vector (mode="numeric",length = length(toPredictionBikeData))

        Ncategorys <- length(historicModel$catVariables)

        for(iRow in 1:length(toPredictionBikeData[,1]))
        {
                catName <- as.character(historicModel$catVariables[[1]])
                
                label <- toPredictionBikeData[iRow,catName]
                
                for(icate in 2:Ncategorys)
                {
                        catName <- as.character(historicModel$catVariables[[icate]])
                        label <- paste(label,toPredictionBikeData[iRow,catName],sep=".")
                }
                
                predictedValues[iRow] <- predict.lm(historicModel[[label]],toPredictionBikeData[iRow,])[1]
        }
        
        predictedValues <- pmax(predictedValues , 0) 
        
         predictedValues <- ceiling(predictedValues) 

        predictedValues
}


