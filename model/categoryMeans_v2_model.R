### This model splits by categories in variables and computes the mean

require(plyr)

categoryMeans_v2_model <- function(data, optL = list(target="count",catVariables = .(season, hour, workingday, year))) {
  if (is.null(optL$target)) optL$target = "count"
  categoryModel <- ddply(data, optL$catVariables, function(splitDataSet){
    mean(splitDataSet[, optL$target])    
  })
  lastCol <- ncol(categoryModel)
  colnames(categoryModel)[lastCol] <- "mean"
  
  return(categoryModel)
}

categoryMeans_v2_predict <- function(data, model) {
  colNames = colnames(model)
  varNames = colNames[-length(colNames)]
  
  apply(data, 1, function(rowData) {
    ind = 1:nrow(model)
    for (var in varNames) {
      ind = intersect(ind, which(model[,var]==as.numeric(rowData[var])))
    }
    foundCategory <- ( length(ind) > 0 )
    if( foundCategory ) {
      predictionValue <- mean(model$mean[ind])
    } else {
      predictionValue <- NA
    }
  })
}