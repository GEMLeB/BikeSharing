## Adds days from the beginning. 
# Needs completeTime_transformation 

addDaysFromBeginning_transform <- function( bikeData, optL = list() ){
  dayOfYear <- as.numeric(format(bikeData$day, "%j"))
  bikeData$daysFromBeginning <- ifelse(bikeData$year == 2011, dayOfYear, dayOfYear + 365 )
  
  return(bikeData)
}
