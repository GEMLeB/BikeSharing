#setwd('C:/Development/01-kaggel/01-Bicing/BikeSharing/scripts')
source('./general/kaggle_loss.R')
source('./general/random_cv.R')
source('./model/categoryLM_model.R')
source('./transform/completeTime_v2_transform.R')
source('./transform/loadData_transform.R')

#funcio transform----
require(lubridate)

loadData_transform()
datosAll <- completeTime_v2_transform(datosAll)
datosTrain <- completeTime_v2_transform(datosTrain)
datosTest <- completeTime_v2_transform(datosTest)

indAllTrain <- which(datosAll$day<20)
indAllTest <- which(datosAll$day>=20)

datosTrain <- datosAll[indAllTrain,]
datosTest <- datosAll[indAllTest,]

realValues <-  datosAll$count[indAllTest]

#-------------

model <- categoryLM_model (datosTrain)
predictedValues <- categoryLinear_predict (model,datosTest)
kaggle_loss(realValues, predictedValues)
