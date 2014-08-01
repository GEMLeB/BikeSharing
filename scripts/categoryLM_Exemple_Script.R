#setwd('C:/Development/01-kaggel/01-Bicing/BikeSharing/scripts')
source('./general/kaggle_loss.R')
source('./general/random_cv.R')
source('./model/categoryLM_model.R')
source('./transform/completeTime_v3_transform.R')
source('./transform/loadData_transform.R')

#funcio transform----

loadData_transform()

datosAll <- completeTime_v3_transform(datosAll)

indAllTrain <- which(datosAll$day <  20)
indAllTest  <- which(datosAll$day >= 20)

datosTrain <- datosAll[indAllTrain, ]
datosTest  <- datosAll[indAllTest, ]

realValues <-  datosAll$count[indAllTest]

#-------------

          model <- categoryLM_model(datosTrain)
predictedValues <- categoryLinear_predict(model, datosTest)
kaggle_loss(realValues, predictedValues) # 0.3490176



model <- categoryLM_model(datosTrain,optL = list(catVariables = .(season, hour, workingday, year),
                                                  lmVariables = .(count_lag_1, count_lag_24)))
predictedValues <- categoryLinear_predict(model, datosTest)
kaggle_loss(realValues, predictedValues)



