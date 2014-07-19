# Basic script to show how to load data, and use loss function using UCI data

source('./general/kaggle_loss.R')
source('./general/random_cv.R')
source('./model/categoryMeans_model.R')
source('./transform/completeTime_transform.R')
source('./transform/completeTime_v2_transform.R')
source('./transform/loadData_transform.R')

loadData_transform()
datosAll <- completeTime_v2_transform(datosAll)
datosTrain <- completeTime_v2_transform(datosTrain)
datosTest <- completeTime_v2_transform(datosTest)

indAllTrain <- which(datosAll$day<20)
indAllTest <- which(datosAll$day>=20)

# Checking UCI and Kaggle data  ----
head(datosAll)
head(datosTrain)

# Checking number of registers between UCI and Kaggle datar  ----
nrow(datosAll) == nrow(datosTrain) + nrow(datosTest)
length(indAllTrain) == nrow(datosTrain)
length(indAllTest) == nrow(datosTest)

# Checking values between UCI and Kaggle datar  ----
model1 <- mean(datosTrain$count)
model2 <- mean(datosAll$count[indAllTrain])
model1 == model2

# Checking mean benchmark model of Kaggle Leaderboard  ----
model <- mean(datosTrain$count)
predictedValues <- rep(model, length(indAllTest))
realValues <-  datosAll$count[indAllTest]
kaggle_loss(realValues, predictedValues)

# Creating new benchmark using binsMean model (categoryMeans_mode)  ----
model <- categoryMeans_model(datosTrain)
predictedValues <- categoryMeans_predict(datosTest, model)
realValues <-  datosAll$count[indAllTest]
kaggle_loss(realValues, predictedValues)


