# Script created to check some data stuff. It is not supposed to be shared.
# This script only provids some basic charts of the data.
# This script is awfully coded and the author knows it.

if (!'devtools' %in% installed.packages()[,1]) install.packages('devtools')
if (!'rplot' %in% installed.packages()[,1]) install_github("rplot", "rocalabern")
if (!'rmodel' %in% installed.packages()[,1]) install_github("rmodel", "rocalabern")

# Load Data ----
llibrary('rplot')
library('rmodel')
require(sqldf)
require(lubridate)

source('./transform/completeTime_v2_transform.R')
source('./transform/loadData_transform.R')

loadData_transform()
datosAll <- completeTime_v2_transform(datosAll)
datosTrain <- completeTime_v2_transform(datosTrain)
datosTest <- completeTime_v2_transform(datosTest)

# Create daily data ----
datosTrainDate = data.frame(date=unique(datosTrain$date))
datosTrainDate = data.frame(date=sqldf("select date from datosTrain group by date order by date")$date)
datosTrainDate$season = sqldf("select date, avg(season) as season from datosTrain group by date")$season
datosTrainDate$month = sqldf("select date, avg(month) as month from datosTrain group by date")$month
datosTrainDate$weekday = sqldf("select date, avg(weekday) as weekday from datosTrain group by date")$weekday
datosTrainDate$holiday = sqldf("select date, avg(holiday) as holiday from datosTrain group by date")$holiday
datosTrainDate$workingday = sqldf("select date, avg(workingday) as workingday from datosTrain group by date")$workingday
datosTrainDate$weather = sqldf("select date, avg(weather) as weather from datosTrain group by date")$weather
datosTrainDate$temp = sqldf("select date, avg(temp) as temp from datosTrain group by date")$temp
datosTrainDate$atemp = sqldf("select date, avg(atemp) as temp from datosTrain group by date")$atemp
datosTrainDate$humidity = sqldf("select date, avg(humidity) as temp from datosTrain group by date")$humidity
datosTrainDate$windspeed = sqldf("select date, avg( windspeed) as temp from datosTrain group by date")$ windspeed
datosTrainDate$windspeed = sqldf("select date, avg( windspeed) as temp from datosTrain group by date")$ windspeed
colsC = NULL
for (i in 0:23) {
  labelCol = sprintf("C%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsC = c(colsC, labelCol)
}
colsCN = NULL
for (i in 0:23) {
  labelCol = sprintf("CN%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsCN = c(colsCN, labelCol)
}
colsCS = NULL
for (i in 0:23) {
  labelCol = sprintf("CS%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsCS = c(colsCS, labelCol)
}
colsCSN = NULL
for (i in 0:23) {
  labelCol = sprintf("CSN%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsCSN = c(colsCSN, labelCol)
}
colsRG = NULL
for (i in 0:23) {
  labelCol = sprintf("RG%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsRG = c(colsRG, labelCol)
}
colsRGN = NULL
for (i in 0:23) {
  labelCol = sprintf("RGN%02.0f", i)
  datosTrainDate[, labelCol] = -1
  colsRGN = c(colsRGN, labelCol)
}
for (i in 1:nrow(datosTrainDate)) {
  ind = which( datosTrain$date == datosTrainDate$date[i] )
  dfTemp = datosTrain[ind, c("hour", "count", "casual", "registered")]
  if (nrow(dfTemp)==24) {
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("C%02.0f", dfTemp$hour[h])] = dfTemp$count[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("CN%02.0f", dfTemp$hour[h])] = 24*dfTemp$count[h]/sum(dfTemp$count)
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("CS%02.0f", dfTemp$hour[h])] = dfTemp$casual[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("CSN%02.0f", dfTemp$hour[h])] = 24*dfTemp$casual[h]/sum(dfTemp$casual)
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("RG%02.0f", dfTemp$hour[h])] = dfTemp$registered[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDate[i, sprintf("RGN%02.0f", dfTemp$hour[h])] = 24*dfTemp$registered[h]/sum(dfTemp$registered)
    }    
  }
}
ind1 = which(datosTrainDate$C00 == -1)
datosTrainDate = datosTrainDate[-ind1, ]

# Plots ----
r.plot.new(main="shapes", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsC]), max(datosTrainDate[, colsC])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsC])
}
r.plot.new(main="shapes normalized", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN])
}
r.plot.new(main="wday", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=wday(datosTrainDate$date[i]))
}
r.plot.new(main="workingday", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=round(datosTrainDate$workingday[i]))
}
r.plot.new(main="workingday = 0 (casual/registered)", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  if (round(datosTrainDate$workingday[i])<0.5) {
    r.plot.add(x=0:23, y=datosTrainDate[i, colsCSN], icol=1)
    r.plot.add(x=0:23, y=datosTrainDate[i, colsRGN], icol=2)
  }
}
r.plot.new(main="workingday = 1 (casual/registered)", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  if (round(datosTrainDate$workingday[i])>0.5) {
    r.plot.add(x=0:23, y=datosTrainDate[i, colsCSN], icol=1)
    r.plot.add(x=0:23, y=datosTrainDate[i, colsRGN], icol=2)
  }
}
r.plot.new(main="season", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=round(datosTrainDate$season[i]))
}
r.plot.new(main="month", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=round(datosTrainDate$month[i]))
}
r.plot.new(main="weather", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=round(datosTrainDate$weather[i]))
}

# Shapes K-means ----
nclusters <- 2
x <- datosTrainDate[, colsCN]
km <- kmeans(x, nclusters)

r.plot.new(main="clusters", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=km$cluster[i])
}

r.plot.new(main="clusters & centroides", xlim=c(0,23), ylim=c(min(datosTrainDate[, colsCN]), max(datosTrainDate[, colsCN])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=0:23, y=datosTrainDate[i, colsCN], icol=km$cluster[i])   
}
for (i in 1:nclusters) {  
  for (h in 0:23) {
    r.plot.add(x=h, y=km$centers[i, 1+h], type='p', cex=1.5, col=rgb(1,1,1,0.8))
    r.plot.add(x=h, y=km$centers[i, 1+h], type='p', cex=1.0, icol=i)
  }  
}

x_pca <- prcomp(x)
outcolors = c(rgb(0,0,1,0.8), rgb(1,0,0,0.8))
incolors = c(rgb(1,0,0,0.8), rgb(0,0,1,0.8))
r.plot.new(main="PCA", xlim=c(min(x_pca$x[,1]),max(x_pca$x[,1])), ylim=c(min(x_pca$x[,2]),max(x_pca$x[,2])))
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=x_pca$x[i,1], y=x_pca$x[i,2], col=outcolors[1+round(datosTrainDate$workingday[i])], cex=1.5, type='p')
}
for (i in 1:nrow(datosTrainDate)) {
  r.plot.add(x=x_pca$x[i,1], y=x_pca$x[i,2], col=incolors[km$cluster[i]], type='p')
}

# Altres ----
r.plot.new(main="Hourly (casual/registered)", x=datosTrain$casual, y=datosTrain$registered)
for (i in 1:nrow(datosTrain)) {
  if (round(datosTrain$workingday[i])<0.5) {
    r.plot.add(x=datosTrain$casual[i], y=datosTrain$registered[i], type='p', icol=1)
  } else {
    r.plot.add(x=datosTrain$casual[i], y=datosTrain$registered[i], type='p', icol=2)
  }
}
r.plot.new(main="Daily (casual/registered)", x=rowSums(datosTrainDate[, colsCS]), y=rowSums(datosTrainDate[, colsRG]))
for (i in 1:nrow(datosTrainDate)) {
  if (round(datosTrainDate$workingday[i])<0.5) {
    r.plot.add(x=sum(datosTrainDate[i, colsCS]), y=sum(datosTrainDate[i, colsRG]), type='p', icol=1)
  } else {
    r.plot.add(x=sum(datosTrainDate[i, colsCS]), y=sum(datosTrainDate[i, colsRG]), type='p', icol=2)
  }
}


datosTrainDateNorm = data.frame(date=datosTrainDate$date)
datosTrainDateNorm$count =  -1
datosTrainDateNorm$countN =  -1
sumWorkingDays = 0
sumNonWorkingDays = 0
nWorkingDays = 0
nNonWorkingDays = 0
for (i in 1:nrow(datosTrainDate)) {
  datosTrainDateNorm$count[i] = sum(datosTrainDate[i, colsC])
  datosTrainDateNorm$countN[i] = sum(datosTrainDate[i, colsC])
  if (round(datosTrainDate$workingday[i])<0.5) {
    sumNonWorkingDays = sumNonWorkingDays + sum(datosTrainDate[i, colsC])
    nNonWorkingDays = nNonWorkingDays + 1
  } else {
    sumWorkingDays = sumWorkingDays + sum(datosTrainDate[i, colsC])
    nWorkingDays = nWorkingDays + 1
  }
}
sumNonWorkingDays = sumNonWorkingDays / nNonWorkingDays
sumWorkingDays = sumWorkingDays / nWorkingDays
for (i in 1:nrow(datosTrainDate)) {
  if (round(datosTrainDate$workingday[i])<0.5) {
    datosTrainDateNorm$countN[i] = datosTrainDateNorm$countN[i] / sumNonWorkingDays
  } else {
    datosTrainDateNorm$countN[i] = datosTrainDateNorm$countN[i] / sumWorkingDays
  }
}
r.plot(datosTrainDateNorm$count)
r.plot(datosTrainDateNorm$countN)

# Debug ----
