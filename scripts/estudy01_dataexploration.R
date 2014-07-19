# Script created to check some data stuff. It is not supposed to be shared.
# This script only provids some basic charts of the data.
# This script is awfully coded and the author knows it.

if (!'devtools' %in% installed.packages()[,1]) install.packages('devtools')
if (!'rplot' %in% installed.packages()[,1]) install_github("rplot", "rocalabern")
if (!'rmodel' %in% installed.packages()[,1]) install_github("rmodel", "rocalabern")

# Load Data ----
rm(list = ls())

library('rplot')
library('rmodel')
require(sqldf)
require(lubridate)

datosAll = read.csv('./data/raw/hour.csv', sep = ",", header=T)
datosTrain = read.csv('./data/raw/train.csv', sep = ",", header=T)
datosTest = read.csv('./data/raw/test.csv', sep = ",", header=T)
names(datosAll) = c("instant", "dateday", "season", "year", "month", "hour", "holiday", "weekday", "workingday", "weathersit", "temp", "atemp", "humidity", "windspeed", "casual", "registered", "count")

# Create columns ----
datosTrain$datetime <- ymd_hms(datosTrain$datetime)
datosTrain$day <- as.Date(datosTrain$datetime)
datosTrain$hour <- hour(datosTrain$datetime)
datosTrain$month <- month(datosTrain$datetime)
datosTrain$weekday <- wday(datosTrain$datetime)

# Create daily data ----
datosTrainDay = data.frame(day=unique(datosTrain$day))
datosTrainDay = data.frame(day=sqldf("select day from datosTrain group by day order by day")$day)
datosTrainDay$season = sqldf("select day, avg(season) as season from datosTrain group by day")$season
datosTrainDay$month = sqldf("select day, avg(month) as month from datosTrain group by day")$month
datosTrainDay$weekday = sqldf("select day, avg(weekday) as weekday from datosTrain group by day")$weekday
datosTrainDay$holiday = sqldf("select day, avg(holiday) as holiday from datosTrain group by day")$holiday
datosTrainDay$workingday = sqldf("select day, avg(workingday) as workingday from datosTrain group by day")$workingday
datosTrainDay$weather = sqldf("select day, avg(weather) as weather from datosTrain group by day")$weather
datosTrainDay$temp = sqldf("select day, avg(temp) as temp from datosTrain group by day")$temp
datosTrainDay$atemp = sqldf("select day, avg(atemp) as temp from datosTrain group by day")$atemp
datosTrainDay$humidity = sqldf("select day, avg(humidity) as temp from datosTrain group by day")$humidity
datosTrainDay$windspeed = sqldf("select day, avg( windspeed) as temp from datosTrain group by day")$ windspeed
datosTrainDay$windspeed = sqldf("select day, avg( windspeed) as temp from datosTrain group by day")$ windspeed
colsC = NULL
for (i in 0:23) {
  labelCol = sprintf("C%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsC = c(colsC, labelCol)
}
colsCN = NULL
for (i in 0:23) {
  labelCol = sprintf("CN%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsCN = c(colsCN, labelCol)
}
colsCS = NULL
for (i in 0:23) {
  labelCol = sprintf("CS%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsCS = c(colsCS, labelCol)
}
colsCSN = NULL
for (i in 0:23) {
  labelCol = sprintf("CSN%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsCSN = c(colsCSN, labelCol)
}
colsRG = NULL
for (i in 0:23) {
  labelCol = sprintf("RG%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsRG = c(colsRG, labelCol)
}
colsRGN = NULL
for (i in 0:23) {
  labelCol = sprintf("RGN%02.0f", i)
  datosTrainDay[, labelCol] = -1
  colsRGN = c(colsRGN, labelCol)
}
for (i in 1:nrow(datosTrainDay)) {
  ind = which( datosTrain$day == datosTrainDay$day[i] )
  dfTemp = datosTrain[ind, c("hour", "count", "casual", "registered")]
  if (nrow(dfTemp)==24) {
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("C%02.0f", dfTemp$hour[h])] = dfTemp$count[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("CN%02.0f", dfTemp$hour[h])] = 24*dfTemp$count[h]/sum(dfTemp$count)
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("CS%02.0f", dfTemp$hour[h])] = dfTemp$casual[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("CSN%02.0f", dfTemp$hour[h])] = 24*dfTemp$casual[h]/sum(dfTemp$casual)
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("RG%02.0f", dfTemp$hour[h])] = dfTemp$registered[h]
    }
    for (h in 1:nrow(dfTemp)) {
      datosTrainDay[i, sprintf("RGN%02.0f", dfTemp$hour[h])] = 24*dfTemp$registered[h]/sum(dfTemp$registered)
    }    
  }
}
ind1 = which(datosTrainDay$C00 == -1)
datosTrainDay = datosTrainDay[-ind1, ]

# Plots ----
r.plot.new(main="shapes", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsC]), max(datosTrainDay[, colsC])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsC])
}
r.plot.new(main="shapes normalized", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN])
}
r.plot.new(main="wday", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=wday(datosTrainDay$day[i]))
}
r.plot.new(main="workingday", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=round(datosTrainDay$workingday[i]))
}
r.plot.new(main="workingday = 0 (casual/registered)", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  if (round(datosTrainDay$workingday[i])<0.5) {
    r.plot.add(x=0:23, y=datosTrainDay[i, colsCSN], icol=1)
    r.plot.add(x=0:23, y=datosTrainDay[i, colsRGN], icol=2)
  }
}
r.plot.new(main="workingday = 1 (casual/registered)", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  if (round(datosTrainDay$workingday[i])>0.5) {
    r.plot.add(x=0:23, y=datosTrainDay[i, colsCSN], icol=1)
    r.plot.add(x=0:23, y=datosTrainDay[i, colsRGN], icol=2)
  }
}
r.plot.new(main="season", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=round(datosTrainDay$season[i]))
}
r.plot.new(main="month", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=round(datosTrainDay$month[i]))
}
r.plot.new(main="weather", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=round(datosTrainDay$weather[i]))
}

# Shapes K-means ----
nclusters <- 2
x <- datosTrainDay[, colsCN]
km <- kmeans(x, nclusters)

r.plot.new(main="clusters", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=km$cluster[i])
}

r.plot.new(main="clusters & centroides", xlim=c(0,23), ylim=c(min(datosTrainDay[, colsCN]), max(datosTrainDay[, colsCN])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=0:23, y=datosTrainDay[i, colsCN], icol=km$cluster[i])   
}
for (i in 1:nclusters) {  
  for (h in 0:23) {
    r.plot.add(x=h, y=km$centers[i, 1+h], type='p', cex=1.5, col=rgb(1,1,1,0.8))
    r.plot.add(x=h, y=km$centers[i, 1+h], type='p', cex=1.0, icol=i)
  }  
}

x_pca <- prcomp(x)
summary(x_pca)
biplot(x_pca)

outcolors = c(rgb(0,0,1,0.8), rgb(1,0,0,0.8))
incolors = c(rgb(1,0,0,0.8), rgb(0,0,1,0.8))
r.plot.new(main="PCA", xlim=c(min(x_pca$x[,1]),max(x_pca$x[,1])), ylim=c(min(x_pca$x[,2]),max(x_pca$x[,2])))
for (i in 1:nrow(datosTrainDay)) {
  r.plot.add(x=x_pca$x[i,1], y=x_pca$x[i,2], col=outcolors[1+round(datosTrainDay$workingday[i])], cex=1.5, type='p')
}
for (i in 1:nrow(datosTrainDay)) {
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
r.plot.new(main="Daily (casual/registered)", x=rowSums(datosTrainDay[, colsCS]), y=rowSums(datosTrainDay[, colsRG]))
for (i in 1:nrow(datosTrainDay)) {
  if (round(datosTrainDay$workingday[i])<0.5) {
    r.plot.add(x=sum(datosTrainDay[i, colsCS]), y=sum(datosTrainDay[i, colsRG]), type='p', icol=1)
  } else {
    r.plot.add(x=sum(datosTrainDay[i, colsCS]), y=sum(datosTrainDay[i, colsRG]), type='p', icol=2)
  }
}


datosTrainDayNorm = data.frame(day=datosTrainDay$day)
datosTrainDayNorm$count =  -1
datosTrainDayNorm$countN =  -1
sumWorkingDays = 0
sumNonWorkingDays = 0
nWorkingDays = 0
nNonWorkingDays = 0
for (i in 1:nrow(datosTrainDay)) {
  datosTrainDayNorm$count[i] = sum(datosTrainDay[i, colsC])
  datosTrainDayNorm$countN[i] = sum(datosTrainDay[i, colsC])
  if (round(datosTrainDay$workingday[i])<0.5) {
    sumNonWorkingDays = sumNonWorkingDays + sum(datosTrainDay[i, colsC])
    nNonWorkingDays = nNonWorkingDays + 1
  } else {
    sumWorkingDays = sumWorkingDays + sum(datosTrainDay[i, colsC])
    nWorkingDays = nWorkingDays + 1
  }
}
sumNonWorkingDays = sumNonWorkingDays / nNonWorkingDays
sumWorkingDays = sumWorkingDays / nWorkingDays
for (i in 1:nrow(datosTrainDay)) {
  if (round(datosTrainDay$workingday[i])<0.5) {
    datosTrainDayNorm$countN[i] = datosTrainDayNorm$countN[i] / sumNonWorkingDays
  } else {
    datosTrainDayNorm$countN[i] = datosTrainDayNorm$countN[i] / sumWorkingDays
  }
}
r.plot(datosTrainDayNorm$count)
r.plot(datosTrainDayNorm$countN)

# Debug ----
