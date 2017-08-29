# prepare the data frame with parameterization 
dfParameters <- data.frame(name = as.character(), value = as.character(), stringsAsFactors = FALSE)
colnames(dfParameters) <- c("name", "value")
# preparation of the data frame with parameters
# pathRawFile - location of csv file with raw data
# rawFileBeg - beginning of time period used for training
# rawFileEnd - end of time period used for training
dfParameters[1,] <- c("pathRawFile", "E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML\\energy-consumption1.csv")
dfParameters[2,] <- c("rawFileBeg", "2012-01-01")
dfParameters[3,] <- c("rawFileEnd", "2014-01-01")
# training
# train the model
# pathModelOut - location of the model file
# trainBeg - beginning of train period
# trainEnd - end of train period
dfParameters[4,] <- c("pathModelOut", "E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML\\svmModel.rds")
dfParameters[5,] <- c("trainBeg", "2012-01-01 00:00")
dfParameters[6,] <- c("trainEnd", "2013-01-01 00:00")
# prepareDataForForecast
# forecastBeg - beginning of forecast period
# forecastEnd - end of forecast period
# resolution - resolution of time grid
# seasonLevel - season (1, 2 or 3) of time grid
dfParameters[7,] <- c("forecastBeg", "2013-12-01 00:00")
dfParameters[8,] <- c("forecastEnd", "2014-01-01 00:00")
dfParameters[9,] <- c("resolution", "hour")
dfParameters[10,] <- c("seasonLevel", "1")
# forecast
# pathModelIn - path of the trained model
dfParameters[11,] <- c("pathModelIn", "E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML\\svmModel.rds")

#### data preparation

source("prepareData.R")
rawDF <- prepareData(dfParameters = dfParameters)

#### training

source("run_train.R")
run(rawDF, dfParameters)

#### prepare data before forecast

source("prepareDataForForecast.R")
dfTimeseries <- prepareDataForForecast(dfParameters)

#### forecast

source("run_forecast.R")
forecastedValues <- run(dfTimeseries, dfParameters)

#### plot

plotDF <- data.frame(time = dfTimeseries$Time, energy = forecastedValues)
plotDF$time <- dfTimeseries$Time 
plotDF$energy = forecastedValues

ggplot(data = plotDF) + geom_line(aes(x = time, y = energy))
