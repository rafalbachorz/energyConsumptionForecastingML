wdir="E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML"
setwd(wdir)

dfParameters <- data.frame(name = as.character(), value = as.character(), stringsAsFactors = FALSE)
colnames(dfParameters) <- c("name", "value")
dfParameters[1,] <- c("path", "E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML\\energy-consumption1.csv")
dfParameters[2,] <- c("subsetBeg", "2012-01-01")
dfParameters[3,] <- c("subsetEnd", "2014-01-01")

source("prepareData.R")
rawDF <- prepareData(dfParameters = dfParameters)

dfParameters <- data.frame(name = as.character(), value = as.character(), stringsAsFactors = FALSE)
colnames(dfParameters) <- c("name", "value")
dfParameters[1,] <- c("path", "E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML\\svmModel.rds")
dfParameters[2,] <- c("trainBeg", "2012-01-01 00:00")
dfParameters[3,] <- c("trainEnd", "2013-01-01 00:00")

source("run_train.R")
run(rawDF, dfParameters)