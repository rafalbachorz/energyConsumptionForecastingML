run <- function(dfData, dfParameters){
  library(timeDate)
  library(dplyr)
  library(TTR)
  library(caret)
  library(e1071)
  library(randomForest)
  
  
  filePath <- as.character(dfParameters %>% filter(name == "path") %>% summarise(value))

  # create day of week feature
  dfData$dayOfWeek <- as.factor(dayOfWeek(timeDate(dfData$Time)))
  # create hour feature
  dfData$hour <- as.factor(format(dfData$Time, "%H"))
  
  
  dataset <- data.frame(predict(dummyVars(~ dayOfWeek + hour + seasonLevels + Time, data = dfData), newdata = dfData))
  dataset$Time <- as.POSIXct(dataset$Time, origin = "1970-01-01", tz = "GMT")
  
  
  model <- loadRDS(filePath)
  NdataSetCols <- ncol(dataset)
  prediction <- predict(model, dataset[,1:(NdataSetCols-1)])
  
  return(prediction)
  
  #forecastModel <- loadRDS(filePath)
  
}