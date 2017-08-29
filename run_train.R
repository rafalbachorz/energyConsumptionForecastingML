run <- function(dfData, dfParameters){
  library(timeDate)
  library(dplyr)
  library(TTR)
  library(caret)
  library(e1071)
  library(randomForest)
  
  
  filePath <- as.character(dfParameters %>% filter(name == "pathModelOut") %>% summarise(value))
  subsetBeg <- as.character(dfParameters %>% filter(name == "trainBeg") %>% summarise(value))
  subsetEnd <- as.character(dfParameters %>% filter(name == "trainEnd") %>% summarise(value))
  
  dfData <- dfData %>% filter(!is.na(Energy))
  
  nL <- 24*28
  dfData$season <- EMA(dfData$Energy, n = nL)
  dfData$season[1:nL] <- dfData$season[nL+1]
  
  nBins <- 3
  dfData$seasonLevels <- cut(dfData$season, breaks = nBins, labels = seq(1, nBins))
  
  # create day of week feature
  dfData$dayOfWeek <- as.factor(dayOfWeek(timeDate(dfData$Time)))
  # create hour feature
  dfData$hour <- as.factor(format(dfData$Time, "%H"))
  
  dataset <- data.frame(predict(dummyVars(~ dayOfWeek + hour + seasonLevels + Energy + Time, data = dfData), newdata = dfData))
  dataset$Time <- as.POSIXct(dataset$Time, origin = "1970-01-01", tz = "GMT")
  
  #7 (dayOfWeek) + 24 (hour) + 1 (season) + 1 (response)
  featuresResponseCols <- c(seq(1, 7), seq(8, 8+23), seq(32, 34), 35)
  #featuresResponseCols <- c(seq(1, 7), seq(8, 8+23), 33)
  NdataSetCols <- length(featuresResponseCols)
  
  trainindex <- which(dataset$Time >= as.POSIXct(subsetBeg, format = "%Y-%m-%d %H:%M", tz = "GMT") & dataset$Time < as.POSIXct(subsetEnd, format = "%Y-%m-%d %H:%M", tz = "GMT"))
  
  training <- as.data.frame(dataset[trainindex, featuresResponseCols])
  rownames(training) = NULL
  
  type <- "eps-regression" #regression
  u <- -2 # -3,-2,-1,0,1,2,3
  gam <- 10^{u} 
  w <- 4.5 #1.5,-1,0.5,2,3,4
  cost <- 10^{w}
  
  # support vector machine
  svmFit <- svm(training[,1:(NdataSetCols-1)], 
                training[,NdataSetCols], 
                type = type,
                kernel = "radial",
                gamma = gam,
                cost = cost)
  
  saveRDS(svmFit, file = filePath)
  return(1)
}
