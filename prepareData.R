prepareData <- function(dfParameters){
  library(dplyr)
  
  
  # parse initial df
  filePath <- as.character(dfParameters %>% filter(name == "path") %>% summarise(value))
  subsetBeg <- as.character(dfParameters %>% filter(name == "subsetBeg") %>% summarise(value))
  subsetEnd <- as.character(dfParameters %>% filter(name == "subsetEnd") %>% summarise(value))
  
  #wdir="E:\\R\\energyConsumptionForecast\\energyConsumptionForecastingML"
  #setwd(wdir)
  
  ## preprocess the data
  
  energyData <- read.csv(filePath, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  
  # create the names of the columns
  energyLabels <- "e0"
  for (iii in 1:47){
    energyLabels = c(energyLabels, paste("e", as.character(iii), sep = ""))
  }
  colnames(energyData) <- c("Location", "Utility", "Unit", "Date", energyLabels, "Sum")
  
  energyData <- energyData %>% filter(Location == "Kew site")
  energyData$Date <- as.Date(energyData$Date, format = "%d/%m/%Y")
  
  # just in case
  energyData <- energyData %>% filter(!is.na(Date))
  
  transpose <- function(x, locData, energyLabels){
    rowDF <- data.frame(as.double(x[energyLabels]))
    locData$tmptmp <- x
    colnames(rowDF) <- c("Energy")
    rowDF$Energy <- as.double(rowDF$Energy)
    locData$dataFrame <- rbind(locData$dataFrame, rowDF, make.row.names = TRUE)
  }
  
  locData <- new.env()
  
  locData$dataFrame <- data.frame(Energy = as.double())
  
  #energySubset <- energyData %>% filter(Date >= as.Date("2012-01-01"), Date < as.Date("2014-01-01"))
  energySubset <- energyData %>% filter(Date >= as.Date(subsetBeg), Date < as.Date(subsetEnd))
  
  sink("NUL")
  apply(energySubset, 1, invisible(transpose), locData, energyLabels)
  sink()
  
  timeAxis <- seq(from = as.POSIXlt(min(energySubset$Date), tz = "GMT"), by = 1800, length.out = nrow(locData$dataFrame))
  locData$dataFrame$Time <- timeAxis
  
  locData$dataFrame <- locData$dataFrame %>% group_by(format(Time, "%Y-%m-%d %H")) %>% summarise(mean(Energy), min(Time))
  colnames(locData$dataFrame) <- c("hour", "Energy", "Time")
  locData$dataFrame$hour <- NULL
  
  return(locData$dataFrame)
}