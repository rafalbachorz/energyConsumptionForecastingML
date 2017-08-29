prepareDataForForecast <- function(dfParameters){
  library(dplyr)
  
  forecastBeg <- as.character(dfParameters %>% filter(name == "forecastBeg") %>% summarise(value))
  forecastEnd <- as.character(dfParameters %>% filter(name == "forecastEnd") %>% summarise(value))
  resolution <- as.character(dfParameters %>% filter(name == "resolution") %>% summarise(value))
  seasonLevel <- as.integer(dfParameters %>% filter(name == "seasonLevel") %>% summarise(value))
  
  dfTimeseries <- as.data.frame(seq(from = as.POSIXct(forecastBeg, format = "%Y-%m-%d %H:%M"), to = as.POSIXct(forecastEnd, format = "%Y-%m-%d %H:%M"), by = resolution))
  colnames(dfTimeseries) <- c("Time")
  dfTimeseries$seasonLevels <- factor(x = rep(seasonLevel, nrow(dfTimeseries)), levels = c("1", "2", "3"))
  
  return(dfTimeseries)
  
}