#
# Machine Learning approach to energy consumption forecasting
# R.A. Bachorz
#

library(ggplot2)
library(timeDate)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)

wdir="E:\\R\\energyConsumptionForecast"
setwd(wdir)

## preprocess the data

energyData <- read.csv("energy-consumption1.csv", skip = 2, header = FALSE, stringsAsFactors = FALSE)

head(energyData)
energyLabels <- "e0"
for (iii in 1:47){
  energyLabels = c(energyLabels, paste("e", as.character(iii), sep = ""))
}
colnames(energyData) <- c("Location", "Utility", "Unit", "Date", energyLabels, "Sum")
energyData <- energyData %>% filter(Location == "Kew site")
energyData$Date <- as.Date(energyData$Date, format = "%d/%m/%Y")
#which rows are wrong?
head(energyData)
energyData %>% filter(is.na(Date))
# not needed anymore
#energyData <- energyData %>% filter(!is.na(Date))
min(energyData$Date)
max(energyData$Date)
#local environment
locData <- new.env()

transpose <- function(x, locData, energyLabels){
  rowDF <- data.frame(as.double(x[energyLabels]))
  locData$tmptmp <- x
  colnames(rowDF) <- c("Energy")
  rowDF$Energy <- as.double(rowDF$Energy)
  locData$dataFrame <- rbind(locData$dataFrame, rowDF, make.row.names = TRUE)
}

locData$dataFrame <- data.frame(Energy = as.double())
str(locData$dataFrame)
#locData$dataFrame$Energy <- as.double(locData$dataFrame$Energy)
head(locData$dataFrame)

# one year of data
energySubset <- energyData %>% filter(Date >= as.Date("2012-01-01"), Date < as.Date("2013-01-01"))

#transposeAllTable
energySubset <- energyData

#supress output
sink("NUL")
apply(energySubset, 1, transpose, locData, energyLabels)
sink()

timeAxis <- seq(from = as.POSIXlt(min(energySubset$Date), tz = "GMT"), by = 1800, length.out = nrow(locData$dataFrame))
head(locData$dataFrame)
tail(locData$dataFrame)

tmptmp <- locData$dataFrame
write.table(tmptmp, file = "energyDataTimeValue.csv", sep = ",", row.names = FALSE)
?write.csv

timeAxis <- seq(from = as.POSIXlt(min(energySubset$Date), tz = "GMT"), by = 1800, length.out = nrow(locData$dataFrame))
locData$dataFrame$Time <- timeAxis
head(locData$dataFrame)
tail(locData$dataFrame)
nrow(locData$dataFrame)

# aggregation to one-hour resolution
locData$dataFrame <- locData$dataFrame %>% group_by(format(Time, "%Y-%m-%d %H")) %>% summarise(mean(Energy), min(Time))
colnames(locData$dataFrame) <- c("hour", "Energy", "Time")
locData$dataFrame$hour <- NULL

# there are some NA energies, drop them out
locData$dataFrame %>% filter(is.na(Energy))
locData$dataFrame <- locData$dataFrame %>% filter(!is.na(Energy))

str(locData$dataFrame)


#### seasonal decommposition
library(xts)
library(forecast)
tmptmp <- xts(locData$dataFrame$Energy, order.by = locData$dataFrame$Time)
etsV <- ets(tmptmp)
seasonalTS <- msts(locData$dataFrame$Energy, seasonal.periods = c(24*7))
ttt <- decompose(seasonalTS)
head(ttt)
decomposedDF <- data.frame(ttt[1], ttt[2], ttt[3], ttt[4], Time = locData$dataFrame$Time)
head(decomposedDF)

ggplot(data = decomposedDF) + geom_line(aes(x = Time, y = seasonal), color = "red") +
                              geom_line(aes(x = Time, y = trend), color = "blue") +
                              geom_line(aes(x = Time, y = random), color = "green")

?decompose

fff <- ttt[1]
length(unlist(fff))
plot(ttt)

tbatsOBJ <- tbats(seasonalTS)


attr(tmptmp, 'frequency') <- 60
decomposed <- decompose(as.ts(tmptmp))
length(decomposed[3])
?decompose
plot(decomposed)
stl(tmptmp)
ets
?stl
# fit <- stl(tmptmp, s.window = 9)
# seasonal <- tbats(tmptmp)
# plot(seasonal)
# str(tmptmp)
# ?stl
ts(1:10, frequency = 3, start = c(1959, 1, 1, 0, 0, 0))
# tmptmp <- ts(locData$dataFrame$Energy, )
#   data.frame(time = locData$dataFrame$Time, energy = locData$dataFrame$Energy)
# 
# head(tmptmp)
# str(nottem)
# head(nottem)
####

# plot all data
ggplot(data = locData$dataFrame, aes(x = Time, y = Energy)) + geom_point()

# add simple seasonal behaviour
head(locData$dataFrame)
# seasons: 
# 1 - Jan - Feb
# 2 - Mar - Jun
# 3 - 
nL <- 24*14
locData$dataFrame$season <- EMA(locData$dataFrame$Energy, n = nL)
locData$dataFrame$season[1:nL] <- locData$dataFrame$season[nL+1]

# plot all data
ggplot(data = locData$dataFrame, aes(x = Time, y = Energy)) + geom_point() + geom_line(aes(x = Time, y = season, col = "red"))


# turn into categorical variable
locData$dataFrame$season <- cut(locData$dataFrame$season, breaks = 4, labels = c("1", "2", "3", "4"))
head(locData$dataFrame$season, n = 500)
# normalize
#maxSeason <- max(locData$dataFrame$season)
#minSeason <- min(locData$dataFrame$season)
#locData$dataFrame$season <- (locData$dataFrame$season - minSeason) / (maxSeason - minSeason)
#locData$dataFrame[nL:(nL+50), 'season']



# plot one week
ggplot(data = locData$dataFrame[1:24*7,], aes(x = Time, y = Energy)) + geom_line()

# create day of week feature
locData$dataFrame$dayOfWeek <- as.factor(dayOfWeek(timeDate(locData$dataFrame$Time)))
# create hour feature
locData$dataFrame$hour <- as.factor(format(locData$dataFrame$Time, "%H"))
str(locData$dataFrame)

# convert to "othogonal" space
dataset <- data.frame(predict(dummyVars(~ dayOfWeek + hour + + season + Energy + Time, data = locData$dataFrame), newdata = locData$dataFrame))
dataset$Time <- as.POSIXct(dataset$Time, origin = "1970-01-01", tz = "GMT")

# how do first three days look like? 
head(dataset, n = 72)

index = 1:nrow(dataset)

#### standard approach
#trainindex <- createDataPartition(index,p = 0.75,list = FALSE)

#### history forecast approach
trainindex <- which(dataset$Time < as.POSIXct("2012-11-01 00:00", format = "%Y-%m-%d %H:%M", tz = "GMT") | dataset$Time >= as.POSIXct("2012-12-01 00:00", format = "%Y-%m-%d %H:%M", tz = "GMT"))
#as.POSIXct("2012-01-01 00:00", format = "%Y-%m-%d %H:%M")

lenIDX <- length(index)
trainindex <- 1:(lenIDX - 24*31)
length(trainindex)
####

# check
length(trainindex)/length(index)

##process class sets as data frames
#7 (dayOfWeek) + 24 (hour) + 1 (season) + 1 (response)
featuresResponseCols <- c(seq(1, 7), seq(8, 8+23), seq(32, 35), 36)
#featuresResponseCols <- c(seq(1, 7), seq(8, 8+23), 33)
dataSetCols <- length(featuresResponseCols)

head(dataset[trainindex, featuresResponseCols])
training <- as.data.frame(dataset[trainindex, featuresResponseCols])
rownames(training) = NULL
head(training)
testing = as.data.frame(dataset[-trainindex, featuresResponseCols])
rownames(testing) = NULL
head(testing)


type <- "eps-regression" ##regression
u <- -2 ## -3,-2,-1,0,1,2,3
gam <- 10^{u}; w= 4.5 ##1.5,-1,0.5,2,3,4
cost <- 10^{w}

# support vector machine
svmFit <- svm(training[,1:(dataSetCols-1)], 
              training[,dataSetCols], 
              type = type,
              kernel = "radial",
              gamma = gam,
              cost = cost)
summary(svmFit)
predsvm <- predict(svmFit, testing[,1:(dataSetCols-1)])
# random forest
rfFit <- randomForest(training[,1:(dataSetCols-1)], 
                      training[,dataSetCols],,
                      ntree = 500)
summary(rfFit)
predrf <- predict(rfFit, testing[,1:(dataSetCols-1)])

head(predsvm)
head(predrf)

###EVALUATION
actualTS <- testing[,dataSetCols]
predicTS <- predsvm ##choose appropriate
predicTS <- predrf ##choose appropriate

#str(actualTS)
#str(predicTS)
#length(predicTS)
#length(actualTS)

##1. Evaluation for return prediction. Residual sum of squares
ssr <- sum((actualTS - predicTS)^2); ssr
##Normalize Residual Mean Square Error (NRMSE)
nrmse <- sqrt(ssr/((length(actualTS)-1)*var(actualTS))); nrmse
##percentage of outperforming direct sample mean (sample expected value)
pcorrect = (1-nrmse)*100; pcorrect
##For visual comparison
plotData <- data.frame(actualTS, predicTS)
colnames(plotData) <- c("real", "predicted")
plotData$time <- dataset[-trainindex, "Time"]
plotData$delta <- plotData$predicted - plotData$real
plotData$dow <- dayOfWeek(timeDate(plotData$time))

ggplot(data = plotData) + geom_line(aes(x = time, y = real), color = "red") + geom_line(aes(x = time, y = predicted), color = "blue") + 
  theme(legend.position = "bottom") + xlab("Time (forecasted period)")

ggplot(data = plotData) + geom_line(aes(x = time, y = delta)) + ggtitle("Residue distribution") + xlab("Time (forecasted period)") +
  ylab("Energy delta") + guides(color = "none")

plotData$time <- as.POSIXct(plotData$time, tz = "GMT")
plotData %>%  group_by(dow) %>% summarise (minDelta = min(delta), maxDelta = max(delta))
plotData %>% filter(as.Date(time) < as.Date("2012-12-22")) %>% group_by(dow) %>% summarise (minDelta = min(delta), maxDelta = max(delta))





head(plotData)

ggplot(data = plotData, aes(x = real, y = predicted)) + geom_point()
ggplot(data = dataset[-trainindex,], aes(x = Time, y = Energy)) + geom_point()
dataset[-trainindex, "predictedEnergy"] <- predicTS

ggplot(data = dataset[-trainindex,]) + geom_line(aes(x = Time, y = Energy), color = "red") + geom_line(aes(x = Time, y = predictedEnergy), color = "blue") + 
                                       theme(legend.position = "bottom") + xlab("Time (forecasted period)")


head(plotData)
ggplot(data = plotData)

# Exploratory data analysis
datasetTmp <- dataset
datasetTmp$Time <- as.POSIXct(datasetTmp$Time, tz = "GMT")
head(dataset)
datasetTmp %>% filter(dayOfWeek.Fri == 1, hour.00 == 1) %>% select(Time, Energy)
head(locData$dataFrame)
groupedData <- locData$dataFrame %>% group_by(dayOfWeek, hour) %>% summarise(Emin = min(Energy), Emax = max(Energy), Esd = sd(Energy)) %>% 
                                     mutate(deltaMinMax = Emax - Emin) %>%  arrange(desc(Esd))

DF_Fri_11 <- locData$dataFrame %>% filter(dayOfWeek == "Fri", hour == 11)
DF_Tue_10 <- locData$dataFrame %>% filter(dayOfWeek == "Tue", hour == 10)
DF_Tue_13 <- locData$dataFrame %>% filter(dayOfWeek == "Tue", hour == 13)
DF_Sun_04 <- locData$dataFrame %>% filter(dayOfWeek == "Sun", hour == "04")

head(DF_Tue_14)
ggplot(data = DF_Fri_11, aes(x = Time, y = Energy)) + geom_line()
ggplot(data = DF_Tue_10, aes(x = Time, y = Energy)) + geom_line()
ggplot(data = DF_Tue_13, aes(x = Time, y = Energy)) + geom_line()
ggplot(data = DF_Sun_04, aes(x = Time, y = Energy)) + geom_line()

?as.POSIXlt

##################################################################################
##################################################################################
##################################################################################
##################################################################################


########Nonlinear models#############################
####### SVM and Neural networks ############
install.packages("Metrics")
library(e1071) ##for svm
library(nnet)
library(kernlab)
library(quantmod)
library(caret) ##for some data handling functions
library(Metrics)##Measures of prediction error:mse, mae
library(xts)


##Data:sp500m the S&P500 monthly readings from Jan. 1990 to Jan. 2012
sp500m = readRDS("sp500m.rds")
?readRDS
plot(sp500m['1910/1990'])

tau=1 #data is monthly. Try tau=12 (year), tau=1 (monthly)
ret=diff(log(sp500m),diff=tau)  ##compute tau-period returns

##Model Inputs:
##Define matrix of features (each column is a feature)
#Features: lags 1,2,3,5
feat = merge(na.trim(lag(ret,1)),na.trim(lag(ret,2)),na.trim(lag(ret,3)),na.trim(lag(ret,5)),
             #add other features here,
             all=FALSE)

##add TARGET. We want to predict RETURN
dataset = merge(feat,ret,all=FALSE)

colnames(dataset) = c("lag.1", "lag.2", "lag.3","lag.5",
                      #names of other features,
                      "TARGET")
head(dataset)
##Divide data into training (75%) and testing (25%). Use caret methods
index = 1:nrow(dataset)
trainindex= createDataPartition(index,p=0.75,list=FALSE)
?createDataPartition
##process class sets as data frames
training = as.data.frame(dataset[trainindex,])
rownames(training) = NULL
head(training)
testing = as.data.frame(dataset[-trainindex,])
rownames(testing) = NULL
head(testing)

##Train model
##############################################
##OPTION LAZY: one svm, one nnet built w/o tuning  (or tune by hand)
#parameters that can be tuned
#type="C" ##classification
type <- "eps-regression" ##regression
#type = "C" ##regression
u <- -2 ## -3,-2,-1,0,1,2,3
gam <- 10^{u}; w= 4.5 ##1.5,-1,0.5,2,3,4
cost <- 10^{w}
##The higher the cost produce less support vectors, increases accuracy
##However we may overfit
svmFit = svm (training[,-ncol(training)], training[,ncol(training)],
              type=type,
              kernel= "radial",
              gamma=gam,
              cost=cost
)
??svmFit
summary(svmFit)
##build SVM predictor
head(testing[1,-ncol(testing)])

predsvm = predict(svmFit, testing[1,-ncol(testing)])
head

##A nnet with size hidden layers +skip layer. Max iteration 10^4,
size=6
nnetFit = nnet(training[,-ncol(training)], training[,ncol(training)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
summary(nnetFit) ##gives description w/weights

##build NNET predictor type="raw"
prednet<-predict(nnetFit,testing[,-ncol(testing)],type="raw")

################end of Option Lazy ##############################

###EVALUATION
actualTS=testing[,ncol(testing)] ##the true series to predict
predicTS=predsvm ##choose appropriate
predicTS = prednet

##1. Evaluation for return prediction. Residual sum of squares
ssr= sum((actualTS - predicTS)^2); ssr
##Normalize Residual Mean Square Error (NRMSE)
nrmse = sqrt(ssr/((length(actualTS)-1)*var(actualTS))); nrmse
##percentage of outperforming direct sample mean (sample expected value)
pcorrect = (1-nrmse)*100; pcorrect
##For visual comparison
yl=c(min(actualTS,predicTS),max(actualTS,predicTS)) #set y limits
plot(actualTS,predicTS,ylim=yl)

plot(1:nrow(predicTS),predicTS,ylim=yl)
plot(1:nrow(actualTS),actualTS,ylim=yl)
