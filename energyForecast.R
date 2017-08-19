#
# Machine Learning approach to energy consumption forecasting
# R.A. Bachorz
#

library(ggplot2)
library(timeDate)
library(dplyr)
library(caret)
library(e1071)

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
energyData %>% filter(is.na(Date))
# not needed anymore
#energyData <- energyData %>% filter(!is.na(Date))
minDate <- min(energyData$Date)
maxDate <- max(energyData$Date)

#local environment
locData <- new.env()

#seq(as.Date("2016-01-01"), as.Date("2016-01-02"), by = "hour")
#seq(strptime("2016-01-01 00:00:00", "YYYY-mm-dd hh:mm:ss"), as.Date("2016-01-02"), by = "hour")
#?strptime

#rowDF1 <- data.frame(locData$row[energyLabels])
#rowDF2 <- data.frame(locData$row[energyLabels])
#rbind(rowDF1, rowDF2)
#min(energyData$Date)

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


#supress output
sink("NUL")
apply(energyData[1:365,], 1, invisible(transpose), locData, energyLabels)
sink()
timeAxis <- seq(from = as.POSIXlt(minDate, tz = "GMT"), by = 1800, length.out = nrow(locData$dataFrame))
locData$dataFrame$Time <- timeAxis
head(locData$dataFrame)
nrow(locData$dataFrame)

# there are some NA energies, drop them out
locData$dataFrame %>% filter(is.na(Energy))
locData$dataFrame <- locData$dataFrame %>% filter(!is.na(Energy))

str(locData$dataFrame)
# plot all data
ggplot(data = locData$dataFrame, aes(x = Time, y = Energy)) + geom_point()
# plot one week
ggplot(data = locData$dataFrame[1:24*7,], aes(x = Time, y = Energy)) + geom_point()

# create day of week feature
locData$dataFrame$dayOfWeek <- as.factor(dayOfWeek(timeDate(locData$dataFrame$Time)))
# convert to "othogonal" space
dataset <- data.frame(predict(dummyVars(~ dayOfWeek + Energy + Time, data = locData$dataFrame), newdata = locData$dataFrame))
dataset$Time <- as.POSIXlt(dataset$Time, origin = "1970-01-01", tz = "GMT")

# how do first three days look like? 
head(dataset, n = 72)

index = 1:nrow(dataset)

trainindex <- createDataPartition(index,p = 0.75,list = FALSE)

# check
length(trainindex)/length(index)

##process class sets as data frames
training = as.data.frame(dataset[trainindex, 1:8])
rownames(training) = NULL
head(training)
testing = as.data.frame(dataset[-trainindex, 1:8])
rownames(testing) = NULL
head(testing)

type <- "eps-regression" ##regression
u <- -2 ## -3,-2,-1,0,1,2,3
gam <- 10^{u}; w= 4.5 ##1.5,-1,0.5,2,3,4
cost <- 10^{w}

svmFit <- svm(training[,1:7], training[,8], 
              type = type,
              kernel = "radial",
              gamma = gam,
              cost = cost)

svmFit <- svm(training$Energy ~ training$dayOfWeek.Fri + training$dayOfWeek.Mon + training$dayOfWeek.Sat + training$dayOfWeek.Sun + 
                training$dayOfWeek.Thu + training$dayOfWeek.Tue + training$dayOfWeek.Wed, 
              type = type,
              kernel = "radial",
              gamma = gam,
              cost = cost)

summary(svmFit)
?predict

predsvm <- predict(svmFit, testing[,1:7])
head(predsvm)

###EVALUATION
actualTS <- testing[,8]
predicTS <- predsvm ##choose appropriate

str(actualTS)
str(predicTS)
length(predicTS)
length(actualTS)

##1. Evaluation for return prediction. Residual sum of squares
ssr <- sum((actualTS - predicTS)^2); ssr
##Normalize Residual Mean Square Error (NRMSE)
nrmse <- sqrt(ssr/((length(actualTS)-1)*var(actualTS))); nrmse
##percentage of outperforming direct sample mean (sample expected value)
pcorrect = (1-nrmse)*100; pcorrect
##For visual comparison
yl=c(min(actualTS,predicTS),max(actualTS,predicTS)) #set y limits
plot(actualTS,predicTS,ylim=yl)

?svmFit

?dummySet

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
