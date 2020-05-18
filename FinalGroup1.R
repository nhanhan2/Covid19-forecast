#Final Group 1: Bus Analytics 


#So far: who gets hospitalized and who doesnt
#how many will get hospitalized?

#Importing the Covid 19 Distribution by Countries dataset
spread <- read.csv('covid19data.csv')

#Forecasting per country
#create a smaller dataframe for just spain called spreadinspain
library(dplyr)
spreadinspain <- filter(spread, geoid == "ES") 
spreadinspain2 <- filter(spreadinspain, cases>0) 
#plot using dates and number of cases in spain
library(ggplot2)
sp <- ggplot(data=spreadinspain2, aes(x=daterep, y=cases)) + geom_point()
sp <- sp + labs(title = "Spain Time Series: Number of Cases")
sp <- sp + theme(axis.text.x = element_text(size=8, angle=90))
sp <- sp + theme(axis.text.y = element_text(size=10)) 
sp
#sp <- sp + theme(axis.text.x = element_text(color = "blue", face = "italic", size=8, angle=90))

spreadinus <- filter(spread, geoid == "US")
spreadinus2<- filter(spreadinus, cases>2) 
us <- ggplot(data=spreadinus2, aes(x=daterep, y=cases)) + geom_point()
us <- us + labs(title = "US Time Series: Number of Cases")
us <- us + theme(axis.text.x = element_text(size=6, angle=90))
us <- us + theme(axis.text.y = element_text(size=10)) 
us

spreadinsouthkorea <- filter(spread, geoid == "KR")
spreadinsouthkorea2<- filter(spreadinsouthkorea, cases>2) 
kr <- ggplot(data=spreadinsouthkorea2, aes(x=daterep, y=cases)) + geom_point()
kr <- kr + labs(title = "South Korea Time Series: Number of Cases")
kr <- kr + theme(axis.text.x = element_text(size=5, angle=90))
kr <- kr + theme(axis.text.y = element_text(size=10)) 
kr


#calculate the time between the first case and case 34272 in  US
firstcase<- filter(spreadinus, cases>0) 
firstcase<- filter(spreadinus, cases<34272) 
firstcase$daterep <- as.Date(firstcase$daterep)
with(firstcase, difftime(max(daterep), min(daterep))) #95 days


#Forecast using earlyR
#install.packages('earlyR') #Built for Ebola: not very reliable
library(earlyR)
library(incidence) #part of earlyR
today <- as.Date("2020-05-01")
str(spreadinus) #daterep is being read as factors

spreadinus$daterep <- as.Date(spreadinus$daterep, "%m/%d/%y") #force to be read as dates
i <- incidence(spreadinus$daterep, last_date=today)
i #Not as reliable because it is forcasting based on Ebola
spreadinusSAVE <- spreadinus
#Forecast using Random Forest
spreadinus <- spreadinus[,c(1:5,10)] #Can use select feature in dplyr
spreadinus$day <- as.factor(spreadinus$day)
spreadinus$month <- as.factor(spreadinus$month)
spreadinus$year <- as.factor(spreadinus$year)

#Regressing using RF
#install.packages('randomForest')
library(randomForest)
#spreadinus <- spreadinus[,-1]
#Need to Reorder Data
spreadinus <- spreadinus[order(spreadinus$daterep),] #For ascending order
#for descending order, rev(order())

#creating the random forest model
rf_mod <- randomForest(cases~., data=spreadinus[,-1], ntree=100, mtry=3, importance=TRUE)
########rf_mod <- randomForest(cases~., data=spreadinus, ntree=100, mtry=3, importance=TRUE)
#mtry is the number of variables used/randomly sampled at each stage

#Feature Ranking
varImpPlot(rf_mod, main="Variable Importance") #part of RandomForest package
#IncMSE: Mean Square Error: important when youre regressing
#IncNodePurity: important when you're classifying

#Predicting the current data
pred_rd1 <- predict(rf_mod, spreadinus[,-c(1,5)])
###########pred_rd <- predict(rf_mod, spreadinus[,-c(5)])
current_pred <- plot.ts(pred_rd1) #predictions for current dates in dataset

#plotting original number of cases vs prediction of cases as time series
plot.ts(spreadinus$cases, main="Real and Forecasted Cases", 
        xlab="Day", ylab="Cases")
lines(pred_rd1, col='red')
legend("topleft", c("Real Data", "Forecasted"), fill=c("black", "red"))


#Forecast for 10 more days: create a list of dates to test
#install.packages('lubridate')
library(lubridate)

dates <- data.frame(daterep=seq(as.Date('2020-04-06'), by='days', length=10))
dates$day <- factor(day(dates$daterep))
dates$month <- factor(month(dates$daterep))
dates$year <- factor(year(dates$daterep))
dates$popdata2018 <- rep(327167434, times=10)
dates$popdata2018 <- as.integer(dates$popdata2018)

#Creating a new data frame called combined

combined <- rbind(spreadinus[,-5], dates)

#My model has learnt based on rf_mod. Applying that to predict for 10 new days
pred_rd <- predict(rf_mod, combined[,-1])
#############pred_rd <- predict(rf_mod, combined)
plot(pred_rd)

#Random Forest does not understand the trend unless you make it
#Multiple Regression, and Neural nets outpreform RF
#Make RF understand the Trend - Before Tuesday

install.packages('forecast')
install.packages('rpart')
library(forecast)
library(rpart)
period <- 97
data_ts <- ts(spreadinus$cases, freq=period/7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust=TRUE)$time.series
plot(decomp_ts)
trend_part <- ts(decomp_ts[,2])
trend_fit <- auto.arima(trend_part,approximation=FALSE, stepwise=FALSE, trace=2)
print(summary(trend_fit))
checkresiduals(trend_fit)
plt_arima <- plot(forecast(trend_fit)) #Arima forcast prediction
trend_for <- as.vector(forecast(trend_fit, period)$mean)


library(TTR)
install.packages("smooth")
library(smooth)
sma <- SMA(spreadinus$cases, n=7, interval=TRUE)
print(summary(sma))

pred_rd <- predict(rf_mod, combined[,-1])
pred_rd2 <- predict(rf_mod, combined[,-1]) + trend_for

#Side by side comparison of SMA and ARIMA Forcasts compared to Random Forest
plot(forecast(sma), main="Forecasts from SMA", xlab="Day",
     ylab="Cases", col="Blue") 
lines(pred_rd, col="Black")
lines(spreadinus$cases, col="Red")
legend("topleft", c("SMA", "RF", "Real"), fill=c("Blue", "Black", "Red"))
plot(forecast(trend_fit), main="Forecasts from ARIMA", xlab="Day",
     ylab="Cases", col="Blue")  #arima 

lines(pred_rd, col="Black") #random forest
lines(spreadinus$cases, col="Red")
legend("topleft", c("Arima", "RF", "Real"), fill=c("Blue", "Black", "Red"))



#Side by side plot of two random forest predicitions
par(mfrow=c(1,2))
plot(pred_rd)
plot(pred_rd2)


#Neural Networks
#Fit neural network # install library
install.packages("neuralnet ")
# load library 
library(neuralnet)
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T ) # plot neural network plot(NN)
 #compare SMA with Arima and say arima did best job
#sma doesnt capture the lag as well, lag effect

#sma closer to rf, arima closer to real line


