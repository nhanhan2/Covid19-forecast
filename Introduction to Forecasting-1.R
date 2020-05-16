#Importing the data

data <- read.csv('diagnosisCovid19.csv')

#Missing values?
library(questionr)
missinvaluestable <- freq.na(data)
missinvaluestable

#Selecting the relevant variables
library(dplyr)
data1 <- select(patient_id, patient_age_quantile, sars_cov_2_exam_result,
                patient_addmited_to_regular_ward_1_yes_0_no, dpatient_addmited_to_semi_intensive_unit_1_yes_0_no,
                patient_addmited_to_intensive_care_unit_1_yes_0_no)

data1 <- data[,2:6]

str(data1)

#Regression - response variable is continuous (Decision Trees, Multiple (glm/gaussian), Random Forest)
#Classification - response variable is categorical (Logistic, SVM, KNN, Decision Trees, Random Forest)
#Random Forest for forecasting
#Random Forest - Ensemble of decision trees - bagging technique
#Bagging/Bootstrapping - way of sampling  - with replacement - Classification/Regression
#AVERAGE OF THE ERROR/ACCURACY - combine different classification
#Cross-validation (k-fold) - without replacement

#Encoding- No need to encode if you are using Random Forest
#Encoding to create a new variable which will be the response variable
data1$sars_cov_2_exam_result <- as.numeric(data1$sars_cov_2_exam_result)
data1$patient_addmited_to_regular_ward_1_yes_0_no <- as.numeric(data1$patient_addmited_to_regular_ward_1_yes_0_no)
data1$patient_addmited_to_semi_intensive_unit_1_yes_0_no <- as.numeric(data1$patient_addmited_to_semi_intensive_unit_1_yes_0_no)
data1$patient_addmited_to_intensive_care_unit_1_yes_0_no <- as.numeric(data1$patient_addmited_to_intensive_care_unit_1_yes_0_no)

#Creating the response variable
data1$admitted <- ifelse(data1$patient_addmited_to_regular_ward_1_yes_0_no + data1$patient_addmited_to_semi_intensive_unit_1_yes_0_no
                         + data1$patient_addmited_to_intensive_care_unit_1_yes_0_no > 3, 1, 0)

#Converting the response variable to factor
data1$admitted <- as.factor(data1$admitted)

#Predictions - Classification
#Applying Decision Tree
require(tree)
tree.admitted <- tree(admitted ~ sars_cov_2_exam_result + patient_age_quantile, data=data1)
summary(tree.admitted)
#Accuracy is 96.988%!

#Applying Random Forest
#Splitting the data into training and test sets
library(caTools)
set.seed(123)
split = sample.split(data1, SplitRatio = 0.75)
training_set = subset(data1, split==TRUE)
test_set = subset(data1, split==FALSE)


#Random Forests - capable of regression and classification
#Promising technique for forecasting
#Ensembling technique in which you are creating multiple decision trees so as to not rely on one
library(randomForest)
set.seed(123)
classifier = randomForest(admitted~patient_age_quantile + sars_cov_2_exam_result, data= training_set,
                          ntree = 400, imporance=TRUE)

y_pred= predict(classifier, newdata = test_set[,-6])

con_matrix <- table(y_pred, test_set[,6])
#Accuracy = 97%
# My test is positive and my age quantile is 17, will I be admitted?

roc.curve(test_set[,6], y_pred, plotit = TRUE)

#Balance of classes
library(ROSE)
library(caret) #ConfusionMatrix #Better for undersampling
library(rpart) 
library(questionr)
tab <- table(data1$admitted)
data2<-data1[,c(1,2,6)]
data1$sars_cov_2_exam_result <- as.factor(data1$sars_cov_2_exam_result)

tab <- table(data2$admitted)

data.over <- ovun.sample(admitted~. , data=data2, method="over", N=10948)$data
tab <- table(data.over$admitted)
tab

#Applying Random Forest again

library(caTools)
split = sample.split(data.over, SplitRatio = 0.75)
training_set = subset(data.over, split==TRUE)
test_set = subset(data.over, split==FALSE)

library(randomForest)
set.seed(123)
classifier = randomForest(admitted~., data= training_set,
                          ntree = 400, importance=TRUE)

#Predicing the test results
y_pred = predict(classifier, newdata = test_set[,-3]) #3 is where admitted (response) var is

#Confusion Matrix
con_matrix <- table(y_pred, test_set[,3])

#Looking for accuracy measures - recall and precision
accuracy.meas(test_set[,3], y_pred)

#ROC Curve
roc.curve(test_set[,3], y_pred, plotit = TRUE)
#Accuracy is 76%.

#So far: Who gets hospitalized and who doesn't
#How many will get hospitalized?

#Importing the Covid 19 Distribution by Countries dataset
spread <- read.csv('covid-19-cases.csv')
#Forecasting per country
#Create a smaller dataframe for just Spain called spreadinspain
library(dplyr)
spreadinspain <- filter(spread, countriesandterritories =="Spain")
#Create a plot for spreadinspain (over time)

ggplot(data=spreadinspain, aes(x=daterep, y=cases ))+geom_point()

#Filtering the data to segment US
spreadinus <- filter(spread, countriesandterritories =="United_States_of_America")

ggplot(data=spreadinus, aes(x=daterep, y=cases ))+geom_point()

#South Korea
spreadinsk <- filter(spread, countriesandterritories == "South_Korea")

ggplot(data=spreadinsk, aes(x=daterep, y=cases ))+geom_point()

#Calculate the time between the first case and 34,272 in the US?
#Use ifelse to calcualte the month in which cases increased

#Forecast using earlyR
install.packages("earlyR") #Built for Ebola
library(earlyR)
library(incidence)
today <- as.Date("2020-05-01")
str(spreadinus)
spreadinus$daterep <- as.Date(spreadinus$daterep) #Coerced to dates
i <- incidence(spreadinus$daterep, last_date = today)
i

#Forecast using Random Forest
spreadinus <- spreadinus[,c(1:5,10)] #Can also use select in dplyr
#Coercing day month and year to be factors
spreadinus$day <- as.factor(spreadinus$day)
spreadinus$month <- as.factor(spreadinus$month)
spreadinus$year <- as.factor(spreadinus$year)

#Regress using RF
library(randomForest)
spreadinus <- spreadinus[,-1]
rf_mod <- randomForest(cases~., data=spreadinus, ntree=100, mtry = 3, importance=TRUE)
#ntree is the number of trees you are building
#mtry is the number of variables used/randomly sampled at each stage
#rank the features (Feature Ranking)
importance(rf_mod)
varImpPlot(rf_mod, main="Variable Importance")

#Reordering the data
spreadinus <- spreadinus[order(spreadinus$daterep),  ] #Ascending order
#For descending order, rev(order())

pred_rf <- predict(rf_mod, spreadinus[,-4]) #Spreadinus data without Cases
plot(pred_rf)

#Forecast for 10 more days
#install.packages("lubridate")
library(lubridate)
dates <- data_frame(daterep = seq(as.Date('2020-04-06'), by ='days', length =10))
dates$day <- factor(day(dates$daterep))
dates$month <- factor(month(dates$daterep))
dates$year <- factor(year(dates$daterep))
dates$popdata2018 <- rep(327167434, times=10)
dates$popdata2018 <- as.integer(dates$popdata2018)

#Creating a new data frame called combined
combined <- rbind(spreadinus[,-5], dates) #Removing cases

#my model has learnt based on rf_mod. Applying that to predict for 10 new days
pred_rf2 <- predict(rf_mod, combined) #Not using daterep
plot(pred_rf2)

