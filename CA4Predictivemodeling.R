################### Start of header ######################
# Title: CA 4 Prediction Modeling (Crime in Ireland)
#  
# Description of CA 4
#
# <This code is for CA 4, which is data
#  from the previous dataset in CA3 has been used
#  to build a prediction model.
#
# Author: <Roshini Darmireddi>  
# Date: <24-05-2020>
#
################### End of header ########################

#----------LOADING AND EXPLORING DATASET-------------#

library(readr)
data.crime <- read_csv("/Users/Roshini/Desktop/Data Science/CA3/crime-in-ireland/IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv")
View(data.crime)
summary(data.crime)

#modifying data for prediction
# First of all we are going to create a copy of the dataset

#Removing data from 2003 to 2008
data_copy <- data.crime[ , c(-1, -2, -4, -6, -7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-72,-71,-70)]
str(data_copy)
View(data_copy)
crimedata_5years  <- data_copy[ , -c(2:22)]
View(crimedata_5years)

crimedata_5years$'2014Total' <-crimedata_5years$`2014Q1` + crimedata_5years$`2014Q2` + crimedata_5years$`2014Q3` + crimedata_5years$`2014Q4`
crimedata_5years$'2015Total' <-crimedata_5years$`2015Q1` + crimedata_5years$`2015Q2` + crimedata_5years$`2015Q3` + crimedata_5years$`2015Q4`
crimedata_5years$'2016Total' <-crimedata_5years$`2016Q1` + crimedata_5years$`2016Q2` + crimedata_5years$`2016Q3` + crimedata_5years$`2016Q4`
crimedata_5years$'2017Total' <-crimedata_5years$`2017Q1` + crimedata_5years$`2017Q2` + crimedata_5years$`2017Q3` + crimedata_5years$`2017Q4`
crimedata_5years$'2018Total' <-crimedata_5years$`2018Q1` + crimedata_5years$`2018Q2` + crimedata_5years$`2018Q3` + crimedata_5years$`2018Q4`

crimedata_5years  <- crimedata_5years[ , -c(2:21)]
View(crimedata_5years)

crimedata_5years$offencesfrom2014to2018 <- crimedata_5years$`2014Total`+crimedata_5years$`2015Total`+crimedata_5years$`2016Total`+crimedata_5years$`2017Total`+crimedata_5years$`2018Total`
View(crimedata_5years)

#Yearly data 
years <- c( 2014, 2015, 2016, 2017, 2018)
numberofoffences <- c(225775, 225017, 199076, 213742, 215176)

noofoffences_yearwise<- data.frame(years, numberofoffences)

View(noofoffences_yearwise)

# adding all quarterly data 
install.packages("tidyr")
library(tidyr)



#OFFENCEs happend in 5 years

library(dplyr)
library(magrittr)

# solution
crimedata_5years %<>% mutate_if(is.character,as.numeric)


OFFENCES_2018 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `2018Total` = sum(`2018Total`))

View(OFFENCES_2018)

OFFENCES_2017 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `2017Total` = sum(`2017Total`))

View(OFFENCES_2017)

OFFENCES_2016 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `2016Total` = sum(`2016Total`))

View(OFFENCES_2016)

OFFENCES_2015 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `2015Total` = sum(`2015Total`))

View(OFFENCES_2015)

OFFENCES_2014 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `2014Total` = sum(`2014Total`))

View(OFFENCES_2014)

offencesfrom2014to2018 <- crimedata_5years %>% group_by(`OFFENCE CODE`) %>% summarise(., `offencesfrom2014to2018` = sum(`offencesfrom2014to2018`))

View(offencesfrom2014to2018)

yearwise_offencedata <- data.frame(crimedata_5years$`OFFENCE CODE`, OFFENCES_2018$`2018Total`,
                                   OFFENCES_2017$`2017Total`, OFFENCES_2016$`2016Total`, OFFENCES_2015$`2015Total`, OFFENCES_2014$`2014Total`, offencesfrom2014to2018$offencesfrom2014to2018)

column_names <- c('OFFENCECODE', 'TOTALOFFENCESin2018', 'TOTALOFFENCESin2017','TOTALOFFENCESin2016', 'TOTALOFFENCESin2015','TOTALOFFENCESin2014', 'offencesfrom2014to2018')
colnames(yearwise_offencedata) <- column_names
View(yearwise_offencedata)

#linear regression model with Cime rate in ireland and alcohol dataset 
linear_model <- lm(OFFENCECODE ~ offencesfrom2014to2018, data = yearwise_offencedata)
linear_model
summary(linear_model)

# plot year wise offence data to see relationship between the response(crime) and
# predictor

plot(noofoffences_yearwise$years,noofoffences_yearwise$numberofoffences,
     xlab="Crime Rate", ylab="offences",
     main = "Scatter plot showing regression line for Crime, predicted from offencecode")
abline(linear_model)

#Graph shows a there is **no** relationship between offencecode and  and offences happened in year 2014-19 variable

cor(yearwise_offencedata$offencesfrom2014to2018,yearwise_offencedata$OFFENCECODE)
confint(linear_model)

# Scatter plots helps to visualise any linear relationships between the dependent variable 
# and independent variables

scatter.smooth(x = yearwise_offencedata$OFFENCECODE, 
               y = yearwise_offencedata$offencesfrom2014to2018, 
               main = "offences ~ crimehappened",
               xlab = "offencecode",
               ylab = "crimehappened")


# Box Plot for offences
par(mfrow = c(1, 2)) # divide graph area in 2 columns
# box plot for ‘offences’
boxplot(yearwise_offencedata$OFFENCECODE, main = "OFFENCE code", 
        sub = paste("Outlier rows: ", boxplot.stats(yearwise_offencedata$offencesfrom2014to2018)$out))

# box plot for 'Years'
boxplot(noofoffences_yearwise$years, main = "Crime Rate", 
        sub = paste("Outlier rows: ", boxplot.stats(noofoffences_yearwise$numberofoffences)$out))

# Skewness function to examine normality of data
install.packages("e1071")
library(e1071)
# Density Plot
# Divide graph area in 2 columns
par(mfrow = c(1, 2))

# Density plot for numberof offences yearwise
plot(density(noofoffences_yearwise$years), main = "Density Plot :years",
     ylab = "numberofoffences",
     sub = paste("Skewness:", round(e1071::skewness(noofoffences_yearwise$numberofoffences), 2)))

# fill the area with blue
polygon(density(noofoffences_yearwise$years), col = "blue")

# Density plot for offence_codewise
plot(density(yearwise_offencedata$OFFENCECODE), main = "Density Plot :Crime Rate",
     ylab = "offencecode",
     sub = paste("Skewness:", round(e1071::skewness(yearwise_offencedata$offencesfrom2014to2018), 2)))

# Filling the area with green
polygon(density(yearwise_offencedata$OFFENCECODE), col = "green")

#------------------------------------------------------------------

# Now, calculating correlation test between years and offenceshappened
cor(yearwise_offencedata$OFFENCECODE, yearwise_offencedata$offencesfrom2014to2018)

# to build linear regression model on full data
linearMod <- lm(OFFENCECODE ~ offencesfrom2014to2018, data = yearwise_offencedata)
linearMod

# summary of model
summary(linearMod)

model_summary <- summary(linearMod)
# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

# get beta estimate for Income
beta.estimate <- model_coeffs["offencesfrom2014to2018", "Estimate"]
beta.estimate
# get std.error for Income
std_error <- model_coeffs["offencesfrom2014to2018", "Std. Error"]
std_error
# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(yearwise_offencedata) - ncol(yearwise_offencedata)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# sample chooses a random sample
# from 1:all records from yearwise_offencedata , 80% of rows
no_of_records <- sample(1:nrow(yearwise_offencedata), 0.8 * nrow(yearwise_offencedata))
# model training data
training_data <- yearwise_offencedata[no_of_records,]
training_data
# test data
testing_data <- yearwise_offencedata[-no_of_records,]
testing_data


# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lm_model <- lm(OFFENCECODE ~ offencesfrom2014to2018, data = training_data)

# model summary
summary(lm_model)



# predict  from testing data
lm_predicted <- predict(lm_model, testing_data)
summary(lm_predicted)
# make actuals_predicteds dataframe.
lm_actuals_preds <- data.frame(cbind(actuals = testing_data$offencesfrom2014to2018, 
                                     predicted = lm_predicted))
head(lm_actuals_preds)
AIC(linearMod)
BIC(linearMod)
correlation_accuracy <- cor(lm_actuals_preds)
correlation_accuracy

lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))
lm_min_max_accuracy

# MAPE
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals)) / lm_actuals_preds$actuals)
lm_mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)
plot(gvmodel)

#-----------------------------------------------------------------------------------------------------------------

#TIMESERIES data
#Time series is taken as my data consists of years from 2010 to 2019 .
crimes_data <- ts(noofoffences_yearwise$numberofoffences[0:5], start= c(2014), end = c(2018), frequency = 1)
crimes_data
plot(crimes_data)

start(crimes_data)
end(crimes_data)
frequency(crimes_data)

# Installing the necesary time series and forecasting libraries.
install.packages("tseries")
install.packages("forecast")
library(forecast)
library(tseries)

#-----------Model estimation-------------
# Plotting the Auto corelation function Acf() plot.
acf_results <- Acf(crimes_data, main = "ACF of crimes_data")
acf_results
#the measure of autocorrelation doen not cross the dashed blue line,
# then that specific lag is not significantly correlated with the associated
pacf_results <- Pacf(crimes_data, main = "PACF of crimes_data")
pacf_results

#---------Arima model--------------
arima_model <- arima(crimes_data, order = c(1,0,1))
arima_model
#AIC values are high (112.88).

#-----------------auto arima model-----------------
auto_arima_model <- auto.arima(crimes_data)
auto_arima_model
#AIC value is 109.97


#-----------finding accuracy for both the models ---------
accuracy(auto_arima_model)
#MAPE is 3.628893
accuracy(arima_model)
#MAPE(Mean absolute percentage error)  value is 3.0313.

#--------Evaluation the models.----------------
#residuals is difference between the true and predicted values.
qqnorm(arima_model$residuals,
       main = "Normal Q-Q Plot (Estimated ARIMA Model)")
qqline(arima_model$residuals)

Box.test(arima_model$residuals, type = "Ljung-Box")
#p value is 0.675 which is greater than 0.05. so null hypothesis is accepted

#----------qq plot for auto arima model--------

qqnorm(auto_arima_model$residuals,
       main = "Normal Q-Q Plot (Estimated ARIMA Model)")
qqline(arima_model$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
#p value is 0.8651 which is greater than 0.05.


#------------training and testing data----------
#training data is taken for years 2014 to 2016
train_data <- window(x = crimes_data, start=c(2014), end=c(2016))
#testing data is taken for years 2017 to 2018
test_data <- window(x=crimes_data, start=c(2017), end=c(2018))
train_data
test_data
#-----------fitting the model--------


fit <- arima(train_data, c(1,0,1))
fit
#AIC value is 72.03

#---------Forecasting----------
predict_arima <- forecast(arima_model)
predict_arima
plot(forecast(arima_model, 5), xlab = "Years", ylab = "noofoffences")

#-------forecasting auto arima model--------
predict_auto_arima <- forecast(auto_arima_model)
predict_auto_arima
plot(forecast(auto_arima_model, 5), xlab = "Year", ylab = "no of offences")
