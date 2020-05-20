rm(list = ls())
library(ggplot2)
library(fpp2)
library(Rmisc)


file <- "~/SeniorStats/HW9/STA372_Homework9_Question2.dat.txt"
Sales_table <- read.table(file, header = FALSE, sep = "")
colnames(Sales_table) <- c("Time", "Quarter", "Sales")
Sales_table$TimeSq <- Sales_table$Time^2
Sales_table$LogSales <- log(Sales_table$Sales)
head(Sales_table)

#(a) Why is it important to analyze log(Sales) rather than Sales?

print("It is important to analyze log(sales) rather than Sales, as log(sales) gives the ability to stabilize the variance of the strong seasonal pattern, and provide more evidence of an upward trend") 

#(b) Use the stl command to seasonally adjust log(Sales) and store the 
#results in log_A.

LogSales_time_series <- ts(Sales_table$LogSales, frequency=4)


LogSales_time_series_components <- stl(LogSales_time_series, s.window=7)

LogSales_time_series_components
plot(LogSales_time_series_components)
seasonal <- LogSales_time_series_components$time.series[,1]
seasonal
Sales_table$LogA <- Sales_table$LogSales - seasonal
head(Sales_table)

#From here on out, use the first 23 observations for estimations and 
#last 4 observations for out-of-sample forecasts

#(C) Use the ets command to select the best exponential smoothing model 
#for log_A. Be sure to allow for additive and multiplicative errors, and 
#additive damped and non-damped trends. It is not necessary to allow for 
#a multiplicative trend. Store the results in the object result_ets. 
#Are the errors from the best model independent? Use the forecast command 
#to forecast log_A in quarters 24-27.

result_ets <- (Sales_table$LogA)

Selection <- ets(result_ets, model="ZZN", damped = NULL, allow.multiplicative.trend = FALSE)
Selection

print("The best model is the MAN model.")
#The errors from the best model are in fact independent

forecast(result_ets, h=4)

#(d) use the commands to compute the out-of-sample RMSE and MAE for 
#the four forecasts of log_A.

log_A_time_series <- ts(Sales_table$LogA)

accuracy_result_ets <- result_ets %>% forecast(h=4) %>% accuracy(log_A_time_series)
accuracy_result_ets[,c("RMSE", "MAE")]

#(e) Use the Arima command to estimate an ARIMA(1, 1, 0) model for log_A. 
#Do not include a drift term (which is the default, or equivalently, set 
#include.constant=FALSE). Are the residuals from this model independent? 
#Use the forecast command to forecast log_A in quarters 24-27.


###########################################################################################
#result_Arima <- Arima(log_A_time_series, order=c(1, 1, 0), include.constant = FALSE)
Sales_table
log_A_time_series_train <- ts(Sales_table$LogA)
result_Arima <- Arima(log_A_time_series_train, order=c(1, 1, 0), include.constant = FALSE)
###########################################################################################
result_Arima
log_A_time_series <- ts(Sales_table[,3])
###########################################################################################
#result_Arima <- Arima(log_A_time_series, order=c(1, 1, 0), include.constant = FALSE)

log_A_time_series_train <- ts(Sales_table[1:23,3])
result_Arima <- Arima(log_A_time_series_train, order=c(1, 1, 0), include.constant = FALSE)
forecast(result_Arima, h = 4)
ggAcf(result_Arima$residuals)

print("The residuals are as expected for this Arima process.")


#(f) Use the accuracy command to compute the out-of-sample
#RMSE and MAE for the four forecasts of log_A
accuracy_result_Arima <- accuracy(forecast(result_Arima, h=4), log_A_time_series)
accuracy_result_Arima[,c("RMSE", "MAE")]

#(g) Which model (exponential smoothing or ARIMA(1, 1, 0)) 
#should be used to forecast log_A in quarters 28-31? Why?

print("Based on the RMSE values for exponentional smoothing and ARIMA(1,1,0), the RMSE value for the training set and test set are .07667 and .3218, respectively which are much lower than the RMSE values for ARIMA(1,1,0) of 65.511 and 111.064 for the training set and test set, respectively. Therefore, based on my data exponential smoothing is a better forecast.")

#(h) Using the appropriate model and all 27 observations,
#compute a forecast and 95% confidence interval for Sales in quarter 28.

result_ets_Holt<-ets(log_A_time_series, model="ZZN", damped = NULL)
result_ets_Holt
tail(result_ets_Holt$states)
seasonal
forecast(result_ets_Holt, h =5)
