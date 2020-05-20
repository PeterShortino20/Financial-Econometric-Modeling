library(dplyr)
library(ggplot2)
library(fpp2)
library(tseries)
install.packages('fGarch')
library(fGarch)
rm(list = ls())

#import data
## military_data <- read.csv("/Users/karthikvaidyanathan/Desktop/Military_Data_start_1_1_59.csv)
military_data <- read.csv("~/SeniorStats/ProjectSenStats/Military_Data_start_1_1_59.csv", header = TRUE)
View(military_data)
military_data<-as.data.frame(military_data)

figure = ggplot()
figure <- figure + geom_point(aes(military_data$Time, military_data$Decimal))
figure <- figure + geom_line(aes(military_data$Time, military_data$Decimal))
figure <- figure + ggtitle("Proportion of Military Spending to GDP over time (from 1/1/1959)")
figure <- figure + xlab("Quarters Starting in 1/1/1959") + ylab("Military Spending to GDP")
print(figure)

#transform into time series 
mil_data_decimal_ts <- ts(military_data$Decimal, frequency = 4)
mil_data_percent_ts_components <- stl(mil_data_decimal_ts, s.window = 7)
View(mil_data_percent_ts_components$time.series)
military_data$seasonal <- mil_data_percent_ts_components$time.series[,1]
military_data$Decimal_deseasonalized <- military_data$Decimal - military_data$seasonal

ggAcf(military_data$Decimal_deseasonalized)
adf.test(military_data$Decimal_deseasonalized)

first_diff<-diff(military_data$Decimal_deseasonalized, differences = 1)
military_data$firstdiff[1]<-NA
military_data$firstdiff[2:241]<-first_diff
ggAcf(military_data$firstdiff[2:241])
first_diff_ts <- ts(military_data$firstdiff[2:241])
adf.test(first_diff_ts)

mean_first_diff <- mean(military_data$firstdiff[2:241])
figure = ggplot()
figure <- figure + geom_point(aes(military_data$Time, military_data$firstdiff))
figure <- figure + geom_line(aes(military_data$Time, military_data$firstdiff))
figure <- figure + geom_hline(aes(yintercept = mean_first_diff), lty=1)
print(figure)

mil_data_decimal_deseasonal_ts<-ts(military_data$Decimal_deseasonalized)
mil_data_deseasonal_edit <- ts(mil_data_decimal_deseasonal_ts[1:237])
mil_data_deseasonal_edit
result_ets <- ets(mil_data_deseasonal_edit, model = "ZZN", damped = NULL)
result_ets
tail(result_ets$states)
ggAcf(result_ets$residuals)
forecast(result_ets, h=4)

accuracy_result_ets <- result_ets %>% forecast(h=4) %>% accuracy(mil_data_decimal_deseasonal_ts)
accuracy_result_ets[,c("RMSE", "MAE")]

result_ARIMA <- auto.arima(mil_data_deseasonal_edit, ic = "aicc", test = "adf", stepwise = FALSE, approximation = FALSE, trace = TRUE)
result_ARIMA

accuracy_result_ARIMA <- result_ARIMA %>% forecast(h=4) %>% accuracy(mil_data_decimal_deseasonal_ts)                    
accuracy_result_ARIMA[,c("RMSE", "MAE")]

result_arch1 <- garchFit(mil_data_deseasonal_edit~garch(1,0), data=mil_data_deseasonal_edit, include.constant=T,trace=F)
summary(result_arch1) 
head(result_arch1@residuals)
tail(result_arch1@residuals)
head(result_arch1@sigma.t)
tail(result_arch1@sigma.t)
predict(result_arch1, 5)

acf_eta <- ggAcf(result_arch1@residuals)
acf_eta
acf_eta$data$Freq[1:9]
result_arch1

result_arch2 <- garchFit(mil_data_deseasonal_edit~garch(1,1), data=mil_data_deseasonal_edit, include.constant=T,trace=F)
summary(result_arch2) 
head(result_arch2@residuals)
tail(result_arch2@residuals)
head(result_arch2@sigma.t)
tail(result_arch2@sigma.t)
predict(result_arch2, 5)


acf_eta2 <- ggAcf(result_arch2@residuals)
acf_eta2
acf_eta2$data$Freq[1:9]
result_arch2

