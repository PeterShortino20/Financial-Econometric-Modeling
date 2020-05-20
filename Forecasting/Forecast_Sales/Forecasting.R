rm(list = ls())
library(ggplot2)
library(fpp2)
library(Rmisc)

file <- "~/SeniorStats/HW8/STA372_Homework8_Question2.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "")
colnames(data_table) <- c("Week","Applications")

## problem (a)
mean_Applications <- mean(data_table$Applications)
mean_Applications
figure <- ggplot()
figure <- figure + geom_point(aes(data_table$Week,data_table$Applications))
figure <- figure + geom_line(aes(data_table$Week,data_table$Applications))
figure <- figure + scale_y_continuous()
figure <- figure + geom_hline(aes(yintercept=mean_Applications), lty=2)
figure <- figure + ggtitle("Applications vs. Weeks")
figure <- figure + xlab("Week") + ylab("Applications")
print(figure)
print("The Applicationstime series at first glances appears to be a stationary time series. This is due to the relatively  constant volatility and a cuddling to the mean of the time series.")

## problem (b)
Applications_diff <- diff(data_table$Applications, differences = 1)
data_table$Applications_diff[1]<- NA
data_table$Applications_diff[2:104] <- Applications_diff
ggAcf(data_table$Applications_diff[2:104])
print("Yes, the autocorrelation function provides us evidence that the Applications time series is stationary - via page 12 of Lecture Notes Arima Models Part 1 Stationary")

##problem (c)
Applications_time_series <- ts(data_table[,2])
result <- Arima(Applications_time_series, order = c(1,0,0))
result
result$aicc
result$sigma2
result2 <- Arima(Applications_time_series,order = c(2,0,0))
result2
result2$aicc
result2$sigma2
result3 <- Arima(Applications_time_series,order = c(3,1,0))
result3
result3$aicc
result$sigma2
print("We should use the AR(2) model as it gave us the best AICc...lowest")

##problem (d)
result2$residuals
plot(result2$residuals)
ggAcf(result2$residuals)
print("The model does appear to do a good job modeling the pattern in Application data.")
##problem(e)
forecast(result2, h=4)
print("The width of the CIs reflects the variability or better known as the standard deviation of the past data. Standard deviation effects the outcome of future forecasts")
##problem(f)
predict(result2, n.ahead = 50)
print("The forcasts are indeed approaching a common value in this case around 66.854. The standard erros appear to be approaching a common value in this case around 7.736287. This is caused due to the error characteristics of this time series. The error term takes into account previous observations, thus the forcasts will always be quasi bound by what happened previously.")
##problem(g)
firstfore<-forecast(result2, h=50)
plot(firstfore)
firstfore
print("The confidence intervals presented are related to the problem f predict functions output because the predict fuctions uses the same parameters to predict a future out of sample value which then gives residuals and the errors. This additional information is what is used to calulate variability from the forcast which gives a penultimate confidence interval which is given by forcast function.")
print("The confidence intervals are asymptoting out because they are predictions that are becoming less accurate through time.")
