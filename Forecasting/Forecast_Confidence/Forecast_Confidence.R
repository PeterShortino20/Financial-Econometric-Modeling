rm(list = ls())
library(ggplot2)
library(fpp2)

file <- "~/SeniorStats/HW5/STA372_Homework5_Question2.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "")
colnames(data_table) <- c("Time", "Quarter", "Sales")
data_table$log_Sales <- log(data_table$Sales)
head(data_table)
tail(data_table)

log_Sales_ts <- ts(data_table[,4], frequency = 4)
result <- stl(log_Sales_ts, s.window = 7)
plot(result)
summary(result)

print("The pattern of seasonality in Publix's sales does look fairly consistent through time")
print("This can possibly be explained due to the stores industry which is grocery, and we have some evidence of this in the Salves v time plot given.")
print("Publix's sales typically are the highest in Quarter 4")

data_table$seasonal <- result$time.series[,1]
data_table$logA <- data_table$log_Sales - data_table$seasonal
data_table$A <- exp(data_table$logA)
tail(data_table)

figure <- ggplot()
figure <- figure + geom_point(data=data_table, aes(Time, logA), color="Black") 
figure <- figure + geom_line(data=data_table, aes(Time, logA), color="Black")
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Seasonally adjusted log(Sales)") + xlab("Time") + ylab("log(A)")
print (figure) 

print("The seasonal adjustment did a good job. We can observe this through the pattern of teh Seasonally adjusted log(Sales) grap ")

print("There is a consistently observable trend through time. The trend hardly varies from its overall stucture.")

logA_time_series <- ts(data_table[,6])
result <- holt(logA_time_series, h=4)
result$model

data_table$forcasts <- result$fitted
data_table$residuals <- result$residuals
head(data_table)
tail(data_table)

head(result$model$state[,1:2])
tail(result$model$state[,1:2])
result
print("The in-sample forcasts are good  in therms of sigma estimate we see only sigma = .0247")

accuracy(result)
ggAcf(data_table$residuals)
summary(result)

print("The RMSE is .023369469, The errors appear to be independent. y4 lag looks peculiar but is the only such occurance in the ACF command result. It is important to have independent errors so you know that no extra infomration is left in the residuals.")

print("Y62 = L61 + B61 + e62 ,,,,,   By hand The one period ahead forcast is 7.894091 + .0247 - .0245 = 7.894291")
print("95% CI is 7.819001 - 1.96(.0245), 7.819001 + 1.96(.0245)....... The CI is (7.770981,7.867021)")

one <- exp(7.770981)
two <- exp(7.867021)
print(one)
print(two)
print("the CI is 2370.796,2609.779")

Sales_time_series <- ts(data_table[,3], frequency=4)
result <- stlm(Sales_time_series, s.window=7, method = "ets", etsmodel = "AAN", lambda=0)
result

forecast(result, h=3)
plot(forecast(result, h=3))


print("The by hand method gave me a CI 95% of (2370.796, 2609.779) while the computed 95% CI is (2355.459, 2593.014)")
