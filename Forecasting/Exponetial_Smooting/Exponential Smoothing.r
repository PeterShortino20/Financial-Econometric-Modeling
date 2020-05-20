rm(list = ls())
library(ggplot2)
library(fpp2)
library(RMSE)
file <- "~/SeniorStats/HW4/STA372_Homework4_Question3.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "")
colnames(data_table) <- c("Week","Y","FixedForcast")

head(data_table)
tail(data_table)


figure <- ggplot()
figure <- figure + geom_point(aes(data_table$Week, data_table$Y))
figure <- figure + geom_line(aes(data_table$Week, data_table$Y))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Time series plot of Y (Solid black line) and in-sample forecasts")
figure <- figure + xlab("Week") + ylab("Y")
print (figure)
print("There is a pattern in Y through time and it is a pattern of increasing over time")

data_table$Error <- data_table$Y - data_table$FixedForcast
RMSE_FixedForcast <- sqrt(sum(data_table$Error^2)/nrow(data_table))
print(RMSE_FixedForcast)

y_time_series <- ts(data_table[,2]) 
result_random_walk <- ses(y_time_series, alpha=1, initial="simple", h=1)
result_random_walk$model
accuracy(result_random_walk)
print("The RMSE here was 63.10765")

data_table$forcast <- result_random_walk$fitted
data_table$residualswalk <- result_random_walk$residuals
head(data_table)
tail(data_table)

result <- ses(y_time_series, initial="simple", h=3)
result$model

print(result)
data_table$forcast2 <- result$fitted
data_table$residuals <-result$residuals
head(data_table)
tail(data_table)

figure2 <- ggplot()  
figure2 <- figure2 + geom_point(aes(data_table$Week, data_table$Y))
figure2 <- figure2 + geom_line(aes(data_table$Week, data_table$Y))
figure2 <- figure2 + geom_point(aes(data_table$Week, data_table$forcast2), color="Red")
figure2 <- figure2 + geom_line(aes(data_table$Week, data_table$forcast2), color="Red", lty=2)
figure2 <- figure2 + scale_y_continuous()
figure2 <- figure2 + ggtitle("Time series plot of Y (Solid black line) and in-sample forecasts") 
figure2 <- figure2 + xlab("Week") + ylab("Y")
print (figure2)
accuracy(result)
print("The RMSE here is 54.70586")



figure3 <- ggplot()
figure3 <- figure3 + geom_point(aes(data_table$Week, data_table$residuals))
figure3 <- figure3 + geom_line(aes(data_table$Week, data_table$residuals))
figure3 <- figure3 + scale_y_continuous()
figure3 <- figure3 + ggtitle("In-sample forecast errors")
figure3 <- figure3 + xlab("Time") + ylab("Residuals")
print (figure3) 

ggAcf(data_table$residuals)
summary(result)

print("The residuals are not independent, this implies that there is information leftover in the residuals that will not be reflected in the formal forcasting.")
print("We should use the model that was used for figure 2, the Simple Exponential smoothing model, not the random walk model")
print("seasonal factor is .975")
seasonal <- .975*839.4273
seasonal
print("The week 67 forcast of volume is 818.4416, and the 95% CI is (732.2058, 946.6488)")
