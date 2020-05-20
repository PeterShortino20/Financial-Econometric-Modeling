rm(list=ls())
library(ggplot2)
library(fpp2)

Sales_table <- read.table("~/SeniorStats/HW2/STA372_Homework2.dat.txt")
colnames(Sales_table) <- c("Time", "Quarter", "Sales")
Sales_table$LogSales <- log(Sales_table$Sales)
head(Sales_table)

figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time, y=Sales_table$Sales), color="Black")
figure <- figure + geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales), linetype=1, color="Black")
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Sales vs. Time") + xlab("Time") + ylab("Sales")
print(figure)

figure2 <- ggplot()
figure2 <- figure2 + geom_point(aes(x=Sales_table$Time, y=Sales_table$LogSales), color="Red")
figure2 <- figure2 + geom_line(aes(x=Sales_table$Time, y=Sales_table$LogSales), linetype=1, color="Red")
figure2 <- figure2 + scale_y_continuous()
figure2 <- figure2 + ggtitle("log(Sales) vs. Time") + xlab("Time") + ylab("log(Sales)")
print(figure2)

print("The log transformation stabilized the variance")

LogSales_time_series <- ts(Sales_table$LogSales, frequency = 4)
LogSales_time_series_components <- stl(LogSales_time_series, s.window =7)
plot(LogSales_time_series_components)
seasonal <- LogSales_time_series_components$time.series[,1]

Sales_table$LogA <- Sales_table$LogSales - seasonal
head(Sales_table)

figure3 <- ggplot()
figure3 <- figure3 + geom_point(aes(x=Sales_table$Time, y=Sales_table$LogA), color="Black")
figure3 <- figure3 + geom_line(aes(x=Sales_table$Time, y=Sales_table$LogA), linetype=1, color="Black")
figure3 <- figure3 + scale_y_continuous()
figure3 <- figure3 + ggtitle("Seasonally adjusted log(Sales)") + xlab("Time") + ylab("LogA")
print(figure3)

Sales_table$A <- exp(Sales_table$LogSales - seasonal)
head(Sales_table)

figure4 <- ggplot()
figure4 <- figure4 + geom_point(aes(x=Sales_table$Time, y=Sales_table$A), color="Black")
figure4 <- figure4 + geom_line(aes(x=Sales_table$Time, y=Sales_table$A), linetype=1, color="Black")
figure4 <- figure4 + scale_y_continuous()
figure4 <- figure4 + ggtitle("Seasonally adjusted Sales") + xlab("Time") + ylab("A")
print(figure4)

print("The stl decomposition procedure did a good job of seasonally adjusting sales")