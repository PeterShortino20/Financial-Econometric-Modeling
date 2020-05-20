rm(list = ls())
install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)
library(ggplot2)
library(fpp2)

Sales_table <- read.table("~/SeniorStats/HW3/STA372_Homework3_Question3.dat.txt")
colnames(Sales_table) <- c("Time", "Quarter", "Sales")
Sales_table$TimeSq <- Sales_table$Time^2
Sales_table$LogSales <- log(Sales_table$Sales)
head(Sales_table)

figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time, y=Sales_table$Sales), color="Black")
figure <- figure + geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales), linetype=1, color="Black")
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Sales vs. Time") + xlab("Time") + ylab("Sales")
print(figure)

print("You can see the seasonality with a spike in sales every Q4")

figure2 <- ggplot()
figure2 <- figure2 + geom_point(aes(x=Sales_table$Time, y=Sales_table$LogSales), color="Red")
figure2 <- figure2 + geom_line(aes(x=Sales_table$Time, y=Sales_table$LogSales), linetype=1, color="Red")
figure2 <- figure2 + scale_y_continuous()
figure2 <- figure2 + ggtitle("log(Sales) vs. Time") + xlab("Time") + ylab("log(Sales)")
print(figure2)

LogSales_time_series <- ts(Sales_table[,4], frequency = 4)
LogSales_time_series_components <- stl(LogSales_time_series, s.window =7)
plot(LogSales_time_series_components)
Sales_table$seasonal <- LogSales_time_series_components$time.series[,1]
Sales_table$LogA <- Sales_table$LogSales - Sales_table$seasonal
head(Sales_table)

figure3 <- ggplot()
figure3 <- figure3 + geom_point(aes(x=Sales_table$Time, y=Sales_table$LogA), color="Red")
figure3 <- figure3 + geom_line(aes(x=Sales_table$Time, y=Sales_table$LogA), linetype=1, color="Red")
figure3 <- figure3 + scale_y_continuous()
figure3 <- figure3 + ggtitle("LogA vs. Time") + xlab("Time") + ylab("LogA")
print(figure3)

reg1 <- lm(Sales_table$LogA ~ Sales_table$Time + Sales_table$TimeSq)
summary(reg1)
fittedValues = reg1$coef[1] + reg1$coef[2]*Sales_table$Time + reg1$coef[3]*Sales_table$TimeSq 
resid = residuals(reg1)

figure4 <- ggplot()
figure4 <- figure4 + geom_point(aes(x=Sales_table$Time, y=Sales_table$LogA), color="Black")
figure4 <- figure4 + geom_line(aes(x=Sales_table$Time, y=fittedValues), linetype=1, color="Black")
figure4 <- figure4 + scale_y_continuous()
figure4 <- figure4 + ggtitle("LogA vs. Time with Fitted Values from Regression") + xlab("Time") + ylab("LogA")
print(figure4)

figure5 <- ggplot()
figure5 <- figure5 + geom_point(aes(x=Sales_table$Time, y=resid), color="Black")
figure5 <- figure5 + geom_line(aes(x=Sales_table$Time, y=resid), linetype=1, color="Black")
figure5 <- figure5 + geom_hline(yintercept = 0)
figure5 <- figure5 + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals")
print(figure5)
#plot Autocorrelation
ggAcf(resid)

###Is there evidence of autocorrelation in the plot?
print("There is evidence of some autocorrelation at certain lags we can see this in our plot")
print("This implies that the information left in the residuals is unsound and can therfore lead to a unsound model over time")
tail(Sales_table)

Time<- c(Sales_table$Time,41:44)
Time_sq<- Time^2
seasonal <- c(Sales_table$seasonal, Sales_table$seasonal[37], Sales_table$seasonal[38], Sales_table$seasonal[39], Sales_table$seasonal[40])
Sales_table_extended <- data.frame(Time, Time_sq,seasonal)
tail(Sales_table_extended)

##Compute in sample and out sample forcasts
Sales_table_extended$forcast_logA = reg1$coef[1] + reg1$coef[2]*Sales_table_extended$Time + reg1$coef[3]*Sales_table_extended$Time_sq
Sales_table_extended$forcast_logSales = Sales_table_extended$forcast_logA + Sales_table_extended$seasonal
Sales_table_extended$forcast_Sales = exp(Sales_table_extended$forcast_logSales)
tail(Sales_table_extended)


figure6 <- ggplot()
figure6 <- figure6 + geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales), linetype = 1, color = "Red")
figure6 <- figure6 + geom_line(aes(x=Sales_table_extended$Time, y=Sales_table_extended$forcast_Sales), linetype=2, color="Black")
figure6 <- figure6 + scale_y_continuous()
figure6 <- figure6 + ggtitle("Sales vs Time with Forcasts") + xlab("Time") + ylab("Sales")
print(figure6)
print("According to figure6 the in sample forcasts are not doing a great job reflecting sales as time moves forward. Since our in sample forcasts are off our out of sample forcast is bound to be off. = Not good ")

print("I would not feel completly comforatble using my out of sample forcasts for Q41-44 becasue the future is very uncertain, the in sample forcast was not great, and retail companies can hit blimps or glitches in any given quarter especially in this instance.")

print(Sales_table_extended)

Answer<-CI(Sales_table_extended$forcast_Sales, ci = 0.95)
print(Answer)
print("The calculated 95% Confidence Interval for Quarter41 is 1297295.1 while the Quarter 41 sales value is 10507.67, thus showing that this quarter sales fell outside our Confidence in the signifigance.")

print("My model did not do a great job of predicting these values, as mentioned previously the sales value inside Quarter 41 is not inside our 95% CI.")