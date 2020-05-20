rm(list = ls())
library(ggplot2)
library(fpp2)
library(Rmisc)
library(tseries)

file <- "~/SeniorStats/HW10/STA372_Homework10_Question2.dat.txt"
Sales_table <- read.table(file, header = FALSE, sep = "")
colnames(Sales_table) <- c("Month", "Yield")

## (a)  Plot the monthly U.S. 10-year bond yields for the period January, 1994 - May, 2004. Based on a visual inspection, do the 10-year bond yields appear to be stationary?
figure <- ggplot()
figure <- figure + geom_point(aes(Sales_table$Month,Sales_table$Yield))
figure <- figure + geom_line(aes(Sales_table$Month,Sales_table$Yield))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("10-Year Bond Yields vs. Time")
figure <- figure + xlab("Month") + ylab("10-Year Bond Yield")
print(figure)
print("The 10-Year Bond Yields appears to not be stationary")

## (B) Compute the autocorrelation function and augmented Dickey-Fuller test statistic for the monthly U.S. 10-year bond yields. Do they provide evidence that the time series of bond yields is nonstationary? Why or why not?
diff_Yielddiff <- diff(Sales_table$Yield, difference=1)
Sales_table$diff_Yielddiff[1] <- NA
Sales_table$diff_Yielddiff[2:125] <- diff_Yielddiff
head(Sales_table)
tail(Sales_table)
ggAcf(Sales_table$Yield)
print("The ggAcf function reveals that there is evidence that the time series of bond yields is nonstationary.The significance of the residual lag is dying off over time.")

#(C)
figure2 <- ggplot()
figure2 <- figure2 + geom_point(aes(Sales_table$Month,Sales_table$diff_Yielddiff))
figure2 <- figure2 + geom_line(aes(Sales_table$Month,Sales_table$diff_Yielddiff))
figure2 <- figure2 + scale_y_continuous()
figure2 <- figure2 + ggtitle("First Difference vs Time")
figure2 <- figure2 + xlab("Month") + ylab("First differences of Bond Price")
print(figure2)
print("The First Differences appear to be stationary over time.")

## (D)
y_time_series <- ts(Sales_table[,2])
adf.test(y_time_series)
ggAcf(Sales_table$diff_Yielddiff[2:125])
print("The Acf function reveals evidence that the first differences are stationary, because there is very little value left in the residuals through time.")

## (E)
result <- auto.arima(y_time_series, ic="aicc", test="adf", stepwise=FALSE,approximation=FALSE, trace=TRUE)
result
ggAcf(result$residuals) 
print("The best model is the Arima(0,1,1) model, I had a hunch it would be a (0,1,0) intially (random), so I wasnt greatly surprised when the Arima(0,1,1) was shown as the best when realting it back to my experience with my autocorrelation functions for the bond yields and their first differences")
#(f)
print("The parameters of this best Arima(0,1,1) model are s.e of .090 and the ma1 of .322. Generally the errors are independent barrign the one divergence of this through time at lag 5.")

##(g)

forecast(result, h = 5)
##(h)
print("The five forecasts are the same becuase the model here (ARIMA(0,1,1)) is most similar to a basic expotential smoothing model which in where the forecasts are the same through time. ")

##(I)
print("The 80% CIs thorugh time will naturally slowly converge as we can see in the forecasts that the 80% CIs are slowly converging to a certain point on both the High and low side.")




