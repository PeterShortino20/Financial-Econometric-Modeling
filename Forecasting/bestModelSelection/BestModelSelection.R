rm(list = ls())
library(ggplot2)
library(fpp2)
library(Rmisc)

file <- "~/SeniorStats/HW7/STA372_Homework7_Question1.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "")
colnames(data_table) <- c("Time","People_World")

figure <- ggplot()
figure <- figure + geom_point(aes(data_table$Time,data_table$People_World))
figure <- figure + geom_line(aes(data_table$Time,data_table$People_World))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("People_World vs. Time")
figure <- figure + xlab("Time") + ylab("People_World")
print(figure)

print("Over the last 12 months the # of Tumblr users growth rate went from increasing very rapidly to a very subdued rate with certain month over month periods where total users went down.")
print("Intuitivly  I beleive Holt's Exponential smoothing model may be the reasonable model to use.")

print("People_World153 = (1.547)^152 * 19,020,118),  = 1.207093439E36 users")
print("I do not think this growth rate can continue into the future. It would be impossible.")

## problem c 

print("The 772 million user projection seems much more reasonable than the impossible amount computed in part (b). This also raises my suspicions on accuracy.")

People_World_time_series <- ts(data_table[,2])
result_AAN <- ets(People_World_time_series, model ="AAN", damped = FALSE)
result_AAN
ggAcf(result_AAN$residuals)
forecast_AAN <- forecast(result_AAN, h=115)
forecast_AAN
plot(forecast(result_AAN, h=115))
write.csv(forecast_AAN$mean, file = 'Forecasts_AAn.csv')
print("The resulting value is 928 million, this is a reasonable valuation for an app of its kind in my opinion. To this point I would be comfortable using this model unless a better outcome could be reached using another model")


People_World_time_series2 <- ts(data_table[,2])
result_MMN <- ets(People_World_time_series2, model ="MMN", damped = FALSE)
result_MMN
ggAcf(result_MMN$residuals)
forecast_MMN <- forecast(result_MMN, h=115)
forecast_MMN
plot(forecast(result_MMN, h=115))
write.csv(forecast_MMN$mean, file = 'Forecasts_MMn1.csv')

print("The resulting value of the company is 520 million given the MMN model.")
print("If i were a potential investor I would be comfortable using this as it seems to be more conservative in its result.")
print("I would have much less confidence in in my forcasts later in the forecasting period, thus I would be extremly skeptical of any confidence of the value made from the possible large scope in the variability of the users.")

People_World_time_series3 <- ts(data_table[,2])
result_MMdN <- ets(People_World_time_series3, model ="MMN", damped = TRUE)
result_MMdN
ggAcf(result_MMdN$residuals)
forecast_MMdN <- forecast(result_MMdN, h=115)
forecast_MMdN
plot(forecast(result_MMdN, h=115))
write.csv(forecast_MMdN$mean, file = 'Forecasts_MMdN1.csv')

print("The resulting value of the company given the MMdN model is 501 million users. I would be much more comfortable using this model, as the confidence intervals are very reasonable.")

selection <- ets(People_World_time_series, model="ZZN", damped = NULL, allow.multiplicative.trend = TRUE)
selection

print("The ets best selection produced the ets MMdN model.")

##problem h 
print("Using a simulation I could model the error and the variance in that error and use a thematic lattice for deciding the errors and their present/future pull on the forcast. A for loop can be used to simulate error data and variance for each point and thus the nexxt point forward has data to work with.")
