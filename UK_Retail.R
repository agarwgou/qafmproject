#install.packages("car")
library(dplyr)
library(ggplot2)
library(corrplot)
library(tseries)
library(fpp2)
library(urca)
library(tidyr)
library(nortest)
library(tseries)
library(car)
library(stats)
library(zoo)
source("Data_Transformation.R")

#Load Data
uk_data <- read.csv("Combined_timeseries_UK_v2_smoothened.csv")
#uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")
uk_data <- subset(uk_data, select = -DATE)

uk_data_ts <- ts(uk_data[, -1], start = c(2002, 1), frequency = 4)
autoplot(uk_data_ts[,"Europe.and.UK.Retail.Banking"])

new_uk_data <- add_diff_columns(uk_data)
new_uk_data <- add_lag_columns(new_uk_data)

file_path <- "new_uk_data.csv"
write.csv(new_uk_data, file = file_path, row.names = FALSE)

# Working on Loss time series
loss_ts <- ts(uk_data$Europe.and.UK.Retail.Banking, start = c(2002, 1), frequency = 4)
nsdiffs(loss_ts)
ndiffs(loss_ts)

kpss_output<-ur.kpss(loss_ts)
summary(kpss_output)

adf_output <- adf.test(loss_ts)
adf_output

loss_ts_D1 <- diff(loss_ts)
nsdiffs(loss_ts_D1)
ndiffs(loss_ts_D1)

kpss_output<-ur.kpss(loss_ts_D1)
summary(kpss_output)

adf_output <- adf.test(loss_ts_D1)
adf_output

autoplot(loss_ts_D1)

BoxCox.lambda(loss_ts)
loss_ts_boxcoxD1 <- (diff(BoxCox(loss_ts,lambda=BoxCox.lambda(loss_ts))))
autoplot(loss_ts_boxcoxD1)
nsdiffs(loss_ts_boxcoxD1)
ndiffs(loss_ts_boxcoxD1)

kpss_output<-ur.kpss(loss_ts_boxcoxD1)
summary(kpss_output)

adf_output <- adf.test(loss_ts_boxcoxD1)
adf_output

loss_ts_test <- diff(log(loss_ts))
autoplot(loss_ts_test)

## 

autoplot(ts(new_uk_data$Europe.and.UK.Commercial.Banking_LogD1, start = c(2002, 1), frequency = 4))

new_uk_data$RGDPMRPUKQ_LogD1

diff(log(new_uk_data$RGDPMRPUKQ))

## KPSS Test

kpss_output<-ur.kpss(loss_ts)
summary(kpss_output)

## AUTO ARIMA

tsdisplay(loss_ts_boxcoxD1)
split_point <- 44
loss_ts_boxcoxD1_train <- ts(loss_ts_boxcoxD1[1:split_point],start = c(2002,2),frequency=4)
loss_ts_boxcoxD1_test <- ts(loss_ts_boxcoxD1[(split_point+1):length(loss_ts_boxcoxD1)],start=c(2013,2),frequency=4)

model_arima <- auto.arima(loss_ts_boxcoxD1_train)
summary(model_arima)

## External Variable

## Perform 
ndiff_results <- list()

for (col in names(new_uk_data)) {
  ndiff_result <- ndiffs(new_uk_data[[col]])
  ndiff_results[[col]] <- ndiff_result
}

file_path <- "ndiff_results.csv"
write.csv(ndiff_results, file = file_path, row.names = FALSE)

lm_model_results <- list()
for (col in names(new_uk_data)){
  print(col)
  data <- data.frame(new_uk_data$Europe.and.UK.Retail.Banking_LogD1,new_uk_data[[col]])
  data <- na.omit(data)
  data <- data[!apply(data, 1, function(row) any(!is.finite(row))), ]
  print(data)
  lm_model <- lm(paste(colnames(data)[1],"~",colnames(data)[2]), data=data)
  lm_model_results[[col]] <- summary(lm_model)$adj.r.squared
}

file_path <- "lm_model_results_Retail.csv"
write.csv(lm_model_results, file = file_path, row.names = FALSE)

corrplot(cor(uk_data_ts))

#Variables Choosen - EurogdpperCapita_D1_L3, EurogdpperCapita_D2_L3, EurogdpperCapita_LogD1_L3, UKhouseIndexD2_L4

eurogdppercapita_D1_L3 <- ts(new_uk_data$eurogdppercapita_D1_L3, start = c(2003, 1), frequency = 4)
autoplot(eurogdppercapita_D1_L3)

eurogdppercapita_D2_L3 <- ts(new_uk_data$eurogdppercapita_D2_L3, start = c(2003, 2), frequency = 4)
autoplot(eurogdppercapita_D2_L3)

eurogdppercapita_LogD1_L3 <- ts(new_uk_data$eurogdppercapita_LogD1_L3, start = c(2003, 1), frequency = 4)
autoplot(eurogdppercapita_LogD1_L3)

UKhouseindex_D2_L4 <- ts(new_uk_data$UKhouseindex_D2_L4, start = c(2003, 3), frequency = 4)
autoplot(UKhouseindex_D2_L4)

eurogdppercapita_D1_L3_train <- window(eurogdppercapita_D1_L3, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))
eurogdppercapita_D2_L3_train <- window(eurogdppercapita_D2_L3, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))
eurogdppercapita_LogD1_L3_train <- window(eurogdppercapita_LogD1_L3, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))
UKhouseindex_D2_L4_train <- window(UKhouseindex_D2_L4, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))
loss_ts_boxcoxD1_train <- window(loss_ts_boxcoxD1, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))
loss_ts_train <- window(loss_ts, start=as.yearqtr("2005 Q1"), end=as.yearqtr("2013 Q4"))

eurogdppercapita_D1_L3_test <- window(eurogdppercapita_D1_L3, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))
eurogdppercapita_D2_L3_test <- window(eurogdppercapita_D2_L3, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))
eurogdppercapita_LogD1_L3_test <- window(eurogdppercapita_LogD1_L3, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))
UKhouseindex_D2_L4_test <- window(UKhouseindex_D2_L4, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))
loss_ts_boxcoxD1_test <- window(loss_ts_boxcoxD1, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))
loss_ts_test <- window(loss_ts, start=as.yearqtr("2014 Q1"), end=as.yearqtr("2015 Q4"))

#Multi Variate OLS
df_ols = cbind(eurogdppercapita_D1_L3_train,eurogdppercapita_D2_L3_train,eurogdppercapita_LogD1_L3_train,UKhouseindex_D2_L4_train,loss_ts_boxcoxD1_train)
ols <- lm(loss_ts_boxcoxD1_train~ eurogdppercapita_D1_L3_train + eurogdppercapita_D2_L3_train + eurogdppercapita_LogD1_L3_train + UKhouseindex_D2_L4_train, data = df_ols)
summary(ols)

ex_macro_train = cbind(eurogdppercapita_LogD1_L3_train,UKhouseindex_D2_L4_train)
ex_macro_test = cbind(eurogdppercapita_LogD1_L3_test,UKhouseindex_D2_L4_test)

##Auto Arima
#arimax_model <- auto.arima(loss_ts_boxcoxD1_train,xreg=ex_macro_train) #loss_ts_boxcoxD1_train
#arimax_model

## Arima Model
acf(loss_ts_boxcoxD1_train)
pacf(loss_ts_boxcoxD1_train)

arimax_model <- Arima(loss_ts_boxcoxD1_train,order=c(2,0,1),xreg=ex_macro_train)
summary(arimax_model)

checkresiduals(arimax_model)

arimax_forecast <- predict(arimax_model, newxreg=ex_macro_test,n.ahead=8)

accuracy <- sqrt(mean((arimax_forecast$pred - loss_ts_boxcoxD1_test)^2))
accuracy

autoplot(arimax_forecast$pred)
autoplot(loss_ts_boxcoxD1_test)


####