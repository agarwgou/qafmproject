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
source("Data_Transformation.R")

#Load Data
uk_data <- read.csv("Combined_timeseries_UK_v2_smoothened.csv")
#uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")
uk_data <- subset(uk_data, select = -DATE)

uk_data_ts <- ts(uk_data[, -1], start = c(2002, 1), frequency = 4)
uk_data_ts
autoplot(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])

new_uk_data <- add_diff_columns(uk_data)
new_uk_data <- add_lag_columns(new_uk_data)

file_path <- "new_uk_data.csv"
write.csv(new_uk_data, file = file_path, row.names = FALSE)

# Working on Loss time series
loss_ts <- ts(uk_data$Europe.and.UK.Commercial.Banking, start = c(2002, 1), frequency = 4)
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

model_arima <- auto.arima(loss_ts_boxcox_1diff_train)
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
  data <- data.frame(new_uk_data$Europe.and.UK.Commercial.Banking_LogD1,new_uk_data[[col]])
  data <- na.omit(data)
  data <- data[!apply(data, 1, function(row) any(!is.finite(row))), ]
  print(data)
  lm_model <- lm(paste(colnames(data)[1],"~",colnames(data)[2]), data=data)
  lm_model_results[[col]] <- summary(lm_model)$adj.r.squared
}

file_path <- "lm_model_results.csv"
write.csv(lm_model_results, file = file_path, row.names = FALSE)


corrplot(cor(uk_data_ts))

# Selected Variables = 

autoplot(ts(new_uk_data$CPIUK, start=c(2002,1),frequency=4))
ndiffs(ts(new_uk_data$CPIUK, start=c(2002,1),frequency=4))

cpi_uk_ts <- ts(uk_data$CPI_UK, start = c(2002, 1), frequency = 4)
cpi_uk_ts_boxcox_1diff <- diff(BoxCox(cpi_uk_ts, lambda=BoxCox.lambda(cpi_uk_ts)))
(cpi_uk_ts_boxcox_1diff)

cpi_uk_ts_boxcox_1diff_train <- ts(cpi_uk_ts_boxcox_1diff[1:split_point],start = c(2002,2),frequency=4)
cpi_uk_ts_boxcox_1diff_test <- ts(cpi_uk_ts_boxcox_1diff[(split_point+1):length(cpi_uk_ts_boxcox_1diff)],start = c(2013,2),frequency=4)


gb_10y_yield_ts <- ts(uk_data$GBMT10UK, start = c(2002, 1), frequency = 4)
gb_10y_yield_ts_boxcox_1diff <- diff(BoxCox(gb_10y_yield_ts, lambda=BoxCox.lambda(gb_10y_yield_ts)))
(gb_10y_yield_ts_boxcox_1diff)

gb_10y_yield_ts_boxcox_1diff_train <- ts(gb_10y_yield_ts_boxcox_1diff[1:split_point],start = c(2002,2),frequency=4)
gb_10y_yield_ts_boxcox_1diff_test <- ts(gb_10y_yield_ts_boxcox_1diff[(split_point+1):length(gb_10y_yield_ts_boxcox_1diff)],start = c(2013,2),frequency=4)

ex_macro_train = cbind(cpi_uk_ts_boxcox_1diff_train,gb_10y_yield_ts_boxcox_1diff_train)
ex_macro_test = cbind(cpi_uk_ts_boxcox_1diff_test,gb_10y_yield_ts_boxcox_1diff_test)

arimax_model <- auto.arima(loss_ts_boxcox_1diff_train,xreg=ex_macro_train)
arimax_model

arimax_forecast <- predict(arimax_model, newxreg=ex_macro_test,n.ahead=11)

accuracy <- sqrt(mean((arimax_forecast$pred - loss_ts_boxcox_1diff_test)^2))
accuracy









summary(lm_model_results[["depositrate"]])

new_uk_data$Europe.and.UK.Commercial.Banking
new_uk_data[[col]]
new_uk_data

lm_model <- lm(Europe.and.UK.Commercial.Banking~CPIUK_D1_L2, data=new_uk_data)
summary(lm_model)

new_uk_data$Europe.and.UK.Commercial.Banking
new_uk_data[["CPIUK_D1_L2"]]

cor(new_uk_data$Europe.and.UK.Commercial.Banking,new_uk_data$CPIUK)

#stats <- gather(new_uk_data, factor_key=TRUE)

# Initialize a list to collect KPSS test results
kpss_results <- list()

# Loop through each column and perform the KPSS test
for (col in names(new_uk_data)) {
  kpss_result <- ur.kpss(new_uk_data[[col]])
  kpss_results[[col]] <- kpss_result
}


summary(kpss_results[['deposit_rate']])


for (col in names(new_uk_data)) {
  kpss_result <- kpss.test(new_uk_data[[col]])
  kpss_results[[col]] <- kpss_result$p.value
}


nsdiff_results <- list()

for (col in names(new_uk_data)) {
  nsdiff_result <- nsdiffs(new_uk_data[[col]])
  nsdiff_results[[col]] <- nsdiff_result
}

file_path <- "nsdiff_results.csv"
write.csv(ndiff_results, file = file_path, row.names = FALSE)


cpi_uk_ts <- ts(uk_data$CPI_UK, start = c(2002, 1), frequency = 4)
cpi_uk_ts_boxcox_1diff <- diff(BoxCox(cpi_uk_ts, lambda=BoxCox.lambda(cpi_uk_ts)))
(cpi_uk_ts_boxcox_1diff)

cpi_uk_ts_boxcox_1diff_train <- ts(cpi_uk_ts_boxcox_1diff[1:split_point],start = c(2002,2),frequency=4)
cpi_uk_ts_boxcox_1diff_test <- ts(cpi_uk_ts_boxcox_1diff[(split_point+1):length(cpi_uk_ts_boxcox_1diff)],start = c(2013,2),frequency=4)


gb_10y_yield_ts <- ts(uk_data$GBMT10UK, start = c(2002, 1), frequency = 4)
gb_10y_yield_ts_boxcox_1diff <- diff(BoxCox(gb_10y_yield_ts, lambda=BoxCox.lambda(gb_10y_yield_ts)))
(gb_10y_yield_ts_boxcox_1diff)

gb_10y_yield_ts_boxcox_1diff_train <- ts(gb_10y_yield_ts_boxcox_1diff[1:split_point],start = c(2002,2),frequency=4)
gb_10y_yield_ts_boxcox_1diff_test <- ts(gb_10y_yield_ts_boxcox_1diff[(split_point+1):length(gb_10y_yield_ts_boxcox_1diff)],start = c(2013,2),frequency=4)

ex_macro_train = cbind(cpi_uk_ts_boxcox_1diff_train,gb_10y_yield_ts_boxcox_1diff_train)
ex_macro_test = cbind(cpi_uk_ts_boxcox_1diff_test,gb_10y_yield_ts_boxcox_1diff_test)

arimax_model <- auto.arima(loss_ts_boxcox_1diff_train,xreg=ex_macro_train)
arimax_model

arimax_forecast <- predict(arimax_model, newxreg=ex_macro_test,n.ahead=11)

accuracy <- sqrt(mean((arimax_forecast$pred - loss_ts_boxcox_1diff_test)^2))
accuracy



ndiff_results[['marginallending_LogD1_L1']]

new_uk_data$marginallending_LogD1

kpss.test(loss_ts)
summary(kpss_output)

