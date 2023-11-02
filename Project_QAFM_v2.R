#setwd("SMU_MQF/QF603_QAFM/Project")
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(corrplot)
library(tseries)
library(fpp2)
library(urca)

#Load Data
uk_data <- read.csv("Combined_timeseries_UK_v2.csv")
uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")
#uk_data <- lapply(uk_data, as.numeric)

uk_data_ts <- ts(uk_data[, -1], start = c(2002, 1), frequency = 4)
uk_data_ts
autoplot(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])


#sapply(uk_data, class)

#Plot UK Commercial
date_column <- uk_data$DATE
value_column <- uk_data$Europe.and.UK.Commercial.Banking
value_column
# value_column <- diff(log(value_column))
# date_column <- date_column[-1]
# date_column
# value_column
plot(date_column,value_column, type="l", xlab="Date",ylab="Operational Losses",main="Europe.and.UK.Commercial.Banking")



### Create diff columns

#uk_data <- uk_data %>% mutate(CPI_UK_D1 = CPI_UK - lag(CPI_UK,n=1))
#uk_data

#uk_data <- uk_data %>% mutate(CPI_UK_BoxCox = BoxCox(CPI_UK,lambda=BoxCox.lambda(loss_ts)))
#uk_data

#Load Data
uk_data <- read.csv("Combined_timeseries_UK_v2.csv")
uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")

add_diff_columns <- function(df) {
  col_names <- names(df)
  for (col_name in col_names){
    if (col_name == "DATE") {
      next
    }
    new_col_name1 <- paste0(col_name, "_D1")
    new_col_name2 <- paste0(col_name, "_D2")
    new_col_name3 <- paste0(col_name, "_BoxCox")
    new_col_name4 <- paste0(col_name, "_BoxCoxD1")
    new_col_name5 <- paste0(col_name, "_QoQ")
    
    df <- df %>%
      mutate(!!new_col_name1 := .data[[col_name]] - lag(.data[[col_name]],n=1))
    
    df <- df %>%
      mutate(!!new_col_name2 := .data[[new_col_name1]] - lag(.data[[new_col_name1]],n=1))
    
    df <- df %>%
      mutate(!!new_col_name3 := BoxCox(.data[[new_col_name1]],lambda=BoxCox.lambda(.data[[new_col_name1]])))
    
    df <- df %>%
      mutate(!!new_col_name4 := .data[[new_col_name3]] - lag(.data[[new_col_name3]],n=1))
    
    df <- df %>%
      mutate(!!new_col_name5 := .data[[col_name]] - lag(.data[[col_name]],n=4))
  }
  return(df)
}

add_lag_columns <- function(df) {
  col_names <- names(df)
  for (col_name in col_names){
    if (col_name == "DATE") {
      next
    }
    new_col_name1 <- paste0(col_name, "_L1")
    new_col_name2 <- paste0(col_name, "_L2")
    new_col_name3 <- paste0(col_name, "_L3")
    new_col_name4 <- paste0(col_name, "_L4")

    df <- df %>%
      mutate(!!new_col_name1 := lag(.data[[col_name]],n=1))
    
    df <- df %>%
      mutate(!!new_col_name2 := lag(.data[[col_name]],n=2))
    df <- df %>%
      mutate(!!new_col_name3 := lag(.data[[col_name]],n=3))
    df <- df %>%
      mutate(!!new_col_name4 := lag(.data[[col_name]],n=4))
  }
  
  return(df)
}

#  df <- df %>%
#   mutate(!!paste0(column_name, "_Diff", n_periods) := .data[[column_name]] - lag(.data[[column_name]], n = n_periods))
#  return(df)
#}

#new_uk_data <- add_difference_column(uk_data,"CPI_UK",1)

new_uk_data <- add_diff_columns(uk_data)
new_uk_data <- add_lag_columns(new_uk_data)

split_point <- 44
# Working on Loss time series
loss_ts <- ts(uk_data$Europe.and.UK.Commercial.Banking, start = c(2002, 1), frequency = 4)

(loss_ts)

ndiffs(loss_ts)
nsdiffs(loss_ts)

kpss_output<-ur.kpss((loss_ts))
summary(kpss_output)

BoxCox.lambda(loss_ts)
loss_ts_boxcox_1diff <- diff(BoxCox(loss_ts, lambda=BoxCox.lambda(loss_ts)))
loss_ts_boxcox_1diff

loss_ts_boxcox_1diff_train <- ts(loss_ts_boxcox_1diff[1:split_point],start = c(2002,2),frequency=4)
loss_ts_boxcox_1diff_test <- ts(loss_ts_boxcox_1diff[(split_point+1):length(loss_ts_boxcox_1diff)],start=c(2013,2),frequency=4)

model_arima <- auto.arima(loss_ts_boxcox_1diff_test)
summary(model_arima)

autoplot(forecast(model_arima,h=20))

## Adding Exogenous variable
#See Correlation Matrix
corr_matrix <- cor(uk_data_ts)
corrplot(corr_matrix)

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

loss_ts_boxcox_1diff_forecast <- arimax_forecast$pred 
loss_ts_boxcox_1diff_test
loss_ts_boxcox_1diff_forecast

autoplot(arimax_forecast$pred)
autoplot(loss_ts_boxcox_1diff_test)



#Test for Stationarity
# KPSS H0 : Stationarity, ADF H0: Unit root(Non-stationarity)

autoplot(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])
uk_data_ts[,"Europe.and.UK.Commercial.Banking"]
kpss_test <- kpss.test(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])
print(kpss_test)
adf_test <- adf.test(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])
print(adf_test)

acf((uk_data_ts[,"Europe.and.UK.Commercial.Banking"]))
pacf((uk_data_ts[,"Europe.and.UK.Commercial.Banking"]))

EuropeandUKCommercial_log_1df <- ts(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])), start = c(2022,2), frequency=1)
autoplot(EuropeandUKCommercial_log_1df)

kpss_test <- kpss.test(EuropeandUKCommercial_log_1df)
print(kpss_test)
adf_test <- adf.test(EuropeandUKCommercial_log_1df)
print(adf_test)

uk_data_ts

uk_data_ts_log_1df <- ts(diff(log(uk_data_ts[,-1])), start = c(2002,2), frequency=4)
uk_data_ts_log_2df

### Regression
regression_model <- lm(Europe.and.UK.Commercial.Banking ~ CPIUKQ, uk_data)
summary(regression_model)

# Create a scatterplot
plot(uk_data$CPIUKQ, uk_data$Europe.and.UK.Commercial.Banking, main = "Univariate Regression", xlab = "CPIUKQ", ylab = "Europe.and.UK.Commercial.Banking Diff_Log")

# Add the regression line
abline(regression_model, col = "red")


## Auto Arima

# acf(value_column)
# pacf(value_column)
# 
# library(forecast)
# auto_arima_model <- auto.arima(value_column)
# auto_arima_model
# 
# forecast_values <- forecast(auto_arima_model, h = 5)
# plot(forecast_values, main = "Auto ARIMA Forecast")


## Plot all graph together
#library(tidyr)
#data_long <- pivot_longer(uk_data, cols = -DATE, names_to = "Series", values_to = "Value")
#ggplot(data_long, aes(DATE, Value, color=Series, linetype=Series, group=Series)) + geom_line() + scale_y_continuous(label=dollar)


### RWA Data

rwa_data <- read.csv("OR_RWA_Banks.csv")
rwa_data_ts <- ts(rwa_data[, -1], start = c(2008, 4), frequency = 4)
autoplot(rwa_data_ts)

Deutsche.Bank.AG <- rwa_data_ts[, "Deutsche.Bank.AG"]

autoplot(diff(BoxCox(Deutsche.Bank.AG, lambda=BoxCox.lambda(Deutsche.Bank.AG))))

autoplot(diff(diff(Deutsche.Bank.AG)))

autoplot(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])))

DB_start <- start(diff(diff(Deutsche.Bank.AG)))
DB_end <- end(diff(diff(Deutsche.Bank.AG)))

Europe_Commercial_start <- start(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])))
Europe_Commercial_end <- end(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])))

DB_RWA_filtered <- window(diff(diff(Deutsche.Bank.AG)), end=Europe_Commercial_end)
autoplot(DB_RWA_filtered)

Europe_Commercial_filtered <- window(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])),start=DB_start)
autoplot(Europe_Commercial_filtered)
  
cor(DB_RWA_filtered,Europe_Commercial_filtered)

DB_RWA_filtered

Europe_Commercial_filtered

data <- data.frame(DB_RWA_filtered,Europe_Commercial_filtered)

model <- lm(DB_RWA_filtered~Europe_Commercial_filtered, data=data)

summary(model)

#common_index <- intersect(seq_along(diff(diff(Deutsche.Bank.AG))), seq_along(diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"]))))
#aligned_ts1 <- diff(diff(Deutsche.Bank.AG))[common_index]
#aligned_ts2 <- diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"]))[common_index]

#cor(aligned_ts1, aligned_ts2)

aligned_ts2

diff(diff(Deutsche.Bank.AG))

diff(log(uk_data_ts[,"Europe.and.UK.Commercial.Banking"]))

autoplot(diff((uk_data_ts[,"Europe.and.UK.Commercial.Banking"])))

ndiffs(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])

common_index <- intersect(seq_along(ts1), seq_along(ts2))

cor(Deutsche.Bank.AG,uk_data_ts[,"Europe.and.UK.Commercial.Banking"])

