library(dplyr)
library(ggplot2)
library(corrplot)
library(tseries)
library(fpp2)
library(urca)

#Load Data
uk_data <- read.csv("Combined_timeseries_UK_v2.csv")
uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")

uk_data_ts <- ts(uk_data[, -1], start = c(2002, 1), frequency = 4)
uk_data_ts
autoplot(uk_data_ts[,"Europe.and.UK.Commercial.Banking"])

