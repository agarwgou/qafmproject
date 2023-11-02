library(dplyr)
library(ggplot2)
library(corrplot)
library(tseries)
library(fpp2)
library(urca)

#Load Data
##uk_data <- read.csv("Combined_timeseries_UK_v2.csv")
##uk_data$DATE <- as.Date(uk_data$DATE, format ="%d/%m/%y")

## Function for adding Diffs
add_diff_columns <- function(df) {
  col_names <- names(df)
  for (col_name in col_names){
    if (col_name == "DATE") {
      next
    }
    new_col_name1 <- paste0(col_name, "_D1")
    new_col_name2 <- paste0(col_name, "_D2")
    new_col_name3 <- paste0(col_name, "_BoxCox")
    new_col_name4 <- paste0(col_name, "_LogD1")
    new_col_name5 <- paste0(col_name, "_QoQ")
    
    df <- df %>%
      mutate(!!new_col_name1 := .data[[col_name]] - lag(.data[[col_name]],n=1))
    
    df <- df %>%
      mutate(!!new_col_name2 := .data[[new_col_name1]] - lag(.data[[new_col_name1]],n=1))
    
#    df <- df %>%
#      mutate(!!new_col_name3 := BoxCox(.data[[col_name]],lambda=BoxCox.lambda(.data[[col_name]])))
    
    df <- df %>%
      mutate(!!new_col_name4 := log(.data[[col_name]]) - log(lag(.data[[col_name]],n=1)))
    
    df <- df %>%
      mutate(!!new_col_name5 := .data[[col_name]] - lag(.data[[col_name]],n=4))
  }
  return(df)
}

## Function for adding Lags

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

## Apply functions to add diffs and lags
##new_uk_data <- add_diff_columns(uk_data)
##new_uk_data <- add_lag_columns(new_uk_data)

##warnings()


