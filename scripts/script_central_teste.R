library(forecast)
library(tidyverse)

source('scripts/forecasting_methods.R')
source('scripts/get_ts_features.R')
# GET DATA ----
  # # GET MONTHLY DATA
df <- Mcomp::M3
ind_use <- lapply(df, function(x) {x$period == "MONTHLY"}) %>% unlist()
df <- df[ind_use]

  # # SAMPLE 50 
set.seed(193827)
ind_use <- sample(seq(length(df)), size = 50, replace = FALSE)
df <- df[ind_use]
save(df, file = 'temp_files/ts_data.RData')
load('temp_files/ts_data.RData')

# GET FORECASTS ----
ts_forecasts <- lapply(df, function(x) {
  forecast_methods(y = x$x, h = 18)
})
save(ts_forecasts, file = 'temp_files/ts_forecasts.RData')
load('temp_files/ts_forecasts.RData')

# GET TSERIES_FEATURES ----
ts_features <- lapply(df, function(x) {
  get_ts_features(y_ts = x$x)
})
save(ts_features, file = 'temp_files/ts_features.RData')
load('temp_files/ts_features.RData')

# GET OPTIMAL WEIGHTS ----
