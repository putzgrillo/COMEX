# creates list with forecast information 
# inputs:
  # y_ts: time series
  # horizon: number of steps ahead
  # ...: passing only to rolling samples (to evaluate how to use ... for multiple functions)
# output: list with
  # forecasts at each given moment: moment of prediction (t_pred), moment of full information (t_info) and steps_ahead
  # time series features at each moement of prediction
# obs: using redundancy on yearmon to be able to filter ts later
create_information_history <- function(y_ts, horizon, ...) {
  
  # create data
  ts_samples <- create_rolling_ts(y = y_ts, h = horizon, ...)
  
  # get date_reference (not the index ref, it can change with sliding window)
  ts_index <- lapply(ts_samples, function(x) { x$analysis |> time() |> max() |> zoo::as.yearmon() |> zoo::as.Date()})
  
  # forecasts samples
  ts_forecasts <- lapply(ts_samples, function(x) {forecast_methods(y = x$analysis,
                                                                   h = horizon)})
  
  # informations available at forecasting moment 
  ts_information <- mapply(function(pred, actual, moment) {data.frame(t_pred = rep(moment, horizon) |> zoo::as.yearmon(),
                                                                      step_ahead = seq(horizon),
                                                                      t_info = moment %m+% months(seq(horizon)) |> zoo::as.yearmon(),
                                                                      y = as.numeric(actual$assess),
                                                                      pred$mean)  },
                           pred = ts_forecasts, actual = ts_samples, moment = ts_index,
                           SIMPLIFY = FALSE) |> do.call(what = rbind)
  
  # features available at the moment
  ts_features <- mapply(function(x, moment) {data.frame(t_pred = rep(moment, horizon) |> zoo::as.yearmon(),
                                                        get_ts_features(y_ts = x$analysis))},
                        x = ts_samples, moment = ts_index, 
                        SIMPLIFY = FALSE) |> do.call(what = rbind)
  
  # result
  result <- list(ts_information = ts_information,
                 ts_features = ts_features)
return(result)
}

