# creates list with forecast information 
# inputs:
  # y_ts: time series
  # horizon: number of steps ahead
  # min_obs_lasso: minimum amount of (forecast) observations to apply combination strategy
  # ...: passing only to rolling windows (to evaluate how to use ... for multiple functions, forecasting, error measure and rolling windows)
# output: list with
  # forecasts at each given moment: moment of prediction (t_pred), moment of full information (t_info) and steps_ahead
  # forecasts at each given moment: forecasts errors
  # time series features at each moement of prediction
# obs: using redundancy on yearmon to be able to filter ts later
create_information_history <- function(y_ts, horizon, 
                                       min_obs_lasso = 12,
                                       loss = c("loss_mape", "loss_rmse", "loss_mase", "loss_residuals"), ...) {
  
  # create data
  ts_samples <- create_rolling_ts(y = y_ts, h = horizon, ...)
  
  ##
  # get date_reference (not the index ref, it can change with sliding window)
  ts_index <- lapply(ts_samples, function(x) { x$analysis |> time() |> max() |> zoo::as.yearmon() |> zoo::as.Date()})
  
  ##
  # forecasts samples
  ts_forecasts <- lapply(ts_samples, function(x) {forecast_methods(y = x$analysis,
                                                                   h = horizon)})
  ##
  ## informations available at forecasting moment 
  ts_information <- mapply(function(pred, actual, moment) {data.frame(t_pred = rep(moment, horizon) |> zoo::as.yearmon(),
                                                                      step_ahead = seq(horizon),
                                                                      t_info = moment %m+% months(seq(horizon)) |> zoo::as.yearmon(),
                                                                      y = as.numeric(actual$assess),
                                                                      pred$mean)  },
                           pred = ts_forecasts, actual = ts_samples, moment = ts_index,
                           SIMPLIFY = FALSE) |> do.call(what = rbind)
  
  ##
  ## combine forecast methods 
  dates_ref <- unique(ts_information$t_pred)[-seq(min_obs_lasso)]
  ts_combination <- vector("list", length(dates_ref))
  for (w in seq_along(ts_combination)) {
    temp_ys <- base::subset(ts_information, t_pred < dates_ref[w])
    temp_newfct <- base::subset(ts_information, t_pred == dates_ref[w])                                               # time varying estimation should be here
    ################################################################################################################## # obs: CAREFUL WITH COLUMNS POSITIONS
    ts_combination[[w]] <-  combination_methods(new_fcts = as_mts(y_mts = temp_newfct[, -c(1:4)],
                                                                  x_date = temp_newfct[, 3]),
                                                y_actual = matrix(temp_ys[, 4], ncol = 1),
                                                y_fcts = as.matrix(temp_ys[, -c(1:4)]))
  }
  
  
  ## get weights from combinations
  ts_weights <- mapply(function(comb, tm) {data.frame(t_pred = rep(tm, nrow(comb$weights)) |> zoo::as.yearmon(),
                                                      method = rownames(comb$weights),
                                                      comb$weights)  },
                       comb = ts_combination, tm = ts_index[-seq(min_obs_lasso)],
                       SIMPLIFY = FALSE) |> do.call(what = rbind)
  row.names(ts_weights) <- NULL
  
  ## get forecasts from combinations
  ts_information_comb <- mapply(function (comb, tm) {data.frame(t_pred = rep(tm, horizon) |> zoo::as.yearmon(),
                                                        step_ahead = seq(horizon),
                                                        t_info = tm %m+% months(seq(horizon)) |> zoo::as.yearmon(),
                                                        comb$forecasts)},
                        comb = ts_combination, tm = ts_index[-seq(min_obs_lasso)],
                        SIMPLIFY = FALSE) |> do.call(what = rbind)
  ##
  ## update ts_information
  ts_information <- merge(x = ts_information, y = ts_information_comb, all = TRUE) 
  
  
  ##
  ## forecasting errors 
  ts_errors <- setNames(vector("list", length(loss)), loss)
  for(w in seq_along(ts_errors)) {
    # errors all candidate methods
    errors_forecats  <- mapply(function (ts_obj, ts_fc, tm) {data.frame(t_pred = rep(tm, horizon) |> zoo::as.yearmon(),
                                                                        step_ahead = seq(horizon),
                                                                        t_info = tm %m+% months(seq(horizon)) |> zoo::as.yearmon(),
                                                                        get_loss(actual = ts_obj$assess,
                                                                                 predicted = ts_fc$mean,
                                                                                 time_series = ts_obj$analysis,
                                                                                 grouped = FALSE,
                                                                                 loss_metric = loss[w]))},
                               ts_obj = ts_samples, ts_fc = ts_forecasts, tm = ts_index,
                               SIMPLIFY = FALSE) |> do.call(what = rbind)
    
    # errors combinations
    errors_combinations  <- mapply(function (ts_obj, ts_comb, tm) {data.frame(t_pred = rep(tm, horizon) |> zoo::as.yearmon(),
                                                                              step_ahead = seq(horizon),
                                                                              t_info = tm %m+% months(seq(horizon)) |> zoo::as.yearmon(),
                                                                              get_loss(actual = ts_obj$assess,
                                                                                       predicted = ts_comb$forecasts,
                                                                                       time_series = ts_obj$analysis,
                                                                                       grouped = FALSE,
                                                                                       loss_metric = loss[w]))},
                                   ts_obj = ts_samples[-seq(min_obs_lasso)], ts_comb = ts_combination, tm = ts_index[-seq(min_obs_lasso)],
                                   SIMPLIFY = FALSE) |> do.call(what = rbind)
    
    
    
  ts_errors[[w]] <- merge(x = errors_forecats, y = errors_combinations, all = TRUE)
    
  }
  
  # features available at the moment
  ts_features <- mapply(function(x, moment) {data.frame(t_pred = moment |> zoo::as.yearmon(),
                                                        get_ts_features(y_ts = x$analysis))},
                        x = ts_samples, moment = ts_index, 
                        SIMPLIFY = FALSE) |> do.call(what = rbind)
  
  # result
  result <- list(ts_forecasts = ts_information,
                 ts_errors = ts_errors,
                 ts_features = ts_features,
                 ts_weights = ts_weights)
return(result)
}

