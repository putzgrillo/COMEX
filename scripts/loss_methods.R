# INPUTS 
  # actual: a vector with actual values of the out-of-sample (y)
  # predicted: a vector (or matrix) of estimated values (each candidate model must be in the columns)
  # time_series: the training sample of the ts vector
  # grouped: if horizon greather than 1, should the loss be summed?
  # loss_metric: should be the name of a function whose arguments are: (i) 
                  # # #                           y for actual values
                  # # #                           y_hat for predicted
get_loss <- function(actual, predicted, time_series = NULL, grouped = FALSE, loss_metric = "loss_rmse") {
  # CALL METHOD
  loss <- do.call(what = loss_metric,
                  args = list(y = actual, 
                              y_hat = predicted, 
                              ts_sample = time_series))
  # SUMMARISE IF GROUPED
  if (grouped) {
    loss <- apply(loss, 2, function(x) {mean(x)})
  }
  
return(loss)
}



# LOSS MAPE ----
loss_mape <- function(y, y_hat, ts_sample = NULL) {
  c_names <- colnames(y_hat)             # ugly fix to keep names in mts (not to include operations in the beginning)
  loss <- abs(y - y_hat) / y * 100
  colnames(loss) <- c_names
return(loss)
}
  
# LOSS sMAPE ----
loss_smape <- function(y, y_hat, ts_sample = NULL) {
  c_names <- colnames(y_hat)             # ugly fix to keep names in mts (not to include operations in the beginning)
  loss <- abs(y - y_hat) / (abs(y) + abs(y_hat)) * 200
  colnames(loss) <- c_names
return(loss)
}

# LOSS MASE ----
# ts_sample is the time series available (training)
loss_mase <- function(y, y_hat, ts_sample) {
  ts_frequency <- frequency(ts_sample)
  mase_denominator <- mean(abs(base::diff(ts_sample, lag = ts_frequency)))
  c_names <- colnames(y_hat)             # ugly fix to keep names in mts (not to include operations in the beginning)
  loss <- abs(y - y_hat) / mase_denominator
  colnames(loss) <- c_names
return(loss)
}

# LOSS RMSE ----
loss_rmse <- function(y, y_hat, ts_sample = NULL) {
  c_names <- colnames(y_hat)             # ugly fix to keep names in mts (not to include operations in the beginning)
  loss <- (y - y_hat) ** 2
  colnames(loss) <- c_names
return(loss)
}
