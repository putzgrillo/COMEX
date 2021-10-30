# FORECASTING METHODS ----
forecast_methods <- function(y, h, 
                             forecast_methods = c("forecast_arima", "forecast_ets", "forecast_naive",
                                                  "forecast_rwd", "forecast_snaive", "forecast_stlm", 
                                                  "forecast_tbats", "forecast_thetaf")) {
  # RUN ALL METHODS
  lapply(setNames(as.list(forecast_methods), forecast_methods), 
         function(x) {do.call(what = x,
                              args = list(y = y, h = h))
         }) |> 
    do.call(what = cbind)
}
# ARIMA ----
forecast_arima <- function(y, h) {
  forecast::forecast(object = forecast::auto.arima(y = y, stepwise = FALSE, approximation = TRUE),
                     h = h)$mean
}

# ETS ----
forecast_ets <- function(y, h) {
  forecast::forecast(object = forecast::ets(y = y,opt.crit = "mse"),
                     h = h)$mean
}

# TBATS ----
forecast_tbats <- function(y, h) {
  forecast::forecast(object = forecast::tbats(y = y, use.parallel = FALSE),
                     h = h)$mean
}

# THETAF ----
forecast_thetaf <- function(y, h) {
  forecast::thetaf(y = y, h = h)$mean
}

# RANDOM-WALK WITH DRIFT ----
forecast_rwd <- function(y, h) {
  forecast::rwf(y = y, drift = TRUE, h = h)$mean
}

# STLM_AR ----
forecast_stlm <- stlm_ar_forec <- function(y, h) {
  model <- tryCatch({
    forecast::stlm(y, modelfunction = stats::ar)
  }, error = function(e) forecast::auto.arima(y, d = 0, D = 0))
  forecast::forecast(model, h = h)$mean
}

# NAIVE ----
forecast_naive <- function(y, h) {
  forecast::naive(y = y, h = h)$mean
}

# SEASONAL NAIVE ----
forecast_snaive <- function(y, h) {
  if(frequency(y) == 1) {
    forecast::naive(y = y, h = h)$mean
  } else {
    forecast::snaive(y = y, h = h)$mean
  }
}
