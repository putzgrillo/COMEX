# ALL METHODS MUST HAVE THE FOLLOWING
  # # INPUTS
      # y: time series
      # h: prediction horizon (steps ahead)
      # cl: confidence level (*100)
  # # OUTPUTS
      # a (hx3) matrix (using as.data.frame on the forecast::forecast output object)

# FORECASTING METHODS (COMBINING METHODS) ----
forecast_methods <- function(y, h, alpha = 0.05,
                             forecast_methods = c("forecast_arima", "forecast_ets", "forecast_naive",
                                                  "forecast_rwd", "forecast_snaive", "forecast_stlm", 
                                                  "forecast_tbats", "forecast_thetaf")) {
  # RUN ALL METHODS
  forecasts <- lapply(setNames(as.list(forecast_methods), forecast_methods), 
                      function(x) {do.call(what = x,
                                           args = list(y = y, h = h, cl = (1-alpha) * 100 ))
                      })
  
  list_forecasts <- list(
    mean = lapply(forecasts, function(x) {x$mean}) |> do.call(what = cbind),
                      # if residuals are not considered to be symmetric,
    lower = lapply(forecasts, function(x) {x$lower}) |> do.call(what = cbind),
    upper = lapply(forecasts, function(x) {x$upper}) |> do.call(what = cbind) 
                      # if residuals are to be considered symmetric (radius saves ~226MB for 9 models, h = 5 and 100,000 series)
                      #                                                     saves ~488MB for 9 models, h = 24 and 100,000 series)
    # radius = lapply(forecasts, function(x) {x$upper - x$lower}) |> do.call(what = cbind)
  )
return(list_forecasts)
}

# ARIMA ----
forecast_arima <- function(y, h, cl) {
  forecast::forecast(object = forecast::auto.arima(y = y, 
                                                   stepwise = TRUE, 
                                                   approximation = TRUE),
                     h = h,
                     level = cl)
}

# ETS ----
forecast_ets <- function(y, h, cl) {
  forecast::forecast(object = forecast::ets(y = y,opt.crit = "mse"),
                     h = h,
                     level = cl) 
}

# TBATS ----
forecast_tbats <- function(y, h, cl) {
  forecast::forecast(object = forecast::tbats(y = y, use.parallel = FALSE),
                     h = h,
                     level = cl) 
}

# THETAF ----
forecast_thetaf <- function(y, h, cl) {
  forecast::thetaf(y = y, h = h, level = cl)
}

# RANDOM-WALK WITH DRIFT ----
forecast_rwd <- function(y, h, cl) {
  forecast::rwf(y = y, drift = TRUE, h = h, level = cl) 
}

# STLM_AR ----
forecast_stlm <- function(y, h, cl) {
  model <- tryCatch({
    forecast::stlm(y, modelfunction = stats::ar)
  }, error = function(e) forecast::auto.arima(y, d = 0, D = 0))
  forecast::forecast(model, h = h, level = cl)
}

# NAIVE ----
forecast_naive <- function(y, h, cl) {
  forecast::naive(y = y, h = h, level = cl)
}

# SEASONAL NAIVE ----
forecast_snaive <- function(y, h, cl) {
  if(frequency(y) == 1) {
    forecast::naive(y = y, h = h, level = cl)
  } else {
    forecast::snaive(y = y, h = h, level = cl)
  }
}
