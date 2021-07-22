# treinar modelo ----
fit_arima <- function(df_rsmple, y, v_ts) {
  df_rsmple %>%
    analysis() %>%
    select("v_ts" = v_ts, "y" = y) %>% 
    tk_ts(start = .$v_ts[[1]] %>% as.yearqtr(),
          frequency = 4, silent = TRUE) %>%
    auto.arima(., 
               stepwise = FALSE,
               max.order = 8L)
}

# roll_rs$arima <- map(roll_rs$splits, fit_arima, y = "VALOR", v_ts = "DT_Q")
# split <- roll_rs$splits[[1]]
# mod <- roll_rs$arima[[1]]

# avaliação modelo ----
get_extrap_arima <- function(split, mod, y, v_ts) {
  n <- nrow(assessment(split))
  df_predicted <- assessment(split) %>%
    rename("v_ts" = v_ts, "y" = y) %>% 
    mutate(
      pred = as.numeric(as.vector(forecast(mod, h = n)$mean)),
      pct_error = abs(pred - y) / y 
    )
  return(df_predicted)
}

#
