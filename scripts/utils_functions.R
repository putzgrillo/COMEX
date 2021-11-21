# inputs 
create_classification_table <- function(features, errors) {
  winning_methods <- colnames(errors)[apply(errors, 1, function(x) {which.min(x)})]
  tibble(
    best_method = winning_methods,
    n_ahead = seq(nrow(errors)),
    features
  )
}

as_mts <- function(y_mts, x_date, freq = 12) {
  y_mts <- as.data.frame(y_mts)
  
  mts <- lapply(y_mts, function(x) {
    ts(x, frequency = freq, start = min(x_date), end = max(x_date))
  }) |> do.call(what = cbind)
  
return(mts)
}
