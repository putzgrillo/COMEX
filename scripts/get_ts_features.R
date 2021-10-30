# INPUTS
  # # y_ts: is the time series (preferably one)
  # # scaled: flag to determine if the series should be scaled before calculating the features
# OUTPUT
  # # a (n X 41) tibble with the features
get_ts_features <- function(y_ts, scaled = TRUE) {
  ts_features <- tsfeatures::tsfeatures(
    y_ts,
    features = c("acf_features", "arch_stat", 
                 "crossing_points", "entropy", 
                 "flat_spots", "heterogeneity",
                 "holt_parameters", "hw_parameters", 
                 "hurst", "lumpiness", 
                 "nonlinearity", "pacf_features", 
                 "stl_features", "stability",  
                 "unitroot_kpss", "unitroot_pp"),
    scale = scaled
  ) %>% 
    tibble::add_column("n_observations" = length(y_ts)) %>%
    replace(is.na(.), 0)
  
return(ts_features)
}
