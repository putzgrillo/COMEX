# INPUTS:
  # y: tibble/df/tsibble
  # h: steps ahead
  # max_windows: number of distinct rolling origins (if null, all possible)
  # min_obs = minimum number of observations to fit models
# HIDDEN PARAMETERS
  # choose type of max_windows: (i) random, (ii) most recent
  # choose output: (i) indexes or (ii) list

create_rolling_ts <- function(y, h, max_windows = NA, min_obs = 60, keep_origin = TRUE) {
  # create indexes
  n <- length(y)
  stops <- seq(min_obs, (n - h))
  if (keep_origin) {
    starts <- rep(1, length(stops))
  } else {
    starts <-  stops - min_obs + 1 
  }

  # subset if and only if max_windows is not null and n_resamples greater than max_windows 
  n_resamples <- length(starts)
  
  if (!is.na(max_windows) & n_resamples > max_windows) {
    index_rs <- seq(from = n_resamples - max_windows + 1, to = n_resamples)        # the most recent samples 
    # index_rs <- sample(seq_along(starts), size = max_windows, replace = FALSE)     # at random
    
    starts <- starts[index_rs]
    stops <- stops[index_rs]
  }
  
  # result
  rs <- mapply(function (begin, end) {list(analysis = base::subset(y, start = begin, end = end), 
                                           assess = base::subset(y, start = end, end = end + h - 1))},
               begin = starts, end = stops, SIMPLIFY = FALSE)
return(rs)
}


