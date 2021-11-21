# INPUTS
  # new_fcts: a time series! 
  # y_actual: vector with already known information (t-1 information)
  # candidate forecasts of already know realizations (t-1 information)
# OUTPUT: a list with
  # a matrix of combined forecasts
  # a matrix of weights
combination_methods <- function(new_fcts, y_actual, y_fcts, 
                                weight_methods = c("weights_peLASSO",
                                                   "weights_1N",
                                                   "weights_selection")) {
  # GET WEIGHTS
  weights_matrix <- lapply(setNames(as.list(weight_methods), weight_methods), 
                           function(x) {do.call(what = x,
                                                args = list(actual = y_actual, 
                                                            forecasts = y_fcts))
                           }) |> do.call(what = rbind)
  
  # GET COMBINED PREDICTIONS
  weighted_fcts <- new_fcts %*% t(weights_matrix)
  weighted_fcts <- as_mts(y_mts = weighted_fcts,
                          x_date = time(new_fcts))
  
  # LIST COMBINATIONS
  list_combinations <- list(forecasts = weighted_fcts,
                            weights = weights_matrix)

return(list_combinations)
}

# GENERATE WEIGHTS ----
# GENERATE WEIGHTS: peLASSO ----
    # actual is the values known after forecasts were done
    # forecasts is the pool of forecasts made in the past (without the actual information available)
    # family is the distribution family of the variable 
    # constraint if sum of weights should be equal to one, default is false (peLASSO article)
weights_peLASSO <- function(actual, forecasts, family = 'gaussian', constraint = FALSE) {
  # first step, lasso
  cv_lasso <- glmnet::cv.glmnet(y = actual,
                        x = forecasts,
                        family = "gaussian",
                        alpha = 1,
                        intercept = FALSE)
  
  weights_lasso <- coef(cv_lasso, s = "lambda.1se")[-1,]
  
  # second step, eRidge (only if more than one variable left)
  n_nonzero <- sum(weights_lasso > 0)
  if (n_nonzero > 1) {
    cv_pelasso <- glmnet::cv.glmnet(y = actual,
                                    x = forecasts[, weights_lasso != 0],
                                    family = "gaussian",
                                    alpha = 0,
                                    intercept = FALSE)
    
    weights_pelasso <- coef(cv_pelasso, s = "lambda.1se")[-1,]
  } else {
    weights_pelasso <- weights_lasso[weights_lasso != 0]
    }

  # if constraint, set sum of weigths to one
  if (constraint) { weights_pelasso <- exp(weights_pelasso) / sum(exp(weights_pelasso))  }
  
  # return results
  weights <- weights_lasso                               # all first step weights
  weights[weights_lasso != 0] <- weights_pelasso         # substitute weights
return(weights)
}

  
# GENERATE WEIGHTS: 1/N ----
weights_1N <- function(actual, forecasts) {
  n_methods <- ncol(forecasts)
  weights_1n <- 1 / rep(n_methods, n_methods)
  names(weights_1n) <- colnames(forecasts)
return(weights_1n)
}


# GENERATE WEIGHTS: BEST MODEL SELECTION (currently a very naive approach) ----
      # entry is a matrix (if ts and mts, function might not work)
weights_selection <- function(actual, forecasts) {
  # get errors
  errors <- forecasts
  for (w in seq(ncol(errors))) {
    errors[, w] <- actual - forecasts[,w]
  }
  # select minimum rmse
  ind_best <- apply(errors, 2, function(x) {sum(x**2)}) |> which.min()
  
  # return results
  weights_min <- rep(0, ncol(forecasts))
  weights_min[ind_best] <- 1
  names(weights_min) <- colnames(forecasts)
return(weights_min)
}

# GENERATE WEIGHTS: BEST N MODEL SELECTION (currently a very naive approach) ----
      # entry is a matrix (if ts and mts, function might not work)
weights_1nbest <- function(actual, forecasts, n_best = 3) {
  # get errors
  errors <- forecasts
  for (w in seq(ncol(errors))) {
    errors[, w] <- actual - forecasts[,w]
  }
  # select minimum rmse
  ind_best <- apply(errors, 2, function(x) {sum(x**2)}) |> order()
  ind_best <- ind_best[seq(n_best)]
  
  # return results
  weights_1nbest <- rep(0, ncol(forecasts))
  weights_1nbest[ind_best] <- 1 / n_best
  names(weights_1nbest) <- colnames(forecasts)
  
return(weights_1nbest)
}
