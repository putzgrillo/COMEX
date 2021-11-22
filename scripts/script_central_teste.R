library(forecast)
library(tidyverse)
library(lubridate)

source('scripts/forecasting_methods.R')
source('scripts/get_ts_features.R')
source('scripts/loss_measures.R')
source('scripts/create_rolling_ts.R')
source('scripts/create_information_history.R')
source('scripts/combination_methods.R')
source('scripts/utils_functions.R')
# FULL FUNCTION ----
# train_meta INPUTS: (i) TIME SERIES; (i) HORIZON
# DO ROLLING WINDOWS INSIDE FUNCTIONS

# GET DATA ----
  # # GET MONTHLY DATA
df <- Mcomp::M3
ind_use <- lapply(df, function(x) {x$period == "MONTHLY" & length(x$x) > 90}) %>% unlist()
df <- df[ind_use]
  # # SAMPLE 500
set.seed(193827)
ind_use <- sample(seq(length(df)), size = 1000, replace = FALSE)
df <- df[ind_use]
  # SEPARATE INTO VALIDATION AND 
n_ahead <- 12
df <- lapply(df, function(x) {list(train = x$x,
                                   validation = base::subset(x$xx, start = 1, end = n_ahead) )})

save(df, file = 'temp_files/ts_data.RData')
load('temp_files/ts_data.RData')



# GET FORECAST INFORMATIONS AT POSSIBLE TIMES (VERY TIME CONSUMING)
library(parallel)
nucleos <- parallel::detectCores() - 1

# CREATE MULTIPLE LISTS
Sys.time()
ts_simulation <- vector("list")

# CREATE INDICES FOR TEMP_MATRIX
max_series_iteration <- 30
index_lower <- seq(from = 1, to = length(df), by = max_series_iteration)
index_upper <- c(index_lower[-1] - 1, length(df))

# START_PROCEDURE
for (w in seq_along(index_lower)) {
  # create temp df with max_series_iteration series
  temp_df <- df[index_lower[w]:index_upper[w]]
  # run create_information_history function
  temp_simulation <- mclapply(temp_df, function(x) {
    create_information_history(y_ts = x$train,           # ts 
                               horizon = n_ahead,              # how many steps ahead should be predicted?
                               min_obs_lasso = 12,       # minimum number of observations to employ combination
                               min_obs = 60,             # minimum number of observations in rolling_ts
                               max_windows = 45)         # maximum number of rolling_ts's (must be bigger than min_obs_lasso)
  }, mc.cores = nucleos)
  # append 
  ts_simulation <- append(ts_simulation, temp_simulation)
  #
  print(Sys.time())
}



t0 <- proc.time()
ts_simulation <- mclapply(df, function(x) {
  create_information_history(y_ts = x$train,           # ts 
                             horizon = 6,              # how many steps ahead should be predicted?
                             min_obs_lasso = 12,       # minimum number of observations to employ combination
                             min_obs = 60,             # minimum number of observations in rolling_ts
                             max_windows = 60)         # maximum number of rolling_ts's (must be bigger than min_obs_lasso)
}, mc.cores = nucleos)
proc.time() - t0

save(ts_simulation, file = "ts_history_simulation.RData")
load("ts_history_simulation.RData")

# GENERATE NEW COMBINATIONS
      # FAZER INTERAÇÃO, A CADA PERÍODO CONSIDERADO, ESTIMAR LASSO COM INFORMAÇÕES ANTERIORES
      # FOR W IN T_PRED, FILTER(LA, T_INFO < T_PRED)
      # NA BASE FILTRADA, APLICAR MÉTODOS DE SELEÇÃO 


lapply(ts_simulation, function(x) {
  
  x 
  
  
  combination_methods(new_fcts = la,
                      y_actual = le,
                      y_fcts = )
  })



library(tidyverse)
library(glmnet)

pred_date <- '1991-06-01'
y_2combine <- ts_simulation$N1925$ts_forecasts %>% filter(step_ahead == 1 & t_pred < pred_date)
y_test <- y_2combine[, -c(1:3)]
y_actual <- actual <- matrix(y_test[, 1], ncol = 1)
y_fcts <- forecasts <- as.matrix(y_test[, -1])

new_fcts <- ts_simulation$N1925$ts_forecasts %>% filter(t_pred == pred_date)
new_fcts <- as_mts(y = new_fcts[, -c(1:4)],
                   x_date = new_fcts[, 3])


# to do:
  # combine forecasts GARCH/copula-like 
  # estimate distribution of errors (predict variance with garch model)
  # try finding a copula structure among the errors of predicted 






la <-  lapply(ts_simulation, function(x) {x$ts_errors}) %>%
  bind_rows(.id = "Variavel") %>%
  pivot_longer(-c("Variavel", "t_pred", "step_ahead", "t_info"), 
               names_to = "fct_candidate", 
               values_to = "fct_error")


ggplot(la, aes(x = fct_error, fill = as_factor(step_ahead))) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(limits = c(0, 100)) +
  facet_wrap(~fct_candidate) +
  theme_bw()


ggplot(la, aes(x = fct_error, fill = fct_candidate)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(limits = c(0, 100)) +
  facet_wrap(~step_ahead) +
  theme_bw()


