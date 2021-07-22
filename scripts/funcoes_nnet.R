# df_rsmple <- roll_rs$splits[[1]]
# y <- "VALOR"
# v_ts <- "DT_Q"
# n_test <- 4


# obs: weight_decay (penalty) e dropout não são mutuamente excludentes, contudo,
# está indisponível na implementação de single_layer_nnet no keras
  # It can be easily seen that weight decay operates on weights, while dropout operates on nodes. 

# treinar modelo ----
  # obs: paralelização está na aplicação, não na busca
fit_nn <- function(df_rsmple, y, v_ts, n_test = 4, ...) {
  # dados
  df_t <- df_rsmple %>%
    analysis() %>%
    rename("v_ts" = v_ts, "y" = y) 
  
  # treino e teste (apenas uma validação com os mais recentes)
  n_obs <- nrow(df_t)
  df_split <- rolling_origin(df_t,
                             initial = n_obs - n_test,
                             assess = n_test)
  # receita nnet
  nn_recipe <- recipe(y ~ ., data = df_t) %>%
    step_date(v_ts, features = c("quarter", "semester", "year")) %>%
    update_role(v_ts, CO_NCM, ORIGEM, VARIAVEL, NO_SH4_ING, NO_ISIC4_GRUPO, new_role = "ID") 
  
  # fluxo de trabalho
  nn_model <- 
    mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
      # dropout = tune(),
      # activation = tune()
    ) %>% 
    set_engine(
      "nnet", 
      num.threads = 1, 
      importance = "impurity"
    ) %>% 
    set_mode("regression") 
  
  nn_workflow <- 
    workflow() %>% 
    add_model(nn_model) %>% 
    add_recipe(nn_recipe)
  
  # hiperparâmetros (intervalos possíveis)
  nn_parameters <- nn_workflow %>%
    parameters() %>%
    update(
      hidden_units = hidden_units(range = c(1L, 25L)),
      penalty = penalty(range = c(-10, 0)), #weight_decay
      epochs = epochs(range = c(10L, 1000L))#,
      # dropout = dropout(range = c(0, 1)),
      # activation = activation(values = values_activation)
    )

  # primeira camada de otimização de hiperparâmetros (bayesian futility)
  tempo <- proc.time()
  nn_initial_tune <- nn_workflow %>%
    tune_bayes(object = .,
               resamples = df_split,
               initial = 10,    # ~3x mais que a quantidade de hiperparâmetros
               param_info = nn_parameters,
               control = control_bayes(verbose = FALSE,
                                       no_improve = 10L,
                                       save_pred = TRUE),
               metrics = metric_set(rmse))
  proc.time() - tempo
  
  # segunda camada de otimização de hiperparâmetros (simmulated annealing)
  nn_final_tune <- nn_workflow %>%
    tune_sim_anneal(object = .,
                    resamples = df_split,
                    iter = 2L,
                    initial = nn_initial_tune,
                    param_info = nn_parameters,
                    control = control_sim_anneal(verbose = FALSE,
                                                 no_improve = 100L,
                                                 restart = 8L,
                                                 radius = c(0.05, 0.15),
                                                 flip = 0.75,
                                                 cooling_coef = 0.05
                    ),
                    metrics = metric_set(rmse))
  
  # selecionar modelo com melhores hiperparâmetros
  nn_best_hyperparameters <- 
    nn_final_tune %>% 
    select_best(metric = "rmse")
  
  # rodar modelo com melhores hiperparãnetros e dados full
  nn_best_model <-
    nn_workflow %>%
    finalize_workflow(nn_best_hyperparameters) %>%
    fit(., df_t)
  
return(nn_best_model)
}


# avaliação modelo ----
            # tempo <- proc.time()
            # teste <- map(roll_rs$splits, fit_nn, y = "VALOR", v_ts = "DT_Q")
            # proc.time() - tempo
            # split <- roll_rs$splits[[1]]
            # mod <- teste[[1]]
get_extrap_nn <- function(split, mod, y, v_ts) {
  n <- nrow(assessment(split))
  # Get assessment data
  df_predicted <- assessment(split) %>%
    rename("v_ts" = v_ts, "y" = y) %>%
    mutate(
      pred = as.numeric({predict(mod, new_data = .)}),
      pct_error = abs(pred - y) / y 
    ) 
  
return(df_predicted)
}
