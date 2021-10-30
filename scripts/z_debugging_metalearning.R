#teste sem instalar forma 

dir_R <- "/mnt/2892419C92416F7E/fforma/R/"
arquivos <- list.files(dir_R)
for(w in seq_along(arquivos)) {
  source(paste(dir_R, arquivos[w], sep = ""))
}

library(forecast)
library(memoise)
library(tsfeatures)

# FUNÇÃO TRAINING_METALEARNING: PASSO A PASSO ----
  # FUNÇÃO TRAINING_METALEARNING: INPUTS ----
train_dataset <- Mcomp::M3[sample(length(Mcomp::M3), 30)]
forec_methods = M4_forec_methods()
objective = "averaging"
chunk_size=NULL
save_foldername=NULL

  # FUNÇÃO TRAINING_METALEARNING INÍCIO FUNÇÃO ----
num_workers <- autodetect_num_workers()
chunk_size <- calculate_chunk_size(train_dataset, num_workers)

train_proc <- function(lentry, methods_list) {
  lentry <- temporal_holdout(lentry)
  lentry <- calc_forecasts(lentry, methods_list)
  lentry <- calc_features(lentry)
  lentry <- calc_mase_smape_errors(lentry)
}

train_dataset <- chunk_xapply(train_dataset, chunk_size, 
                              save_foldername = NULL, "train_call",
                              future.apply::future_lapply,
                              train_proc, forec_methods)

train_dataset <- process_owa_errors(train_dataset)

# lapply(train_dataset[[1]], print) # sneakpeek
bayes_resume_filename <- NULL
bayes_save_filename <- "meta_bayes_hypersearch.rds"
if (!is.null(save_foldername)) {
  bayes_save_filename <- paste(save_foldername, "/", bayes_save_filename, collapse="", sep="")
  if (file.exists(bayes_save_filename)) {
    bayes_resume_filename <- bayes_save_filename
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # #  essa parte do código serve para encontrar a # # # # # # # # # 
# # # # # # # #  melhor combinação de hiperparâmetros # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

    # FUNÇÃO TRAINING_METALEARNING: CHAMAR FUNÇÃO  hyperparameter_search ----
        # bayes_results <- hyperparameter_search(train_dataset, objective = objective,
        #                                        n_iter=5, n.cores=num_workers,
        #                                        rand_points = 4,
        #                                        save_filename=bayes_save_filename,
        #                                        resume_filename=bayes_resume_filename)

        # best_params <- bayes_results[which.min(bayes_results[, ncol(bayes_results)]), ]

# bounds=list(max_depth=c(2L,50L),
#             eta=c(0.001, 1.0),
#             gamma=c(0.00001, 2.0),
#             min_child_weight=c(0.00001, 5.0),
#             subsample=c(0.5,1.0),
#             colsample_bytree=c(0.5,1.0),
#             nrounds=c(1L,500L))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # RESUMIR FUNÇÃO USANDO UMA LISTA QUALQUER # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

best_params <- data.frame(max_depth = 30, 
                          eta = 0.02, 
                          gamma = 0.9, 
                          min_child_weight = 2.1,
                          subsample = 0.7, 
                          colsample_bytree = 0.6, 
                          nrounds = 321,  
                          Value = 5)

# FUNÇÃO TRAINING_METALEARNING: CHAMAR FUNÇÃO .train_from_bayes_res ----
    # meta_model <- .train_from_bayes_res(data = train_dataset, bayes_res = best_params, n.cores = num_workers)
data <- create_feat_classif_problem(dataset = train_dataset)
    # .train_data_from_bayes_res(data, bayes_res, n.cores)              # <<<<<< OUTPUT DA FUNÇÃO .train_from_bayes_res


# SIMULAR OUTPUT DA FUNÇÃO ACIMA /\ (LINHA 87)
# FUNÇÃO TRAINING_METALEARNING: CHAMAR FUNÇÃO .train_from_bayes_res: CHAMAR FUNÇÃO .train_data_from_bayes_res

param <- as.list({bayes_res = best_params})
param$nthread <- {n.cores = 1}
nrounds <- param$nrounds
param$nrounds <- NULL
param$Value <- NULL


# FUNÇÃO TRAINING_METALEARNING: CHAMAR FUNÇÃO .train_from_bayes_res: CHAMAR FUNÇÃO .train_data_from_bayes_res: CHAMAR FUNÇÃO train_selection_ensemble
# train_selection_ensemble(data = data$data, errors = data$errors, param, nrounds)
    # INPUTS train_selection_ensemble 
errors <- data$errors
data <- data$data
params <- param
# nrounds

    # FUNÇÃO train_selection_ensemble
dtrain <- xgboost::xgb.DMatrix(data)
attr(dtrain, "errors") <- errors

params$objective <- error_softmax_obj
params$silent <- 0
params$num_class= length(errors[1,])

bst <- xgboost::xgb.train(params=params, dtrain, nrounds)

# A DIFERENÇA ENTRE O XGBOOST PADRÃO E O XGBOOST CUSTOMIZADO ESTÁ A PARTIE DE:
#   handle <- xgb.Booster(params, append(watchlist, dtrain), xgb_model) # LINHA 291 DO CÓDIGO https://github.com/pmontman/customxgboost/blob/master/R/xgb.train.R
# ESSA FUNÇÃO NÃO EXISTE NO PACOTE PADRÃO, FOI FEITO UM FIX.

# NA LISTA DE PARAMS, CHAMA-SE A FUNÇÃO error_softmax_obj

#https://hippocampus-garden.com/lgbm_custom/