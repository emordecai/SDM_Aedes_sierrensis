# Automate running multiple models, saving, and visualizing outputs

source('config.R')
source('plotting_functions.R')

data_path <- "data/aedes_all_spp_3folds_2026-06-24.csv"

# specify the models to fit
model_specs <- list(
  list(
    mname = "model1",
    features = c("bio01", "bio04", "bio12", "bio15",
                 "forest", "shrub", "grass", "wetland", "crop", "urban")
  ),
  list(
    mname = "model2",
    features = c("bio08", "bio09", "bio16", "bio17",
                 "forest", "shrub", "grass", "wetland", "crop", "urban")
  ),
  list(
    mname = "model3",
    features = c("bio03", "bio05", "bio06", "bio15", "bio16",
      "forest", "shrub", "grass", "wetland", "crop", "urban")
  ),
  list(
    mname = "model4",
    features = c("bio04", "bio08", "bio10", "bio16", "bio18",
                 "forest", "shrub", "grass", "wetland", "crop", "urban")
  ),
  list(
    mname = "model5",
    features = c("bio01", "bio04", "bio08", "bio09", "bio12", "bio15", "bio18",
                 "forest", "shrub", "grass", "wetland", "crop", "urban")
  ),
  list(
    mname = "model6",
    features = c("bio08", "bio09", "bio16", "bio17",
                 "forest", "urban")
  ),
  list(
    mname = "model7",
    features = c("bio04", "bio08", "bio10", "bio16", "bio18",
                 "forest", "urban")
  ),
  list(
    mname = "model8",
    features = c("bio04", "bio08", "bio10", "bio16", "bio18")
  )
  # add as many as you want
)

# save the model list for future reproducibility
saveRDS(model_specs, "data/model_specs.RDS")

# model fitting function
run_model_pipeline <- function(mname, feature_cols, data_path, seed = 123) {
  
  # ---------------------------
  # Reproducibility
  # ---------------------------
  set.seed(seed)
  
  # ---------------------------
  # Directory setup
  # ---------------------------
  dir.create(mname, showWarnings = FALSE)
  dir.create(file.path(mname, "model_fits"), showWarnings = FALSE)
  dir.create(file.path(mname, "plots"), showWarnings = FALSE)
  
  # ---------------------------
  # Load + subset data
  # ---------------------------
  xd <- read.csv(data_path, header = TRUE)
  
  analysis_data <- xd %>%
    dplyr::select(presence, fold, dplyr::all_of(feature_cols)) %>%
    dplyr::slice_sample(prop = 1)  # reproducible because seed is set above
  
  filename <- file.path("data", paste0("analysis_data_all_spp_", mname, ".csv"))
  write.csv(analysis_data, file = filename, row.names = FALSE)
  analysis_data <- read.csv(filename, header = TRUE)
  
  # ---------------------------
  # Setup
  # ---------------------------
  ntrees.max <- 200
  k <- length(unique(analysis_data$fold))
  
  outcome_col <- "presence"
  fold_col <- "fold"
  
  results <- vector("list", k)
  feature_importance <- vector("list", k)
  shap_values <- vector("list", k)
  models <- vector("list", k)
  params_list <- vector("list", k)   # <-- NEW
  
  # ---------------------------
  # CV loop
  # ---------------------------
  for (j in 1:k) {
    
    # fold-specific seed for stability across runs
    set.seed(seed + j)
    
    cat("==========", mname, "Fold", j, "==========\n")
    
    train <- analysis_data[analysis_data$fold != j, ]
    test  <- analysis_data[analysis_data$fold == j, ]
    
    X_train <- data.matrix(train[, feature_cols])
    X_test  <- data.matrix(test[, feature_cols])
    
    y_train <- train[[outcome_col]]
    y_test  <- test[[outcome_col]]
    
    d_train <- xgb.DMatrix(X_train, label = y_train)
    d_test  <- xgb.DMatrix(X_test, label = y_test)
    
    scale_weight <- sum(y_train == 0) / sum(y_train == 1)
    
    # ---------------------------
    # Bayesian Optimization
    # ---------------------------
    xgb_cv_bayes <- function(eta, max.depth, min.child.weight,
                             subsample, colsample_bytree) {
      
      cv <- xgb.cv(
        params = list(
          booster = "gbtree",
          eta = eta,
          max_depth = as.integer(max.depth),
          min_child_weight = as.integer(min.child.weight),
          subsample = subsample,
          colsample_bytree = colsample_bytree,
          objective = "binary:logistic",
          eval_metric = "logloss",
          scale_pos_weight = scale_weight
        ),
        data = d_train,
        nrounds = ntrees.max,
        nfold = 5,
        early_stopping_rounds = 10,
        verbose = 0
      )
      
      best <- min(cv$evaluation_log$test_logloss_mean)
      list(Score = -best, Pred = NULL)
    }
    
    best_params <- BayesianOptimization(
      FUN = xgb_cv_bayes,
      bounds = list(
        eta = c(0.01, 0.3),
        max.depth = c(2L, 10L),
        min.child.weight = c(1L, 15L),
        subsample = c(0.6, 1),
        colsample_bytree = c(0.6, 1)
      ),
      init_points = 8,
      n_iter = 25,
      verbose = TRUE
    )
    
    final_params <- list(
      booster = "gbtree",
      eta = best_params$Best_Par["eta"],
      max_depth = as.integer(best_params$Best_Par["max.depth"]),
      min_child_weight = as.integer(best_params$Best_Par["min.child.weight"]),
      subsample = best_params$Best_Par["subsample"],
      colsample_bytree = best_params$Best_Par["colsample_bytree"],
      objective = "binary:logistic",
      eval_metric = "logloss",
      scale_pos_weight = scale_weight
    )
    
    # store params per fold
    params_list[[j]] <- final_params
    
    # determine best rounds
    xgb_cv <- xgb.cv(
      params = final_params,
      data = d_train,
      nrounds = ntrees.max,
      nfold = 5,
      early_stopping_rounds = 10,
      verbose = 0
    )
    
    best_nrounds <- which.min(xgb_cv$evaluation_log$test_logloss_mean)
    
    model <- xgb.train(
      params = final_params,
      data = d_train,
      nrounds = best_nrounds,
      verbose = 0
    )
    
    models[[j]] <- list(model = model, feature_names = colnames(X_train))
    
    # evaluation
    pred_test <- predict(model, d_test)
    pred_train <- predict(model, d_train)
    
    auc_out <- as.numeric(roc(y_test, pred_test, quiet = TRUE)$auc)
    auc_in  <- as.numeric(roc(y_train, pred_train, quiet = TRUE)$auc)
    
    logloss <- function(y, p, eps = 1e-15) {
      p <- pmin(pmax(p, eps), 1 - eps)
      -mean(y * log(p) + (1 - y) * log(1 - p))
    }
    
    ll <- logloss(y_test, pred_test)
    
    feature_importance[[j]] <- xgb.importance(model = model)
    
    shap <- predict(model, X_train, predcontrib = TRUE)
    shap <- as.matrix(shap)[, -ncol(shap), drop = FALSE]
    colnames(shap) <- colnames(X_train)
    
    shap_long <- shap.prep(
      shap_contrib = as.data.frame(shap),
      X_train = X_train
    )
    
    shap_values[[j]] <- shap_long
    
    results[[j]] <- list(
      fold = j,
      auc_in = auc_in,
      auc_out = auc_out,
      logloss = ll,
      nrounds = best_nrounds
    )
  }
  
  # ---------------------------
  # Save outputs
  # ---------------------------
  saveRDS(
    list(results, feature_importance, shap_values),
    file.path(mname, "model_fits",
              paste0("model_outputs_BOinloop_all_spp_", mname, ".rds"))
  )
  
  # ---------------------------
  # Select best fold params
  # ---------------------------
  fold_logloss <- sapply(results, `[[`, "logloss")
  best_fold <- which.min(fold_logloss)
  
  final_params <- params_list[[best_fold]]
  
  # ---------------------------
  # Final model
  # ---------------------------
  X_full <- data.matrix(analysis_data[, feature_cols])
  y_full <- analysis_data[[outcome_col]]
  
  d_full <- xgb.DMatrix(X_full, label = y_full)
  
  best_nrounds_all <- as.integer(median(sapply(results, `[[`, "nrounds")))
  
  final_model <- xgb.train(
    params = final_params,
    data = d_full,
    nrounds = best_nrounds_all,
    verbose = 0
  )
  
  saveRDS(
    list(
      final_model = final_model,
      models = models,
      feature_cols = feature_cols,
      final_params = final_params,   # <-- also saved for transparency
      best_fold = best_fold
    ),
    file.path(mname, "model_fits",
              paste0("xgb_models_bundle_", mname, ".rds"))
  )
  
  # ---------------------------
  # AUC summary
  # ---------------------------
  auc_in  <- sapply(results, `[[`, "auc_in")
  auc_out <- sapply(results, `[[`, "auc_out")
  
  auc_outputs <- rbind(
    data.frame(Fold = 1:k, AUC_In = auc_in, AUC_Out = auc_out),
    c("all", mean(auc_in), mean(auc_out))
  )
  
  write.csv(
    auc_outputs,
    file.path(mname, "model_fits",
              paste0("model_auc_table_", mname, ".csv")),
    row.names = FALSE
  )
}

# run all models

### Run models as a list in sequence
# lapply(model_specs, function(spec) {
#   run_model_pipeline(
#     mname = spec$mname,
#     feature_cols = spec$features,
#     data_path = data_path
#   )
# })

# parallelize
plan(multisession)

RNGkind("L'Ecuyer-CMRG")

future_lapply(
  model_specs,
  function(spec) {
    library(xgboost)
    library(dplyr)
    library(pROC)
    library(SHAPforxgboost)
    
    run_model_pipeline(spec$mname, spec$features, data_path)
  },
  future.seed = TRUE
)
