# 5. Fit the model using Bayesian hyperparameter optimization

source('config.R')


# Specify a model name and create a directory for it if needed
mname = "model8"

folder_path <- mname

if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

if (!dir.exists(paste(mname,"model_fits", sep = "/"))) {
  dir.create(paste(mname,"model_fits", sep = "/"))
}

if (!dir.exists(paste(mname,"plots", sep = "/"))) {
  dir.create(paste(mname,"plots", sep = "/"))
}


# Read in data points with covariates and folds assigned
xd = read.csv("data/aedes_all_spp_3folds_2026-06-24.csv", header=TRUE)

### 2. Define outcome, predictors, and categorical variables #####

# Keep only columns needed (including fold for CV)
analysis_data_v3 <- xd %>%
  dplyr::select(
    presence,
    fold,
    # bio01,
    # bio02,
    bio04,
    # bio05,
    bio08,
    # bio09,
    bio10,
    # bio11,
    # bio12,
    # bio15,
    bio16,
    # bio17,
    bio18,
    # bio19,
    forest,
    shrub,
    grass,
    wetland,
    crop,
    urban
  ) %>%
  # Shuffle rows (helps avoid ordering artifacts)
  dplyr::slice_sample(prop = 1)

filename = paste0("data/analysis_data_all_spp_", mname, ".csv")
write.csv(analysis_data_v3, file = filename, row.names=FALSE)
analysis_data_v3 = read.csv(filename, header = TRUE)

# Setup
ntrees.max <- 200
k <- length(unique(analysis_data_v3$fold))

outcome_col <- "presence"
fold_col <- "fold"

feature_cols <- setdiff(names(analysis_data_v3), c(outcome_col, fold_col))

results <- vector("list", k)
feature_importance <- vector("list", k)
shap_values <- vector("list", k)
models <- vector("list", k)

stopifnot(all(feature_cols %in% names(analysis_data_v3)))
# Loop through spatial CV folds
for (j in 1:k) {
  
  cat("========== Fold", j, "==========\n")
  
  train <- analysis_data_v3[analysis_data_v3$fold != j, ]
  test  <- analysis_data_v3[analysis_data_v3$fold == j, ]
  
  X_train <- data.matrix(train[, feature_cols])
  X_test  <- data.matrix(test[, feature_cols])
  
  y_train <- train[[outcome_col]]
  y_test  <- test[[outcome_col]]
  
  d_train <- xgb.DMatrix(X_train, label = y_train)
  d_test  <- xgb.DMatrix(X_test, label = y_test)
  
  scale_weight <- sum(y_train == 0) / sum(y_train == 1)
  
  # BO
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
  
  # Build final parameter set
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
  
  # determine optimal number of trees
  xgb_cv <- xgb.cv(
    params = final_params,
    data = d_train,
    nrounds = ntrees.max,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_nrounds <- which.min(xgb_cv$evaluation_log$test_logloss_mean)
  
  # train model
  model <- xgb.train(
    params = final_params,
    data = d_train,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  models[[j]] <- list(
    model = model,
    feature_names = colnames(X_train)
  )
  
  
  # evaluate performance
  pred_test <- predict(model, d_test)
  pred_train <- predict(model, d_train)
  
  auc_out <- as.numeric(roc(y_test, pred_test, quiet = TRUE)$auc)
  auc_in  <- as.numeric(roc(y_train, pred_train, quiet = TRUE)$auc)
  
  logloss <- function(y, p, eps = 1e-15) {
    p <- pmin(pmax(p, eps), 1 - eps)
    -mean(y * log(p) + (1 - y) * log(1 - p))
  }
  
  ll <- logloss(y_test, pred_test)
  
  # feature importance + SHAP
  feature_importance[[j]] <- xgb.importance(model = model)
  
  # SHAP contributions
  shap <- predict(
    model,
    X_train,
    predcontrib = TRUE
  )
  
  # Ensure matrix structure
  shap <- as.matrix(shap)
  
  # Drop bias column
  shap <- shap[, -ncol(shap), drop = FALSE]
  
  # Assign column names
  colnames(shap) <- colnames(X_train)
  
  # Convert to data.frame (SHAPforxgboost expects this)
  shap <- as.data.frame(shap)
  
  # Generate long SHAP table
  stopifnot(ncol(shap) == ncol(X_train))
  stopifnot(all(colnames(shap) == colnames(X_train)))
  
  shap_long <- shap.prep(
    shap_contrib = shap,
    X_train = X_train
  )
  
  shap_values[[j]] <- shap_long
  
  # store results
  results[[j]] <- list(
    fold = j,
    auc_in = auc_in,
    auc_out = auc_out,
    logloss = ll,
    nrounds = best_nrounds
  )
}

# save outputs
outputs <- list(results, feature_importance, shap_values)
saveRDS(outputs, paste0(mname, "/model_fits/model_outputs_BOinloop_all_spp_", mname, ".rds"))

# =========================
# FINAL MODEL - trained on all data
# =========================

X_full <- data.matrix(analysis_data_v3[, feature_cols])
y_full <- analysis_data_v3[[outcome_col]]

d_full <- xgb.DMatrix(X_full, label = y_full)

# use median number of trees across folds
best_nrounds_all <- as.integer(median(sapply(results, `[[`, "nrounds")))

final_model <- xgb.train(
  params = final_params,   # uses last fold's tuned params (simple approach)
  data = d_full,
  nrounds = best_nrounds_all,
  verbose = 0
)


# Save both the final model and the individual CV models and feature columns for later use
saveRDS(
  list(
    final_model = final_model,
    models = models,
    feature_cols = feature_cols
  ),
  paste0(mname, "/model_fits/xgb_models_bundle_", mname, ".rds")
)


# ---------------------------
# Summarize fold results
# ---------------------------
fold_logloss <- sapply(results, `[[`, "logloss")
cat("Fold logloss values:", fold_logloss, "\n")
cat("Mean logloss:", mean(fold_logloss), "SD:", sd(fold_logloss), "\n")

auc_in  <- sapply(results, `[[`, "auc_in")
auc_out <- sapply(results, `[[`, "auc_out")

data.frame(
  Fold = 1:k,
  AUC_In  = auc_in,
  AUC_Out = auc_out
)

cat("Mean AUC (in): ", mean(auc_in), "\n")
cat("Mean AUC (out):", mean(auc_out), "\n")
# Check that AUCs are consistent across folds and in- vs. out-of-sample

auc_outputs = rbind(
  data.frame(
    Fold = 1:k,
    AUC_In  = auc_in,
    AUC_Out = auc_out
  ),
  c('all', mean(auc_in), mean(auc_out))
)

filename = paste0(mname, "/model_fits/model_auc_table", mname, ".csv")
write.csv(auc_outputs, filename, row.names=FALSE)

