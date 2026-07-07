# ============================================================
# Random forest subset-search function for any dataset
# - chooses the best predictor combination by LOOCV R^2
# - fits final random forest
# - makes predicted vs observed plot
# - makes permutation importance plot
# ============================================================

library(ggplot2)
library(randomForest)

rf_subset_analysis <- function(data,
                               response_var,
                               predictor_vars,
                               categorical_vars = c("Month", "Site", "Estuary"),
                               transform_response = TRUE,
                               remove_outliers = FALSE,
                               ntree = 500,
                               save_prefix = NULL) {
  
  # ----------------------------------------------------------
  # Basic checks on data and names
  # ----------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (is.null(names(data)) || any(is.na(names(data))) || any(names(data) == "")) {
    stop("Your data has missing or blank column names. Please fix the column names first.")
  }
  
  response_var <- trimws(as.character(response_var))[1]
  predictor_vars <- trimws(as.character(predictor_vars))
  categorical_vars <- trimws(as.character(categorical_vars))
  
  predictor_vars <- predictor_vars[!is.na(predictor_vars) & nzchar(predictor_vars)]
  categorical_vars <- categorical_vars[!is.na(categorical_vars) & nzchar(categorical_vars)]
  
  if (is.na(response_var) || !nzchar(response_var)) {
    stop("response_var is blank or invalid.")
  }
  
  if (!response_var %in% names(data)) {
    stop(paste0("response_var '", response_var, "' is not in the data."))
  }
  
  if (length(predictor_vars) == 0) {
    stop("No predictor variables were supplied after cleaning blanks and NAs.")
  }
  
  missing_preds <- predictor_vars[!predictor_vars %in% names(data)]
  if (length(missing_preds) > 0) {
    stop(paste("These predictor_vars are missing from the data:",
               paste(missing_preds, collapse = ", ")))
  }
  
  # Remove response if accidentally included in predictors
  predictor_vars <- setdiff(unique(predictor_vars), response_var)
  
  if (length(predictor_vars) == 0) {
    stop("No valid predictor variables remain after removing the response variable.")
  }
  
  # ----------------------------------------------------------
  # Keep only relevant columns
  # ----------------------------------------------------------
  keep_vars <- unique(c(response_var, predictor_vars))
  keep_vars <- keep_vars[keep_vars %in% names(data)]
  
  df <- data[, keep_vars, drop = FALSE]
  
  # Remove categorical vars if present
  categorical_vars <- categorical_vars[categorical_vars %in% names(df)]
  if (length(categorical_vars) > 0) {
    df <- df[, setdiff(names(df), categorical_vars), drop = FALSE]
  }
  
  # Keep complete cases only
  df <- df[complete.cases(df), , drop = FALSE]
  
  if (nrow(df) < 5) {
    stop("Too few complete rows after filtering.")
  }
  
  # ----------------------------------------------------------
  # Keep numeric predictors only
  # ----------------------------------------------------------
  candidate_vars <- setdiff(names(df), response_var)
  candidate_vars <- candidate_vars[sapply(df[, candidate_vars, drop = FALSE], is.numeric)]
  
  if (length(candidate_vars) < 1) {
    stop("No numeric predictor variables remain after filtering.")
  }
  
  # ----------------------------------------------------------
  # Optional outlier removal on response
  # ----------------------------------------------------------
  if (remove_outliers) {
    q1 <- quantile(df[[response_var]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[response_var]], 0.75, na.rm = TRUE)
    iqr_val <- IQR(df[[response_var]], na.rm = TRUE)
    
    lower <- q1 - 1.5 * iqr_val
    upper <- q3 + 1.5 * iqr_val
    
    df <- df[df[[response_var]] >= lower & df[[response_var]] <= upper, , drop = FALSE]
    
    if (nrow(df) < 5) {
      stop("Too few rows left after outlier removal.")
    }
  }
  
  # ----------------------------------------------------------
  # Response transformation
  # ----------------------------------------------------------
  if (transform_response) {
    if (any(df[[response_var]] < 0, na.rm = TRUE)) {
      stop("transform_response = TRUE requires non-negative response values for log1p().")
    }
    
    df$.response_transformed <- log1p(df[[response_var]])
    back_transform <- function(x) pmax(0, expm1(x))
  } else {
    df$.response_transformed <- df[[response_var]]
    back_transform <- function(x) x
  }
  
  # ----------------------------------------------------------
  # Helper functions
  # ----------------------------------------------------------
  calc_metrics <- function(obs, pred) {
    r2 <- cor(obs, pred)^2
    rmse <- sqrt(mean((obs - pred)^2))
    mae <- mean(abs(obs - pred))
    data.frame(R2 = r2, RMSE = rmse, MAE = mae)
  }
  
  evaluate_subset <- function(vars, df, response_col, transformed_col) {
    n <- nrow(df)
    loo_pred <- numeric(n)
    
    for (i in seq_len(n)) {
      train_idx <- setdiff(seq_len(n), i)
      
      rf_fit <- randomForest(
        x = df[train_idx, vars, drop = FALSE],
        y = df[[transformed_col]][train_idx],
        ntree = ntree,
        importance = FALSE
      )
      
      pred_val <- predict(rf_fit, newdata = df[i, vars, drop = FALSE])
      loo_pred[i] <- back_transform(pred_val)
    }
    
    obs <- df[[response_col]]
    metrics <- calc_metrics(obs, loo_pred)
    
    data.frame(
      Predictor_Set = paste(vars, collapse = " + "),
      n_predictors = length(vars),
      LOOCV_R2 = metrics$R2,
      LOOCV_RMSE = metrics$RMSE,
      LOOCV_MAE = metrics$MAE,
      stringsAsFactors = FALSE
    )
  }
  
  # ----------------------------------------------------------
  # Exhaustive subset search
  # ----------------------------------------------------------
  set.seed(123)
  
  all_results <- list()
  counter <- 1
  
  for (k in seq_along(candidate_vars)) {
    combos <- combn(candidate_vars, k, simplify = FALSE)
    
    for (vars in combos) {
      message("Testing subset: ", paste(vars, collapse = " + "))
      all_results[[counter]] <- evaluate_subset(
        vars = vars,
        df = df,
        response_col = response_var,
        transformed_col = ".response_transformed"
      )
      counter <- counter + 1
    }
  }
  
  results_df <- do.call(rbind, all_results)
  
  # ----------------------------------------------------------
  # Choose best subset:
  # highest LOOCV R2, then lowest RMSE, then fewer predictors
  # ----------------------------------------------------------
  results_df <- results_df[order(-results_df$LOOCV_R2,
                                 results_df$LOOCV_RMSE,
                                 results_df$n_predictors), ]
  
  best_row <- results_df[1, , drop = FALSE]
  best_vars <- strsplit(best_row$Predictor_Set, " \\+ ")[[1]]
  
  # ----------------------------------------------------------
  # Fit final model
  # ----------------------------------------------------------
  rf_final <- randomForest(
    x = df[, best_vars, drop = FALSE],
    y = df$.response_transformed,
    ntree = ntree,
    importance = TRUE
  )
  
  # Training predictions
  train_pred_trans <- predict(rf_final, newdata = df[, best_vars, drop = FALSE])
  train_pred <- back_transform(train_pred_trans)
  train_obs <- df[[response_var]]
  train_metrics <- calc_metrics(train_obs, train_pred)
  
  # OOB predictions
  oob_pred <- back_transform(rf_final$predicted)
  oob_metrics <- calc_metrics(train_obs, oob_pred)
  
  # ----------------------------------------------------------
  # Predicted vs observed plot
  # ----------------------------------------------------------
  plot_df <- data.frame(
    Observed = train_obs,
    Predicted = train_pred
  )
  
  p_pred_obs <- ggplot(plot_df, aes(x = Observed, y = Predicted)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_classic(base_size = 14) +
    labs(
      x = response_var,
      y = paste0("Predicted ", response_var),
      title = "Random forest predicted vs observed"
    ) +
    annotate(
      "text",
      x = min(plot_df$Observed, na.rm = TRUE),
      y = max(plot_df$Predicted, na.rm = TRUE),
      hjust = 0,
      vjust = 1,
      label = paste0(
        "Training R² = ", round(train_metrics$R2, 3), "\n",
        "OOB R² = ", round(oob_metrics$R2, 3), "\n",
        "Best LOOCV R² = ", round(best_row$LOOCV_R2, 3)
      )
    )
  
  # ----------------------------------------------------------
  # Permutation importance plot
  # ----------------------------------------------------------
  imp <- importance(rf_final, type = 1)
  
  imp_df <- data.frame(
    Variable = rownames(imp),
    PermImportance = imp[, 1],
    row.names = NULL
  )
  
  imp_df <- imp_df[imp_df$Variable %in% best_vars, , drop = FALSE]
  imp_df <- imp_df[order(imp_df$PermImportance), , drop = FALSE]
  
  p_importance <- ggplot(imp_df, aes(x = reorder(Variable, PermImportance),
                                     y = PermImportance)) +
    geom_col() +
    coord_flip() +
    theme_classic(base_size = 14) +
    labs(
      x = "Predictor",
      y = "Permutation importance (%IncMSE)",
      title = "Random forest permutation importance"
    )
  
  # ----------------------------------------------------------
  # Optional file output
  # ----------------------------------------------------------
  if (!is.null(save_prefix)) {
    write.csv(results_df, paste0(save_prefix, "_subset_search_results.csv"), row.names = FALSE)
    write.csv(best_row, paste0(save_prefix, "_best_subset.csv"), row.names = FALSE)
    write.csv(imp_df, paste0(save_prefix, "_permutation_importance.csv"), row.names = FALSE)
    
    ggsave(paste0(save_prefix, "_predicted_vs_observed.png"),
           p_pred_obs, width = 7, height = 6, dpi = 300)
    
    ggsave(paste0(save_prefix, "_permutation_importance.png"),
           p_importance, width = 7, height = 5, dpi = 300)
  }
  
  # ----------------------------------------------------------
  # Return results
  # ----------------------------------------------------------
  return(list(
    best_predictors = best_vars,
    best_subset_table = best_row,
    subset_search_results = results_df,
    final_model = rf_final,
    training_metrics = train_metrics,
    oob_metrics = oob_metrics,
    importance_table = imp_df,
    predicted_vs_observed_plot = p_pred_obs,
    permutation_importance_plot = p_importance,
    data_used = df
  ))
}

# EXAMPLE
# out <- rf_subset_analysis(
#  data = dat,
#  response_var = "mean_Chla_ug",
#  predictor_vars = c("mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um"),
#  categorical_vars = c("Month", "Site", "Estuary"),
#  transform_response = TRUE,
#  remove_outliers = FALSE,
#  save_prefix = "chlorophyll_rf"
#)

# LEAVE SAVE PREFIX EMPTY UNLESS YOU WANT FULL OUTPUTS SAVED

# out$best_predictors
# out$training_metrics
# out$oob_metrics

# print(out$predicted_vs_observed_plot)
# print(out$permutation_importance_plot)

Biomass_rf <- rf_subset_analysis(
  data = read_excel("Data/Biomass_data_forest.xlsx"),
  response_var = "mean_Chla_ug",
  predictor_vars = c("mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um"),
  categorical_vars = c("Month", "Site", "Estuary"),
  transform_response = TRUE,
  remove_outliers = FALSE,
  save_prefix =
)

Biomass_rf$best_predictors
Biomass_rf$training_metrics
Biomass_rf$oob_metrics

print(Biomass_rf$predicted_vs_observed_plot)
print(Biomass_rf$permutation_importance_plot)

Hill_0_rf <- rf_subset_analysis(
  data = read_excel("Data/Diversity_data_forest.xlsx"),
  response_var = "mean_Hill.0",
  predictor_vars = c("mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um"),
  categorical_vars = c("Month", "Site", "Estuary"),
  transform_response = TRUE,
  remove_outliers = FALSE,
  save_prefix =
)

Hill_0_rf$best_predictors
Hill_0_rf$training_metrics
Hill_0_rf$oob_metrics

print(Hill_0_rf$predicted_vs_observed_plot)
print(Hill_0_rf$permutation_importance_plot)

Hill_1_rf <- rf_subset_analysis(
  data = read_excel("Data/Diversity_data_forest.xlsx"),
  response_var = "mean_Hill.1",
  predictor_vars = c("mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um"),
  categorical_vars = c("Month", "Site", "Estuary"),
  transform_response = TRUE,
  remove_outliers = FALSE,
  save_prefix =
)

Hill_1_rf$best_predictors
Hill_1_rf$training_metrics
Hill_1_rf$oob_metrics

print(Hill_1_rf$predicted_vs_observed_plot)
print(Hill_1_rf$permutation_importance_plot)

Hill_2_rf <- rf_subset_analysis(
  data = read_excel("Data/Diversity_data_forest.xlsx"),
  response_var = "mean_Hill.2",
  predictor_vars = c("mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um"),
  categorical_vars = c("Month", "Site", "Estuary"),
  transform_response = TRUE,
  remove_outliers = FALSE,
  save_prefix =
)

Hill_2_rf$best_predictors
Hill_2_rf$training_metrics
Hill_2_rf$oob_metrics

print(Hill_2_rf$predicted_vs_observed_plot)
print(Hill_2_rf$permutation_importance_plot)
