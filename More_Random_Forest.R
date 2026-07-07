# ============================================================
# Random Forest subset search for chlorophyll a
# Response: mean_Chla_ug
# Categorical variables excluded: Month, Site, Estuary
# The script:
#   1) tests every predictor combination
#   2) selects the best by LOOCV R^2
#   3) fits the final random forest
#   4) makes predicted vs observed plot
#   5) makes horizontal permutation importance plot
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)

# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------
dat <- read_excel("Data/Biomass_data_forest.xlsx")

# ------------------------------------------------------------
# 2. Remove categorical variables
# ------------------------------------------------------------
dat <- dat %>%
  select(-Month, -Site, -Estuary)

# ------------------------------------------------------------
# 3. Keep only numeric columns and the response
# ------------------------------------------------------------
response_name <- "mean_Chla_ug"

numeric_vars <- names(dat)[sapply(dat, is.numeric)]
candidate_vars <- setdiff(numeric_vars, response_name)

model_df <- dat %>%
  select(all_of(c(response_name, candidate_vars))) %>%
  filter(complete.cases(.))

# ------------------------------------------------------------
# 4. Optional outlier removal on response
#    Uncomment to use
# ------------------------------------------------------------
# Q1 <- quantile(model_df[[response_name]], 0.25, na.rm = TRUE)
# Q3 <- quantile(model_df[[response_name]], 0.75, na.rm = TRUE)
# IQR_value <- IQR(model_df[[response_name]], na.rm = TRUE)
# lower <- Q1 - 1.5 * IQR_value
# upper <- Q3 + 1.5 * IQR_value
# model_df <- model_df %>%
#   filter(.data[[response_name]] >= lower,
#          .data[[response_name]] <= upper)

# ------------------------------------------------------------
# 5. Transform response
# ------------------------------------------------------------
model_df$Chla_log <- log1p(model_df[[response_name]])

# ------------------------------------------------------------
# 6. Helper function for metrics
# ------------------------------------------------------------
calc_metrics <- function(obs, pred) {
  r2 <- cor(obs, pred)^2
  rmse <- sqrt(mean((obs - pred)^2))
  mae <- mean(abs(obs - pred))
  data.frame(R2 = r2, RMSE = rmse, MAE = mae)
}

# ------------------------------------------------------------
# 7. LOOCV for one predictor subset
# ------------------------------------------------------------
evaluate_subset <- function(vars, df, response_col = "mean_Chla_ug") {
  n <- nrow(df)
  loo_pred <- numeric(n)
  
  for (i in seq_len(n)) {
    train_idx <- setdiff(seq_len(n), i)
    
    rf_fit <- randomForest(
      x = df[train_idx, vars, drop = FALSE],
      y = log1p(df[[response_col]][train_idx]),
      ntree = 500,
      importance = FALSE
    )
    
    pred_log <- predict(rf_fit, newdata = df[i, vars, drop = FALSE])
    loo_pred[i] <- pmax(0, expm1(pred_log))
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

# ------------------------------------------------------------
# 8. Test every predictor combination
# ------------------------------------------------------------
set.seed(123)

all_results <- list()
counter <- 1

for (k in seq_along(candidate_vars)) {
  combos <- combn(candidate_vars, k, simplify = FALSE)
  
  for (vars in combos) {
    cat("Testing subset:", paste(vars, collapse = " + "), "\n")
    all_results[[counter]] <- evaluate_subset(vars, model_df, response_name)
    counter <- counter + 1
  }
}

results_df <- bind_rows(all_results)

# Save search results
# write.csv(results_df, "rf_subset_search_results.csv", row.names = FALSE)

# ------------------------------------------------------------
# 9. Choose the best subset
#    Highest LOOCV R2, then lowest RMSE, then fewer predictors
# ------------------------------------------------------------
best_row <- results_df %>%
  arrange(desc(LOOCV_R2), LOOCV_RMSE, n_predictors) %>%
  slice(1)

best_vars <- unlist(strsplit(best_row$Predictor_Set, " \\+ "))

cat("\nBest predictor set:\n")
print(best_vars)
print(best_row)

# ------------------------------------------------------------
# 10. Fit final model on the full data using the best subset
# ------------------------------------------------------------
rf_best <- randomForest(
  x = model_df[, best_vars, drop = FALSE],
  y = model_df$Chla_log,
  ntree = 500,
  importance = TRUE
)

# Training predictions on original scale
train_pred_log <- predict(rf_best, newdata = model_df[, best_vars, drop = FALSE])
train_pred <- pmax(0, expm1(train_pred_log))
train_obs <- model_df[[response_name]]

train_metrics <- calc_metrics(train_obs, train_pred)

# OOB predictions from randomForest object
# These are on the transformed scale, so back-transform them too
oob_pred <- pmax(0, expm1(rf_best$predicted))
oob_metrics <- calc_metrics(train_obs, oob_pred)

# Combine metrics
final_metrics <- data.frame(
  Metric = c("Training R2", "Training RMSE", "Training MAE",
             "OOB R2", "OOB RMSE", "OOB MAE",
             "Best LOOCV R2", "Best LOOCV RMSE", "Best LOOCV MAE",
             "Number of predictors"),
  Value = c(train_metrics$R2, train_metrics$RMSE, train_metrics$MAE,
            oob_metrics$R2, oob_metrics$RMSE, oob_metrics$MAE,
            best_row$LOOCV_R2, best_row$LOOCV_RMSE, best_row$LOOCV_MAE,
            best_row$n_predictors)
)

print(final_metrics)
# write.csv(final_metrics, "rf_final_metrics.csv", row.names = FALSE)

# ------------------------------------------------------------
# 11. Predicted vs observed plot
# ------------------------------------------------------------
plot_df <- data.frame(
  Observed = train_obs,
  Predicted = train_pred
)

p1 <- ggplot(plot_df, aes(x = Observed, y = Predicted)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(
    x = "Observed chlorophyll a (ug/L)",
    y = "Predicted chlorophyll a (ug/L)",
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

print(p1)
# ggsave("predicted_vs_observed_best_subset_rf.png", p1, width = 7, height = 6, dpi = 300)

# ------------------------------------------------------------
# 12. Permutation importance plot for the final model
#     For regression randomForest models, type = 1 gives %IncMSE
# ------------------------------------------------------------
imp <- importance(rf_best, type = 1)

imp_df <- data.frame(
  Variable = rownames(imp),
  PermImportance = imp[, 1]
) %>%
  filter(Variable %in% best_vars) %>%
  arrange(PermImportance)

write.csv(imp_df, "rf_permutation_importance_best_subset.csv", row.names = FALSE)

p2 <- ggplot(imp_df, aes(x = reorder(Variable, PermImportance),
                         y = PermImportance)) +
  geom_col() +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    x = "Predictor",
    y = "Permutation importance (%IncMSE)",
    title = "Random forest permutation importance"
  )

print(p2)
# ggsave("permutation_importance_best_subset_rf.png", p2, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# 13. Optional: save the final model object
# ------------------------------------------------------------
# saveRDS(rf_best, "rf_best_subset_model.rds")



### Hill.0

# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------
dat <- read_excel("Data/Diversity_data_forest.xlsx")

# ------------------------------------------------------------
# 2. Remove categorical variables
# ------------------------------------------------------------
dat <- dat %>%
  select(-Month, -Site, -Estuary, -mean_Hill.1, -mean_Hill.2)

# ------------------------------------------------------------
# 3. Keep only numeric columns and the response
# ------------------------------------------------------------
response_name <- "mean_Hill.0"

numeric_vars <- names(dat)[sapply(dat, is.numeric)]
candidate_vars <- setdiff(numeric_vars, response_name)

model_df <- dat %>%
  select(all_of(c(response_name, candidate_vars))) %>%
  filter(complete.cases(.))

# ------------------------------------------------------------
# 4. Optional outlier removal on response
#    Uncomment to use
# ------------------------------------------------------------
# Q1 <- quantile(model_df[[response_name]], 0.25, na.rm = TRUE)
# Q3 <- quantile(model_df[[response_name]], 0.75, na.rm = TRUE)
# IQR_value <- IQR(model_df[[response_name]], na.rm = TRUE)
# lower <- Q1 - 1.5 * IQR_value
# upper <- Q3 + 1.5 * IQR_value
# model_df <- model_df %>%
#   filter(.data[[response_name]] >= lower,
#          .data[[response_name]] <= upper)

# ------------------------------------------------------------
# 5. Transform response
# ------------------------------------------------------------
model_df$Chla_log <- log1p(model_df[[response_name]])

# ------------------------------------------------------------
# 6. Helper function for metrics
# ------------------------------------------------------------
calc_metrics <- function(obs, pred) {
  r2 <- cor(obs, pred)^2
  rmse <- sqrt(mean((obs - pred)^2))
  mae <- mean(abs(obs - pred))
  data.frame(R2 = r2, RMSE = rmse, MAE = mae)
}

# ------------------------------------------------------------
# 7. LOOCV for one predictor subset
# ------------------------------------------------------------
evaluate_subset <- function(vars, df, response_col = "mean_Chla_ug") {
  n <- nrow(df)
  loo_pred <- numeric(n)
  
  for (i in seq_len(n)) {
    train_idx <- setdiff(seq_len(n), i)
    
    rf_fit <- randomForest(
      x = df[train_idx, vars, drop = FALSE],
      y = log1p(df[[response_col]][train_idx]),
      ntree = 500,
      importance = FALSE
    )
    
    pred_log <- predict(rf_fit, newdata = df[i, vars, drop = FALSE])
    loo_pred[i] <- pmax(0, expm1(pred_log))
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

# ------------------------------------------------------------
# 8. Test every predictor combination
# ------------------------------------------------------------
set.seed(123)

all_results <- list()
counter <- 1

for (k in seq_along(candidate_vars)) {
  combos <- combn(candidate_vars, k, simplify = FALSE)
  
  for (vars in combos) {
    cat("Testing subset:", paste(vars, collapse = " + "), "\n")
    all_results[[counter]] <- evaluate_subset(vars, model_df, response_name)
    counter <- counter + 1
  }
}

results_df <- bind_rows(all_results)

# Save search results
# write.csv(results_df, "rf_subset_search_results.csv", row.names = FALSE)

# ------------------------------------------------------------
# 9. Choose the best subset
#    Highest LOOCV R2, then lowest RMSE, then fewer predictors
# ------------------------------------------------------------
best_row <- results_df %>%
  arrange(desc(LOOCV_R2), LOOCV_RMSE, n_predictors) %>%
  slice(1)

best_vars <- unlist(strsplit(best_row$Predictor_Set, " \\+ "))

cat("\nBest predictor set:\n")
print(best_vars)
print(best_row)

# ------------------------------------------------------------
# 10. Fit final model on the full data using the best subset
# ------------------------------------------------------------
rf_best <- randomForest(
  x = model_df[, best_vars, drop = FALSE],
  y = model_df$Chla_log,
  ntree = 500,
  importance = TRUE
)

# Training predictions on original scale
train_pred_log <- predict(rf_best, newdata = model_df[, best_vars, drop = FALSE])
train_pred <- pmax(0, expm1(train_pred_log))
train_obs <- model_df[[response_name]]

train_metrics <- calc_metrics(train_obs, train_pred)

# OOB predictions from randomForest object
# These are on the transformed scale, so back-transform them too
oob_pred <- pmax(0, expm1(rf_best$predicted))
oob_metrics <- calc_metrics(train_obs, oob_pred)

# Combine metrics
final_metrics <- data.frame(
  Metric = c("Training R2", "Training RMSE", "Training MAE",
             "OOB R2", "OOB RMSE", "OOB MAE",
             "Best LOOCV R2", "Best LOOCV RMSE", "Best LOOCV MAE",
             "Number of predictors"),
  Value = c(train_metrics$R2, train_metrics$RMSE, train_metrics$MAE,
            oob_metrics$R2, oob_metrics$RMSE, oob_metrics$MAE,
            best_row$LOOCV_R2, best_row$LOOCV_RMSE, best_row$LOOCV_MAE,
            best_row$n_predictors)
)

print(final_metrics)
# write.csv(final_metrics, "rf_final_metrics.csv", row.names = FALSE)

# ------------------------------------------------------------
# 11. Predicted vs observed plot
# ------------------------------------------------------------
plot_df <- data.frame(
  Observed = train_obs,
  Predicted = train_pred
)

p1 <- ggplot(plot_df, aes(x = Observed, y = Predicted)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(
    x = "Observed chlorophyll a (ug/L)",
    y = "Predicted chlorophyll a (ug/L)",
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

print(p1)
# ggsave("predicted_vs_observed_best_subset_rf.png", p1, width = 7, height = 6, dpi = 300)

# ------------------------------------------------------------
# 12. Permutation importance plot for the final model
#     For regression randomForest models, type = 1 gives %IncMSE
# ------------------------------------------------------------
imp <- importance(rf_best, type = 1)

imp_df <- data.frame(
  Variable = rownames(imp),
  PermImportance = imp[, 1]
) %>%
  filter(Variable %in% best_vars) %>%
  arrange(PermImportance)

write.csv(imp_df, "rf_permutation_importance_best_subset.csv", row.names = FALSE)

p2 <- ggplot(imp_df, aes(x = reorder(Variable, PermImportance),
                         y = PermImportance)) +
  geom_col() +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    x = "Predictor",
    y = "Permutation importance (%IncMSE)",
    title = "Random forest permutation importance"
  )

print(p2)
# ggsave("permutation_importance_best_subset_rf.png", p2, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# 13. Optional: save the final model object
# ------------------------------------------------------------
# saveRDS(rf_best, "rf_best_subset_model.rds")