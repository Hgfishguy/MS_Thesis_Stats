# Random forest analysis for chlorophyll a (mean_Chla_ug)
# - compares raw vs log1p response
# - searches all predictor subsets from the numeric variables
# - excludes Month, Site, and Estuary because they are categorical
# - selects the model with the lowest LOOCV RMSE on the original scale
# - produces predicted vs observed plots with training R2 in the title

suppressPackageStartupMessages({
  library(readxl)
  library(randomForest)
  library(ggplot2)
})

set.seed(42)

#-----------------------------
# 1) Read and clean data
#-----------------------------
dat <- read_excel("Data/Biomass_data_forest.xlsx")

# Keep only the response plus numeric predictors
keep_vars <- c("mean_Chla_ug", "mean_DIN_uM", "mean_PO4_uM", "mean_500um", "mean_63um", "mean_less63um")
dat <- dat[, keep_vars]

# Remove rows with missing values
 dat <- na.omit(dat)

response_name <- "mean_Chla_ug"
predictor_names <- setdiff(names(dat), response_name)
response <- dat[[response_name]]

#-----------------------------
# 2) Helper functions
#-----------------------------
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae <- function(obs, pred) mean(abs(obs - pred))

safe_r2 <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

make_subsets <- function(vars) {
  out <- list()
  k <- 1L
  for (i in seq_along(vars)) {
    cmb <- combn(vars, i, simplify = FALSE)
    for (j in seq_along(cmb)) {
      out[[k]] <- cmb[[j]]
      k <- k + 1L
    }
  }
  out
}

# Fit a single RF and return OOB predictions on the original scale
fit_rf_oob <- function(x, y, transform = c("raw", "log1p"), mtry) {
  transform <- match.arg(transform)
  y_fit <- if (transform == "log1p") log1p(y) else y

  rf <- randomForest(
    x = x,
    y = y_fit,
    ntree = 200,
    mtry = mtry,
    importance = TRUE,
    na.action = na.omit
  )

  pred <- rf$predicted
  if (transform == "log1p") pred <- expm1(pred)

  list(
    model = rf,
    pred = pred,
    r2 = safe_r2(y, pred),
    rmse = rmse(y, pred),
    mae = mae(y, pred)
  )
}

# LOOCV using a chosen subset/mtry/transform
loocv_rf <- function(dat, response_name, predictors, transform, mtry) {
  n <- nrow(dat)
  obs <- dat[[response_name]]
  pred <- numeric(n)

  for (i in seq_len(n)) {
    train_dat <- dat[-i, , drop = FALSE]
    test_dat  <- dat[i, , drop = FALSE]

    y_train <- train_dat[[response_name]]
    x_train <- train_dat[, predictors, drop = FALSE]
    x_test  <- test_dat[, predictors, drop = FALSE]

    y_fit <- if (transform == "log1p") log1p(y_train) else y_train

    rf <- randomForest(
      x = x_train,
      y = y_fit,
      ntree = 200,
      mtry = mtry,
      importance = FALSE,
      na.action = na.omit
    )

    p <- predict(rf, newdata = x_test)
    if (transform == "log1p") p <- expm1(p)
    pred[i] <- p
  }

  list(
    pred = pred,
    r2 = safe_r2(obs, pred),
    rmse = rmse(obs, pred),
    mae = mae(obs, pred)
  )
}

# Choose mtry by OOB RMSE for a given subset and transform
choose_mtry <- function(dat, response_name, predictors, transform) {
  x <- dat[, predictors, drop = FALSE]
  y <- dat[[response_name]]
  p <- length(predictors)

  scores <- data.frame(mtry = seq_len(p), rmse = NA_real_, r2 = NA_real_, mae = NA_real_)
  for (m in seq_len(p)) {
    fit <- fit_rf_oob(x, y, transform = transform, mtry = m)
    scores$rmse[m] <- fit$rmse
    scores$r2[m] <- fit$r2
    scores$mae[m] <- fit$mae
  }

  scores[which.min(scores$rmse), , drop = FALSE]
}

#-----------------------------
# 3) Search all predictor subsets
#-----------------------------
subsets <- make_subsets(predictor_names)
search_results <- list()
idx <- 1L

for (subset in subsets) {
  for (transform in c("raw", "log1p")) {
    best_m <- choose_mtry(dat, response_name, subset, transform)
    loocv <- loocv_rf(dat, response_name, subset, transform, mtry = best_m$mtry)

    search_results[[idx]] <- data.frame(
      predictors = paste(subset, collapse = ", "),
      transform = transform,
      mtry = best_m$mtry,
      oob_rmse = best_m$rmse,
      oob_r2 = best_m$r2,
      oob_mae = best_m$mae,
      loocv_r2 = loocv$r2,
      loocv_rmse = loocv$rmse,
      loocv_mae = loocv$mae,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
}

search_df <- do.call(rbind, search_results)
search_df <- search_df[order(search_df$loocv_rmse, -search_df$loocv_r2), ]
# write.csv(search_df, "random_forest_model_search_results.csv", row.names = FALSE)

#-----------------------------
# 4) Fit final model from the best LOOCV result
#-----------------------------
best <- search_df[1, ]
best_predictors <- strsplit(best$predictors, ", ")[[1]]
best_transform <- best$transform
best_mtry <- best$mtry

# Final model fit on all data
x_all <- dat[, best_predictors, drop = FALSE]
y_all <- dat[[response_name]]
y_fit_all <- if (best_transform == "log1p") log1p(y_all) else y_all

final_rf <- randomForest(
  x = x_all,
  y = y_fit_all,
  ntree = 200,
  mtry = best_mtry,
  importance = TRUE,
  na.action = na.omit
)

train_pred <- predict(final_rf, newdata = x_all)
if (best_transform == "log1p") train_pred <- expm1(train_pred)
train_r2 <- safe_r2(y_all, train_pred)
train_rmse <- rmse(y_all, train_pred)
train_mae <- mae(y_all, train_pred)

# Optional: LOOCV predictions for the final model (recommended for reporting)
final_cv <- loocv_rf(dat, response_name, best_predictors, best_transform, best_mtry)

#-----------------------------
# 5) Save results and plot
#-----------------------------
cat("Best model:\n")
cat("  Transform:", best_transform, "\n")
cat("  Predictors:", paste(best_predictors, collapse = ", "), "\n")
cat("  mtry:", best_mtry, "\n")
cat(sprintf("  Training R2 = %.3f, RMSE = %.3f, MAE = %.3f\n", train_r2, train_rmse, train_mae))
cat(sprintf("  LOOCV R2 = %.3f, RMSE = %.3f, MAE = %.3f\n", final_cv$r2, final_cv$rmse, final_cv$mae))

plot_df <- data.frame(
  Observed = y_all,
  Predicted = train_pred
)

p <- ggplot(plot_df, aes(Observed, Predicted)) +
  geom_point(size = 2.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate(
    "text",
    x = min(plot_df$Observed),
    y = max(plot_df$Predicted),
    hjust = 0,
    vjust = 1,
    label = paste0(
      "Training R² = ", round(train_r2, 3),
      "\nLOOCV R² = ", round(final_cv$r2, 3),
      "\nPredictors: ", paste(best_predictors, collapse = ", "),
      "\nTransform: ", best_transform
    )
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Observed chlorophyll a (ug/L)",
    y = "Predicted chlorophyll a (ug/L)",
    title = "Random forest predicted vs observed"
  )

# ggsave("predicted_vs_observed_random_forest_best_model.png", p, width = 7, height = 7, dpi = 300)

# If you want variable importance from the final model:
importance(final_rf)
varImpPlot(final_rf)

imp <- importance(rf.model, type = 1)

imp_df <- data.frame(
  Variable = rownames(imp),
  PermImportance = imp[, 1]
)

# Optional: keep only the chosen predictors in case anything extra is present
imp_df <- imp_df[imp_df$Variable %in% colnames(best.vars), ]

# Order bars from lowest to highest importance
imp_df <- imp_df[order(imp_df$PermImportance), ]

# Horizontal bar plot
ggplot(imp_df, aes(x = reorder(Variable, PermImportance),
                   y = PermImportance)) +
  geom_col() +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    x = "Predictor",
    y = "Permutation importance (%IncMSE)",
    title = "Random forest permutation importance"
  )
