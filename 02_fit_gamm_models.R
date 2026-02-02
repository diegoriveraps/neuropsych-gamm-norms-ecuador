############################################################
# Project: neuropsych-gamm-ecuador
# Script: 02_fit_gamm_models.R
# Purpose:
#   Fit GAM(M) models for multiple neuropsych outcomes using mgcv,
#   evaluate generalization via a single holdout split (train/test),
#   compute overfit signals (Test vs Train), and store final models.
#
# Key design choices (international/reproducible conventions):
#   - One global seed, one global split used across all outcomes/families
#   - Holdout split (NOT cross-validation)
#   - Train-only fit for holdout evaluation; Full-data fit for final storage
#   - Consistent metrics across outcomes: MSE/MAE on response scale
#   - Clear family tags (pois/qpois/nb/gamma) for model naming
#
# Outputs:
#   - model_results (data.frame)
#   - stored_models (list of full-data fitted models)
#   - Excel file with results (optional)
#   - TXT summaries (optional)
############################################################

## =========================
## 0) Setup
## =========================
suppressPackageStartupMessages({
  library(mgcv)
  library(dplyr)
  library(openxlsx)  # for write.xlsx
})

# ---- Assumes you already created `data` in 01_data_prep... ----
stopifnot(exists("data"))
stopifnot(is.data.frame(data))

CONFIG <- list(
  seed = 1234,
  train_prop = 0.60,     
  out_dir = ".",
  results_xlsx = "gamm_model_comparison_holdout.xlsx",
  summary_txt  = "gamm_models_summary.txt",
  
  poisson_vars = c(
    "STROOP_word", "STROOP_color", "STROOP_wc",
    "SDMT_aciertos", "WCST_category", "WCST_perseverative", "WCST_error"
  ),
  gamma_vars = c("TMT_A_time", "TMT_B_time")
)

# Global seed + global split (same split for every model)
set.seed(CONFIG$seed)
n <- nrow(data)
train_index_global <- sample.int(n = n, size = floor(CONFIG$train_prop * n))
test_index_global  <- setdiff(seq_len(n), train_index_global)

## =========================
## 1) Helpers
## =========================

# MSE/MAE on response scale
compute_mse <- function(y, yhat) mean((y - yhat)^2, na.rm = TRUE)
compute_mae <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

# Deviance-based dispersion proxy (mainly meaningful for count models)
dispersion_deviance <- function(model) {
  rd <- residuals(model, type = "deviance")
  sum(rd^2, na.rm = TRUE) / df.residual(model)
}

# Safer percent change (avoid division by 0)
pct_change <- function(test, train, eps = 1e-12) 100 * (test - train) / (train + eps)

# Define the model formula (edit here to standardize across outcomes)
build_formula <- function(response) {
  # IMPORTANT FIX:
  #   Replace ti(education_z, sex, bs='fs') (not appropriate) with:
  #   s(education_z, sex, bs = 'fs')  -> factor-smooth interaction
  as.formula(paste0(
    response, " ~ ",
    "s(age_participant) + ",
    "s(education_z) + ",
    "te(age_participant, education_z) + ",
    "s(education_z, sex, bs = 'fs') + ",
    "s(region, bs = 're')"
  ))
}

# Fit model on training split and evaluate on train/test
holdout_eval <- function(formula, response, family, train_idx, test_idx, data) {
  train_data <- data[train_idx, , drop = FALSE]
  test_data  <- data[test_idx,  , drop = FALSE]
  
  # Ensure response exists and remove rows with missing response
  train_data <- train_data[!is.na(train_data[[response]]), , drop = FALSE]
  test_data  <- test_data[!is.na(test_data[[response]]),  , drop = FALSE]
  
  model_train <- gam(
    formula = formula,
    data    = train_data,
    family  = family,
    method  = "REML"
  )
  
  pred_train <- predict(model_train, newdata = train_data, type = "response")
  pred_test  <- predict(model_train, newdata = test_data,  type = "response")
  
  list(
    Train_MSE = compute_mse(train_data[[response]], pred_train),
    Train_MAE = compute_mae(train_data[[response]], pred_train),
    Test_MSE  = compute_mse(test_data[[response]],  pred_test),
    Test_MAE  = compute_mae(test_data[[response]],  pred_test)
  )
}

# Fit final (apparent) model on full dataset and compute apparent error
fit_full_and_metrics <- function(formula, response, family, data) {
  df_full <- data[!is.na(data[[response]]), , drop = FALSE]
  
  model_full <- gam(
    formula = formula,
    data    = df_full,
    family  = family,
    method  = "REML"
  )
  
  pred_full <- predict(model_full, newdata = df_full, type = "response")
  
  list(
    model_full = model_full,
    AIC = AIC(model_full),
    BIC = BIC(model_full),
    DevianceExplained = summary(model_full)$dev.expl,
    R2_Adjusted = summary(model_full)$r.sq,
    Apparent_MSE = compute_mse(df_full[[response]], pred_full),
    Apparent_MAE = compute_mae(df_full[[response]], pred_full)
  )
}

# Main wrapper: evaluates holdout + stores full model
fit_one_outcome <- function(response, family, family_tag, data, train_idx, test_idx) {
  fml <- build_formula(response)
  
  # Full-data fit (for storage + AIC/BIC/etc.)
  full <- fit_full_and_metrics(fml, response, family, data)
  
  # Holdout eval (train fit only)
  hold <- holdout_eval(fml, response, family, train_idx, test_idx, data)
  
  # Overfit signal
  overfit_mse_pct <- pct_change(hold$Test_MSE, hold$Train_MSE)
  overfit_mae_pct <- pct_change(hold$Test_MAE, hold$Train_MAE)
  ratio_mse <- hold$Test_MSE / (hold$Train_MSE + 1e-12)
  ratio_mae <- hold$Test_MAE / (hold$Train_MAE + 1e-12)
  
  # Dispersion: meaningful mostly for count families
  disp <- if (family_tag %in% c("pois", "qpois", "nb")) dispersion_deviance(full$model_full) else NA_real_
  
  # Return both: result row + stored model
  result_row <- data.frame(
    Response = response,
    Family   = family_tag,
    AIC      = full$AIC,
    BIC      = full$BIC,
    DevianceExplained = full$DevianceExplained,
    R2_Adjusted       = full$R2_Adjusted,
    
    Test_MSE  = hold$Test_MSE,
    Test_MAE  = hold$Test_MAE,
    Split_Train_MSE = hold$Train_MSE,
    Split_Train_MAE = hold$Train_MAE,
    
    Overfit_MSE_pct = overfit_mse_pct,
    Overfit_MAE_pct = overfit_mae_pct,
    TestTrain_MSE_ratio = ratio_mse,
    TestTrain_MAE_ratio = ratio_mae,
    
    Apparent_MSE = full$Apparent_MSE,
    Apparent_MAE = full$Apparent_MAE,
    
    Dispersion = disp,
    stringsAsFactors = FALSE
  )
  
  model_name <- paste0(response, "_", family_tag)
  
  list(
    row = result_row,
    model_name = model_name,
    model_full = full$model_full
  )
}

## =========================
## 2) Fit models (loop)
## =========================

stored_models <- list()
results_list  <- list()

# Count outcomes: Poisson, Quasi-Poisson, Negative Binomial
count_families <- list(
  list(fam = poisson(link = "log"),     tag = "pois"),
  list(fam = quasipoisson(link = "log"), tag = "qpois"),
  list(fam = nb(link = "log"),          tag = "nb")
)

i <- 0

for (response in CONFIG$poisson_vars) {
  for (ff in count_families) {
    i <- i + 1
    fit <- fit_one_outcome(
      response   = response,
      family     = ff$fam,
      family_tag = ff$tag,
      data       = data,
      train_idx  = train_index_global,
      test_idx   = test_index_global
    )
    results_list[[i]] <- fit$row
    stored_models[[fit$model_name]] <- fit$model_full
  }
}

# Positive continuous outcomes: Gamma-log
for (response in CONFIG$gamma_vars) {
  i <- i + 1
  fit <- fit_one_outcome(
    response   = response,
    family     = Gamma(link = "log"),
    family_tag = "gamma",
    data       = data,
    train_idx  = train_index_global,
    test_idx   = test_index_global
  )
  results_list[[i]] <- fit$row
  stored_models[[fit$model_name]] <- fit$model_full
}

model_results <- bind_rows(results_list)

# Optional: sort by generalization (largest overfit first)
# model_results <- model_results %>% arrange(desc(Overfit_MSE_pct))

print(model_results)

## =========================
## 3) Export model comparison table
## =========================
out_xlsx <- file.path(CONFIG$out_dir, CONFIG$results_xlsx)
openxlsx::write.xlsx(model_results, out_xlsx, rowNames = FALSE)
cat("\nSaved model comparison table to:", out_xlsx, "\n")

## =========================
## 4) Export summaries to TXT
## =========================
# If you only want "final" families (e.g., NB + Gamma), filter here:
models_final <- stored_models[grepl("(_nb$|_gamma$)", names(stored_models))]

out_txt <- file.path(CONFIG$out_dir, CONFIG$summary_txt)
cat("Summaries of Stored GAM(M) Models\n", file = out_txt)

for (nm in names(models_final)) {
  cat("\n-----------------------------------\n", file = out_txt, append = TRUE)
  cat("Model:", nm, "\n", file = out_txt, append = TRUE)
  cat("-----------------------------------\n", file = out_txt, append = TRUE)
  
  sm <- capture.output(summary(models_final[[nm]]))
  cat(sm, sep = "\n", file = out_txt, append = TRUE)
}

cat("\nSaved model summaries to:", out_txt, "\n")
cat("\nSession info:\n")
print(sessionInfo())
