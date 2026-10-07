# ===============================
# RQ2 — Machine Learning
# ===============================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble); library(ggplot2)
  library(xgboost)      # XGBoost
  library(ranger)       # Random Forest (opzionale)
})

#--------------------------
# 0) DATA PREP
#--------------------------
dat0 <- dfm_ext %>%
  arrange(country, Year) %>%
  mutate(country = factor(country),
         Year    = as.numeric(Year)) %>%
  select(country, Year, CO2_pc_log, everything()) %>%
  na.omit()

target <- "CO2_pc_log"
id_cols <- c("country","Year", target)

# prendi tutte le feature disponibili (numeric/dummy) tranne id/target
feats <- setdiff(names(dat0), id_cols)
is_num <- sapply(dat0[feats], function(x) is.numeric(x) || is.integer(x))
feats  <- feats[is_num]

#--------------------------
# 1) HELPERS
#--------------------------
scale_in_fold <- function(train, test, cols){
  mu  <- sapply(train[cols], mean, na.rm = TRUE)
  sdv <- sapply(train[cols],  sd,  na.rm = TRUE); sdv[sdv==0|is.na(sdv)] <- 1
  zf <- function(df) as.data.frame(mapply(function(x,m,s) (x-m)/s, df[cols], mu, sdv, SIMPLIFY=FALSE))
  train2 <- train; train2[cols] <- zf(train)
  test2  <- test;  test2[cols]  <- zf(test)
  list(train=train2, test=test2)
}
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6, eta = 0.05,
  subsample = 0.8, colsample_bytree = 0.8,
  min_child_weight = 3, lambda = 1
)
xgb_rounds <- 1200

#--------------------------
# 2) GROUP K-FOLD per PAESE
#--------------------------
set.seed(42)
K <- 5
cty <- unique(dat0$country)
fold_id <- sample(rep(1:K, length.out = length(cty)))
fold_map <- tibble(country = cty, fold = fold_id)

res_gkf_xgb <- list(); res_gkf_rf <- list()

for (k in 1:K) {
  te_cty <- filter(fold_map, fold == k)$country
  train  <- filter(dat0, !country %in% te_cty)
  test   <- filter(dat0,  country %in% te_cty)
  if (nrow(test)==0 || nrow(train)<50) next
  
  # scaling in-fold SOLO sulle feature
  scaled <- scale_in_fold(train, test, feats)
  tr <- scaled$train; te <- scaled$test
  
  # ===== XGBOOST
  dtr <- xgb.DMatrix(as.matrix(tr[ , feats, drop=FALSE]), label = tr[[target]])
  dte <- xgb.DMatrix(as.matrix(te[ , feats, drop=FALSE]), label = te[[target]])
  xgb_fit <- xgb.train(params=xgb_params, data=dtr, nrounds=xgb_rounds, verbose=0)
  xgb_pred <- predict(xgb_fit, dte)
  res_gkf_xgb[[k]] <- tibble(
    scheme="GroupKFold", fold=k, model="XGB",
    n_test=nrow(te), RMSE=rmse(te[[target]], xgb_pred),
    MAE=mae(te[[target]], xgb_pred), R2=r2(te[[target]], xgb_pred)
  )
  if (k==K) { xgb_last_fit <- xgb_fit; xgb_last_feats <- feats; xgb_last_test <- te }
  
  # ===== Random Forest 
  rf_fit <- ranger(
    formula = as.formula(paste(target, "~ .")),
    data = cbind(tr[ , c(target, feats), drop=FALSE]),
    num.trees = 800, mtry = max(2, floor(sqrt(length(feats)))),
    min.node.size = 5, importance="permutation", seed=123
  )
  rf_pred <- predict(rf_fit, data = te[ , feats, drop=FALSE])$predictions
  res_gkf_rf[[k]] <- tibble(
    scheme="GroupKFold", fold=k, model="RF",
    n_test=nrow(te), RMSE=rmse(te[[target]], rf_pred),
    MAE=mae(te[[target]], rf_pred), R2=r2(te[[target]], rf_pred)
  )
  if (k==K) rf_last_fit <- rf_fit
}

gkf_metrics <- bind_rows(bind_rows(res_gkf_xgb), bind_rows(res_gkf_rf)) %>%
  group_by(model, scheme) %>%
  summarise(folds=n(), N_test=sum(n_test),
            RMSE=mean(RMSE), MAE=mean(MAE), R2=mean(R2), .groups="drop")
print(gkf_metrics)

#--------------------------
# 3) BLOCKED CV (tempo + paese)
#--------------------------
time_cut <- 2015
set.seed(43)
fold_id2 <- sample(rep(1:K, length.out = length(cty)))
fold_map2 <- tibble(country = cty, fold = fold_id2)

res_blk_xgb <- list(); res_blk_rf <- list()

for (k in 1:K) {
  te_cty <- filter(fold_map2, fold == k)$country
  train  <- dat0 %>% filter(!(country %in% te_cty), Year <  time_cut)
  test   <- dat0 %>% filter( (country %in% te_cty), Year >= time_cut)
  if (nrow(test)==0 || nrow(train)<50) next
  
  scaled <- scale_in_fold(train, test, feats)
  tr <- scaled$train; te <- scaled$test
  
  # XGB
  dtr <- xgb.DMatrix(as.matrix(tr[ , feats, drop=FALSE]), label = tr[[target]])
  dte <- xgb.DMatrix(as.matrix(te[ , feats, drop=FALSE]), label = te[[target]])
  xgb_fit <- xgb.train(params=xgb_params, data=dtr, nrounds=xgb_rounds, verbose=0)
  xgb_pred <- predict(xgb_fit, dte)
  res_blk_xgb[[k]] <- tibble(
    scheme="BlockedST", fold=k, model="XGB",
    n_test=nrow(te), RMSE=rmse(te[[target]], xgb_pred),
    MAE=mae(te[[target]], xgb_pred), R2=r2(te[[target]], xgb_pred)
  )
  
  # RF (opz.)
  rf_fit <- ranger(
    formula = as.formula(paste(target, "~ .")),
    data = cbind(tr[ , c(target, feats), drop=FALSE]),
    num.trees=800, mtry=max(2, floor(sqrt(length(feats)))),
    min.node.size=5, importance="permutation", seed=123
  )
  rf_pred <- predict(rf_fit, data = te[ , feats, drop=FALSE])$predictions
  res_blk_rf[[k]] <- tibble(
    scheme="BlockedST", fold=k, model="RF",
    n_test=nrow(te), RMSE=rmse(te[[target]], rf_pred),
    MAE=mae(te[[target]], rf_pred), R2=r2(te[[target]], rf_pred)
  )
}

blk_metrics <- bind_rows(bind_rows(res_blk_xgb), bind_rows(res_blk_rf)) %>%
  group_by(model, scheme) %>%
  summarise(folds=n(), N_test=sum(n_test),
            RMSE=mean(RMSE), MAE=mean(MAE), R2=mean(R2), .groups="drop")
print(blk_metrics)

#--------------------------
# 4) INTERPRETABILITÀ (XGB)
#--------------------------
if (exists("xgb_last_fit")) {
  # Importanza (gain)
  imp <- xgb.importance(model = xgb_last_fit)
  print(head(imp, 20))
  
  # SHAP (sottoinsieme test ultimo fold) 
  teS <- if (exists("xgb_last_test")) xgb_last_test else dat0 %>% sample_n(min(1000, nrow(dat0)))
  teS_mat <- as.matrix(teS[ , xgb_last_feats, drop=FALSE])
  shap <- predict(xgb_last_fit, xgb.DMatrix(teS_mat), predcontrib = TRUE)
  shap <- as.data.frame(shap)

  bias_col <- intersect(colnames(shap), c("BIAS","BASE"))
  if (length(bias_col)) shap[[bias_col]] <- NULL
  
  shap_imp <- tibble(
    feature = colnames(shap),
    mean_abs_SHAP = sapply(shap, function(x) mean(abs(x)))
  ) %>% arrange(desc(mean_abs_SHAP))
  print(head(shap_imp, 20))
  
  ggplot(shap_imp %>% slice_max(mean_abs_SHAP, n=15),
         aes(x=reorder(feature, mean_abs_SHAP), y=mean_abs_SHAP)) +
    geom_col() + coord_flip() +
    labs(title="XGBoost — SHAP mean |value|",
         x=NULL, y="mean |SHAP|") + theme_minimal()
}
