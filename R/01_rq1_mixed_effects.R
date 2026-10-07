library(dplyr)
library(lmtest)
library(sandwich)
library(robustbase)
library(robust)
require(corrgram)
library(readxl)
library(writexl)
library(tidyverse)
library(lubridate)
library(forecast)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(prophet)


# Portable data path: place the project dataset in data/dataset_unificato.xlsx
DATA_PATH <- Sys.getenv("CO2_DATA_PATH", unset = file.path("data", "dataset_unificato.xlsx"))
df <- read_excel(DATA_PATH, sheet = "Sheet1")

# Conta i missing per colonna
missing <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variabile", values_to = "missing") %>%
  filter(missing > 0) %>%
  arrange(desc(missing)) %>%
  mutate(percentuale = round(missing / nrow(df) * 100, 2))

print(missing)

# Dizionario corretto
rename_vars <- c(
  CO2_pc         = "Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita)",
  GDP_pc         = "GDP per capita (constant 2015 US$)",
  GDP_per_energy = "GDP per unit of energy use (constant 2021 PPP $ per kg of oil equivalent)",
  Energy_pc      = "Energy use (kg of oil equivalent per capita)",
  Elec_cons_pc   = "Electric power consumption (kWh per capita)",
  Losses_elec    = "Electric power transmission and distribution losses (% of output)",
  Energy_imports = "Energy imports, net (% of energy use)",
  Renewables_total = "Renewable energy consumption (% of total final energy consumption)",
  Combustibles_renew = "Combustible renewables and waste (% of total energy)",
  Coal_share     = "Electricity production from coal sources (% of total)",
  Gas_share      = "Electricity production from natural gas sources (% of total)",
  Oil_share      = "Electricity production from oil sources (% of total)",
  Hydro_share    = "Electricity production from hydroelectric sources (% of total)",
  Nuclear_share  = "Electricity production from nuclear sources (% of total)",
  Other_renew_share = "Electricity production from renewable sources, excluding hydroelectric (% of total)",
  Industry_GDP   = "Industry (including construction), value added (% of GDP)",
  Services_GDP   = "Services, value added (% of GDP)",
  Agri_GDP       = "Agriculture, forestry, and fishing, value added (% of GDP)",
  Urban_pct      = "Urban population (% of total population)",
  Access_elec    = "Access to electricity (% of population)",
  CO2_damage     = "Adjusted savings: carbon dioxide damage (current US$)"
)

df <- df %>% rename(any_of(rename_vars))

# Matrice di correlazione 
df_num <- df %>% select(where(is.numeric))
cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")

corrplot(cor_mat, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

write.csv(as.data.frame(cor_mat), "correlation_matrix.csv", row.names = TRUE)

# Rimuovi variabili non utili
remove_vars <- c("CO2_damage", "Combustibles_renew", "Agri_GDP", "Losses_elec")
df_ren <- df %>% select(-any_of(remove_vars))

# Matrice di correlazione pulita
df_num_clean <- df_ren %>% select(where(is.numeric))
cor_mat_clean <- cor(df_num_clean, use = "pairwise.complete.obs", method = "pearson")

corrplot(cor_mat_clean, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

write.csv(as.data.frame(cor_mat_clean), "correlation_matrix_clean.csv", row.names = TRUE)


png("correlation_heatmap.png", width = 1800, height = 1200, res = 180)

# Aumenta lo spazio sopra per farci stare il titolo
par(mar = c(2, 2, 6, 2))  

#matrice di correlazione
corrplot(cor_mat_clean,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         mar = c(0,0,2,0))  
mtext("Correlation Matrix",
      side = 3,           # 3 = top
      line = 4,         # distanza dal plot
      cex = 1.4,          # dimensione del titolo
      font = 2)           # 2 = grassetto

write.csv(as.data.frame(cor_mat_clean), "correlation_matrix_clean.csv", row.names = TRUE)

 dev.off()


# Explorative analysis
hist(log(df$CO2_pc + 1))
summary(df$CO2_pc)
png("CO2_pc_boxplot.png", width = 800, height = 800)
boxplot(df$CO2_pc, horizontal = TRUE,
        xlab = "CO2 per capita (tCO2e/person)",
        main = "Boxplot of CO2 per capita")

hist(df$CO2_pc,
     breaks = 20,                 
     col = "lightblue",               
     border = "white",                  
     main = "Histogram of CO2 per capita",
     xlab = "CO2 per capita (tCO2e/person)",
     ylab = "Frequency")


abline(v = mean(df$CO2_pc, na.rm = TRUE),
       col = "red", lwd = 2, lty = 2)


df_num_clean %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") %>%
  ggplot(aes(x = valore)) +
  geom_histogram(bins = 30, fill = "grey70", color = "black") +
  facet_wrap(~ variabile, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuzioni delle variabili numeriche")

# Dummy creation
make_presence_dummies <- function(df, country_col = "Country Name",
                                  sources = c("Coal_share", "Gas_share", "Oil_share",
                                              "Hydro_share", "Nuclear_share", "Other_renew_share"),
                                  threshold = 0.5) {
  by_country <- df %>%
    group_by(.data[[country_col]]) %>%
    summarise(across(all_of(sources),
                     ~ as.integer(any(. > threshold, na.rm = TRUE)),
                     .names = "Has_{.col}"),
              .groups = "drop")
  df %>% left_join(by_country, by = country_col)
}

sources_vec <- c("Coal_share","Gas_share","Oil_share","Hydro_share","Nuclear_share","Other_renew_share")
df_with_dummies <- make_presence_dummies(df, "Country Name", sources_vec, threshold = 0.5) %>%
  rename(country = `Country Name`)

write_xlsx(df_with_dummies, "dataset_with_dummies.xlsx")

# Feature engineering
df_with_dummies <- df_with_dummies %>%
  mutate(
    CO2_pc_log       = log1p(CO2_pc),
    GDP_pc_log       = log1p(GDP_pc),
    Elec_cons_pc_log = log1p(Elec_cons_pc)
  )

std <- function(x) as.numeric(scale(x))
num_feats <- c("GDP_pc_log","Elec_cons_pc_log","Urban_pct","Renewables_total",
               "Coal_share","Gas_share","Nuclear_share",
               "Industry_GDP","Services_GDP","Energy_imports","Year")

df_with_dummies <- df_with_dummies %>%
  mutate(across(all_of(num_feats), std, .names = "z_{.col}"))

# Dataset finale per i modelli
model_vars <- c("CO2_pc_log", "country", "Year",
                paste0("z_", num_feats),
                "Has_Coal_share","Has_Gas_share","Has_Nuclear_share")

dfm <- df_with_dummies %>%
  select(any_of(model_vars)) %>%
  na.omit()

# --- MODELLI LINEARI ---
lm_test <- lm(CO2_pc_log ~ . - country , data = dfm)
summary(lm_test)

library(car)
vif(lm_test)
plot(lm_test,1)
step(lm_test, direction="both")

lm_test_2 <- lm(CO2_pc_log ~ z_GDP_pc_log + z_Elec_cons_pc_log +
                  z_Urban_pct + z_Renewables_total + z_Coal_share + z_Gas_share +
                  z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_Year +
                  Has_Coal_share + Has_Nuclear_share,
                data = dfm)
summary(lm_test_2)
plot(lm_test_2,1)

lm_test_poly <- lm(CO2_pc_log ~ z_GDP_pc_log + I(z_GDP_pc_log^2) +
                     z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
                     z_Urban_pct + z_Renewables_total + z_Coal_share + z_Gas_share +
                     z_Industry_GDP + z_Services_GDP + z_Energy_imports +
                     z_Year + I(z_Year^2) +
                     Has_Coal_share + Has_Nuclear_share,
                   data = dfm)
summary(lm_test_poly)
plot(lm_test_poly,1)
vif(lm_test_poly)
library(lmtest)
dwtest(lm_test_poly)

# --- MIXED EFFECTS ---
form_poly <- as.formula(
  CO2_pc_log ~ z_GDP_pc_log + I(z_GDP_pc_log^2) +
    z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
    z_Urban_pct + z_Renewables_total +
    z_Coal_share + z_Gas_share +
    z_Industry_GDP + z_Services_GDP + z_Energy_imports +
    z_Year +
    Has_Coal_share + Has_Nuclear_share +
    (1 + z_Year | country)
)

mod_poly <- lmer(form_poly, data = dfm, REML = TRUE)
summary(mod_poly)
r.squaredGLMM(mod_poly)

re_country <- ranef(mod_poly)$country
head(re_country)
dotplot(ranef(mod_poly, condVar = TRUE))
acf(residuals(mod_poly), main = "ACF residui - mixed esteso")

# Residui del modello misto
resid_mod <- residuals(mod_poly)

# Test Durbin-Watson
dwtest(resid_mod ~ dfm$Year)
acf(resid_mod, main = "Autocorrelation of residuals")

#  robust erorr
library(clubSandwich)

 
V_rob <- vcovCR(mod_poly, cluster = dfm$country, type = "CR2")

# Coefficienti + p-value robusti (Satterthwaite)
robust_tbl <- coef_test(mod_poly, vcov = V_rob, test = "Satterthwaite")
print(robust_tbl)

# ======================================================================
# TEST AGGIUNTIVO: aggiungo Oil_share e GDP_per_energy
# ======================================================================

# Creo z_Oil_share e z_GDP_per_energy 
df_with_dummies_ext <- df_with_dummies %>%
  mutate(
    z_Oil_share        = if (!"z_Oil_share" %in% names(.)) as.numeric(scale(Oil_share)) else z_Oil_share,
    z_GDP_per_energy   = if (!"z_GDP_per_energy" %in% names(.)) as.numeric(scale(GDP_per_energy)) else z_GDP_per_energy
  )

model_vars_ext <- c(model_vars, "z_Oil_share", "z_GDP_per_energy")
dfm_ext <- df_with_dummies_ext %>%
  select(any_of(model_vars_ext)) %>%
  na.omit()

dfm_ext$country <- factor(dfm_ext$country)

# OLS test per VIF sul set esteso 
library(car)
lm_check_ext <- lm(
  CO2_pc_log ~ z_GDP_pc_log + I(z_GDP_pc_log^2) +
    z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
    z_Urban_pct + z_Renewables_total +
    z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
    z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
    z_Year + Has_Coal_share + Has_Nuclear_share,
  data = dfm_ext
)
summary(lm_check_ext)
vif(lm_check_ext)

# Mixed effects ESTESO (aggiungo Oil + Efficienza) 
form_poly_ext <- as.formula(
  CO2_pc_log ~ z_GDP_pc_log + I(z_GDP_pc_log^2) +
    z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
    z_Urban_pct + z_Renewables_total +
    z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
    z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
    z_Year +
    Has_Coal_share + Has_Nuclear_share +
    (1 + z_Year | country)
)

mod_poly_ext <- lmer(form_poly_ext, data = dfm_ext, REML = TRUE)
summary(mod_poly_ext)
r.squaredGLMM(mod_poly_ext)

#Errori robusti 

V_rob_ext <- vcovCR(mod_poly_ext, cluster = dfm_ext$country, type = "CR2")
robust_tbl_ext <- coef_test(mod_poly_ext, vcov = V_rob_ext, test = "Satterthwaite")
print(robust_tbl_ext)

# ACF residui del modello esteso 
acf(residuals(mod_poly_ext), main = "ACF residui - mixed esteso")

#------------------------------------------------------#
#TEST MODELLO FINALE
#------------------------------------------------------#

model_vars_ext <- c(model_vars, "z_Oil_share", "z_GDP_per_energy")
dfm_ext <- df_with_dummies_ext %>%
  select(any_of(model_vars_ext)) %>%
  na.omit()
dfm_ext$country <- factor(dfm_ext$country)
#linear model

lm_check_ext <- lm(CO2_pc_log ~ 
  dfm_ext$z_Elec_cons_pc_log + I(dfm_ext$z_Elec_cons_pc_log^2) +
    z_Urban_pct + z_Renewables_total +
    z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
    z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
    z_Year + Has_Coal_share + Has_Nuclear_share,
  data = dfm_ext)

summary(lm_check_ext)
vif(lm_check_ext)
plot(lm_check_ext,1)
dwtest(lm_check_ext)
resettest(lm_check_ext)
bptest(lm_check_ext)
# --- Mixed effects 
form_poly_final <- as.formula(
  CO2_pc_log ~ 
    z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
    z_Urban_pct + z_Renewables_total +
    z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
    z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
    z_Year +
    Has_Coal_share + Has_Nuclear_share +
    (1 + z_Year | country)
)

mod_poly_final <- lmer(form_poly_final, data = dfm_ext, REML = TRUE)
summary(mod_poly_final)
r.squaredGLMM(mod_poly_final)

# --- Errori robusti (
V_rob_final <- vcovCR(mod_poly_final, cluster = dfm_ext$country, type = "CR2")
robust_tbl_final <- coef_test(mod_poly_final, vcov = V_rob_final, test = "Satterthwaite")
print(robust_tbl_final)

# ACF residui del modello  (solo check visivo)
acf(residuals(mod_poly_final), main = "ACF residui - mixed esteso")

#test con AR(1)

library(nlme)

form_fix <- CO2_pc_log ~ 
  z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
  z_Urban_pct + z_Renewables_total +
  z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
  z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
  z_Year + Has_Coal_share + Has_Nuclear_share

m_ar1 <- lme(
  fixed = form_fix,
  random = ~ 1 | country,
  correlation = corAR1(form = ~ Year | country),
  data = dfm_ext,
  method = "REML"
)



summary(m_ar1)

library(MuMIn)

r.squaredGLMM(m_ar1)

# --- Residui e fitted ---
resid_ar1 <- resid(m_ar1, type = "normalized")   # residui normalizzati
fitted_ar1 <- fitted(m_ar1)

# 1) Residui vs Fitted
plot(fitted_ar1, resid_ar1,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted (AR1 model)")
abline(h = 0, lty = 2, col = "red")

# 2) QQ-plot per normalità residui
qqnorm(resid_ar1, main = "QQ-plot residuals")
qqline(resid_ar1, col = "red")

# 3) ACF dei residui
acf(resid_ar1, main = "ACF of residuals ")

# 4) Test Ljung-Box per autocorrelazione residui
Box.test(resid_ar1, lag = 10, type = "Ljung-Box")

# 5) Durbin-Watson test (attenzione: va fatto sui modelli lm, qui lo applichiamo ai residui)
dwtest(resid_ar1 ~ dfm_ext$Year)

# 6) VIF per multicollinearità (si fa su un modello lm con le stesse variabili fisse)
lm_vif <- lm(form_fix, data = dfm_ext)  # solo effetti fissi
vif(lm_vif)

# 7) Histogram of residuals
hist(resid_ar1, breaks = 30, col = "lightblue", main = "Histogram of residuals", xlab = "Residuals")

# 1) quanta varianza c'è nei random effects?
VarCorr(m_ar1)

# 2) estrai varianze (random intercept e residual)
vc <- VarCorr(m_ar1)
var_u0 <- as.numeric(vc[1, "Variance"])      # random intercept country
var_e  <- as.numeric(vc[nrow(vc), "Variance"]) # residual

ICC <- var_u0 / (var_u0 + var_e)
ICC


ctrl <- lmeControl(msMaxIter=200, opt="nlminb")

# modello a φ libero (ML per confronto di likelihood)
m_free <- update(m_ar1, method = "ML")

phis <- seq(0.90, 0.999, by = 0.001)
ll   <- rep(NA_real_, length(phis))

for(i in seq_along(phis)){
  phi <- phis[i]
  fit_i <- try(update(m_free,
                      correlation = corAR1(value = phi, fixed = TRUE, form = ~ Year | country)),
               silent = TRUE)
  if(!inherits(fit_i, "try-error")) ll[i] <- logLik(fit_i)
}

# massimo della loglikelihood
ll_max <- max(ll, na.rm = TRUE)
crit   <- qchisq(0.95, df = 1) / 2             # 1.92...

ok     <- which(2*(ll_max - ll) <= qchisq(0.95,1))  # punti nel CI 95%
phi_hat <- coef(m_free$modelStruct$corStruct, unconstrained = FALSE)

cat("phi_hat:", as.numeric(phi_hat), "\n")
cat("95% CI approx (profile): [",
    min(phis[ok], na.rm = TRUE), ", ",
    max(phis[ok], na.rm = TRUE), "]\n", sep = "")

# ============================
# GroupKFold per PAESE (nlme)
# ============================
library(purrr)
library(tibble)

# --- dataset già pronto come nel tuo script ---
df_fit <- dfm_ext %>%
  arrange(country, Year) %>%
  mutate(country = factor(country),
         Year    = as.numeric(Year))

# formula dei fissi 
form_fix <- CO2_pc_log ~ 
  z_Elec_cons_pc_log + I(z_Elec_cons_pc_log^2) +
  z_Urban_pct + z_Renewables_total +
  z_Coal_share + z_Gas_share + z_Oil_share + z_Nuclear_share +
  z_Industry_GDP + z_Services_GDP + z_Energy_imports + z_GDP_per_energy +
  z_Year + Has_Coal_share + Has_Nuclear_share

# --- helper: metrica ---
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

# --- helper: fit "safe" con AR(1) ---
fit_lme_ar1_safe <- function(data){
  ctrl <- lmeControl(msMaxIter = 200, opt = "nlminb", msVerbose = FALSE)
  fit <- try(lme(
    fixed = form_fix,
    random = ~ 1 | country,
    correlation = corAR1(form = ~ Year | country),
    data = data, method = "REML", control = ctrl
  ), silent = TRUE)
  if (inherits(fit, "try-error")) {
    # fallback senza AR(1) se proprio non converge
    fit <- lme(fixed = form_fix, random = ~ 1 | country,
               data = data, method = "REML", control = ctrl)
  }
  fit
}

# --- GroupKFold per paese ---
set.seed(42)
K <- 5
cty <- unique(df_fit$country)
fold_id <- sample(rep(1:K, length.out = length(cty)))
fold_map <- tibble(country = cty, fold = fold_id)

# raccolgo tutte le predizioni per micro-averaging
all_pred <- list()
by_fold  <- list()

for (k in 1:K) {
  te_cty <- filter(fold_map, fold == k)$country
  tr <- filter(df_fit, !country %in% te_cty)
  te <- filter(df_fit,  country %in% te_cty)
  
  if (nrow(te) == 0 || nrow(tr) < 50) next
  
  fit <- fit_lme_ar1_safe(tr)
  
  # Predizione su paesi MAI visti
  yhat <- try(predict(fit, newdata = te, level = 0), silent = TRUE)
  if (inherits(yhat, "try-error")) next
  
  # salva per micro-averaging
  all_pred[[k]] <- tibble(y = te$CO2_pc_log, yhat = as.numeric(yhat))
  
  # metriche per fold
  by_fold[[k]] <- tibble(
    fold = k,
    n_test = nrow(te),
    RMSE = rmse(te$CO2_pc_log, yhat),
    MAE  = mean(abs(te$CO2_pc_log - yhat)),
    R2   = 1 - sum((te$CO2_pc_log - yhat)^2) / sum((te$CO2_pc_log - mean(te$CO2_pc_log))^2)
  )
}

# --- risultati ---
gkf_by_fold <- bind_rows(by_fold)
print(gkf_by_fold)

gkf_all <- bind_rows(all_pred)
gkf_overall <- tibble(
  N    = nrow(gkf_all),
  RMSE = rmse(gkf_all$y, gkf_all$yhat),
  MAE  = mean(abs(gkf_all$y - gkf_all$yhat)),
  R2   = 1 - sum((gkf_all$y - gkf_all$yhat)^2) / sum((gkf_all$y - mean(gkf_all$y))^2)
)
cat("\n=== GroupKFold (micro-averaged) ===\n")
print(gkf_overall)

#validazione per tempo
library(nlme); library(dplyr); library(purrr); library(tibble)

time_cut <- 2015
set.seed(42)
K <- 5
cty <- sort(unique(dfm_ext$country))
fold_id <- sample(rep(1:K, length.out=length(cty)))
fold_map <- tibble(country=cty, fold=fold_id)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
fit_lme_ar1_safe <- function(data){
  ctrl <- lmeControl(msMaxIter=200, opt="nlminb")
  fit <- try(lme(
    fixed = form_fix, random = ~1|country,
    correlation = corAR1(form = ~ Year | country),
    data=data, method="REML", control=ctrl
  ), silent=TRUE)
  if (inherits(fit,"try-error")) lme(fixed=form_fix, random=~1|country, data=data, method="REML", control=ctrl) else fit
}

all_pred <- list(); by_fold <- list()
for (k in 1:K){
  te_cty <- filter(fold_map, fold==k)$country
  train <- dfm_ext %>% filter(!(country %in% te_cty)) %>% filter(Year <  time_cut)
  test  <- dfm_ext %>% filter( (country %in% te_cty)) %>% filter(Year >= time_cut)
  if (nrow(test)==0 || nrow(train)<50) next
  fit  <- fit_lme_ar1_safe(train)
  yhat <- try(predict(fit, newdata=test, level=0), silent=TRUE)  # SOLO fissi
  if (inherits(yhat,"try-error")) next
  all_pred[[k]] <- tibble(y=test$CO2_pc_log, yhat=as.numeric(yhat))
  by_fold[[k]] <- tibble(
    fold=k, n_test=nrow(test),
    RMSE=rmse(test$CO2_pc_log, yhat),
    MAE =mean(abs(test$CO2_pc_log - yhat)),
    R2  =1 - sum((test$CO2_pc_log - yhat)^2)/sum((test$CO2_pc_log - mean(test$CO2_pc_log))^2)
  )
}

st_by_fold <- bind_rows(by_fold); print(st_by_fold)
st_all <- bind_rows(all_pred)
st_overall <- tibble(
  N    = nrow(st_all),
  RMSE = rmse(st_all$y, st_all$yhat),
  MAE  = mean(abs(st_all$y - st_all$yhat)),
  R2   = 1 - sum((st_all$y - st_all$yhat)^2) / sum((st_all$y - mean(st_all$y))^2)
)
cat("\n=== Spatio-temporal blocked CV ===\n"); print(st_overall)

#validazione loco


rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
fit_lme_ar1_safe <- function(data){
  ctrl <- lmeControl(msMaxIter=200, opt="nlminb")
  fit <- try(lme(
    fixed = form_fix, random = ~1|country,
    correlation = corAR1(form = ~ Year | country),
    data = data, method="REML", control=ctrl
  ), silent=TRUE)
  if (inherits(fit,"try-error")) lme(fixed=form_fix, random=~1|country, data=data, method="REML", control=ctrl) else fit
}

countries <- sort(unique(dfm_ext$country))
all_pred <- list(); by_cty <- list()

for (cty in countries){
  tr <- filter(dfm_ext, country != cty)
  te <- filter(dfm_ext, country == cty)
  if (nrow(te)==0 || nrow(tr)<50) next
  fit  <- fit_lme_ar1_safe(tr)
  yhat <- predict(fit, newdata=te, level=0)  # SOLO fissi → no leakage
  all_pred[[cty]] <- tibble(y=te$CO2_pc_log, yhat=as.numeric(yhat))
  by_cty[[cty]] <- tibble(
    country=cty, n_test=nrow(te),
    RMSE=rmse(te$CO2_pc_log, yhat),
    MAE=mean(abs(te$CO2_pc_log - yhat)),
    R2 = 1 - sum((te$CO2_pc_log - yhat)^2) / sum((te$CO2_pc_log - mean(te$CO2_pc_log))^2)
  )
}

loco_by_cty <- bind_rows(by_cty)
loco_all    <- bind_rows(all_pred)
loco_overall <- tibble(
  N    = nrow(loco_all),
  RMSE = rmse(loco_all$y, loco_all$yhat),
  MAE  = mean(abs(loco_all$y - loco_all$yhat)),
  R2   = 1 - sum((loco_all$y - loco_all$yhat)^2) / sum((loco_all$y - mean(loco_all$y))^2)
)

print(loco_overall); print(loco_by_cty)

ac <- ACF(m_ar1, type = "correlation") # ACF dei residui normalizzati per gruppo
plot(ac, alpha = 0.05)  

ACF(m_ar1, type="normalized")


df_res <- data.frame(country=dfm_ext$country,
                     year=dfm_ext$Year,
                     res=resid(m_ar1, type="normalized"))
res_year <- aggregate(res ~ year, df_res, mean)
plot(res_year, type="b")



#variazione baseline
# Predizione solo con effetti fissi
pred_fix <- predict(m_ar1, level = 0)

# Residui rispetto agli effetti fissi
resid_fix <- dfm_ext$CO2_pc_log - pred_fix

country_dev <- aggregate(resid_fix ~ country, data = dfm_ext, mean)
country_dev <- country_dev[order(country_dev$resid_fix), ]

head(country_dev)   # paesi "virtuosi" (sotto atteso)
tail(country_dev)   # paesi "inefficienti" (sopra atteso)


