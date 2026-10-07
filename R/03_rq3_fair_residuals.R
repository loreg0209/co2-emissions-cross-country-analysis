# RQ3 — Country deviations ('fair residuals')
library(dplyr)
library(ggplot2)

# Predizione SOLO effetti fissi (fair baseline)
pred_fix <- predict(m_ar1, level = 0)

# Residuo "fair": osservato - atteso (dato solo struttura)
df_res <- dfm_ext %>%
  mutate(
    pred_fix  = as.numeric(pred_fix),
    resid_fix = CO2_pc_log - pred_fix,
    year      = as.numeric(Year),
    country   = as.factor(country)
  )

#Insight A — Distribuzione delle deviazioni (diagnostica “macro”)
#A1) Istogramma + densità
bw <- diff(range(df_res$resid_fix, na.rm = TRUE)) / 40

ggplot(df_res, aes(x = resid_fix)) +
  geom_histogram(binwidth = bw, fill = "grey70", color = "black") +
  geom_density(aes(y = after_stat(count) * bw), linewidth = 0.8, col = "darkred") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of country-year 'fair residuals'",
       x = "Fair residual (Observed − Predicted fixed effects)",
       y = "Frequency")


#A2) Boxplot per capire outlier globali
ggplot(df_res, aes(x = "", y = resid_fix)) +
  geom_boxplot(
    fill = "grey85", color = "grey25",
    width = 0.35, outlier.alpha = 0.35, outlier.size = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.7, color = "grey30") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  ) +
  labs(
    title = "Global spread of fair residuals",
    y = "Fair residual (Observed − Predicted fixed effects)"
  )

#Insight B — Stabilità nel tempo (persistenti o “episodici”?)
#B1) Media per decennio + heatmap 
df_decade <- df_res %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(country, decade) %>%
  summarise(mean_resid = mean(resid_fix, na.rm = TRUE),
            n = n(), .groups = "drop")


ord_cty <- df_res %>%
  group_by(country) %>%
  summarise(mean_resid = mean(resid_fix, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_resid) %>%
  pull(country)

df_decade$country <- factor(df_decade$country, levels = ord_cty)

ggplot(df_decade, aes(x = factor(decade), y = country, fill = mean_resid)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Fair residuals by decade (country averages)",
       x = "Decade", y = NULL, fill = "Mean residual") +
  theme(axis.text.y = element_text(size = 7))

#B2) “Mean vs Volatility”: chi è sopra atteso e instabile
df_country_stats <- df_res %>%
  group_by(country) %>%
  summarise(
    mean_resid = mean(resid_fix, na.rm = TRUE),
    sd_resid   = sd(resid_fix, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_country_stats, aes(x = mean_resid, y = sd_resid)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.7, color = "grey30") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Average deviation vs volatility (country level)",
       x = "Mean fair residual", y = "SD of fair residuals")



#Insight C — “Top/Bottom con incertezza” (non solo ranking)
df_country_ci <- df_res %>%
  group_by(country) %>%
  summarise(
    mean_resid = mean(resid_fix, na.rm=TRUE),
    se = sd(resid_fix, na.rm=TRUE)/sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    lo = mean_resid - 1.96*se,
    hi = mean_resid + 1.96*se
  )

topN <- 12
plot_tbl <- bind_rows(
  df_country_ci %>% slice_max(mean_resid, n = topN),
  df_country_ci %>% slice_min(mean_resid, n = topN)
) %>%
  arrange(mean_resid)

plot_tbl <- plot_tbl %>%
  mutate(sign = ifelse(mean_resid >= 0, "pos", "neg"))

ggplot(plot_tbl, aes(x = mean_resid, y = reorder(country, mean_resid), color = sign)) +
  geom_point(size = 2.6) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  scale_color_manual(values = c(neg = "dodgerblue3", pos = "firebrick2"), guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Country 'fair residuals' with 95% CI",
       x = "Mean fair residual (Observed − Predicted fixed effects)",
       y = NULL)


