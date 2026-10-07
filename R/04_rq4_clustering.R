library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(purrr)
library(factoextra)


# This script is designed to run after 01_rq1_mixed_effects.R.
# It reuses the cleaned dataset created there.
if (!exists("df_with_dummies_ext")) {
  stop("Run R/01_rq1_mixed_effects.R first.")
}

df <- df_with_dummies_ext %>%
  mutate(
    country = as.character(country),
    year = as.numeric(Year)
  )

df_win <- df %>% filter(year >= 2015, year <= 2021)

# =========================
# 1) Variabili RQ4 (dominanti/significative dal report)
# =========================
vars_cluster <- c(
  "Elec_cons_pc",       # dominant scale driver
  "Renewables_total",   # strongest mitigating
  "Coal_share",         # strongest fossil intensity
  "GDP_per_energy",     # efficiency
   "Gas_share",   # additional mitigation
    "Nuclear_share" ,
  "Services_GDP"
  
)

vars_present <- intersect(vars_cluster, names(df))
vars_missing <- setdiff(vars_cluster, vars_present)
if (length(vars_missing) > 0) message("⚠️ Variabili mancanti escluse: ", paste(vars_missing, collapse=", "))

year_min <- 2015
year_max <- 2021
min_years <- 5

# =========================
# 2) Complete-case a livello country-year + filtro paesi con anni sufficienti
# =========================
df_cc <- df %>%
  filter(year >= year_min, year <= year_max) %>%
  select(country, year, all_of(vars_present)) %>%
  na.omit()   # elimina righe con almeno un NA tra le variabili di clustering

keep_countries <- df_cc %>%
  count(country, name = "n_years_complete") %>%
  filter(n_years_complete >= min_years) %>%
  pull(country)

df_cc <- df_cc %>% filter(country %in% keep_countries)

# =========================
# 3) Country profile = average over 2015–2021 
# =========================
feat_country <- df_cc %>%
  group_by(country) %>%
  summarise(
    across(all_of(vars_present), mean),
    n_years = n_distinct(year),
    .groups = "drop"
  )

# =========================
# 4) Standardizza
# =========================
Xz <- scale(feat_country %>% select(all_of(vars_present)))
rownames(Xz) <- feat_country$country

# =========================
# 5) Scegli k (silhouette)
# =========================
set.seed(123)
n_dist <- nrow(unique(as.data.frame(Xz)))
if (n_dist < 2) stop("Troppi pochi profili distinti: allenta filtri o riduci variabili.")

k_grid <- 2:min(8, n_dist)  

sil_tbl <- purrr::map_dfr(k_grid, function(k){
  km <- kmeans(Xz, centers = k, nstart = 100)
  ss <- cluster::silhouette(km$cluster, dist(Xz))
  tibble::tibble(k = k, silhouette = mean(ss[,3]))
})

ggplot(sil_tbl, aes(k, silhouette)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "RQ4 — Silhouette vs k (dominant energy-profile variables)",
       x = "k", y = "Mean silhouette")

k_final <- sil_tbl$k[which.max(sil_tbl$silhouette)]
message("k scelto (silhouette) = ", k_final)

# =========================
# 6) K-means finale + PCA 
# =========================
set.seed(123)
km_final <- kmeans(Xz, centers = k_final, nstart = 200)

cluster_df <- feat_country %>%
  transmute(country, cluster = factor(km_final$cluster))

pca <- prcomp(Xz, center = FALSE, scale. = FALSE)

ok_ell <- all(table(cluster_df$cluster) >= 3)

fviz_pca_ind(
  pca, geom = "point",
  habillage = cluster_df$cluster,
  addEllipses = ok_ell, ellipse.level = 0.68,
  repel = TRUE
) + ggtitle("RQ4 — PCA projection of country clusters") + theme(plot.title = element_text(hjust = 0.5))

# profili medi (interpretazione)
centroids_raw <- feat_country %>%
  left_join(cluster_df, by = "country") %>%
  group_by(cluster) %>%
  summarise(n_countries = n(),
            across(all_of(vars_present), mean),
            .groups = "drop")

centroids_raw

table(cluster_df$cluster)

library(purrr)
library(ggrepel)

# =========================
# GRAFICO 1 — PCA scatter + label paesi
# =========================
pca <- prcomp(Xz, center = FALSE, scale. = FALSE)
scores <- as.data.frame(pca$x[,1:2])
scores$country <- rownames(Xz)
scores <- scores %>% left_join(cluster_df, by = "country")

ev <- (pca$sdev^2) / sum(pca$sdev^2)
ev1 <- round(100*ev[1], 1)
ev2 <- round(100*ev[2], 1)

p1 <- ggplot(scores, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2.7, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, max.overlaps = 80) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "PCA of Energy Profile Clusters",
    subtitle = paste0("Explained variance: ", ev1, "% + ", ev2, "% | K = ", k_final),
    x = paste0("PC1 (", ev1, "%)"),
    y = paste0("PC2 (", ev2, "%)"),
    color = "Cluster"
  )

print(p1)

# =========================
# GRAFICO 2 — Heatmap profili cluster (mean z-score)
# =========================
Z <- as.data.frame(Xz)
Z$country <- rownames(Xz)
Z <- Z %>% left_join(cluster_df, by = "country")

prof <- Z %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars_present), mean), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "z_mean")

p2 <- ggplot(prof, aes(x = cluster, y = variable, fill = z_mean)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, name = "Mean z") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Cluster Profiles (Mean Standardized Values)",
    subtitle = "Country averages, 2015–2021",
    x = "Cluster", y = NULL
  )

print(p2)

# =========================
# GRAFICO 3 — CO2 per cluster nel tempo (mean + IQR)
# =========================
co2_by_cluster_year <- df_win %>%
  inner_join(cluster_df, by = "country") %>%
  group_by(cluster, year) %>%
  summarise(
    mean_co2 = mean(CO2_pc, na.rm = TRUE),
    q25 = quantile(CO2_pc, 0.25, na.rm = TRUE),
    q75 = quantile(CO2_pc, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(co2_by_cluster_year, aes(x = year, y = mean_co2, color = cluster, fill = cluster)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "CO2 Emissions per Capita by Cluster",
    subtitle = "Mean with interquartile range (IQR), 2015–2021",
    x = "Year",
    y = "tCO2 per capita",
    color = "Cluster",
    fill = "Cluster"
  )

print(p3)
