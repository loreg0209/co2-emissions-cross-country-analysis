# Structural Drivers of Per-Capita CO₂ Emissions

This project was developed for the **Data Science Lab** course at the University of Milano-Bicocca.

The study investigates the structural drivers of per-capita CO₂ emissions across countries over time by combining **econometric modelling, machine learning, residual-based country analysis and clustering**.

The analysis uses World Bank World Development Indicators and covers more than 100 countries over the period **1990–2021**.

---

## Project Overview

The project is structured around four research questions:

1. **RQ1 — Structural drivers:** Which socio-economic and energy variables explain cross-country and temporal variation in per-capita CO₂ emissions?
2. **RQ2 — Predictive capacity:** How accurately can Random Forest and XGBoost predict per-capita CO₂ emissions?
3. **RQ3 — Country deviations:** Which countries emit more or less CO₂ than expected after accounting for structural drivers?
4. **RQ4 — Energy-system archetypes:** Can countries be grouped into meaningful clusters according to their energy mix, efficiency and demand profile?

The project deliberately combines explanatory and predictive methods rather than relying on a single modelling approach.

---

## Data

The analysis is based on the **World Bank World Development Indicators (WDI)**.

The dataset includes indicators describing:

- per-capita CO₂ emissions
- electricity consumption
- energy efficiency
- coal, gas, oil, nuclear and renewable shares
- energy imports
- GDP and sectoral composition
- urbanisation
- electricity access

The modelling dataset was built using complete observations across the selected variables. Continuous predictors were standardized, while skewed variables such as CO₂ emissions and electricity consumption were log-transformed where appropriate.

---

## RQ1 — Mixed-Effects Model with AR(1)

The main inferential analysis uses a **linear mixed-effects model** with:

- country-specific random intercepts
- nonlinear terms for electricity consumption
- an AR(1) residual correlation structure
- energy-mix, efficiency, economic and demographic predictors

The AR(1) specification accounts for the strong temporal persistence of national energy systems and substantially improves residual autocorrelation diagnostics.

### Main findings

Electricity consumption per capita emerged as the dominant positive driver of emissions.

The model also identified:

- coal, oil and natural gas as positive emission drivers
- renewable energy as the strongest mitigating factor
- higher energy productivity as associated with lower emissions
- nuclear generation as an additional mitigating factor
- urbanisation as positively associated with per-capita emissions

The final model was evaluated using grouped, spatio-temporal and leave-one-country-out validation strategies.

---

## RQ2 — Machine Learning

Two nonlinear predictive models were used:

- **Random Forest**
- **XGBoost**

Validation was designed specifically for panel data to reduce leakage across countries and time.

### Validation strategies

- GroupKFold by country
- spatio-temporal blocked cross-validation
- Leave-One-Country-Out validation

XGBoost achieved the strongest predictive performance, with out-of-sample R² values approximately in the **0.91–0.94** range and RMSE around **0.18–0.21**.

Model interpretation was supported through:

- feature importance
- SHAP values
- nonlinear threshold analysis

The machine-learning results broadly confirmed the same hierarchy of drivers found by the econometric model.

---

## RQ3 — Country-Level “Fair Residuals”

To examine country-specific deviations, predictions were generated using only the fixed-effects component of the AR(1) model.

The resulting residuals represent:

> observed emissions − structurally expected emissions

This allows countries to be compared after accounting for observable differences in energy demand, fuel composition, economic structure and urbanisation.

The analysis includes:

- the global distribution of fair residuals
- country-level average deviations with 95% confidence intervals
- persistence of deviations across decades
- mean deviation versus temporal volatility

This separates structural emission intensity from unexplained country-specific over- or under-performance.

---

## RQ4 — Energy Profile Clustering

Countries were grouped using contemporary average energy-system profiles for **2015–2021**.

The clustering variables include:

- electricity consumption per capita
- renewable energy
- coal and gas shares
- nuclear energy
- GDP per unit of energy use
- services share of GDP

Variables were standardized before clustering.

The number of clusters was selected using the **silhouette criterion**, resulting in **six energy-system archetypes**.

PCA was used for visualization and interpretation of the cluster structure.

Examples of the identified profiles include:

- high-demand, relatively low-carbon systems
- coal-dependent systems
- low-demand, renewable-intensive systems
- gas-dominated systems
- diversified and efficiency-oriented systems
- fossil-tilted, intermediate-demand systems

The clusters were subsequently compared in terms of per-capita CO₂ emissions and recent emission trajectories.

---

## Key Results

The combined analysis shows that:

- electricity demand is the strongest structural driver of per-capita CO₂ emissions
- renewable energy and energy efficiency have substantial mitigating effects
- fossil-fuel composition remains a major determinant of national emission levels
- XGBoost provides strong predictive validation of the econometric findings
- country-specific deviations remain persistent even after controlling for structural factors
- contemporary energy systems can be summarized into six interpretable archetypes

The project demonstrates how econometric inference, machine learning and unsupervised learning can be combined to study the same environmental problem from complementary perspectives.

---

## Technologies

- R
- tidyverse
- ggplot2
- `nlme`
- `lme4`
- `MuMIn`
- `clubSandwich`
- `xgboost`
- `ranger`
- `cluster`
- `factoextra`
- PCA
- K-Means
- SHAP analysis

---

## Report

The complete project report, including methodology, diagnostics, figures and detailed interpretation, is available here:

[Read the full report](docs/DSLabReport-2025.pdf)

---

## Authors

Lorenzo Gulizia  
Hrutuja Girish Kargirwar

Master's Degree in Data Science  
University of Milano-Bicocca  
Academic Year 2024/2025
