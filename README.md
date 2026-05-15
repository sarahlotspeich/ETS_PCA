Sampling on Principal Components to Strategically Validate Error-Prone
Data While Balancing Multiple Models
================
Sarah C. Lotspeich and Cole Manschot
15 May 2026

## Setup

``` r
# Load packages (can be installed from CRAN)
library(dplyr) ## for data wrangling
library(tidyr) ## for data pivoting
library(mice) ## for imputation
library(ggplot2) ## for pretty plots
library(latex2exp) ## for LaTex in plots
library(corrplot) ## to plot correlation matrix

# Load packages (can be installed from GitHub)
## Run once: devtools::install_github("sarahlotspeich/auditDesignR")
library(auditDesignR) ## for validation study designs
```

## NHANES Data and Models

Our analysis dataset merges demographic, examination, laboratory, and
nutrition information from the 2021-2023 [National Health and Nutrition
Examination Survey
(NHANES)](https://www.cdc.gov/nchs/nhanes/index.html).

``` r
## Read in data from GitHub
nhanes_data = read.csv("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/NHANES-Analysis/analysis_data_orig.csv")

## Convert factor exposures, subset to necessary columns
nhanes_data = nhanes_data |> 
  mutate(RIAGENDR = factor(x = RIAGENDR, 
                           levels = c(1, 2), 
                           labels = c("Male", "Female")), 
         RIDRETH1 = factor(x = RIDRETH1, 
                           levels = c(3, 1, 2, 4, 5), 
                           labels = c("Non-Hispanic White", 
                                      "Mexican American", 
                                      "Other Hispanic", 
                                      "Non-Hispanic Black", 
                                      "Other Race (Including Multi-Racial)")), 
         DMDEDUC2 = factor(x = DMDEDUC2, 
                           levels = c(5, 1, 2, 3, 4), 
                           labels = c("College Graduate or Above", 
                                      "< 9th Grade", 
                                      "9-11th Grade", 
                                      "High School Grad/GED or Equivalent",
                                      "Some College or AA Degree")), 
         RIDAGEYR = RIDAGEYR / 10) |> ## rescale age to 10-year increments
  dplyr::select(SEQN, Y1:XSTAR5, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2)  

## Define vector of additional (error-free) exposures
Z = c("RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2")
```

We will consider five models for the data application. These are
motivated by connecting dietary measures to outcomes that relate to
clinically relevant outcomes and deficiencies. The models considered are
as follows:

| Outcome | Exposure | Clinical Relevance |
|----|----|----|
| $Y_1$: Vitamin D | $X_1$: Calcium Intake | Vitamin D facilitates calcium absorption |
| $Y_2$: Resting Heart Rate | $X_2$: Caffeine Intake | Caffeine can affect the heart rate and cardiovascular function |
| $Y_3$: HDL Cholesterol | $X_3$: Saturated Fat Intake | HDL cholesterol is associated cardiovascular disease and saturated fat is influenced HDL levels |
| $Y_4$: Insulin | $X_4$: Alcohol Consumption | Insulin resistance is associated with numerous co-morbidities and alcohol may impact insulin sensitivity |
| $Y_5$: Folate | $X_5$: Folate Food | Low levels of iron contribute to anemia risk and is related to iron intake |

## Simulating Error-Free Nutrient Intake

These five models relate relevant health outcomes to dietary factors
which individuals may have some level of control over. However,
diary-based measurements of dietary intake are often confounded by
recall bias and measurement error from mapping foods and proportions to
nutrient intake. For illustration, we simulate the more accurate dietary
intake exposures that might be obtained via additional, more invasive
testing (e.g., of blood or urine samples).

``` r
# //////////////////////////////////////////////////////////////////////////////
# Simulate error-free continuous exposures Xj|Xj* //////////////////////////////
# //////////////////////////////////////////////////////////////////////////////
### For reproducibility 
set.seed(918) 
### Simulate random errors (with variance relative to the variance of X*s)
varXSTARs = as.numeric(
  apply(X = nhanes_data[, c("XSTAR1", "XSTAR2", "XSTAR3", "XSTAR4", "XSTAR5")], ### calculate Var(X1), ..., Var(X5)
        MARGIN = 2, 
        FUN = var)
  )
U = MASS::mvrnorm(n = nrow(nhanes_data), 
                  mu = rep(0, 5), ### mean vector
                  Sigma = diag( ### variance-covariance matrix
                    varXSTARs / 4, ### assuming uncorrelated errors with Var(Uj) = Var(X*j) / 4
                    nrow = 5)
                  ) 
### Subtract random errors from error-prone exposures (NHANES) to create simulated error-free exposures 
#### Classical additive measurement error model: X* = X + U --> X = X* - U 
X = nhanes_data[, paste0("XSTAR", 1:5)] - U 
colnames(X) = paste0("X", 1:5)
nhanes_data = nhanes_data |> 
  bind_cols(X)

### Check sample size (subset to complete cases on Y, X*, Z)
nhanes_data |> 
  nrow()
```

    ## [1] 2388

*Note:* The `nhanes_data` including simulated exposure measurement error
can be found in this repository as
[`analysis_data_with_errors.csv`](NHANES-Analysis/analysis_data_with_errors.csv).

## Descriptive Statistics

<img src="README_files/figure-gfm/unnamed-chunk-3-1.png" alt=""  />

``` r
## Estimate covariance of X* variables
cov(nhanes_data[, paste0("XSTAR", 1:5)]) 
```

    ##              XSTAR1     XSTAR2     XSTAR3    XSTAR4     XSTAR5
    ## XSTAR1 311220.62986 10222.4015 5508.79197  86.62312 30146.7031
    ## XSTAR2  10222.40152 30523.5473  479.53636 137.98007  2356.5188
    ## XSTAR3   5508.79197   479.5364  281.98449  25.36496   805.3598
    ## XSTAR4     86.62312   137.9801   25.36496 476.47932   451.9767
    ## XSTAR5  30146.70310  2356.5188  805.35976 451.97665 19967.5159

``` r
cor_matrix = cor(nhanes_data[, paste0("XSTAR", 1:5)])

library(ggcorrplot)
plot_corr = ggcorrplot(cor_matrix,
           lab = TRUE,          # adds correlation coefficients
           colors = c("#E69F00", "white", "#56B4E9")) + # orange - white - blue
  scale_x_discrete(labels = c(TeX("$X_1^*$ (Calcium)"),
                              TeX("$X_2^*$ (Caffeine)"),
                              TeX("$X_3^*$ (Saturated Fat)"),
                              TeX("$X_4^*$ (Alcohol)"),
                              TeX("$X_5^*$ (Food Folate)"))) +
  scale_y_discrete(labels = c(TeX("$X_1^*$  (Calcium)"),
                              TeX("$X_2^*$ (Caffeine)"),
                              TeX("$X_3^*$ (Saturated Fat)"),
                              TeX("$X_4^*$ (Alcohol)"),
                              TeX("$X_5^*$ (Food Folate)"))) +
  theme_minimal(base_size = 14) + 
  xlab("Error-Prone Nutrient Intake Exposure Value") + 
  ylab("Error-Prone Nutrient Intake Exposure Value") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10))
```

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## ℹ The deprecated feature was likely used in the ggcorrplot package.
    ##   Please report the issue at <https://github.com/kassambara/ggcorrplot/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
## Save 
ggsave(filename = "~/Documents/ETS_PCA/NHANES-Analysis/Xstar-Correlation.png", 
       plot = plot_corr, 
       device = "png", 
       width = 8, 
       height = 8)
```

``` r
## Fit PCA on X* variables (using correlation matrix)
pc = princomp(nhanes_data[, paste0("XSTAR", 1:5)], cor = TRUE) 
### Summarize PCA on X* variables 
summary(pc) 
```

    ## Importance of components:
    ##                          Comp.1    Comp.2    Comp.3    Comp.4     Comp.5
    ## Standard deviation     1.395683 1.0089750 0.9774561 0.8229064 0.63359606
    ## Proportion of Variance 0.389586 0.2036061 0.1910841 0.1354350 0.08028879
    ## Cumulative Proportion  0.389586 0.5931921 0.7842762 0.9197112 1.00000000

``` r
### Print PCA loadings 
pc$loadings
```

    ## 
    ## Loadings:
    ##        Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
    ## XSTAR1  0.589  0.240  0.148  0.234  0.721
    ## XSTAR2  0.221        -0.966              
    ## XSTAR3  0.589  0.149         0.418 -0.674
    ## XSTAR4  0.132 -0.939         0.288       
    ## XSTAR5  0.491 -0.177  0.184 -0.825 -0.112
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
    ## SS loadings       1.0    1.0    1.0    1.0    1.0
    ## Proportion Var    0.2    0.2    0.2    0.2    0.2
    ## Cumulative Var    0.2    0.4    0.6    0.8    1.0

``` r
### Extract the first principal component
nhanes_data$pc1 = pc$scores[, 1] 
### Plot score vs. X* and Y
plot_loadings = nhanes_data |> 
  dplyr::select(pc1, Y1:Y5, XSTAR1:XSTAR5) |> 
  pivot_longer(cols = Y1:XSTAR5, names_to = "Variable", values_to = "Value") |>
  mutate(Model = sub("Y|XSTAR", "", Variable), 
         Variable = paste0(sub(pattern = "XSTAR", 
                               replacement = "$X^*_", 
                               x = sub(pattern = "Y", 
                                       replacement = "$Y_", 
                                       x = "XSTAR1")), "$"), 
         Variable = factor(x = Variable, 
                           levels = c(paste0("$Y_", 1:5, "$"), 
                                      paste0("$X^*_", 1:5, "$")), 
                           labels = TeX(c(paste0("$Y_", 1:5, "$"), 
                                      paste0("$X^*_", 1:5, "$")))), 
         ) |> 
  ggplot(aes(x = pc1, y = Value, color = Model)) + 
  geom_point() + 
  ggthemes::scale_color_colorblind(guide = "none") + 
  facet_wrap(~Variable, 
             ncol = 5, 
             labeller = label_parsed) + 
  theme_minimal(base_size = 14) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        legend.title = element_text(face = "bold"), 
        legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())
# Save it 
plot_loadings
```

<img src="README_files/figure-gfm/PCA-1.png" alt=""  />

``` r
ggsave(filename = "~/Documents/ETS_PCA/NHANES-Analysis/PCA-Loadings.png", 
       plot = plot_covar, 
       device = "png", 
       width = 10, 
       height = 6)
```

## Fitting the Models Under Different Partial Validation Designs

For the data application in the manuscript, we fit each of the five
models of interest assuming that only $n = 250$ of the $N =$ 2388
individuals had validated exposure information $X_1, \dots, X_5$
measured. For the other \$N - n = \$ 2138 individuals, $X_1, \dots, X_5$
are missing and must be multiply imputed to fit the models.

``` r
## Set validation study size
n = 250

## For reproducibility (affects SRS only)
set.seed(918)

## Initialize empty dataframe to hold estimates from the 5 models
fits = data.frame()
```

### Simple Random Sampling (SRS)

``` r
## Simple random sampling 
V_srs = sample_srs(phI = nrow(nhanes_data), ### Phase I sample size
                   phII = n) ### Phase II (validation study) sample size)

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
des_srs = nhanes_data |> 
  bind_cols(data.frame(V = V_srs)) |> 
  mutate(X1 = ifelse(test = V == 1, yes = X1, no = NA), 
         X2 = ifelse(test = V == 1, yes = X2, no = NA), 
         X3 = ifelse(test = V == 1, yes = X3, no = NA), 
         X4 = ifelse(test = V == 1, yes = X4, no = NA), 
         X5 = ifelse(test = V == 1, yes = X5, no = NA))

## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Imputation model depends on the validation study design and number of imputations
  ### Which variables go into the imputation model 
  imp_mod_vars = c(paste0("X", j), paste0("XSTAR", j), Z) #### All include Xj, Xj*, Z
  imp_mod_vars = c(imp_mod_vars, paste0("Y", j)) #### Multiple imputation adds Yj 
  
  ### Impute and fit model 
  #### Multiple imputation
  mice_dat = mice(m = 75,
                  data = des_srs[, imp_mod_vars], 
                  method = "norm",
                  printFlag = FALSE)
  
  #### Fit analysis model to the imputed data (separately) 
  after_imp_fit = with(data = mice_dat, 
                       expr = glm(formula = as.formula(paste0("Y", j, "~", "X", j, "+", paste(Z, collapse = "+"))), 
                                  family = "gaussian"))
  
  #### Pool the analysis models from each imputation
  pool_imp_fit = summary(pool(after_imp_fit)) |> 
    dplyr::select(-df)
  
  ### Save coefficient estimates
  fits = fits |> 
    bind_rows(data.frame(cbind(model = j, design = "SRS", pool_imp_fit)))
}
```

### Extreme Tail Sampling on $X_1^*$ (ETS-$X_1^*$)

``` r
## ETS on X1*
V_etsXSTAR1 = sample_ets(ets_dat = nhanes_data$XSTAR1, ### Sample on X1*
                         phI = nrow(nhanes_data), ### Phase I sample size
                         phII = n) ### Phase II (validation study) sample size)

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
des_etsXSTAR1 = nhanes_data |> 
  bind_cols(data.frame(V = V_etsXSTAR1)) |> 
  mutate(X1 = ifelse(test = V == 1, yes = X1, no = NA), 
         X2 = ifelse(test = V == 1, yes = X2, no = NA), 
         X3 = ifelse(test = V == 1, yes = X3, no = NA), 
         X4 = ifelse(test = V == 1, yes = X4, no = NA), 
         X5 = ifelse(test = V == 1, yes = X5, no = NA))

## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Imputation model depends on the validation study design and number of imputations
  ### Which variables go into the imputation model 
  imp_mod_vars = c(paste0("X", j), paste0("XSTAR", j), Z) #### All include Xj, Xj*, Z
  imp_mod_vars = unique(c(imp_mod_vars, "XSTAR1")) #### ETS-X1 adds X1* 
  imp_mod_vars = c(imp_mod_vars, paste0("Y", j)) #### Multiple imputation adds Yj
  
  ### Impute and fit model 
  #### Multiple imputation
  mice_dat = mice(m = 75,
                  data = des_etsXSTAR1[, imp_mod_vars], 
                  method = "norm",
                  printFlag = FALSE)
  
  #### Fit analysis model to the imputed data (separately) 
  after_imp_fit = with(data = mice_dat, 
                       expr = glm(formula = as.formula(paste0("Y", j, "~", "X", j, "+", paste(Z, collapse = "+"))), 
                                  family = "gaussian"))
  
  #### Pool the analysis models from each imputation
  pool_imp_fit = summary(pool(after_imp_fit)) |> 
    dplyr::select(-df)
  
  ### Save coefficient estimates
  fits = fits |> 
    bind_rows(data.frame(cbind(model = j, design = "ETS (X1*)", pool_imp_fit)))
}
```

### Extreme Tail Sampling on $PC_1^*$ (ETS-$PC_1^*$)

``` r
## ETS on PC1*
V_etsPCstar1 = sample_pca(pca_dat = nhanes_data[, paste0("XSTAR", 1:5)], ## sample on first PC of X1*, ..., X5*
                          phI = nrow(nhanes_data), ## Phase I sample size
                          phII = n) ## Phase II (validation study) sample size

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
des_etsPCstar1 = nhanes_data |> 
  bind_cols(data.frame(V = V_etsPCstar1)) |> 
  mutate(X1 = ifelse(test = V == 1, yes = X1, no = NA), 
         X2 = ifelse(test = V == 1, yes = X2, no = NA), 
         X3 = ifelse(test = V == 1, yes = X3, no = NA), 
         X4 = ifelse(test = V == 1, yes = X4, no = NA), 
         X5 = ifelse(test = V == 1, yes = X5, no = NA))

## Since we sampled on PC1*, need to add it to the analytical dataset so 
### we can include it in the imputation models 
des_etsPCstar1$pc1 = pc$scores[, 1] ### extract the first principal component

## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Imputation model depends on the validation study design and number of imputations
  ### Which variables go into the imputation model 
  imp_mod_vars = c(paste0("X", j), paste0("XSTAR", j), Z) #### All include Xj, Xj*, Z
  imp_mod_vars = c(imp_mod_vars, "pc1") #### ETS-PCA adds pc 
  imp_mod_vars = c(imp_mod_vars, paste0("Y", j)) #### Multiple imputation adds Yj
  
  ### Impute and fit model 
  #### Multiple imputation
  mice_dat = mice(m = 75,
                  data = des_etsPCstar1[, imp_mod_vars], 
                  method = "norm",
                  printFlag = FALSE)
  
  #### Fit analysis model to the imputed data (separately) 
  after_imp_fit = with(data = mice_dat, 
                       expr = glm(formula = as.formula(paste0("Y", j, "~", "X", j, "+", paste(Z, collapse = "+"))), 
                                  family = "gaussian"))
  
  #### Pool the analysis models from each imputation
  pool_imp_fit = summary(pool(after_imp_fit)) |> 
    dplyr::select(-df)
  
  ### Save coefficient estimates
  fits = fits |> 
    bind_rows(data.frame(cbind(model = j, design = "ETS (PC1*)", pool_imp_fit)))
}
```

## Results

<img src="README_files/figure-gfm/unnamed-chunk-6-1.png" alt=""  />

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" alt=""  />

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" alt=""  />

``` r
library(patchwork)
both = (plot_coeff / plot_ciwidth) + 
  plot_annotation(tag_levels = 'A')
## Save it 
ggsave(plot = both, 
       filename = "~/Documents/ETS_PCA/NHANES-Analysis/nhanes_forest_bar.pdf", 
       device = "pdf", 
       width = 12, 
       height = 10)
```
