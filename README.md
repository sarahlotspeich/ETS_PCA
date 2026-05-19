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
library(ggcorrplot) ## for correlation plot

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
                                      "Some College or AA Degree"))) |> 
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
## For reproducibility 
set.seed(918) 

## Calculate Var(X1*), ..., Var(X5*) in NHANES 
varXSTARs = as.numeric(
  apply(X = nhanes_data[, c("XSTAR1", "XSTAR2", "XSTAR3", "XSTAR4", "XSTAR5")], 
        MARGIN = 2, 
        FUN = var)
  )
varU = varXSTARs / 4

## Simulate random errors (with variance relative to the variance of X*s)
U = MASS::mvrnorm(n = nrow(nhanes_data), 
                  mu = rep(0, 5), ### mean vector
                  Sigma = diag( ### variance-covariance matrix
                    varU, ### assuming uncorrelated errors with Var(Uj) = Var(X*j) / 4
                    nrow = 5)
                  ) 

## Subtract random errors from error-prone exposures (NHANES) to create simulated error-free exposures
### Classical additive measurement error model: X* = X + U --> X = X* - U 
X = nhanes_data[, paste0("XSTAR", 1:5)] - U 
colnames(X) = paste0("X", 1:5)
nhanes_data = nhanes_data |> 
  bind_cols(X)

## Check sample size (subset to complete cases on Y, X*, Z)
nhanes_data |> 
  nrow()
```

    ## [1] 2388

*Note:* The `nhanes_data` including simulated exposure measurement error
can be found in this repository as
[`analysis_data_with_errors.csv`](NHANES-Analysis/analysis_data_with_errors.csv).

``` r
# Write a helper function (for later) 
simulate_error_free = function() {
  ## Simulate random errors (with variance relative to the variance of X*s)
  U = MASS::mvrnorm(n = nrow(nhanes_data), 
                    mu = rep(0, 5), ### mean vector
                    Sigma = diag( ### variance-covariance matrix
                      varXSTARs / 4, ### assuming uncorrelated errors with Var(Uj) = Var(X*j) / 4
                      nrow = 5)
                    ) 
  
  ## Subtract random errors from error-prone exposures (NHANES) to create simulated error-free exposures
  ### Classical additive measurement error model: X* = X + U --> X = X* - U 
  X = nhanes_data[, paste0("XSTAR", 1:5)] - U 
  colnames(X) = paste0("X", 1:5)
  
  ## Return simulated X1,...X5 
  return(X)
}
```

## Descriptive Statistics and Principal Components Analysis

### Error-Prone Exposures

#### Versus Error-Free Exposures

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" alt=""  />

#### Correlation Matrix

<img src="README_files/figure-gfm/corrplot X*-1.png" alt=""  />

#### Principal Components Analysis of Error-Prone Exposures

``` r
## Inspect numeric summaries X* variables (different scales/variability)
summary(nhanes_data[, paste0("XSTAR", 1:5)]) 
```

    ##      XSTAR1           XSTAR2           XSTAR3           XSTAR4       
    ##  Min.   :   0.0   Min.   :   0.0   Min.   :  0.00   Min.   :  0.000  
    ##  1st Qu.: 518.0   1st Qu.:  33.0   1st Qu.: 15.48   1st Qu.:  0.000  
    ##  Median : 769.0   Median : 120.0   Median : 23.58   Median :  0.000  
    ##  Mean   : 885.7   Mean   : 156.2   Mean   : 26.62   Mean   :  7.717  
    ##  3rd Qu.:1128.0   3rd Qu.: 210.0   3rd Qu.: 33.93   3rd Qu.:  0.000  
    ##  Max.   :9266.0   Max.   :1920.0   Max.   :208.84   Max.   :448.100  
    ##      XSTAR5      
    ##  Min.   :   0.0  
    ##  1st Qu.: 124.0  
    ##  Median : 184.0  
    ##  Mean   : 213.9  
    ##  3rd Qu.: 267.0  
    ##  Max.   :2064.0

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
### Extract the first principal component
nhanes_data$pc1 = pc$scores[, 1] 
```

##### Scree Plot

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" alt=""  />

##### Loadings Plot

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" alt=""  />

## Fitting the Models Under Different Partial Validation Designs

For the data application in the manuscript, we fit each of the five
models of interest assuming that only $n = 250$ of the $N =$ 2388
individuals had validated exposure information $X_1, \dots, X_5$
measured. For the other $N - n =$ 2138 individuals, $X_1, \dots, X_5$
are missing and must be multiply imputed to fit the models.

``` r
## Set validation study size
n = 250

## For reproducibility (affects SRS only)
set.seed(918)

## Initialize empty dataframe to hold estimates from the 5 models
fits = data.frame()
```

### Full Validation (Gold Standard)

A key advantage to simulating the validation data $\pmb{X}$ is that we
can actually compare each of the partially validated analyses to the
“gold standard” (i.e., if all $N$ patients could be validated).

``` r
## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Fit analysis model to the original (complete) data (separately) 
  gs_fit = glm(formula = as.formula(paste0("Y", j, "~", "X", j, "+", paste(Z, collapse = "+"))), 
               data = nhanes_data, 
               family = "gaussian")
  
  ### Summary of analysis model
  summ_gs_fit = coefficients(summary(gs_fit)) 
  
  ### Reformat summary to merge with MI models later
  summ_gs_fit = summ_gs_fit |> 
    data.frame() |> 
    mutate(term = rownames(summ_gs_fit)) |> 
    rename(estimate = Estimate, 
           std.error = Std..Error, 
           statistic = t.value, 
           p.value = Pr...t..)
  
  ### Save coefficient estimates
  fits = fits |> 
    bind_rows(data.frame(cbind(model = j, design = "GS", summ_gs_fit)))
}
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

``` r
# Write a helper function (for later) 
run_srs_analysis = function(data, val_size = 250, num_imp = 75) {
  ## Initialize empty dataframe to hold estimates from the 5 models
  fits = data.frame()
  
  ## Simple random sampling 
  V_srs = sample_srs(phI = nrow(data), ### Phase I sample size
                     phII = val_size) ### Phase II (validation study) sample size)
  
  ## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
  des_srs = data |> 
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
    mice_dat = mice(m = num_imp,
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
  ### Return all models' pooled coefficient estimates 
  return(fits)
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

``` r
# Write a helper function (for later) 
run_etsXstar1_analysis = function(data, val_size = 250, num_imp = 75) {
  ## Initialize empty dataframe to hold estimates from the 5 models
  fits = data.frame()
  
  ## ETS on X1*
  V_etsXSTAR1 = sample_ets(ets_dat = data$XSTAR1, ### Sample on X1*
                           phI = nrow(data), ### Phase I sample size
                           phII = n) ### Phase II (validation study) sample size)
  
  ## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
  des_etsXSTAR1 = data |> 
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
  
  ### Return all models' pooled coefficient estimates 
  return(fits)
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

``` r
# Write a helper function (for later) 
run_etsPCstar1_analysis = function(data, val_size = 250, num_imp = 75) {
  ## Initialize empty dataframe to hold estimates from the 5 models
  fits = data.frame()
  
  ## ETS on PC1*
  V_etsPCstar1 = sample_pca(pca_dat = data[, paste0("XSTAR", 1:5)], ## sample on first PC of X1*, ..., X5*
                            phI = nrow(data), ## Phase I sample size
                            phII = n) ## Phase II (validation study) sample size

  ## Create analytical dataset, incorporating validation indicators and making unvalidated patients' exposures missing
  des_etsPCstar1 = data |> 
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
  
  ### Return all models' pooled coefficient estimates 
  return(fits)
}
```

## Results

<img src="README_files/figure-gfm/unnamed-chunk-16-1.png" alt=""  />

<img src="README_files/figure-gfm/unnamed-chunk-17-1.png" alt=""  />

### Total Coefficient Variability

    ## # A tibble: 4 × 2
    ##   design     sum_var
    ##   <chr>        <dbl>
    ## 1 ETS (PC1*) 0.00464
    ## 2 ETS (X1*)  0.00557
    ## 3 GS         0.00151
    ## 4 SRS        0.00539
