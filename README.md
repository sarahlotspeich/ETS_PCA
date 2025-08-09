Sampling on Principal Components to Strategically Validate Error-Prone
Data While Balancing Multiple Models
================
Sarah C. Lotspeich and Cole Manschot
9 August 2025

## Setup

``` r
# Load packages (can be installed from CRAN)
library(dplyr) ## for data wrangling
library(MASS) ## for multivariate normal generation
library(mice) ## for imputation
library(ggplot2) ## for pretty plots
library(latex2exp) ## for LaTex in plots


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
### See make_nhanes_data.R for more about dataset curation
nhanes_data = read.csv("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/NHANES-Analysis/analysis_data_orig.csv")

## Convert factor covariates, subset to necessary columns
nhanes_data = nhanes_data |> 
  mutate(RIAGENDR = factor(x = RIAGENDR, 
                           levels = c(1, 2), 
                           labels = c("Male", "Female")), 
         RIDRETH1 = factor(x = RIDRETH1, 
                           levels = c(1, 2, 3, 4, 5), 
                           labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other Race (Including Multi-Racial)")), 
         DMDEDUC2 = factor(x = DMDEDUC2, 
                           levels = c(1, 2, 3, 4, 5), 
                           labels = c("< 9th Grade", "9-11th Grade", "High School Grad/GED or Equivalent", "Some College or AA Degree", "College Graduate or Above"))) |> 
  dplyr::select(SEQN, Y1:X5, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2)  

## Define vector of additional (error-free) covariates
Z = c("RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2")
```

We will consider five models for the data application. These are
motivated by connecting dietary measures to outcomes that relate to
clinically relevant outcomes and deficiencies. The models considered are
as follows:

| Outcome | Covariate | Clinical Relevance |
|----|----|----|
| $Y_1$: Vitamin D | $X_1$: Calcium Intake | Vitamin D facilitates calcium absorption |
| $Y_2$: Resting Heart Rate | $X_2$: Caffeine Intake | Caffeine can affect the heart rate and cardiovascular function |
| $Y_3$: HDL Cholesterol | $X_3$: Saturated Fat Intake | HDL cholesterol is associated cardiovascular disease and saturated fat is influenced HDL levels |
| $Y_4$: Insulin | $X_4$: Alcohol Consumption | Insulin resistance is associated with numerous co-morbidities and alcohol may impact insulin sensitivity |
| $Y_5$: Folate | $X_5$: Folate Food | Low levels of iron contribute to anemia risk and is related to iron intake |

## Simulating Covariate Measurement Error

These five models relate relevant health outcomes to dietary factors
which individuals may have some level of control over. However, accurate
measurements of dietary intake are often confounded by recall bias and
measurement error from mapping foods and proportions to nutrient intake.

``` r
## Simulate error-prone continuous covariates Xj*|Xj
### For reproducibility 
set.seed(918) 
### Simulate random errors (with variance relative to the variance of Xs)
varXs = as.numeric(apply(X = nhanes_data[, c("X1", "X2", "X3", "X4", "X5")], ### calculate Var(X1), ..., Var(X5)
                         MARGIN = 2, 
                         FUN = var))
U = mvrnorm(n = nrow(nhanes_data), 
            mu = rep(0, 5), ### mean vector
            Sigma = diag(varXs / 4, ### variance-covariance matrix
                         nrow = 5)) ### assuming independent errors with Var(Uj) = Var(Xj) / 4
### Add random errors to covariates to create error-prone covariates 
Xstar = nhanes_data[, paste0("X", 1:5)] + U 
colnames(Xstar) = paste0("Xstar", 1:5)
nhanes_data = nhanes_data |> 
  bind_cols(Xstar) |> 
  dplyr::select(SEQN, Y1:X5, Xstar1:Xstar5, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2)
```

*Note:* The `nhanes_data` including simulated covariate measurement
error can be found in this repository as
[`analysis_data_with_errors.csv`](NHANES-Analysis/analysis_data_with_errors.csv).

## Fitting the Models Under Different Partial Validation Designs

For the data application in the manuscript, we fit each of the five
models of interest assuming that only $n = 250$ of the $N =$ 2388
individuals had validated covariate information $X_1, \dots, X_5$
measured. For the other \$N - n = \$ 2138 individuals, $X_1, \dots, X_5$
are missing and must be multiply imputed to fit the models. For
reference, we also include the *Gold Standard (Fully Validated)* fits of
each model, which are the normal linear regression models fit using
$X_1, \dots, X_5$ for all $N =$ 2388 individuals in the original NHANES
sample.

``` r
## Set validation study size
n = 250

## For reproducibility (affects SRS only)
set.seed(918)

## Initialize empty dataframe to hold estimates from the 5 models
fits = data.frame()
```

### Gold Standard (Fully Validated)

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

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' covariates missing
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
  imp_mod_vars = c(paste0("X", j), paste0("Xstar", j), Z) #### All include Xj, Xj*, Z
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
V_etsXstar1 = sample_ets(ets_dat = nhanes_data$Xstar1, ### Sample on X1*
                         phI = nrow(nhanes_data), ### Phase I sample size
                         phII = n) ### Phase II (validation study) sample size)

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' covariates missing
des_etsXstar1 = nhanes_data |> 
  bind_cols(data.frame(V = V_etsXstar1)) |> 
  mutate(X1 = ifelse(test = V == 1, yes = X1, no = NA), 
         X2 = ifelse(test = V == 1, yes = X2, no = NA), 
         X3 = ifelse(test = V == 1, yes = X3, no = NA), 
         X4 = ifelse(test = V == 1, yes = X4, no = NA), 
         X5 = ifelse(test = V == 1, yes = X5, no = NA))

## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Imputation model depends on the validation study design and number of imputations
  ### Which variables go into the imputation model 
  imp_mod_vars = c(paste0("X", j), paste0("Xstar", j), Z) #### All include Xj, Xj*, Z
  imp_mod_vars = unique(c(imp_mod_vars, "Xstar1")) #### ETS-X1 adds X1* 
  imp_mod_vars = c(imp_mod_vars, paste0("Y", j)) #### Multiple imputation adds Yj
  
  ### Impute and fit model 
  #### Multiple imputation
  mice_dat = mice(m = 75,
                  data = des_etsXstar1[, imp_mod_vars], 
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
V_etsPCstar1 = sample_pca(pca_dat = nhanes_data[, paste0("Xstar", 1:5)], ## sample on first PC of X1*, ..., X5*
                          phI = nrow(nhanes_data), ## Phase I sample size
                          phII = n) ## Phase II (validation study) sample size

## Create analytical dataset, incorporating validation indicators and making unvalidated patients' covariates missing
des_etsPCstar1 = nhanes_data |> 
  bind_cols(data.frame(V = V_etsPCstar1)) |> 
  mutate(X1 = ifelse(test = V == 1, yes = X1, no = NA), 
         X2 = ifelse(test = V == 1, yes = X2, no = NA), 
         X3 = ifelse(test = V == 1, yes = X3, no = NA), 
         X4 = ifelse(test = V == 1, yes = X4, no = NA), 
         X5 = ifelse(test = V == 1, yes = X5, no = NA))

## Since we sampled on PC1*, need to add it to the analytical dataset so 
### we can include it in the imputation models 
pc = princomp(nhanes_data[, paste0("Xstar", 1:5)], cor = TRUE) ### Fit PCA on X* variables (using correlation matrix)
des_etsPCstar1$pc1 = pc$scores[, 1] ### extract the first principal component

## Loop over j = 1, ..., 5 to impute and fit each model
for (j in 1:5) {
  ### Imputation model depends on the validation study design and number of imputations
  ### Which variables go into the imputation model 
  imp_mod_vars = c(paste0("X", j), paste0("Xstar", j), Z) #### All include Xj, Xj*, Z
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

### Full Models

### Coefficients on Error-Prone Covariates

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png"  />

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png"  />
