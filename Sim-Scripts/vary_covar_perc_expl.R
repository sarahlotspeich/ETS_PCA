# Load packages
library(MASS) ## to simulate multivariate normal data
library(mice) ## to do imputation

# Source script to simulate data and fit models
devtools::source_url("https://github.com/sarahlotspeich/ETS_PCA/blob/main/Sim-Scripts/sim_data_fit.R?raw=TRUE")

# Choose seed to be used as starting place for each simulation setting
sim_seed = 11422 

# Set number of simulated replicates per setting 
REPS = 1000

# Setting 1: Independent Covariates
set.seed(sim_seed) ## For reproducibility
sett1_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_pca, 
                                      simplify = FALSE))
mean(sett1_results) ## Avg 28% variability explained by PC1*

# Setting 2: Dependent Covariates (Equal Covariances)
## Simulations with five independent covariates X1, X2, X3, X4, X5
## Specify equal covariance between all covariates
equal_cov = matrix(data = 1 / 2, 
                   nrow = 5, 
                   ncol = 5)
diag(equal_cov) = 1
set.seed(sim_seed) ## For reproducibility
sett2_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_pca, 
                                      simplify = FALSE, 
                                      cov_X = equal_cov))
mean(sett2_results) ## Avg 48% variability explained by PC1*

# Setting 3: Dependent Covariates (Unequal Covariances)
## Simulations with five independent covariates X1, X2, X3, X4, X5
## Specify equal covariance between all covariates
unequal_cov = matrix(data = 1 / 2, 
                     nrow = 5, 
                     ncol = 5)
diag(unequal_cov) = 1
unequal_cov[upper.tri(unequal_cov)] = seq(0.05, 0.5, length = 10)
unequal_cov[lower.tri(unequal_cov)] = t(unequal_cov)[lower.tri(unequal_cov)]
set.seed(sim_seed) ## For reproducibility
sett3_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_pca, 
                                      simplify = FALSE, 
                                      cov_X = unequal_cov))
mean(sett3_results) ## Avg 39% variability explained by PC1*