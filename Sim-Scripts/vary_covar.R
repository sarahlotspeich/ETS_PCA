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
## Simulations with five independent covariates X1, X2, X3, X4, X5
## Setting 1A: Simulations with simple random sampling
set.seed(sim_seed) ## For reproducibility
SRS_results = do.call(what = rbind,
                      args = sapply(X = 1:REPS,
                                    FUN = sim_data_fit, 
                                    simplify = FALSE))
## Setting 1B: Simulations with extreme tail sampling on the first principal component
set.seed(sim_seed) ## For reproducibility
ETS_PCA_results = do.call(what = rbind,
                          args = sapply(X = 1:REPS,
                                        FUN = sim_data_fit, 
                                        simplify = FALSE, 
                                        phII = "ETS_PCA"))
## Setting 1C: Simulations with extreme tail sampling on X1
set.seed(sim_seed) ## For reproducibility
ETS_X1_results = do.call(what = rbind,
                         args = sapply(X = 1:REPS,
                                       FUN = sim_data_fit, 
                                       simplify = FALSE, 
                                       phII = "ETS_X1"))
# Combine and save results
SRS_results |> 
  dplyr::mutate(Design = "SRS") |> 
  dplyr::bind_rows(
    ETS_PCA_results |> 
      dplyr::mutate(Design = "ETS (PC1)")
  ) |> 
  dplyr::bind_rows(
    ETS_X1_results |> 
      dplyr::mutate(Design = "ETS (X1)")
  ) |> 
  dplyr::mutate(Design = factor(x = Design, 
                                levels = c("SRS", "ETS (X1)", "ETS (PC1)")), 
                Covar = "Independent Covariates (Zero Covariance)") |> 
  dplyr::left_join(data.frame(Model = paste0("X", 1:5), 
                              Truth = beta1)) |> 
  write.csv(file = paste0("indep_covar_", sim_seed, ".csv"), 
            row.names = FALSE)

# Setting 2: Dependent Covariates (Equal Covariances)
## Simulations with five independent covariates X1, X2, X3, X4, X5
## Specify equal covariance between all covariates
equal_cov = matrix(data = 1 / 2, 
                   nrow = 5, 
                   ncol = 5)
diag(equal_cov) = 1
## Setting 2A: Simulations with simple random sampling
set.seed(sim_seed) ## For reproducibility
SRS_results = do.call(what = rbind,
                      args = sapply(X = 1:REPS,
                                    FUN = sim_data_fit, 
                                    simplify = FALSE,
                                    cov_X = equal_cov))

## Setting 2B: Simulations with extreme tail sampling on the first principal component
set.seed(sim_seed) ## For reproducibility
ETS_PCA_results = do.call(what = rbind,
                          args = sapply(X = 1:REPS,
                                        FUN = sim_data_fit, 
                                        simplify = FALSE, 
                                        phII = "ETS_PCA",
                                        cov_X = equal_cov))
## Setting 2C: Simulations with extreme tail sampling on X1
set.seed(sim_seed) ## For reproducibility
ETS_X1_results = do.call(what = rbind,
                         args = sapply(X = 1:REPS,
                                       FUN = sim_data_fit, 
                                       simplify = FALSE, 
                                       phII = "ETS_X1",
                                       cov_X = equal_cov))
# Combine and save results
SRS_results |> 
  dplyr::mutate(Design = "SRS") |> 
  dplyr::bind_rows(
    ETS_PCA_results |> 
      dplyr::mutate(Design = "ETS (PC1)")
  ) |> 
  dplyr::bind_rows(
    ETS_X1_results |> 
      dplyr::mutate(Design = "ETS (X1)")
  ) |> 
  dplyr::mutate(Design = factor(x = Design, 
                                levels = c("SRS", "ETS (X1)", "ETS (PC1)")), 
                Covar = "Dependent Covariates (Equal Covariance)") |> 
  dplyr::left_join(data.frame(Model = paste0("X", 1:5), 
                              Truth = beta1)) |> 
  write.csv(file = paste0("dep_covar_equal_", sim_seed, ".csv"), 
            row.names = FALSE)

# Setting 3: Dependent Covariates (Unequal Covariances)
## Simulations with five independent covariates X1, X2, X3, X4, X5
## Specify equal covariance between all covariates
unequal_cov = matrix(data = 1 / 2, 
                     nrow = 5, 
                     ncol = 5)
diag(unequal_cov) = 1
unequal_cov[upper.tri(unequal_cov)] = seq(0.05, 0.5, length = 10)
unequal_cov[lower.tri(unequal_cov)] = t(unequal_cov)[lower.tri(unequal_cov)]
## Setting 3A: Simulations with simple random sampling
set.seed(sim_seed) ## For reproducibility
SRS_results = do.call(what = rbind,
                      args = sapply(X = 1:REPS,
                                    FUN = sim_data_fit, 
                                    simplify = FALSE,
                                    cov_X = unequal_cov))
# Setting 3B: Simulations with extreme tail sampling on the first principal component
set.seed(sim_seed) ## For reproducibility
ETS_PCA_results = do.call(what = rbind,
                          args = sapply(X = 1:REPS,
                                        FUN = sim_data_fit, 
                                        simplify = FALSE, 
                                        phII = "ETS_PCA",
                                        cov_X = unequal_cov))
# Setting 3C: Simulations with extreme tail sampling on X1
set.seed(sim_seed) ## For reproducibility
ETS_X1_results = do.call(what = rbind,
                         args = sapply(X = 1:REPS,
                                       FUN = sim_data_fit, 
                                       simplify = FALSE, 
                                       phII = "ETS_X1",
                                       cov_X = unequal_cov))
# Combine and save results
SRS_results |> 
  dplyr::mutate(Design = "SRS") |> 
  dplyr::bind_rows(
    ETS_PCA_results |> 
      dplyr::mutate(Design = "ETS (PC1)")
  ) |> 
  dplyr::bind_rows(
    ETS_X1_results |> 
      dplyr::mutate(Design = "ETS (X1)")
  ) |> 
  dplyr::mutate(Design = factor(x = Design, 
                                levels = c("SRS", "ETS (X1)", "ETS (PC1)")), 
                Covar = "Dependent Covariates (Unequal Covariance)") |> 
  dplyr::left_join(data.frame(Model = paste0("X", 1:5), 
                              Truth = beta1)) |> 
  write.csv(file = paste0("dep_covar_unequal_", sim_seed, ".csv"), 
            row.names = FALSE)