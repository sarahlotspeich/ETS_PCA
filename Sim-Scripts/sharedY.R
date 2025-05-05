# Load packages
library(MASS) ## to simulate multivariate normal data
library(mice) ## to do imputation
library(dplyr) ## to do data wrangling

# Source script to simulate data and fit models
devtools::source_url("https://github.com/sarahlotspeich/ETS_PCA/blob/main/Sim-Scripts/sim_data_fit.R?raw=TRUE")

# Choose seed to be used for each simulation setting
sim_seed = 11422 

# Set number of simulated replicates per setting 
REPS = 1000

# Build covariance matrix with equal dependence between X1,..., X5
equal_cov = matrix(data = 1 / 2, 
                   nrow = 5, 
                   ncol = 5)
diag(equal_cov) = 1

# Loop over different slope settings
N = 1000
pv = 0.1 ## 10% validated
slope_sett = matrix(data = 0, nrow = 5, ncol = 3)
slope_sett[1, 1] = 1 ## only beta1 (on X1) is nonzero
slope_sett[2, 2] = 1 ## only beta2 (on X2) is nonzero
slope_sett[, 3] = 0.2 ## all betas (all Xs) are nonzero
for (r in 1:ncol(slope_sett)) {
  ## Setting 3A: Simulations with simple random sampling
  set.seed(sim_seed) ## For reproducibility
  SRS_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_fit, 
                                      simplify = FALSE,
                                      n = 1000 * pv,
                                      cov_X = equal_cov, 
                                      beta1 = slope_sett[, r]))
  # Setting 3B: Simulations with extreme tail sampling on the first principal component
  set.seed(sim_seed) ## For reproducibility
  ETS_PCA_results = do.call(what = rbind,
                            args = sapply(X = 1:REPS,
                                          FUN = sim_data_fit, 
                                          simplify = FALSE, 
                                          phII = "ETS_PCA",
                                          n = 1000 * pv,
                                          cov_X = equal_cov, 
                                          beta1 = slope_sett[, r]))
  # Setting 3C: Simulations with extreme tail sampling on X1
  set.seed(sim_seed) ## For reproducibility
  ETS_X1_results = do.call(what = rbind,
                           args = sapply(X = 1:REPS,
                                         FUN = sim_data_fit, 
                                         simplify = FALSE, 
                                         phII = "ETS_X1",
                                         n = 1000 * pv,
                                         cov_X = equal_cov, 
                                         beta1 = slope_sett[, r]))
  # Combine and save results
  SRS_results |> 
    mutate(Design = "SRS") |> 
    bind_rows(
      ETS_PCA_results |> 
        mutate(Design = "ETS (PC1)")
    ) |> 
    bind_rows(
      ETS_X1_results |> 
        mutate(Design = "ETS (X1)")
    ) |> 
    mutate(Design = factor(x = Design, 
                           levels = c("SRS", "ETS (X1)", "ETS (PC1)")), 
           Covar = "Dependent Covariates (Equal Covariance)", 
           ValProp = pv) |> 
    left_join(data.frame(Model = paste0("X", 1:5), 
                         Truth = slope_sett[, r])) |> 
    write.csv(file = paste0("sharedY_betaSett_", r, "_", sim_seed, ".csv"), 
              row.names = FALSE)
}

