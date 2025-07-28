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

# Loop over different validation proportions
N = 1000
for (sigmaU in c(0.1, 0.25, 0.5, 1)) {
  ## Setting 3A: Simulations with simple random sampling
  set.seed(sim_seed) ## be reproducible
  SRS_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_fit, 
                                      simplify = FALSE,
                                      cov_X = equal_cov, 
                                      cov_U = diag(x = sigmaU, nrow = 5)))
  # Setting 3B: Simulations with extreme tail sampling on the first principal component
  set.seed(sim_seed) ## be reproducible
  ETS_PCA_results = do.call(what = rbind,
                            args = sapply(X = 1:REPS,
                                          FUN = sim_data_fit, 
                                          simplify = FALSE, 
                                          phII = "ETS_PCA",
                                          cov_X = equal_cov, 
                                          cov_U = diag(x = sigmaU, nrow = 5)))
  # Setting 3C: Simulations with extreme tail sampling on X1
  set.seed(sim_seed) ## be reproducible
  ETS_X1_results = do.call(what = rbind,
                           args = sapply(X = 1:REPS,
                                         FUN = sim_data_fit, 
                                         simplify = FALSE, 
                                         phII = "ETS_X1",
                                         cov_X = equal_cov, 
                                         cov_U = diag(x = sigmaU, nrow = 5)))
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
                  ErrorVar = sigmaU ^ 2) |> 
    left_join(data.frame(Model = paste0("X", 1:5), 
                                Truth = seq(0.5, 2.5, by = 0.5))) |> 
    write.csv(file = paste0("val_errors_", sigmaU * 10, "_", sim_seed, ".csv"), 
              row.names = FALSE)
}