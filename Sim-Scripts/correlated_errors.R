# Load packages
library(MASS) ## to simulate multivariate normal data
library(mice) ## to do imputation
library(dplyr) ## to do data wrangling
library(auditDesignR) ## to do sampling

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
  ## Get the error covariance matrix 
  if (sigmaU == 0.1) {
    error_cov = matrix(
      c(0.10,  0.02, -0.01,  0.03, -0.02,
        0.02,  0.10,  0.03, -0.01,  0.02,
        -0.01,  0.03,  0.10,  0.02, -0.03,
        0.03, -0.01,  0.02,  0.10,  0.01,
        -0.02,  0.02, -0.03,  0.01,  0.10), 
      nrow = 5, ncol = 5, byrow = TRUE)
  } else if (sigmaU == 0.25) {
    error_cov = matrix(c(
      0.25,  0.05, -0.03,  0.07, -0.04,
      0.05,  0.25,  0.08, -0.03,  0.05,
      -0.03,  0.08,  0.25,  0.05, -0.07,
      0.07, -0.03,  0.05,  0.25,  0.03,
      -0.04,  0.05, -0.07,  0.03,  0.25
    ), nrow = 5, ncol = 5, byrow = TRUE)
  } else if (sigmaU == 0.5) {
    error_cov = matrix(c(
      0.50,  0.10, -0.06,  0.14, -0.08,
      0.10,  0.50,  0.16, -0.06,  0.10,
      -0.06,  0.16,  0.50,  0.10, -0.14,
      0.14, -0.06,  0.10,  0.50,  0.06,
      -0.08,  0.10, -0.14,  0.06,  0.50
    ), nrow = 5, ncol = 5, byrow = TRUE)
  } else if (sigmaU == 1) {
    error_cov = matrix(c(
      1.00,  0.20, -0.12,  0.28, -0.16,
      0.20,  1.00,  0.32, -0.12,  0.20,
      -0.12,  0.32,  1.00,  0.20, -0.28,
      0.28, -0.12,  0.20,  1.00,  0.12,
      -0.16,  0.20, -0.28,  0.12,  1.00
    ), nrow = 5, ncol = 5, byrow = TRUE)
  }
  
  ## Setting 3A: Simulations with simple random sampling
  set.seed(sim_seed) ## be reproducible
  SRS_results = do.call(what = rbind,
                        args = sapply(X = 1:REPS,
                                      FUN = sim_data_fit, 
                                      simplify = FALSE,
                                      cov_X = equal_cov, 
                                      cov_U = error_cov))
  # Setting 3B: Simulations with extreme tail sampling on the first principal component
  set.seed(sim_seed) ## be reproducible
  ETS_PCA_results = do.call(what = rbind,
                            args = sapply(X = 1:REPS,
                                          FUN = sim_data_fit, 
                                          simplify = FALSE, 
                                          phII = "ETS_PCA",
                                          cov_X = equal_cov, 
                                          cov_U = error_cov))
  # Setting 3C: Simulations with extreme tail sampling on X1
  set.seed(sim_seed) ## be reproducible
  ETS_X1_results = do.call(what = rbind,
                           args = sapply(X = 1:REPS,
                                         FUN = sim_data_fit, 
                                         simplify = FALSE, 
                                         phII = "ETS_X1",
                                         cov_X = equal_cov, 
                                         cov_U = error_cov))
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
    write.csv(file = paste0("correlated_errors_", sigmaU * 10, "_", sim_seed, ".csv"), 
              row.names = FALSE)
}