# Load packages
library(MASS) ## to simulate multivariate normal data
library(mice) ## to do imputation
library(dplyr) ## to do data wrangling
library(auditDesignR) ## to do validation sampling

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
sigmaU = 1 
for (highest_var in c(0, 1, 5)) {
  ## Multiplier for heteroskedastic errors 
  if (highest_var == 1) {
    hetero_cov = diag(x = sigmaU * seq(1, 0.2, by = -0.2), nrow = 5)
  } else if (highest_var == 5) {
    hetero_cov = diag(x = sigmaU * seq(0.2, 1, by = 0.2), nrow = 5)
  } else if (highest_var == 0) {
    hetero_cov = diag(x = sigmaU, nrow = 5)
  }
  
  set.seed(sim_seed) ## be reproducible
  sett2_results = do.call(what = rbind,
                          args = sapply(X = 1:REPS,
                                        FUN = sim_data_pca, 
                                        simplify = FALSE, 
                                        cov_X = equal_cov, 
                                        cov_U = hetero_cov))
  print(paste0("highest var = ", highest_var, ": ", round(mean(sett2_results), 2)))
}