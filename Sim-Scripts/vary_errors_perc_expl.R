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
  set.seed(sim_seed) ## be reproducible
  sett2_results = do.call(what = rbind,
                          args = sapply(X = 1:REPS,
                                        FUN = sim_data_pca, 
                                        simplify = FALSE, 
                                        cov_X = equal_cov, 
                                        cov_U = diag(x = sigmaU, nrow = 5)))
  print(paste0("sigmaU = ", sigmaU, ": ", round(mean(sett2_results), 2)))
}

# [1] "sigmaU = 0.1: 0.71"
# [1] "sigmaU = 0.25: 0.64"
# [1] "sigmaU = 0.5: 0.57"
# [1] "sigmaU = 1: 0.48"