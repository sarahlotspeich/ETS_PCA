# Set model parameters 
beta0 = 0:4 ## intercept for Yj|Xj,Z
# beta1 = seq(0.5, 2.5, by = 0.5) ## slope on Xj for Yj|Xj,Z
# beta1[2] = 0.5 ## force beta1 in Y2 ~ X2 to be same as Y1 ~ X1
beta2 = seq(0.1, 0.5, by = 0.1) ## slope on Z for Yj|Xj,Z

# Function to simulate data 
sim_data = function(N = 1000, n = 100, beta1 = seq(0.5, 2.5, by = 0.5), cov_X = diag(x = 1, nrow = 5), cov_U = diag(x = 1, nrow = 5), phII = "SRS", same_Y_type = TRUE, shared_Y = FALSE) {
  ## Simulate error-free binary covariate Z
  Z = rbinom(n = N, 
             size = 1,
             prob = 0.3) 
  ## Simulate error-free continuous covariates Xj|Z
  X_Z0 = mvrnorm(n = N, 
                 mu = rep(0, 5), ## mean vector
                 Sigma = cov_X) ## variance-covariance matrix
  X_Z1 = mvrnorm(n = N, 
                 mu = rep(1, 5), ## mean vector
                 Sigma = cov_X) ## variance-covariance matrix
  X = X_Z0
  X[which(Z == 1), ] = X_Z1[which(Z == 1), ]
  colnames(X) = paste0("X", 1:5)
  ## Simulate error-prone continuous covariates Xj*|Xj
  ### Random errors 
  U = mvrnorm(n = N, 
              mu = rep(0, 5), ## mean vector
              Sigma = cov_U) ## variance-covariance matrix
  ### Added to create error-prone covariates 
  Xstar = X + U 
  colnames(Xstar) = paste0("Xstar", 1:5)
  ## Simulate random errors for outcomes
  eps = mvrnorm(n = N, 
                mu = rep(0, 5), ## error mean
                Sigma = diag(x = 1, nrow = 5) ## error variance
  )
  ## Simulate continuous outcomes for each continuous covariate Yj|Xj,Z
  if (same_Y_type & !shared_Y) {
    Y = matrix(data = beta0, nrow = N, ncol = 5, byrow = TRUE) + 
      matrix(data = beta1, nrow = N, ncol = 5, byrow = TRUE) * data.matrix(X) + 
      matrix(data = beta2, nrow = N, ncol = 5, byrow = TRUE) * matrix(Z, nrow = N, ncol = 5, byrow = FALSE) + 
      eps
    colnames(Y) = paste0("Y", 1:5)
  } else if (shared_Y) {
    Y = beta0[1] + 
      data.matrix(X) %*% matrix(data = beta1, ncol = 1) + 
      beta2[1] * Z + eps[, 1]
  } else if (!same_Y_type) {
    ## First two are continuous
    Y1 = beta0[1] + beta1[1] * data.matrix(X)[, 1] + beta2[1] * Z + eps[, 1] 
    Y2 = beta0[2] + beta1[2] * data.matrix(X)[, 2] + beta2[2] * Z + eps[, 2] 
    
    ## Next one is count
    Y3 = rpois(n = N, 
               lambda = exp(beta0[3] + beta1[3] * data.matrix(X)[, 3] + beta2[3] * Z))
    
    ## Last two are binary 
    Y4 = rbinom(n = N, 
                size = 1, 
                prob = 1 / (1 + exp(- (beta0[4] + beta1[4] * data.matrix(X)[, 4] + beta2[4] * Z))))
    Y5 = rbinom(n = N, 
                size = 1, 
                prob = 1 / (1 + exp(- (beta0[5] + beta1[5] * data.matrix(X)[, 5] + beta2[5] * Z))))
    
    ## Put them all together 
    Y = data.matrix(cbind(Y1, Y2, Y3, Y4, Y5))
  }
  ## Simulate validation indicator V
  if (phII == "SRS") {
    ## Simple random sampling
    V = sample_srs(phI = N, ## Phase I sample size
                   phII = n) ## Phase II (validation study) sample size)
    
    ## Put data together
    dat = data.frame(Y, X, Z, Xstar, V)
  } else if (phII == "ETS_PCA") {
    ## Extreme tail sampling on the first principal component
    V = sample_pca(pca_dat = Xstar, ## sample on first PC of X1*, ..., X5*
                   phI = N, ## Phase I sample size
                   phII = n) ## Phase II (validation study) sample size
    
    ## Put data together
    dat = data.frame(Y, X, Z, Xstar, V, pc1)
  } else if (phII == "ETS_X1") {
    ## Extreme tail sampling on X1*
    V = sample_ets(ets_dat = Xstar[, 1], ## sample on X1*
                   phI = N, ## Phase I sample size
                   phII = n) ## Phase II (validation study) sample size
    
    ## Put data together
    dat = data.frame(Y, X, Z, Xstar, V)
  }
  ## make X variables missing if V = 0 (unvalidated)
  dat[which(dat$V == 0), paste0("X", 1:5)] = NA 
  ## Return
  return(dat)
}

# Function to simulate data and then fit single imputation model
sim_data_fit = function(sim_id, N = 1000, n = 100, beta1 = seq(0.5, 2.5, by = 0.5), cov_X = diag(x = 1, nrow = 5), cov_U = diag(x = 1, nrow = 5), phII = "SRS", same_Y_type = TRUE, shared_Y = FALSE, m = 1) {
  ## Simulate data 
  dat = sim_data(N = N, 
                 n = n, 
                 beta1 = beta1, 
                 cov_X = cov_X, 
                 cov_U = cov_U, 
                 phII = phII, 
                 same_Y_type = same_Y_type, 
                 shared_Y = shared_Y) 
  ## Initialize dataframe to hold estimates from the 5 models
  fits = data.frame(id = sim_id, 
                    Model = paste0("X", 1:5), 
                    est_beta0 = NA, 
                    est_beta1 = NA, 
                    est_beta2 = NA)
  ## Loop over j = 1, ..., 5 to impute and fit each model
  for (j in 1:5) {
    ### Imputation model depends on the validation study design and number of imputations
    dat$Ximp = dat[, paste0("X", j)] #### Initialize with Xj 
    
    ### Which variables go into the imputation model 
    imp_mod_vars = c(paste0("X", j), paste0("Xstar", j), "Z") #### All include Xj, Xj*, Z
    if (phII == "ETS_PCA") imp_mod_vars = c(imp_mod_vars, "pc") #### ETS-PCA adds pc 
    if (phII == "ETS_X1") imp_mod_vars = unique(c(imp_mod_vars, "Xstar1")) #### ETS-X1 adds X1* 
    if (m > 1) imp_mod_vars = c(imp_mod_vars, paste0("Y", j)) #### Multiple imputation adds Yj
    
    ### Which type of outcome model to fit 
    family_Yj = "gaussian" 
    if (!same_Y_type & j > 2) {
      if (j == 3) family_Yj = "poisson"
      else family_Yj = "binomial"
    }
    
    ### Impute and fit model 
    if (m == 1) { 
      #### Single imputation
      mice_dat = mice(m = 1, 
                      data = dat[, imp_mod_vars], 
                      method = "norm.predict", 
                      printFlag = FALSE)
      #### Replace missing Xj with imputed values  
      dat$Ximp[is.na(dat[, paste0("X", j)])] = as.vector(unlist(mice_dat$imp[paste0("X", j)]))
      #### Fit analysis model to the imputed data 
      after_imp_fit = glm(formula = as.formula(paste0("Y", j, "~", "Ximp+Z")), 
                          data = dat, 
                          family = family_Yj)
      #### Save coefficient estimates
      fits[j, -c(1:2)] = after_imp_fit$coefficients
    } else { #### Multiple imputation
      #### Multiple imputation
      mice_dat = mice(m = m,
                      data = dat[, imp_mod_vars], 
                      method = "norm",
                      printFlag = FALSE)
      #### Fit analysis model to the imputed data (separately) 
      after_imp_fit = with(data = mice_dat, 
                           expr = glm(formula = as.formula(paste0("Y", j, "~", "X", j, "+Z")), 
                                      family = family_Yj))
      #### Pool the analysis models from each imputation
      pool_imp_fit = pool(after_imp_fit)[, 3]
      #### Save coefficient estimates
      fits[j, -c(1:2)] = pool_imp_fit$estimate
    }
  }
  return(fits)
}

# Function to simulate data and then fit single imputation model
sim_data_pca = function(sim_id, N = 1000, n = 100, beta1 = seq(0.5, 2.5, by = 0.5), cov_X = diag(x = 1, nrow = 5), cov_U = diag(x = 1, nrow = 5), phII = "SRS",  same_Y_type = TRUE, shared_Y = FALSE) {
  ## Simulate data 
  dat = sim_data(N = N, 
                 n = n, 
                 beta1 = beta1, 
                 cov_X = cov_X, 
                 cov_U = cov_U, 
                 phII = phII, 
                 same_Y_type = same_Y_type, 
                 shared_Y = shared_Y) 
  
  ## Calculate PCA 
  pca_dat = princomp(dat[, paste0("Xstar", 1:5)], cor = TRUE) 
  return(as.numeric(pca_dat$sdev[1]^2 / sum(pca_dat$sdev)))
}