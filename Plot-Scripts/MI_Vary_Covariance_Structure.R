# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_dep_covar_equal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_dep_covar_unequal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_indep_covar_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))

# Source script with plot-building functions
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Plot-Scripts/plot_functions.R")

# Define factors
plot_dat = plot_dat |> 
  dplyr::mutate(Covar = factor(x = Covar, 
                               levels = c("Independent Covariates (Zero Covariance)", 
                                          "Dependent Covariates (Equal Covariance)",
                                          "Dependent Covariates (Unequal Covariance)"), 
                               labels = c("Independent Covariates\n(Zero Covariance)", 
                                          "Dependent Covariates\n(Equal Covariance)",
                                          "Dependent Covariates\n(Unequal Covariance)"
                               ))) 

# Boxplot of coefficient estimates
plot_dat |> 
  boxplot_estimates(col_facet_var = Covar)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/MI_Vary_Covariance_Structure.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
plot_dat |> 
  barbell_efficiency(group_by_var = Covar)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/MI_Vary_Covariance_Structure_Barbell.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)
