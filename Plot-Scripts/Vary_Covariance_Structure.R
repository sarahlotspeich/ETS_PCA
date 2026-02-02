# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/dep_covar_equal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/dep_covar_unequal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/indep_covar_11422.csv")
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
                               labels = c("Independent Exposures\n(Zero Covariance)", 
                                          "Dependent Exposures\n(Equal Covariance)",
                                          "Dependent Exposures\n(Unequal Covariance)"))) 

# Boxplot of coefficient estimates
covar_struct_plot = plot_dat |> 
  boxplot_estimates(col_facet_var = Covar)
## Save it 
ggsave(plot = covar_struct_plot, 
	   filename = "~/Documents/ETS_PCA/Plots/Vary_Covariance_Structure.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
covar_struct_barbell_plot = plot_dat |> 
  barbell_efficiency(group_by_var = Covar)
## Save it 
ggsave(plot = covar_struct_barbell_plot, 
	   filename = "~/Documents/ETS_PCA/Plots/Vary_Covariance_Structure_Barbell.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)

# Bar plot of sum of variances
covar_struct_bar_plot = plot_dat |> 
  bar_sum_var(group_by_var = Covar)
## Save it 
ggsave(plot = covar_struct_bar_plot, 
       filename = "~/Documents/ETS_PCA/Plots/Vary_Covariance_Structure_Bar.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)
