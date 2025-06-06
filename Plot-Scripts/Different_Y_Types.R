# Load data 
# p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_errors_10_11422.csv", 
#       "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_errors_1_11422.csv", 
#       "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_errors_2.5_11422.csv", 
#       "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_errors_5_11422.csv")
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_10_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_15_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_20_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_25_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_50_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/diffYtypes_val_prop_95_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))

# Source script with plot-building functions
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Plot-Scripts/plot_functions.R")

# Define factors
plot_dat = plot_dat |> 
  dplyr::mutate(ErrorVar = factor(x = ErrorVar, 
                                  levels = c(0.01, 0.0625, 0.25, 1), 
                                  labels = paste0("Error Variance = ", 
                                                  c(0.01, 0.06, 0.25, 1))))

# Boxplot of coefficient estimates
plot_dat |> 
  boxplot_estimates(col_facet_var = ValProp)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Different_Y_Types.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
plot_dat |> 
  barbell_efficiency(group_by_var = ErrorVar)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Different_Y_Types_Barbell.pdf", 
       device = "pdf", 
       width = 7, 
       height = 5)
