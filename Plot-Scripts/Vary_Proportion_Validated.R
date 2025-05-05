# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/val_prop_10_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/val_prop_15_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/val_prop_20_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/val_prop_25_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))

# Source script with plot-building functions
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Plot-Scripts/plot_functions.R")

# Define factors
plot_dat = plot_dat |> 
  dplyr::filter(ValProp != 0.2) |> 
  dplyr::mutate(ValProp = factor(x = ValProp, 
                                 levels = seq(from = 0.1, to = 0.25, by = 0.05), 
                                 labels = paste0(seq(from = 10, to = 25, by = 5), 
                                                 "% Validated"))) 

# Boxplot of coefficient estimates
plot_dat |> 
  boxplot_estimates(col_facet_var = ValProp)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Vary_Proportion_Validated.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
plot_dat |> 
  barbell_efficiency(group_by_var = ValProp)
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Vary_Proportion_Validated_Barbell.pdf", 
       device = "pdf", 
       width = 7, 
       height = 5)
