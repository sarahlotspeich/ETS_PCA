# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/val_errors_10_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/hetero_errors_1_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/hetero_errors_5_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))

# Source script with plot-building functions
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Plot-Scripts/plot_functions.R")

# Define factors
plot_dat = plot_dat |> 
  dplyr::mutate(Exposure_Highest_Error = if_else(condition = is.na(Exposure_Highest_Error), 
                                                 true = 0, 
                                                 false = Exposure_Highest_Error), 
                Exposure_Highest_Error = factor(x = Exposure_Highest_Error, 
                                                levels = c(1, 5, 0), 
                                                labels = c(latex2exp::TeX("Most Severe for $X_1^*$"), 
                                                           latex2exp::TeX("Most Severe for $X_5^*$"), 
                                                           latex2exp::TeX("Same Severity for $X_1^*,...,X_5^*$"))))

# Boxplot of coefficient estimates
error_var_plot = plot_dat |> 
  boxplot_estimates(col_facet_var = Exposure_Highest_Error)  + 
  facet_grid(cols = vars(Exposure_Highest_Error),
             scales = "free", 
             labeller = labeller(Exposure_Highest_Error = label_parsed, 
                                 Model = label_parsed))
## Save it 
ggsave(plot = error_var_plot, 
	   filename = "~/Documents/ETS_PCA/Plots/Heteroscedastic_Error_Variance.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
error_var_barbell_plot = plot_dat |> 
  barbell_efficiency(group_by_var = Exposure_Highest_Error)  + 
  facet_grid(cols = vars(Exposure_Highest_Error),
             scales = "free", 
             labeller = labeller(Exposure_Highest_Error = label_parsed, 
                                 Model = label_parsed))
## Save it 
ggsave(plot = error_var_barbell_plot, 
	   filename = "~/Documents/ETS_PCA/Plots/Heteroscedastic_Error_Variance_Barbell.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)

# Bar plot of sum of variances
error_var_bar_plot = plot_dat |> 
  bar_sum_var(group_by_var = Exposure_Highest_Error)  + 
  facet_grid(cols = vars(Exposure_Highest_Error),
             scales = "free", 
             labeller = labeller(Exposure_Highest_Error = label_parsed, 
                                 Model = label_parsed))
## Save it 
ggsave(plot = error_var_bar_plot, 
       filename = "~/Documents/ETS_PCA/Plots/Heteroscedastic_Error_Variance_Bar.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)

library(patchwork)
both = (error_var_bar_plot / error_var_barbell_plot) + 
  plot_annotation(tag_levels = 'A')
## Save it 
ggsave(plot = both, 
       filename = "~/Documents/ETS_PCA/Plots/Heteroscedastic_Error_Variance_Bar_Barbell.pdf", 
       device = "pdf", 
       width = 9, 
       height = 8)

# Print sum of variances (to reference in text)
plot_dat |> 
  group_by(Model, Design, Exposure_Highest_Error) |> 
  mutate(var_beta = var(est_beta1)) |> 
  group_by(Design, Exposure_Highest_Error) |> 
  summarize(sum_var_beta = sum(var_beta)) |> 
  arrange(Exposure_Highest_Error)
