# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/sharedY_betaSett_1_11422.csv",
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/sharedY_betaSett_2_11422.csv",
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/sharedY_betaSett_3_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))
plot_dat$beta_sett = rep(x = 1:3, each = 15000)

# Source script with plot-building functions
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Plot-Scripts/plot_functions.R")

# Define factors
plot_dat = plot_dat |> 
  dplyr::mutate(beta_sett = factor(x = beta_sett, 
                                   levels = c(1, 2, 3), 
                                   labels = c(TeX("Only $X_1$"), 
                                              TeX("Only $X_2$"), 
                                              TeX("All $X_1,...,X_5$"))))

# Boxplot of coefficient estimates
plot_dat |> 
  boxplot_estimates(col_facet_var = beta_sett, sharedY = TRUE) + 
  facet_grid(cols = vars(beta_sett),
             rows = vars(Model), 
             scales = "free", 
             labeller = labeller(beta_sett = label_parsed, 
                                 Model = label_parsed))
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/SharedY.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
barbell = plot_dat |> 
  barbell_efficiency(group_by_var = beta_sett, sharedY = TRUE) + 
  facet_grid(cols = vars(beta_sett), 
             scales = "free", 
             labeller = labeller(beta_sett = label_parsed))
## Save it 
ggsave(plot = barbell, 
       filename = "~/Documents/ETS_PCA/Plots/SharedY_Barbell.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)

# Bar plot of sum of variances
bar = plot_dat |> 
  bar_sum_var(group_by_var = beta_sett, sharedY = TRUE)
## Save it 
ggsave(plot = bar, 
       filename = "~/Documents/ETS_PCA/Plots/sharedY_Bar.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)

library(patchwork)
both = (bar / barbell) + 
  plot_annotation(tag_levels = 'A')
## Save it 
ggsave(plot = both, 
       filename = "~/Documents/ETS_PCA/Plots/sharedY_Bar_Barbell.pdf", 
       device = "pdf", 
       width = 8, 
       height = 8)
