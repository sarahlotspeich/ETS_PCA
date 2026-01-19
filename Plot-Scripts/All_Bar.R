source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Covariance_Structure.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Error_Variance.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Proportion_Validated.R")

# Barbell plot of relative efficiency
ggpubr::ggarrange(covar_struct_bar_plot, 
				  error_var_bar_plot, 
				  val_prop_bar_plot,
				  nrow = 3, 
				  ncol = 1, 
				  common.legend = TRUE, 
				  labels = c("A)", "B)", "C)"))
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/All_Bar.pdf", 
	   device = "pdf", 
	   width = 8, 
	   height = 8)
