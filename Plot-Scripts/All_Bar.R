source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Covariance_Structure.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Error_Variance.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Proportion_Validated.R")
source("~/Documents/ETS_PCA/Plot-Scripts/sharedY.R")

# Combined barbell plot of relative efficiency for all settings
library(patchwork)
both = (covar_struct_bar_plot / 
          error_var_bar_plot / 
          val_prop_bar_plot / 
          bar_sharedY) + 
  plot_annotation(tag_levels = 'A')

## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/All_Bar.pdf", 
	   device = "pdf", 
	   width = 8, 
	   height = 8)
