source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Covariance_Structure.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Error_Variance.R")
source("~/Documents/ETS_PCA/Plot-Scripts/Vary_Proportion_Validated.R")
source("~/Documents/ETS_PCA/Plot-Scripts/sharedY.R")

library(patchwork)
both = (covar_struct_barbell_plot / 
          error_var_barbell_plot / 
          val_prop_barbell_plot / 
          barbell_sharedY) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/All_Barbell.pdf", 
	   device = "pdf", 
	   width = 8, 
	   height = 9)
