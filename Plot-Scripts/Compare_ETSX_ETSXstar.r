# Load packages
library(MASS) ## to simulate multivariate normal data
library(dplyr) ## to do data wrangling
library(latex2exp) ## for latex

# Source script to simulate data and fit models
devtools::source_url("https://github.com/sarahlotspeich/ETS_PCA/blob/main/Sim-Scripts/sim_data_fit.R?raw=TRUE")

# Build covariance matrix with equal dependence between X1,..., X5
equal_cov = matrix(data = 1 / 2, 
                   nrow = 5, 
                   ncol = 5)
diag(equal_cov) = 1

# Loop over different validation proportions
all = data.frame()
for (sigmaU in c(0.1, 0.25, 0.5, 1)) {
  set.seed(11422)
  temp = sim_data(n = 1000, 
           cov_X = equal_cov, 
           cov_U = diag(x = sigmaU, nrow = 5)) 
  temp$extreme_X1 = sample_ets(ets_dat = temp$X1, 
                               phI = 1000, 
                               phII = 100)
  temp$extreme_Xstar1 = sample_ets(ets_dat = temp$Xstar1, 
                                   phI = 1000, 
                                   phII = 100)
  all = temp |> 
    mutate(sigmaU = sigmaU) |> 
    bind_rows(all)
}

all = all |> 
  mutate(
    status = case_when(
      extreme_X1 == 1 & extreme_Xstar1 == 1 ~ "Both", 
      extreme_X1 == 1 & extreme_Xstar1 == 0 ~ "ETS-X Only", 
      extreme_X1 == 0 & extreme_Xstar1 == 1 ~ "ETS-X* Only",
      .default = "Neither"
    ), 
    status = factor(x = status, 
                    levels = rev(c("Both", "ETS-X Only", "ETS-X* Only", "Neither")), 
                    labels = rev(c("Both", TeX("ETS-$X_j$ Only"), 
                               TeX("ETS-$X_j^*$ Only"), "Neither")))
  ) |> 
  mutate(ErrorVar = factor(x = sigmaU, 
                           levels = c(0.1, 0.25, 0.5, 1), 
                           labels = paste0("Error Variance = ", 
                                           c(0.1, 0.25, 0.5, 1)))) |> 
  filter(ErrorVar != "Error Variance = 0.1") 

count_df <- all |>
  count(ErrorVar, status)

all |> 
  ggplot(aes(x = X1, y = Xstar1, color = status)) + 
  geom_point(size = 1) + 
  facet_wrap(~ErrorVar) + 
  theme_minimal(base_size = 14) + 
  ggthemes::scale_color_colorblind(name = "Validation Status by Design:", 
                                   labels = parse.labels) + 
  xlab(TeX("True Exposure $X_j$", bold = TRUE)) + 
  ylab(TeX("Error-Prone Exposure $X_j^*$", bold = TRUE)) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        #panel.border = element_rect(color = "black", fill = NA),
        legend.title = element_text(face = "bold"), 
        legend.position = "top", 
        legend.title.align = 0,
        panel.spacing = unit(1, "lines")) +   
  guides(color = guide_legend(override.aes = list(size = 3))) 
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSX_ETSXstar.pdf", 
       device = "pdf", 
       width = 8, 
       height = 5)
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSX_ETSXstar.png", 
       device = "png", 
       width = 8, 
       height = 5)
count_df
