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
sigmaU = 0.5
set.seed(11422)
temp = sim_data(n = 1000, 
                cov_X = equal_cov, 
                cov_U = diag(x = sigmaU, nrow = 5)) 
## Extreme tail sampling on the first principal component
temp$extreme_PCstar1 = sample_pca(pca_dat = temp[, paste0("Xstar", 1:5)], ## sample on first PC of X1*, ..., X5*
                                  phI = 1000, ## Phase I sample size
                                  phII = 100) ## Phase II (validation study) sample size
## Extreme tail sampling on the exposure X1,...X5
temp$extreme_X1 = sample_ets(ets_dat = temp$X1, 
                             phI = 1000, 
                             phII = 100)
temp$extreme_X2 = sample_ets(ets_dat = temp$X2, 
                             phI = 1000, 
                             phII = 100)
temp$extreme_X3 = sample_ets(ets_dat = temp$X3, 
                             phI = 1000, 
                             phII = 100)
temp$extreme_X4 = sample_ets(ets_dat = temp$X4, 
                             phI = 1000, 
                             phII = 100)
temp$extreme_X5 = sample_ets(ets_dat = temp$X5, 
                             phI = 1000, 
                             phII = 100)
pc = princomp(temp[, paste0("Xstar", 1:5)], cor = TRUE)
temp$PCstar1 = pc$scores[, 1]

## Transform wide --> long 
temp_long = temp |> 
  select(PCstar1, X1:X5, starts_with("extreme")) |> 
  tidyr::pivot_longer(cols = c(X1:X5, extreme_X1:extreme_X5), 
                      names_to = c(".value", "j"),
                      names_pattern = "(X|extreme_X)([0-9]+)") |> 
  mutate(
    status = case_when(
      extreme_X == 1 & extreme_PCstar1 == 1 ~ "Both", 
      extreme_X == 1 & extreme_PCstar1 == 0 ~ "ETS-X Only", 
      extreme_X == 0 & extreme_PCstar1 == 1 ~ "ETS-PC* Only",
      .default = "Neither"
    ), 
    status = factor(x = status, 
                    levels = rev(c("Both", "ETS-X Only", "ETS-PC* Only", "Neither")), 
                    labels = rev(c("Both", TeX("ETS-$X_j$ Only"), 
                               TeX("ETS-$PC_1^*$ Only"), "Neither"))), 
    Model = factor(x = j, 
                   levels = 1:5, 
                   labels = c(TeX("Model 1: $Y_1 \\sim X_1$"), 
                              TeX("Model 2: $Y_2 \\sim X_2$"), 
                              TeX("Model 3: $Y_3 \\sim X_3$"), 
                              TeX("Model 4: $Y_4 \\sim X_4$"), 
                              TeX("Model 5: $Y_5 \\sim X_5$")))
  ) 

count_df <- temp_long |>
  count(Model, status)

temp_long |> 
  mutate(Model = as.character(Model)) |> 
  ggplot(aes(x = X, y = PCstar1, color = status)) + 
  geom_point(size = 1) + 
  facet_wrap(~Model, labeller = label_parsed) + 
  theme_minimal(base_size = 14) + 
  ggthemes::scale_color_colorblind(name = "Validation Status by Design:", 
                                   labels = parse.labels) + 
  xlab(TeX("True Exposures $X_1,...,X_5$", bold = TRUE)) + 
  ylab(TeX("First Principal Component $PC_1^*$ of Error-Prone Exposures $X_1^*,...,X_5^*$", bold = TRUE)) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        #panel.border = element_rect(color = "black", fill = NA),
        legend.title = element_text(face = "bold"), 
        legend.position = "top", 
        legend.title.align = 0,
        panel.spacing = unit(1, "lines")) +   
  guides(color = guide_legend(override.aes = list(size = 3))) 
## Save it 
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSX_ETSPCstar.pdf", 
       device = "pdf", 
       width = 8, 
       height = 8)
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSX_ETSPCstar.png", 
       device = "png", 
       width = 8, 
       height = 8)
count_df
