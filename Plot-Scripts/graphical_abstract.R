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
temp = sim_data(N = 10000, 
                n = 1000, 
                cov_X = equal_cov, 
                cov_U = diag(x = sigmaU, nrow = 5), ) 
## Extreme tail sampling on the first principal component
temp$extreme_PCstar1 = sample_pca(pca_dat = temp[, paste0("Xstar", 1:5)], ## sample on first PC of X1*, ..., X5*
                                  phI = 10000, ## Phase I sample size
                                  phII = 1000) ## Phase II (validation study) sample size
## Extreme tail sampling on the exposure X1,...X5
temp$extreme_X1star = sample_ets(ets_dat = temp$Xstar1, 
                                 phI = 10000, 
                                 phII = 1000)

pc = princomp(temp[, paste0("Xstar", 1:5)], cor = TRUE)
temp$PCstar1 = pc$scores[, 1]

## Transform wide --> long 
temp_long = temp |> 
  dplyr::select(PCstar1, X1:X5, starts_with("extreme")) |> 
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

plot_hist_pc1star = temp_long |> 
  mutate(status2 = if_else(condition = extreme_PCstar1 == 0, 
                           true = "Unvalidated", 
                           false = "Validated")) |> 
  ggplot(aes(x = PCstar1, fill = status2, alpha = status2)) + 
  geom_histogram() + 
  theme_minimal(base_size = 14) + 
  #facet_wrap(~var) + 
  scale_fill_manual(values = c("#56b4e9", "#009e73"), 
                    name = "Validation Status:", 
                    labels = parse.labels) + 
  scale_alpha_manual(values = c(0.7, 1), 
                     name = "Validation Status:", 
                     labels = parse.labels, 
                     guide = "none") + 
  xlab("First Principal Component of all Covariates Value (X1,...,Xp)") + 
  ylab("Number of Patients") + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), 
        legend.title.align = 0,
        panel.spacing = unit(1, "lines"), 
        legend.position = "inside",
        legend.position.inside = c(0.2,0.8), 
        legend.background = element_rect(fill = "white", colour = "black")) 
## Save it 
plot_hist_pc1star
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSPCstar_Histogram.png", 
       device = "png", 
       width = 8, 
       height = 5)

plot_hist_X = temp_long |> 
  filter(!grepl(pattern = "5", x = Model)) |> #, 
         #!grepl(pattern = "4", x = Model)) |> 
  mutate(status2 = if_else(condition = extreme_PCstar1 == 0, 
                           true = "Unvalidated", 
                           false = "Validated")) |> 
  ggplot(aes(x = X, fill = status2, alpha = status2)) + 
  geom_histogram() + 
  theme_minimal(base_size = 14) + 
  facet_wrap(~Model, labeller = label_parsed) + 
  scale_fill_manual(values = c("#56b4e9", "#009e73"), 
                    name = "Validation Status:", 
                    labels = parse.labels, 
                    guide = "none") + 
  scale_alpha_manual(values = c(0.7, 1), 
                     name = "Validation Status:", 
                     labels = parse.labels, 
                     guide = "none") + 
  xlab("Covariates Value (X1,...,X4)") + 
  ylab("Number of Patients") + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), 
        legend.title.align = 0,
        panel.spacing = unit(1, "lines"), 
        legend.position = "inside",
        legend.position.inside = c(0.9,0.35), 
        legend.background = element_rect(fill = "white", colour = "black")) 
## Save it 
plot_hist_X
ggsave(filename = "~/Documents/ETS_PCA/Plots/Compare_ETSPCstar_X_Histogram.png", 
       device = "png", 
       width = 7, 
       height = 7)
