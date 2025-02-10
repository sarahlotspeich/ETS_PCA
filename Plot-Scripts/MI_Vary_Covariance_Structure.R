library(ggplot2)
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_dep_covar_equal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_dep_covar_unequal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/mi_indep_covar_11422.csv")
plot_dat = do.call(dplyr::bind_rows, 
                   lapply(X = paste0(p, list.files(p)), 
                          FUN = read.csv))

# Boxplot of coefficient estimates
plot_dat |> 
  dplyr::mutate(Covar = factor(x = Covar, 
                               levels = c("Independent Covariates (Zero Covariance)", 
                                          "Dependent Covariates (Equal Covariance)",
                                          "Dependent Covariates (Unequal Covariance)"), 
                               labels = c("Independent Covariates\n(Zero Covariance)", 
                                          "Dependent Covariates\n(Equal Covariance)",
                                          "Dependent Covariates\n(Unequal Covariance)"
                                          )), 
                Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
                Design = factor(x = Design, 
                                levels = c("SRS", "ETS (X1)", "ETS (PC1)"))) |> 
  ggplot(aes(x = Design, 
             y = est_beta1, 
             fill = Design)) + 
  geom_boxplot() + 
  geom_hline(aes(yintercept = Truth), 
             linetype = 2, 
             color = "red") + 
  facet_grid(cols = vars(Covar), 
             rows = vars(Model), 
             scales = "free") + 
  theme_minimal(base_size = 14) + 
  ggthemes::scale_fill_colorblind(guide = "none") + 
  xlab("Validation Study Design") + 
  ylab(latex2exp::TeX("Coefficient Estimate on X", 
                      bold = TRUE)) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        legend.title = element_text(face = "bold"), 
        legend.position = "top")
ggsave(filename = "~/Documents/ETS_PCA/Plots/MI_Vary_Covariance_Structure.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10)

# Barbell plot of relative efficiency
plot_dat2 = plot_dat |> 
  dplyr::mutate(Covar = factor(x = Covar, 
                               levels = c("Independent Covariates (Zero Covariance)", 
                                          "Dependent Covariates (Equal Covariance)",
                                          "Dependent Covariates (Unequal Covariance)"), 
                               labels = c("Independent Covariates\n(Zero Covariance)", 
                                          "Dependent Covariates\n(Equal Covariance)",
                                          "Dependent Covariates\n(Unequal Covariance)"
                               )), 
                Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
                Design = factor(x = Design, 
                                levels = c("SRS", "ETS (X1)", "ETS (PC1)"))) |> 
  dplyr::group_by(Model, Design, Covar) |> 
  dplyr::summarize(Efficiency = 1 / var(est_beta1)) 
barbell_dat = plot_dat2 |>
  dplyr::group_by(Model, Covar) |> 
  dplyr::summarize(minEff = min(Efficiency), 
                   maxEff = max(Efficiency))

plot_dat2 |> 
  ggplot(aes(x = Model, 
             y = log(Efficiency), 
             color = Design)) + 
  geom_segment(data = barbell_dat,
               aes(x = Model, y = log(minEff),
                   xend = Model, yend = log(maxEff)), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               linewidth = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(size = 4) + 
  facet_grid(cols = vars(Covar), 
             scales = "free") + 
  theme_minimal(base_size = 14) + 
  ggthemes::scale_color_colorblind() + 
  xlab(latex2exp::TeX("Model of $Y_j \\sim X_j$", 
                      bold = TRUE)) + 
  ylab(latex2exp::TeX("Empirical Efficiency of Coefficient Estimate on $X_j$ (Log-Transformed)", 
                      bold = TRUE)) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 6)) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white"), 
        legend.title = element_text(face = "bold"), 
        legend.position = "top") + 
  coord_flip() 
ggsave(filename = "~/Documents/ETS_PCA/Plots/MI_Vary_Covariance_Structure_Barbell.pdf", 
       device = "pdf", 
       width = 7, 
       height = 5)
