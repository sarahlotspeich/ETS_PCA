# Load packages (can be installed from CRAN)
library(dplyr) ## for data wrangling
library(tidyr) ## for data pivoting
library(kableExtra) ## for LaTex table

# Load data 
p = c("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/dep_covar_equal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/dep_covar_unequal_11422.csv", 
      "https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/Sim-Data/indep_covar_11422.csv")
tab_dat = do.call(bind_rows, 
                  lapply(X = paste0(p, list.files(p)), 
                         FUN = read.csv)) |> 
  mutate(Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
         Model = factor(x = Model, 
                        levels = c("Y1 ~ X1", "Y2 ~ X2", "Y3 ~ X3", "Y4 ~ X4", "Y5 ~ X5"), 
                        labels = c("$Y_1 \\sim X_1$", 
                                   "$Y_2 \\sim X_2$",
                                   "$Y_3 \\sim X_3$", 
                                   "$Y_4 \\sim X_4$",
                                   "$Y_5 \\sim X_5$")),
         Design = factor(x = Design, 
                         levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                         labels = c("SRS", "ETS-$X_1^*$", "ETS-$PC_1^*$"))) |> 
  group_by(Covar, Model, Design) |> 
  summarize(
    abs_bias = paste0("$", round(mean(est_beta1 - Truth), 3), "$"),
    rel_bias = paste0("$(", round(abs(mean((est_beta1 - Truth) / Truth) * 100), 1), "\\%)$"),
    ese = sd(est_beta1)
  ) |> 
  mutate(bias = paste(abs_bias, rel_bias))
tab_wide = tab_dat |>
  dplyr::select(Covar, Model, Design, bias, ese) |>
  pivot_wider(
    names_from = Design,
    values_from = c(bias, ese),
    names_glue = "{Design}_{.value}"
  ) |>
  dplyr::select(Covar, Model, 
         starts_with("SRS"), 
         starts_with("ETS-$X"), 
         starts_with("ETS-$PC")) |> 
  mutate(
    re_ets_x  = paste0("$", round((SRS_ese^2) / (`ETS-$X_1^*$_ese`^2), 3), "$"), 
    re_ets_pc = paste0("$", round((SRS_ese^2) / (`ETS-$PC_1^*$_ese`^2), 3), "$"), 
    SRS_ese = paste0("$", round(SRS_ese, 3), "$"), 
    `ETS-$X_1^*$_ese` = paste0("$", round(`ETS-$X_1^*$_ese`, 3), "$"), 
    `ETS-$PC_1^*$_ese` = paste0("$", round(`ETS-$PC_1^*$_ese`, 3), "$"), 
  )

tab_wide |>
  dplyr::select(Covar, Model, 
                SRS_bias, SRS_ese, 
                `ETS-$X_1^*$_bias`, `ETS-$X_1^*$_ese`, re_ets_x, 
                `ETS-$PC_1^*$_bias`, `ETS-$PC_1^*$_ese`, re_ets_pc) |> 
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Covar", "Model", 
                    "Bias (%)", "ESE", 
                    "Bias (%)", "ESE", "RE",
                    "Bias (%)", "ESE", "RE")) |>
  add_header_above(c(" " = 2, 
                     "SRS" = 2, 
                     "$ETS\\text{-}X_1^*$" = 3, 
                     "$ETS\\text{-}PC_1^*$" = 3),
                   escape = FALSE) |> 
  row_spec(0, bold = TRUE)
