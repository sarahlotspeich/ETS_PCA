# Load packages (can be installed from CRAN)
library(dplyr) ## for data wrangling
library(tidyr) ## for data pivoting
library(kableExtra) ## for LaTex table

# Read in fitted models 
fits = read.csv("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/NHANES-Analysis/fits.csv")

# Write code to produce LaTex table code
fits |> 
  mutate(lb = estimate - 1.96 * std.error, 
         ub = estimate + 1.96 * std.error, 
         est_ci = paste0("$", round(estimate, 2), "$ ($", round(lb, 2), "$, $", round(ub, 2), "$)"), 
         term_grouped = if_else(condition = grepl(pattern = "X", x = term), true = "Nutrient Intake", false = term), 
         design = factor(x = design, 
                         levels = c("GS", "SRS", "ETS (X1*)", "ETS (PC1*)"), 
                         labels = c("Gold Standard", "SRS", "ETS-$X_1^*$", "ETS-$PC_1^*$"))) |> 
  dplyr::select(model, design, term_grouped, est_ci) |> 
  pivot_wider(names_from = term_grouped, values_from = est_ci) |> 
  arrange(model) |> 
  dplyr::select(-model) |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  add_header_above(header = c(" " = 5, "Race and Ethnicity" = 4, "Education Level" = 4), bold = TRUE) |> 
  row_spec(row = 0, bold = TRUE) |>
  group_rows(group_label = "Model 1", start_row = 1, end_row = 4, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Model 2", start_row = 5, end_row = 8, italic = TRUE, bold = FALSE) |>
  group_rows(group_label = "Model 3", start_row = 9, end_row = 12, italic = TRUE, bold = FALSE) |>
  group_rows(group_label = "Model 4", start_row = 13, end_row = 16, italic = TRUE, bold = FALSE) |>
  group_rows(group_label = "Model 5", start_row = 17, end_row = 20, italic = TRUE, bold = FALSE)