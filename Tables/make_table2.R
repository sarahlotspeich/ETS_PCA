# Load packages (can be installed from CRAN)
library(dplyr) ## for data wrangling
library(tidyr) ## for data pivoting
library(kableExtra) ## for LaTex table

# Read in fitted models 
fits = read.csv("https://raw.githubusercontent.com/sarahlotspeich/ETS_PCA/refs/heads/main/NHANES-Analysis/fits.csv")

# Write code to produce LaTex table code
tab = fits |> 
  mutate(lb = estimate - 1.96 * std.error, 
         ub = estimate + 1.96 * std.error, 
         #est_ci = paste0("$", round(estimate, 2), "$ ($", round(lb, 2), "$, $", round(ub, 2), "$)"), 
         est_ci = if_else(condition = grepl(pattern = "X", x = term), 
                          true = paste0("$", round(estimate, 2), 
                                        "$ ($", round(lb, 2), "$, $", round(ub, 2), "$)"), 
                          false = paste0("$", round(estimate, 1), 
                                         "$ ($", round(lb, 1), "$, $", round(ub, 1), "$)") 
         ),
         term_grouped = if_else(condition = grepl(pattern = "X", x = term), true = "Nutrient Intake", false = term), 
         design = factor(x = design, 
                         levels = c("GS", "SRS", "ETS (X1*)", "ETS (PC1*)"), 
                         labels = c("Gold Standard", "SRS", "ETS-$X_1^*$", "ETS-$PC_1^*$"))) |> 
  dplyr::select(model, design, term_grouped, est_ci) |> 
  pivot_wider(names_from = term_grouped, values_from = est_ci) |> 
  arrange(model) |> 
  dplyr::select(-model) |>
  t() |> 
  data.frame()

tab |> 
  select(1:4) |> 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  row_spec(row = 1, bold = TRUE) |>
  group_rows(group_label = "Race and Ethnicity", start_row = 6, end_row = 9, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Education Level", start_row = 10, end_row = 13, italic = TRUE, bold = FALSE)

tab |> 
  select(5:8) |> 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  row_spec(row = 1, bold = TRUE) |>
  group_rows(group_label = "Race and Ethnicity", start_row = 6, end_row = 9, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Education Level", start_row = 10, end_row = 13, italic = TRUE, bold = FALSE)

tab |> 
  select(9:12) |> 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  row_spec(row = 1, bold = TRUE) |>
  group_rows(group_label = "Race and Ethnicity", start_row = 6, end_row = 9, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Education Level", start_row = 10, end_row = 13, italic = TRUE, bold = FALSE)

tab |> 
  select(13:16) |> 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  row_spec(row = 1, bold = TRUE) |>
  group_rows(group_label = "Race and Ethnicity", start_row = 6, end_row = 9, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Education Level", start_row = 10, end_row = 13, italic = TRUE, bold = FALSE)

tab |> 
  select(17:20) |> 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcccccccccccc") |> 
  kable_styling() |> 
  row_spec(row = 1, bold = TRUE) |>
  group_rows(group_label = "Race and Ethnicity", start_row = 6, end_row = 9, italic = TRUE, bold = FALSE) |> 
  group_rows(group_label = "Education Level", start_row = 10, end_row = 13, italic = TRUE, bold = FALSE)
