# Load libraries 
library(ggplot2) ## for plots 
library(latex2exp) ## for LaTex
library(ggthemes) ## for colors 
library(dplyr) ## for data wrangling

# Function to parse LaTex labels 
parse.labels <- function(x) parse(text = x)

# Function to create boxplot of coefficient estimates
boxplot_estimates = function(data, col_facet_var, sharedY = FALSE) {
  ## Define factors
  if (sharedY) {
    data = data |> 
      mutate(Model = paste0("Y ~ ", Model), 
             Model = factor(x = Model, 
                            levels = c("Y ~ X1", "Y ~ X2", "Y ~ X3", "Y ~ X4", "Y ~ X5"), 
                            labels = c(TeX("$Y \\sim X_1$"), 
                                       TeX("$Y \\sim X_2$"),
                                       TeX("$Y \\sim X_3$"), 
                                       TeX("$Y \\sim X_4$"),
                                       TeX("$Y \\sim X_5$"))),
             Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
  } else {
    data = data |> 
      mutate(Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
             Model = factor(x = Model, 
                            levels = c("Y1 ~ X1", "Y2 ~ X2", "Y3 ~ X3", "Y4 ~ X4", "Y5 ~ X5"), 
                            labels = c(TeX("$Y_1 \\sim X_1$"), 
                                       TeX("$Y_2 \\sim X_2$"),
                                       TeX("$Y_3 \\sim X_3$"), 
                                       TeX("$Y_4 \\sim X_4$"),
                                       TeX("$Y_5 \\sim X_5$"))),
             Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
  }
  
  ## Create the plot 
  data |> 
    ggplot(aes(x = Design, 
               y = est_beta1, 
               fill = Design)) + 
    geom_boxplot() + 
    geom_hline(aes(yintercept = Truth), 
               linetype = 2, 
               color = "red") + 
    facet_grid(cols = vars({{ col_facet_var }}), # .data[[col_facet_var]]), 
               rows = vars(Model), 
               scales = "free", 
               labeller = labeller(Covar = label_value, 
                                   Model = label_parsed)) + 
    theme_minimal(base_size = 14) + 
    scale_fill_colorblind(guide = "none") + 
    xlab(TeX("Validation Study Design", bold = TRUE)) + 
    ylab(TeX("Coefficient Estimate on $X_j$", bold = TRUE)) + 
    scale_x_discrete(labels = parse.labels) + 
    theme(strip.background = element_rect(fill = "black"), 
          strip.text = element_text(color = "white"), 
          #panel.border = element_rect(color = "black", fill = NA),
          legend.title = element_text(face = "bold"), 
          legend.position = "top")
}

# Function to create barbell plot of empirical efficiency 
barbell_efficiency = function(data, group_by_var, sharedY = FALSE) {
  if (sharedY) {
    data = data |> 
      mutate(Model = paste0("Y ~ ", Model), 
             Model = factor(x = Model, 
                            levels = c("Y ~ X1", "Y ~ X2", "Y ~ X3", "Y ~ X4", "Y ~ X5"), 
                            labels = c(TeX("$Y \\sim X_1$"), 
                                       TeX("$Y \\sim X_2$"),
                                       TeX("$Y \\sim X_3$"), 
                                       TeX("$Y \\sim X_4$"),
                                       TeX("$Y \\sim X_5$"))),
             Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
    
    ## Calculate efficiency (by group)
    data = data |> 
      group_by(Model, Design, {{ group_by_var }}) |> 
      summarize(Efficiency = 1 / var(est_beta1)) 
    barbell_data = data |>
      group_by(Model, {{ group_by_var }}) |> 
      summarize(minEff = min(Efficiency), 
                maxEff = max(Efficiency))
    
    ## Create the plot 
    data |> 
      ggplot(aes(x = Model, 
                 y = log(Efficiency), 
                 color = Design)) + 
      geom_segment(data = barbell_data,
                   aes(x = Model, y = log(minEff),
                       xend = Model, yend = log(maxEff)),
                   color = "#aeb6bf",
                   linewidth = 4.5, 
                   alpha = 0.5) +
      geom_point(size = 4) + 
      facet_grid(cols = vars({{ group_by_var }}), 
                 scales = "free", label = parse.labels) + 
      theme_minimal(base_size = 14) + 
      scale_color_colorblind(labels = parse.labels) + 
      xlab(TeX("Model of $Y \\sim X_j$", bold = TRUE)) + 
      ylab(TeX("Empirical Efficiency of Coefficient Estimate on $X_j$ (Log-Transformed)", bold = TRUE)) + 
      scale_x_discrete(labels = parse.labels) + 
      theme(strip.background = element_rect(fill = "black"), 
            strip.text = element_text(color = "white"), 
            #panel.border = element_rect(color = "black", fill = NA),
            legend.title = element_text(face = "bold"), 
            legend.position = "top", 
            panel.spacing = unit(1, "lines")) + 
      coord_flip() 
  } else {
    data = data |> 
      mutate(Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
             Model = factor(x = Model, 
                            levels = c("Y1 ~ X1", "Y2 ~ X2", "Y3 ~ X3", "Y4 ~ X4", "Y5 ~ X5"), 
                            labels = c(TeX("$Y_1 \\sim X_1$"), 
                                       TeX("$Y_2 \\sim X_2$"),
                                       TeX("$Y_3 \\sim X_3$"), 
                                       TeX("$Y_4 \\sim X_4$"),
                                       TeX("$Y_5 \\sim X_5$"))),
             Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
    
    ## Calculate efficiency (by group)
    data = data |> 
      group_by(Model, Design, {{ group_by_var }}) |> 
      summarize(Efficiency = 1 / var(est_beta1)) 
    barbell_data = data |>
      group_by(Model, {{ group_by_var }}) |> 
      summarize(minEff = min(Efficiency), 
                maxEff = max(Efficiency))
    
    ## Create the plot 
    data |> 
      ggplot(aes(x = Model, 
                 y = log(Efficiency), 
                 color = Design)) + 
      geom_segment(data = barbell_data,
                   aes(x = Model, y = log(minEff),
                       xend = Model, yend = log(maxEff)),
                   color = "#aeb6bf",
                   linewidth = 4.5, 
                   alpha = 0.5) +
      geom_point(size = 4) + 
      facet_grid(cols = vars({{ group_by_var }}), 
                 scales = "free") + 
      theme_minimal(base_size = 14) + 
      scale_color_colorblind(labels = parse.labels) + 
      xlab(TeX("Model of $Y_j \\sim X_j$", bold = TRUE)) + 
      ylab(TeX("Empirical Efficiency of Coefficient Estimate on $X_j$ (Log-Transformed)", bold = TRUE)) + 
      scale_x_discrete(labels = parse.labels) + 
      theme(strip.background = element_rect(fill = "black"), 
            strip.text = element_text(color = "white"), 
            legend.title = element_text(face = "bold"), 
            legend.position = "top", 
            panel.spacing = unit(1, "lines")) + 
      coord_flip() 
  }
}

# Function to create barbell plot of sum of variances 
bar_sum_var = function(data, group_by_var, fill_by_model = FALSE, sharedY = FALSE) {
  if (fill_by_model) {
    # Create sum of variances for beta1 from all models
    data = data |> 
      group_by(Model, Design, {{ group_by_var }}) |> 
      summarize(var_beta = var(est_beta1)) |> 
      mutate(Model = paste0("Y", sub(pattern = "X", replacement = "", x = Model), " ~ ", Model), 
             Model = factor(x = Model, 
                            levels = c("Y1 ~ X1", "Y2 ~ X2", "Y3 ~ X3", "Y4 ~ X4", "Y5 ~ X5"), 
                            labels = c(TeX("$Y_1 \\sim X_1$"), 
                                       TeX("$Y_2 \\sim X_2$"),
                                       TeX("$Y_3 \\sim X_3$"), 
                                       TeX("$Y_4 \\sim X_4$"),
                                       TeX("$Y_5 \\sim X_5$"))),
             Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
    
    ## Create the plot 
    data |> 
      ggplot(aes(x = Design, 
                 y = sum_var_beta, 
                 fill = Design, 
                 color = Design)) + 
      geom_segment(aes(x = Design, y = 0,
                       xend = Design, yend = sum_var_beta),
                   linewidth = 6.5, 
                   alpha = 0.5) +
      geom_point(size = 6) + 
      facet_grid(cols = vars({{ group_by_var }}), 
                 scales = "free") + 
      scale_fill_colorblind(labels = parse.labels, 
                            guide = "none") + 
      theme_minimal(base_size = 14) + 
      scale_fill_colorblind(labels = parse.labels) + 
      xlab(TeX("Design", bold = TRUE)) + 
      ylab(TeX("Sum of Variances Across Models $\\sum_{j=1}^{5}\\widehat{V}(\\hat{\\beta}_{1j})$", 
               bold = TRUE)) + 
      scale_x_discrete(labels = parse.labels) + 
      theme(strip.background = element_rect(fill = "black"), 
            strip.text = element_text(color = "white"), 
            legend.title = element_text(face = "bold"), 
            legend.position = "top", 
            panel.spacing = unit(1, "lines")) + 
      coord_flip() 
  } else {
    # Create sum of variances for beta1 from all models
    data = data |> 
      group_by(Model, Design, {{ group_by_var }}) |> 
      mutate(var_beta = var(est_beta1)) |> 
      group_by(Design, {{ group_by_var }}) |> 
      summarize(sum_var_beta = sum(var_beta)) |> 
      mutate(Design = factor(x = Design, 
                             levels = c("SRS", "ETS (X1)", "ETS (PC1)"), 
                             labels = c("SRS", TeX("ETS-$X_1^*$"), TeX("ETS-$PC_1^*$")))) 
    
    ## Create the plot 
    if (sharedY) {
      data |> 
        ggplot(aes(x = Design, 
                   y = sum_var_beta, 
                   fill = Design, 
                   color = Design)) + 
        geom_segment(aes(x = Design, y = 0,
                         xend = Design, yend = sum_var_beta),
                     linewidth = 6.5, 
                     alpha = 0.5) +
        geom_point(size = 6) + 
        facet_grid(cols = vars({{ group_by_var }}), 
                   scales = "free", 
                   labeller = parse.labels) + 
        theme_minimal(base_size = 14) + 
        scale_fill_colorblind(labels = parse.labels, 
                              guide = "none") + 
        scale_color_colorblind(labels = parse.labels, 
                               guide = "none") + 
        xlab(TeX("Design", bold = TRUE)) + 
        ylab(TeX("Empirical Total Coefficient Variability Across All Models", bold = TRUE)) + 
        scale_x_discrete(labels = parse.labels) + 
        theme(strip.background = element_rect(fill = "black"), 
              strip.text = element_text(color = "white"), 
              legend.title = element_text(face = "bold"), 
              legend.position = "top", 
              panel.spacing = unit(1, "lines")) + 
        coord_flip()  
    } else {
      data |> 
        ggplot(aes(x = Design, 
                   y = sum_var_beta, 
                   fill = Design, 
                   color = Design)) + 
        geom_segment(aes(x = Design, y = 0,
                         xend = Design, yend = sum_var_beta),
                     linewidth = 6.5, 
                     alpha = 0.5) +
        geom_point(size = 6) + 
        facet_grid(cols = vars({{ group_by_var }}), 
                   scales = "free") + 
        theme_minimal(base_size = 14) + 
        scale_fill_colorblind(labels = parse.labels, 
                              guide = "none") + 
        scale_color_colorblind(labels = parse.labels, 
                               guide = "none") + 
        xlab(TeX("Design", bold = TRUE)) + 
        ylab(TeX("Empirical Total Coefficient Variability Across All Models", bold = TRUE)) + 
        scale_x_discrete(labels = parse.labels) + 
        theme(strip.background = element_rect(fill = "black"), 
              strip.text = element_text(color = "white"), 
              legend.title = element_text(face = "bold"), 
              legend.position = "top", 
              panel.spacing = unit(1, "lines")) + 
        coord_flip()   
    }
  }
}