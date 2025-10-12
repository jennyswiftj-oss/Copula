create_comparison_plot <- function(data, stat_type, y_label, y_limits, show_legend = FALSE) {
  
  plot_data <- data %>% filter(statistic == stat_type)
  
  bg_data <- plot_data %>%
    select(var_pair, diff_category, relative_diff) %>%
    distinct()
  
  ordered_pairs <- bg_data %>%
    arrange(relative_diff) %>%
    pull(var_pair)
  
  plot_data <- plot_data %>%
    mutate(var_pair = factor(var_pair, levels = ordered_pairs))
  
  bg_data <- bg_data %>%
    mutate(var_pair = factor(var_pair, levels = ordered_pairs))
  
  p <- ggplot(plot_data, aes(x = var_pair)) +
    geom_tile(data = bg_data, 
              aes(y = mean(y_limits), fill = diff_category),
              alpha = 0.6, height = diff(y_limits), width = 0.9) +
    scale_fill_manual(
      values = category_colors,
      name   = ifelse(stat_type == "correlation", 
                      "Relative Difference (Correlation)", 
                      "Relative Difference (Overlap)"),
      drop  = FALSE,
      guide  = if (show_legend) guide_legend(order = 1) else "none"
    ) +
    
    {if(stat_type == "correlation") geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")} +
    {if(stat_type == "overlap") geom_hline(yintercept = c(75, 85), color = "grey75", linetype = "dashed")} +
    
    ggnewscale::new_scale_fill() +
    geom_boxplot(aes(y = value, fill = model, group = interaction(var_pair, model)),
                 position = position_dodge(width = 0.8),
                 color = "black",
                 alpha = 0.8, 
                 outlier.shape = NA, 
                 width = 0.7,
                 size = 0.5) +
    scale_fill_manual(
      values = c("full copula" = "#3498db", "subgroup copula" = "#e74c3c"),
      name   = "Model",
      guide  = if (show_legend) guide_legend(order = 2) else "none"
    ) +
    
    {if(stat_type == "correlation") 
      geom_point(aes(y = observed, group = interaction(var_pair, model)),
                 position = position_dodge(width = 0.8),
                 color = "#2c3e50", size = 2, shape = 18)} +
    
    labs(x = "Covariate Pairs", y = y_label) +
    coord_cartesian(ylim = y_limits) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = if (show_legend) "top" else "none",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9),
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(4, 2, 4, 2)
    )
  
  return(p)
}
