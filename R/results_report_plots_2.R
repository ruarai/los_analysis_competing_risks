

make_reporting_plots_2 <- function(
  all_means,
  results_dir
) {
  all_means <- remove_bad_fits(all_means)
  
  
  
  plot_group <- function(
    i_coding, i_age_type, y_max,
    
    i_title, i_subtitle = NULL, x_lab = "",
    
    legend_only = FALSE
  ) {
    
    plot_data <- all_means %>%
      filter(coding == i_coding,
             age_type == i_age_type) %>%
      
      mutate(subset_name = factor(subset_name, levels = c("delta", "omi_mix", "omi_HNE")))
    
    p <- ggplot(plot_data) +
      geom_linerange(aes(x = age_class, ymin = mean_lower, ymax = mean_upper, 
                         group = subset_name, color = subset_name),
                     
                     position = position_dodge(width = 0.25),
                     size = 0.6) +
      
      geom_point(aes(x = age_class, y = mean,
                     group = subset_name, color = subset_name),
                 
                 position = position_dodge(width = 0.25),
                 size = 2) +
      
      geom_point(aes(x = age_class, y = mean_upper,
                     group = subset_name, color = subset_name),
                 pch = '-',
                 
                 position = position_dodge(width = 0.25),
                 size = 4, stroke = 2) +
      
      geom_point(aes(x = age_class, y = mean_lower,
                     group = subset_name, color = subset_name),
                 pch = '-',
                 
                 position = position_dodge(width = 0.25),
                 
                 size = 4, stroke = 2) +
      
      scale_color_manual(values = est_cols,
                         labels = est_labels,
                         name = "") +
      
      scale_y_continuous(breaks = scales::breaks_extended(10)) +
      
      coord_cartesian(ylim = c(0, y_max)) +
      
      xlab(x_lab) + ylab("Mean (days)") +
      
      ggtitle(i_title, i_subtitle) +
      
      
      theme_minimal() +
      
      theme(legend.position = if_else(legend_only, "bottom", "none"),
            plot.subtitle = element_text(size = 9, face = "italic"),
            plot.title = element_text(size = 11))
    
    if(legend_only) {
      cowplot::get_legend(p)
    } else{
      p
    }
    
  }
  
  p_legend <- plot_group("ward_to_discharge", "narrow", 14,
             "Mean time to discharge from ward",
             
             legend_only = TRUE)
  
  cowplot::plot_grid(
    cowplot::plot_grid(
      plot_group("ward_to_discharge", "narrow", 14,
                 "Ward length of stay (to discharge)", "ward-to-discharge"),
      
      plot_group("ward_to_ICU", "narrow", 4, x_lab = "Age group",
                 "Ward length of stay (to ICU)", "ward-to-ICU"),
      
      plot_group("ICU_to_postICU", "narrow", 14,
                 "ICU length of stay", "ICU-to-post-ICU"),
      
      
      rel_widths = c(30, 30, 30),
      nrow = 1
    ),
    
    p_legend,
    rel_heights = c(10, 1),
    ncol = 1
  )
  
  ggsave(
    paste0(results_dir, "/means_plot.png"),
    bg = "white",
    width = 9, height = 4.3
  )
  
}

