
source("R/common.R")
source("R/fit_meta.R")

all_means <- remove_bad_fits(tar_read(all_means)) %>%
  
  mutate(period = str_sub(subset_name, end = 1),
         
         subset_name = str_remove(subset_name, "h_"),
         subset_name = str_remove(subset_name, "d_"),
         subset_name = str_remove(subset_name, "om_")) %>%
  
  mutate(period = case_when(period == "h" ~ "Omicron (HNE)",
                            period == "d" ~ "Delta",
                            period == "o" ~ "Omicron-Delta"))


plot_labels <- c(
  "filt_both" = "Filtering both           ",
  "filt_delay" = "Filtering out symptom onset after admission            ",
  "filt_eps" = "Filtering out episodes >48hrs apart           ",
  "filt_none" = "No filtering    "
)

plot_group <- function(
  i_coding, i_age_type, y_max,
  
  i_title, i_subtitle = NULL,
  
  legend_only = FALSE
) {
  
  plot_data <- all_means %>%
    filter(coding == i_coding,
           age_type == i_age_type) %>%
    
    mutate(subset_name  = factor(subset_name, levels = names(plot_labels)))
  
  p <- ggplot(plot_data) +
    geom_linerange(aes(x = age_class, ymin = mean_lower, ymax = mean_upper, 
                       group = subset_name, color = subset_name),
                   
                   position = position_dodge(width = 0.4),
                   size = 0.6) +
    
    geom_point(aes(x = age_class, y = mean,
                   group = subset_name, color = subset_name),
               
               position = position_dodge(width = 0.4),
               size = 2) +
    
    geom_point(aes(x = age_class, y = mean_upper,
                   group = subset_name, color = subset_name),
               pch = '-',
               
               position = position_dodge(width = 0.4),
               size = 4, stroke = 2) +
    
    geom_point(aes(x = age_class, y = mean_lower,
                   group = subset_name, color = subset_name),
               pch = '-',
               
               position = position_dodge(width = 0.4),
               
               size = 4, stroke = 2) +
    
    facet_wrap(~period, ncol = 1) +
    
    ggokabeito::scale_color_okabe_ito(labels = plot_labels, name = "", order = 5:9) +
    
    scale_y_continuous(breaks = scales::breaks_extended(10)) +
    
    coord_cartesian(ylim = c(0, y_max)) +
    
    xlab("Age group") + ylab("Mean (days)") +
    
    ggtitle(i_title, i_subtitle) +
    
    
    theme_minimal() +
    
    theme(legend.position = if_else(legend_only, "bottom", "none"),
          legend.direction = "horizontal",
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
    plot_group("ward_to_discharge", "narrow", 21,
               "Ward length of stay (to discharge)", "ward-to-discharge"),
    
    plot_group("ward_to_ICU", "narrow", 4,
               "Ward length of stay (to ICU)", "ward-to-ICU"),
    
    plot_group("ICU_to_postICU", "wide", 14,
               "ICU length of stay", "ICU-to-post-ICU"),
    
    
    rel_widths = c(30, 30, 23),
    nrow = 1
  ),
  
  p_legend,
  rel_heights = c(10, 1),
  ncol = 1
)

results_dir <- tar_read(results_dir)


ggsave(
  paste0(results_dir, "/sens_means.png"),
  bg = "white",
  width = 9,
  height = 7
)
