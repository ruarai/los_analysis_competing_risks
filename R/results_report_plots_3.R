
make_reporting_plots_3 <- function(
  all_onset_fits,
  results_dir
) {
  
  names(all_onset_fits) <- str_remove(names(all_onset_fits),"trunc_onset_to_ward_")
  
  onset_fit_params <- map_dfr(all_onset_fits, function(x) x$fit, .id = "subset_name")
  
  
  onset_fit_data <- map_dfr(all_onset_fits, function(x) x$data, .id = "subset_name")
  
  onset_fit_ecdf <- onset_fit_data %>%
    group_by(subset_name, age_class = age_class_narrow) %>%
    mutate(y = ecdf(time_to_report)(time_to_report))
  
  
  onset_fit_cdf <- onset_fit_params %>%
    group_by(subset_name, age_class) %>%
    summarise(x = seq(0, 30, by = 0.1),
              y = pgamma(seq(0, 30, by = 0.1), shape = shape, rate = rate))
  
  
  ggplot() +
    
    geom_line(aes(x, y, color = subset_name),
              onset_fit_cdf) +
    
    stat_ecdf(aes(x = time_to_report, y = y, color = subset_name),
              alpha = 0.5,
              onset_fit_ecdf) +
    
    facet_grid(cols = vars(age_class)) +
    
    coord_cartesian(xlim = c(0, 16)) +
    
    scale_color_manual(name = "", values = est_cols, labels = est_labels) +
    
    ylab(NULL) + xlab("Time after symptom onset (days)") +
    
    ggtitle("Time until hospital admission") +
    
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0, size = 10))
  
  ggsave(
    paste0(results_dir, "/onset_plot.png"),
    bg = "white",
    width = 8, height = 4
  )
  
}
