
make_results_survival_plots <- function(
  onset_fits, ward_fits, ICU_fits, postICU_fits,
  
  age_table_narrow, age_table_wide,
  
  results_dir
) {
  
  all_fits <- list(onset_fits, ward_fits, ICU_fits, postICU_fits)
  
  all_data <- map_dfr(
    all_fits, function(x) x$data
  ) %>%
    filter(!(coding %in% c("unknown", "censored")))
  
  
  summ_fit <- function(surv_fit, coding_labels, age_class_labels) {
    
    fit_data <- expand_grid(
      age_class_wide = age_class_labels,
      age_class_narrow = age_class_labels,
      coding = coding_labels
    ) %>%
      filter(!(coding %in% c("unknown", "censored")))
    
    summ_data <- summary(
      surv_fit,
      newdata = fit_data,
      t = seq(0, 40, by = 0.25)
    ) %>%
      map_dfr(identity, .id = 'label')
    
    label_matrix <- summ_data$label %>% str_split_fixed(", ", n = 2)
    
    age_class_col <- str_detect(label_matrix[1,], "age_class") %>% which()
    coding_col <- if_else(age_class_col == 1, 2, 1)
    
    summ_data %>%
      select(-label) %>%
      mutate(age_class = (label_matrix[, age_class_col] %>% str_split_fixed("=", n = 2))[,2],
             coding = (label_matrix[, coding_col] %>% str_split_fixed("=", n = 2))[,2]) %>%
      
      # Special case for onset_to_ward
      
      mutate(coding = if_else(coding == "", "onset_to_ward", coding))
    
  }
  
  all_fits_plot <- map_dfr(
    all_fits, function(x) {
      bind_rows(
        summ_fit(x$fit_wide, unique(x$data$coding), age_table_wide$labels) %>%
          mutate(age_type = "wide"),
        summ_fit(x$fit_narrow, unique(x$data$coding), age_table_narrow$labels) %>%
          mutate(age_type = "narrow")
        
      )
    }
  )
  
  make_ecdf <- function(x, age_class_col) {
    
    obs_data_ecdf_unfilled <- all_data %>%
      ungroup() %>%
      
      group_by(coding, age_class = !!sym(age_class_col)) %>%
      
      mutate(y = ecdf(LoS)(LoS)) %>%
      
      filter(LoS < 40)
    
    # Adding fake endpoints
    obs_data_ecdf_unfilled %>%
      bind_rows(
        obs_data_ecdf_unfilled %>% distinct(coding, age_class) %>% mutate(y = 0, LoS = 0),
        obs_data_ecdf_unfilled %>% distinct(coding, age_class) %>% mutate(y = 1, LoS = 1000)
      )
  }
  
  obs_data_ecdf <- bind_rows(
    make_ecdf(all_data, "age_class_wide") %>% mutate(age_type = "wide"),
    make_ecdf(all_data, "age_class_narrow") %>% mutate(age_type = "narrow")
  )
  
  
  list(
    plot_ecdf = obs_data_ecdf,
    plot_fits = all_fits_plot
  ) %>%
    write_rds(paste0(results_dir, "/surv_plots_data.rds"))
  
  plot_meta <- tribble(
    ~ i_comp, ~ i_age_type,
    "onset_to_ward", "narrow", 
    "ward_to_discharge", "narrow",
    "ward_to_ICU", "narrow", 
    "ward_to_death", "wide",
    "ICU_to_discharge", "narrow",
    "ICU_to_death", "wide",
    "ICU_to_postICU", "narrow",
    "postICU_to_discharge", "narrow",
    "postICU_to_death", "wide"
  )
  
  col_estimates <- ggokabeito::palette_okabe_ito()[2]
  
  
  p_list <- pmap(plot_meta, function(i_comp, i_age_type) {
    
    filter_comp <- . %>% filter(coding == i_comp, age_type == i_age_type)
    
    ggplot() +
      
      geom_step(aes(x = LoS, y = 1 - y),
                obs_data_ecdf %>% filter_comp) +
      
      geom_line(aes(x = time, y =  est),
                color = col_estimates,
                data = all_fits_plot %>% filter_comp) +
      
      geom_ribbon(aes(x = time, ymin = lcl, ymax = ucl),
                  fill = col_estimates,
                  alpha = 0.25,
                  data = all_fits_plot %>% filter_comp) +
      
      
      coord_cartesian(xlim = c(0, 40)) +
      
      xlab("Time (days)") + ylab("Probability") +
      
      facet_wrap(~age_class, scale = "fixed") +
      
      ggtitle(i_comp %>% str_replace_all("_", "-")) +
      
      theme_minimal() +
      theme(plot.title = element_text(face = 'plain',
                                      size = 12),
            plot.title.position = "plot",
            plot.margin = margin(b = 10),
            axis.title.y = element_text(margin = margin(l = 10, r = 5)),
            axis.title.x = element_blank())
    
  })
  
  
  plot_legend <- (ggplot(tibble(b = 1:2,
                                c = 3:4)) +
                    geom_rect(aes(xmin = 0, xmax = b, ymin = 0, ymax = c, fill = 'est')) +
                    geom_line(aes(x = b, y = c, color = 'data')) +
                    
                    scale_fill_manual("",
                                      values = c("est" = col_estimates),
                                      labels = c("est" = "Estimate median and 95% CI")) +
                    
                    scale_color_manual("",
                                       values = c("data" = "black"),
                                       labels = c("data" = "Observed data")) +
                    
                    theme_minimal() +
                    theme(legend.position = 'bottom')) %>%
    cowplot::get_legend()
  
  dir.create(results_dir, showWarnings = FALSE)
  
  cowplot::plot_grid(
    cowplot::plot_grid(plotlist = p_list[1:5], ncol = 1),
    plot_legend,
    rel_heights = c(20, 1), ncol = 1
  )
  
  ggsave(
    paste0(results_dir, "/survival_plots_1.png"),
    bg = 'white',
    width = 8, height = 12
  )
  
  cowplot::plot_grid(
    cowplot::plot_grid(plotlist = p_list[6:9], ncol = 1),
    plot_legend,
    rel_heights = c(20, 1), ncol = 1
  )
  
  
  ggsave(paste0(results_dir, "/survival_plots_2.png"),
         bg = 'white',
         width = 8,
         height = 10)
  
  
}
