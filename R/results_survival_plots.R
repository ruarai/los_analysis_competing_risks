

fit_plots <- fit_meta %>%
  slice(-1) %>%
  pmap(function(i_comp, i_age_type) {
    i_filt <- . %>%
      filter(coding == i_comp, age_type == i_age_type)
    
    
    ggplot() +
      geom_line(aes(x = time, y = val),
                aj_fits %>% i_filt %>% filter(model == "parametric"),
                color = ggokabeito::palette_okabe_ito(2),
                linetype = 'dashed') +
      
      geom_ribbon(aes(x = time, ymin = lower, ymax = upper),
                  aj_fits %>% i_filt %>% filter(model == "parametric"),
                  
                  fill = ggokabeito::palette_okabe_ito(2),
                  alpha = 0.25) +
      
      geom_step(aes(x = time, y = val),
                aj_fits %>% i_filt %>% filter(model == "non-parametric")) +
      
      facet_wrap(~age_class,
                 scales = "free_y",
                 nrow = 1) +
      
      coord_cartesian(xlim = c(0, 40)) +
      
      xlab(NULL) + ylab(NULL) +
      
      ggtitle(NULL, i_comp) +
      
      
      theme_minimal()
    
  })


cowplot::plot_grid(
  plotlist = fit_plots[1:3],
  ncol = 1
  
)


