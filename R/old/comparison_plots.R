

results_dir <- "results/comp_omi_HNE_2022-01-04_delta_2021-11-15"
results_filt <- c("omi_HNE", "delta")
# 
# results_dir <- "results/comp_omi_mix_2022-01-11_delta_2021-11-15"
# results_filt <- c("omi_mix", "delta")

datasets <- list(
  "omi_mix" = read_rds("results/NSW_omi_mix_2022-01-11/surv_plots_data.rds"),
  "delta" = read_rds("results/NSW_delta_2021-11-25/surv_plots_data.rds"),
  "omi_HNE" = read_rds("results/NSW_omi_HNE_2022-01-11/surv_plots_data.rds")
)

all_fits_plot <- map_dfr(datasets, ~ .$plot_fits, .id = "source")
obs_data_ecdf <- map_dfr(datasets, ~ .$plot_ecdf, .id = "source")


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

est_cols <- c(
  "delta" = ggokabeito::palette_okabe_ito()[1],
  "omi_mix" = ggokabeito::palette_okabe_ito()[2],
  "omi_HNE" = ggokabeito::palette_okabe_ito()[3]
)

bad_fits <- tribble(
  ~source, ~coding,
  "omi_mix", "postICU_to_death"
) %>% mutate(drop_bad_fit = TRUE)

p_list <- pmap(plot_meta, function(i_comp, i_age_type) {
  
  filter_comp <- . %>% 
    filter(coding == i_comp, age_type == i_age_type, source %in% results_filt) %>%
    left_join(bad_fits) %>%
    filter(is.na(drop_bad_fit))
    
  
  ggplot() +
    
    geom_step(aes(x = LoS, y = 1 - y, color = source),
              obs_data_ecdf %>% filter_comp) +
    
    geom_line(aes(x = time, y =  est, color = source),
              linetype = 'longdash',
              data = all_fits_plot %>% filter_comp) +
    
    geom_ribbon(aes(x = time, ymin = lcl, ymax = ucl, fill = source),
                alpha = 0.25,
                data = all_fits_plot %>% filter_comp) +
    
    scale_color_manual(values = est_cols) +
    scale_fill_manual(values = est_cols) +
    
    
    coord_cartesian(xlim = c(0, 40)) +
    
    xlab("Time (days)") + ylab("Probability") +
    
    facet_wrap(~age_class, scale = "fixed") +
    
    ggtitle(i_comp %>% str_replace_all("_", "-")) +
    
    theme_minimal() +
    theme(plot.title = element_text(face = 'plain',
                                    size = 12),
          legend.position = 'none',
          plot.title.position = "plot",
          plot.margin = margin(b = 10),
          axis.title.y = element_text(margin = margin(l = 10, r = 5)),
          axis.title.x = element_blank())
  
})

est_cols_adj <- grDevices::adjustcolor(est_cols, alpha.f = 0.25) %>%
  `names<-`(names(est_cols))

est_labels <- c(
  "delta" = "Delta epidemic        ",
  "omi_mix" = "Omicron/Delta mixed epidemic         ",
  "omi_HNE" = "Omicron epidemic (Hunter New England LHD)             "
)


plot_legend <- (ggplot(tibble(b = 1:2,
                              c = 3:4)) +
                  geom_rect(aes(xmin = 0, xmax = b, ymin = 0, ymax = c, fill = 'est')) +
                  geom_line(aes(x = b, y = c, color = 'data')) +
                  
                  scale_fill_manual("",
                                    labels = est_labels[results_filt],
                                    values = est_cols_adj[results_filt]) +
                  
                  scale_color_manual("",
                                     labels = est_labels[results_filt],
                                     values = est_cols[results_filt]) +
                  
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

cowplot::plot_grid(
  cowplot::plot_grid(plotlist = p_list[c(1, 2)], ncol = 1),
  plot_legend,
  rel_heights = c(20, 1), ncol = 1
)

ggsave(
  paste0(results_dir, "/survival_plots_onset_and_ward.png"),
  bg = 'white',
  width = 8, height = 6
)
