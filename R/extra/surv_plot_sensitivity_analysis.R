require(cowplot)
source("R/fit_meta.R")
aj_fits <- tar_read(all_aj) %>%
  remove_bad_fits() %>%
  mutate(
    period = str_sub(subset_name, end = 1),
    subset_name = str_remove(subset_name, "h_"),
    subset_name = str_remove(subset_name, "d_"),
    subset_name = str_remove(subset_name, "om_")
  ) %>%
  mutate(period = case_when(
    period == "h" ~ "Omicron (HNE)",
    period == "d" ~ "Delta",
    period == "o" ~ "Omicron-Delta"
  )) %>%
  filter(subset_name == "filt_both" | subset_name == "filt_none")


max_t_by_coding <- c(
  "onset_to_ward" = 21,
  "ward_to_discharge" = 120,
  "ward_to_ICU" = 14,
  "ward_to_death" = 50,
  "ICU_to_discharge" = 40,
  "ICU_to_death" = 40,
  "ICU_to_postICU" = 30,
  "postICU_to_discharge" = 30,
  "postICU_to_death" = 30
)

results_dir <- tar_read(results_dir)

t_breaks <- seq(0, 60, by = 10)
t_breaks_minor <- seq(0, 60, by = 5)

plot_labels <- c(
  "filt_both" = "Filtering both           ",
  "filt_delay" = "Filtering out symptom onset after admission            ",
  "filt_eps" = "Filtering out episodes >48hrs apart           ",
  "filt_none" = "No filtering    "
)

fit_plots <- fit_meta %>%
  slice(-1) %>%
  pmap(function(i_comp, i_age_type) {
    i_filt <- . %>%
      filter(coding == i_comp, age_type == i_age_type) %>%
      mutate(age_class = factor(age_class, levels = c("0-39", "40-69", "0-69", "70+")))



    ggplot() +
      geom_line(aes(x = time, y = val, color = subset_name),
        aj_fits %>% i_filt() %>% filter(model == "parametric"),
        linetype = "dashed"
      ) +
      geom_ribbon(aes(x = time, ymin = lower, ymax = upper, fill = subset_name),
        aj_fits %>% i_filt() %>% filter(model == "parametric"),
        alpha = 0.25
      ) +
      geom_step(
        aes(x = time, y = val, color = subset_name),
        aj_fits %>% i_filt() %>% filter(model == "non-parametric")
      ) +
      facet_wrap(~ period * age_class) +
      coord_cartesian(xlim = c(0, max_t_by_coding[i_comp])) +
      scale_x_continuous(breaks = t_breaks, minor_breaks = t_breaks_minor) +
      ggokabeito::scale_color_okabe_ito(labels = plot_labels, name = "", order = 5:9) +
      ggokabeito::scale_fill_okabe_ito(labels = plot_labels, name = "", order = 5:9) +
      xlab(NULL) +
      ylab(NULL) +
      ggtitle(NULL, pretty_coding_name[i_comp]) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.subtitle = element_text(face = "italic")
      )
  })


plot(fit_plots[[1]])
