require(cowplot)
source("R/fit_meta.R")
source("R/common.R")
aj_fits <- tar_read(all_aj) %>%
  remove_bad_fits()

results_dir <- tar_read(results_dir)

t_breaks <- seq(0, 60, by = 10)
t_breaks_minor <- seq(0, 60, by = 5)

fit_plots <- fit_meta %>%
  slice(-1) %>%
  pmap(function(i_comp, i_age_type) {
    i_filt <- . %>%
      filter(coding == i_comp, age_type == i_age_type) %>%
      mutate(age_class = factor(age_class, levels = c("0-39", "40-69", "0-69", "70+")))

    n_age_groups <- aj_fits %>%
      i_filt() %>%
      pull(age_class) %>%
      unique() %>%
      length()

    optional_facet <- if_else(
      n_age_groups > 1,
      list(facet_wrap(~age_class,
        scales = "free_y",
        nrow = 1
      )),
      list(NULL)
    )


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
      optional_facet +
      coord_cartesian(xlim = c(0, max_t_by_coding[i_comp])) +

      scale_fill_manual(values = est_cols, labels = est_labels, name = "") +
      scale_color_manual(values = est_cols, labels = est_labels, name = "") +

      scale_x_continuous(breaks = t_breaks, minor_breaks = t_breaks_minor) +
      xlab(NULL) +
      ylab(NULL) +
      ggtitle(NULL, pretty_coding_name[i_comp]) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.subtitle = element_text(face = "italic"),
        text = element_text(family = "Helvetica")
      )
  })


est_cols_adj <- grDevices::adjustcolor(est_cols, alpha.f = 0.25) %>%
  `names<-`(names(est_cols))

plot_legend <- (ggplot(tibble(
  b = 1:2,
  c = 3:4
)) +
  geom_rect(aes(xmin = 0, xmax = b, ymin = 0, ymax = c, fill = "est")) +
  geom_line(aes(x = b, y = c, color = "data")) +
  scale_fill_manual("",
    labels = est_labels,
    values = est_cols_adj
  ) +
  scale_color_manual("",
    labels = est_labels,
    values = est_cols
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")) %>%
  cowplot::get_legend()

cowplot::plot_grid(
  plotlist = fit_plots[1:4],
  ncol = 1
)

plot_grid(
  cowplot::plot_grid(
    plotlist = fit_plots[1:4],
    ncol = 1
  ),
  plot_legend,
  ncol = 1,
  rel_heights = c(20, 1)
)

ggsave(
  paste0(results_dir, "/aj_diag_plots_1.png"),
  bg = "white",
  width = 9, height = 10
)


plot_grid(
  cowplot::plot_grid(
    plotlist = fit_plots[5:8],
    ncol = 1
  ),
  plot_legend,
  ncol = 1,
  rel_heights = c(20, 1)
)
ggsave(
  paste0(results_dir, "/aj_diag_plots_2.png"),
  bg = "white",
  width = 9, height = 10
)
