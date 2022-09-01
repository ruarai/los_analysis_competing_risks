
make_reporting_plots_1 <- function(all_aj,
                                   results_dir) {
  require(cowplot)

  all_aj <- remove_bad_fits(all_aj)


  t_breaks <- seq(0, 60, by = 10)
  t_breaks_minor <- seq(0, 60, by = 5)


  plot_group <- function(i_coding, i_age_type, i_age_class,
                         x_max, i_subtitle, i_title = "",
                         legend_only = FALSE) {
    sub_aj <- all_aj %>%
      filter(
        coding == i_coding,
        age_type == i_age_type,
        age_class == i_age_class
      )

    p <- ggplot() +
      geom_line(aes(x = time, y = val, color = subset_name),
        sub_aj %>% filter(model == "parametric"),
        linetype = "dashed"
      ) +
      geom_ribbon(aes(
        x = time, ymin = lower, ymax = upper,
        group = subset_name, fill = subset_name
      ),
      sub_aj %>% filter(model == "parametric"),
      alpha = 0.25
      ) +
      geom_step(
        aes(x = time, y = val, color = subset_name),
        sub_aj %>% filter(model == "non-parametric")
      ) +
      scale_x_continuous(breaks = t_breaks, minor_breaks = t_breaks_minor) +
      scale_fill_manual(values = est_cols, labels = est_labels, name = "") +
      scale_color_manual(values = est_cols, labels = est_labels, name = "") +
      coord_cartesian(xlim = c(0, x_max)) +
      ggtitle(i_title, str_c("Ages ", i_subtitle)) +
      xlab(NULL) +
      ylab(NULL) +
      theme_minimal() +
      theme(legend.position = if_else(legend_only, "bottom", "none"),
            text = element_text(family = "Helvetica"))

    if (legend_only) {
      get_legend(p)
    } else {
      p
    }
  }


  plot_grid(

    add_sub(
      plot_grid(
        plot_group(
          "ward_to_discharge", "narrow", "0-39", 15, "0-39",
          "Probability of discharge from ward following ward admission"
        ),
        plot_group("ward_to_discharge", "narrow", "40-69", 20, "40-69"),
        plot_group("ward_to_discharge", "narrow", "70+", 35, "70+"),
        nrow = 1,
        rel_widths = c(17, 21, 35)
      ),
      "Time since ward admission (days)",
      size = 11, vjust = 0
    ),
    add_sub(
      plot_grid(
        plot_group(
          "ward_to_ICU", "narrow", "0-39", 15, "0-39",
          "Probability of admission to ICU following ward admission"
        ),
        plot_group("ward_to_ICU", "narrow", "40-69", 15, "40-69"),
        plot_group("ward_to_ICU", "narrow", "70+", 15, "70+"),
        nrow = 1
      ),
      "Time since ward admission (days)",
      size = 11, vjust = 0
    ),
    add_sub(
      plot_grid(
        plot_group(
          "ICU_to_postICU", "narrow", "0-39", 40, "0-39",
          "Probability of admission to ward following ICU admission"
        ),
        plot_group("ICU_to_postICU", "narrow", "40-69", 40, "40-69"),
        plot_group("ICU_to_postICU", "narrow", "70+", 40, "70+"),
        nrow = 1
      ),
      "Time since ICU admission (days)",
      size = 11, vjust = 0
    ),
    plot_group("ward_to_discharge", "narrow", "0-39", 15, "0-39",
      legend_only = TRUE
    ),
    rel_heights = c(10, 10, 10, 1.7),
    ncol = 1
  )

  ggsave(
    paste0(results_dir, "/aj_plots.png"),
    bg = "white",
    width = 9, height = 8
  )
}
