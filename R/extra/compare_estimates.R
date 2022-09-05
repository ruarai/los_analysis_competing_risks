

library(tidyverse)
library(lubridate)

source("R/common.R")
source("R/surv_ward_to_next_static.R")
source("R/surv_ICU_to_next_static.R")


# Load from completed real-time run


get_means <- function(linelist_data_path) {
  
  linelist_data <- read_csv(linelist_data_path)
  
  static_ward_omi_mix <- make_surv_ward_to_next(linelist_data)
  static_ICU_omi_mix <- make_surv_ICU_to_next(linelist_data)
  
  
  bind_rows(
    static_ward_omi_mix$fit_narrow %>%
      filter(coding == "ward_to_discharge" | coding == "ward_to_ICU") %>%
      mutate(subset_name = "omi_mix"),
    static_ICU_omi_mix$fit_narrow %>%
      filter(coding == "ICU_to_postICU") %>%
      mutate(subset_name = "omi_mix"),
  ) %>%
    group_by(subset_name, coding, age_class_narrow) %>%
    summarise(mean_lower = quantile(mean, 0.025),
              mean_upper = quantile(mean, 0.975),
              mean = mean(mean),
              .groups = "drop")
}

old_means <- read_csv("results/NSW_final_earlycutoff/all_means.csv")

means_retro <- get_means("results/NSW_retro_earlycutoff/omi_mix/linelist_data.csv") %>%
  mutate(group = "new")

means_naive <- get_means("results/NSW_final_naive/omi_mix/linelist_data.csv") %>%
  mutate(group = "naive")

plot_data <- bind_rows(
  means_retro,
  means_naive,
  old_means %>%
    filter(age_type == "narrow",
           coding %in% c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU")) %>%
    rename(age_class_narrow = age_class) %>%
    mutate(group = "old")
) %>%
  mutate(
    group = factor(group, c("old", "new", "naive")),
    subset_name = est_labels[subset_name],
    subset_name = factor(subset_name, est_labels),
    coding = pretty_coding_name[coding],
    coding = factor(coding, pretty_coding_name),
  )



plot_data %>%
  filter(subset_name == est_labels["omi_mix"]) %>%
  ggplot() +
  geom_linerange(aes(
    x = age_class_narrow, ymin = mean_lower, ymax = mean_upper,
    group = group, colour = group
  ),
  position = position_dodge(width = 0.25),
  size = 0.5
  ) +
  geom_point(aes(
    x = age_class_narrow, y = mean,
    group = group, colour = group,
    size = 0.1, stroke = 0.2
  ),
  position = position_dodge(width = 0.25),
  size = 2
  ) +
  facet_wrap(~ subset_name * coding,
    ncol = 3,
    scales = "free"
  ) +
  scale_y_continuous(
    breaks = scales::breaks_extended(10),
    expand = expansion(mult = c(0.05, 0.5))
  ) +
  ggokabeito::scale_colour_okabe_ito(
    order = c(5, 7, 6),
    labels = c(
      "old" = "Real-time",
      "new" = "Retrospective",
      "naive" = "Real-time (not accounting for censoring)"
    ),
    name = "Data used for estimate"
  ) +
  xlab("Age group") +
  ylab("Mean") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 9, face = "italic"),
    plot.title = element_text(size = 11),
    strip.text = element_text(hjust = 0, size = 8)
  )



ggsave(
  paste0(results_dir, "/estimates_comparison_2.png"),
  bg = "white",
  width = 9, height = 5
)
