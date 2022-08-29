
source("R/common.R")
source("R/surv_ward_to_next_static.R")
source("R/surv_ICU_to_next_static.R")


linelist_data_omi_mix <- tar_read(linelist_data_omi_mix)

linelist_data_omi_HNE <- tar_read(linelist_data_omi_HNE)

results_dir <- tar_read(results_dir)


static_ward_omi_mix <- make_surv_ward_to_next(linelist_data_omi_mix)
static_ward_omi_HNE <- make_surv_ward_to_next(linelist_data_omi_HNE)

static_ICU_omi_mix <- make_surv_ICU_to_next(linelist_data_omi_mix)
static_ICU_omi_HNE <- make_surv_ICU_to_next(linelist_data_omi_HNE)

save.image("old_data_plot.Rdata")

old_samples <- bind_rows(
  read_csv("results/NSW_final/omi_mix/estimate_samples_share_wide.csv",
    show_col_types = FALSE
  ) %>%
    mutate(subset_name = "omi_mix"),
  read_csv("results/NSW_final/omi_HNE/estimate_samples_share_wide.csv",
    show_col_types = FALSE
  ) %>%
    mutate(subset_name = "omi_HNE")
) %>%
  select(
    sample, age_group, subset_name,
    shape_ward_to_discharge, shape_ward_to_ICU,
    scale_ward_to_discharge, scale_ward_to_ICU,
    shape_ICU_to_postICU,
    scale_ICU_to_postICU
  ) %>%
  filter(age_group %in% c("0-9", "40-49", "70-79")) %>%
  mutate(
    mean_ward_to_discharge = shape_ward_to_discharge * scale_ward_to_discharge,
    mean_ward_to_ICU = shape_ward_to_ICU * scale_ward_to_ICU,
    mean_ICU_to_postICU = shape_ICU_to_postICU * scale_ICU_to_postICU
  ) %>%
  select(c(sample, age_group, subset_name, starts_with("mean"))) %>%
  pivot_longer(starts_with("mean"),
    names_to = "coding",
    values_to = "mean",
    names_prefix = "mean_"
  ) %>%
  mutate(
    age_class_narrow = case_when(
      age_group == "0-9" ~ "0-39",
      age_group == "40-49" ~ "40-69",
      age_group == "70-79" ~ "70+"
    )
  )




plot_data <- bind_rows(
  static_ward_omi_mix$fit_narrow %>%
    filter(coding == "ward_to_discharge" | coding == "ward_to_ICU") %>%
    mutate(subset_name = "omi_mix", group = "new"),
  static_ward_omi_HNE$fit_narrow %>%
    filter(coding == "ward_to_discharge" | coding == "ward_to_ICU") %>%
    mutate(subset_name = "omi_HNE", group = "new"),
  static_ICU_omi_mix$fit_narrow %>%
    filter(coding == "ICU_to_postICU") %>%
    mutate(subset_name = "omi_mix", group = "new"),
  static_ICU_omi_HNE$fit_narrow %>%
    filter(coding == "ICU_to_postICU") %>%
    mutate(subset_name = "omi_HNE", group = "new"),
  old_samples %>%
    mutate(group = "old")
) %>%
  mutate(
    group = factor(group, c("old", "new")),
    subset_name = est_labels[subset_name],
    subset_name = factor(subset_name, est_labels),
    coding = factor(coding, c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU"))
  )



plot_data %>%
  ggplot() +
  stat_summary(aes(x = age_class_narrow, y = mean, colour = group),
    fun.min = ~ quantile(., 0.025, na.rm = TRUE),
    fun = mean,
    fun.max = ~ quantile(., 0.975, na.rm = TRUE),
    size = 0.4,
    position = position_dodge(width = 0.25)
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
    order = c(5, 7),
    labels = c(
      "old" = "Real-time",
      "new" = "Retrospective"
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
  paste0(results_dir, "/estimates_comparison.png"),
  bg = "white",
  width = 9, height = 5
)