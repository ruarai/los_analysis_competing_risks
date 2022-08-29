
make_burden_figure <- function(linelist_raw,
                               date_data_load,
                               date_variant_cutoff,
                               results_dir) {
  ward_occupancy <- read_csv("~/data_private/NSW_occupancy/Ward_2022-04-26_UNSW.csv") %>%
    select(
      date = DATE, date_snapshot = SNAPSHOT_DATE,
      age_group = AGE_GROUP_10YR, count_PCR = PCR_Ward, count_RAT = RAT_Ward
    ) %>%
    pivot_longer(
      cols = c(count_PCR, count_RAT),
      names_prefix = "count_",
      names_to = "type", values_to = "count"
    )

  ICU_occupancy <- read_csv("~/data_private/NSW_occupancy/ICU_2022-04-26_UNSW.csv") %>%
    select(
      date = DATE, date_snapshot = SNAPSHOT_DATE,
      age_group = AGE_GROUP_10YR, count_PCR = PCR_ICU, count_RAT = RAT_ICU
    ) %>%
    pivot_longer(
      cols = c(count_PCR, count_RAT),
      names_prefix = "count_",
      names_to = "type", values_to = "count"
    )

  align_age_groups <- function(x) {
    x <- x %>% str_remove(" years")

    case_when(
      x %in% c("0-9", "10-19", "20-29") ~ "0-39",
      x %in% c("30-39", "40-49", "50-59", "60-69") ~ "40-69",
      x %in% c("70-79", "80-89", "90+") ~ "70+"
    )
  }


  all_occupancy <- bind_rows(
    ward_occupancy %>% mutate(group = "ward"),
    ICU_occupancy %>% mutate(group = "ICU")
  ) %>%
    mutate(age_group = align_age_groups(age_group)) %>%
    group_by(date, age_group, group) %>%
    summarise(count = sum(count)) %>%
    mutate(age_group = factor(age_group, levels = c("70+", "40-69", "0-39"))) %>%
    filter(date <= ymd("2022-02-07"))


  nsw_cases_A <- read_csv("https://data.nsw.gov.au/data/dataset/3dc5dc39-40b4-4ee9-8ec6-2d862a916dcf/resource/24b34cb5-8b01-4008-9d93-d14cf5518aec/download/confirmed_cases_table2_age_group.csv")
  nsw_cases_B <- read_csv("https://data.nsw.gov.au/data/dataset/3dc5dc39-40b4-4ee9-8ec6-2d862a916dcf/resource/4b03bc25-ab4b-46c0-bb3e-0c839c9915c5/download/confirmed_cases_table2_age_group_agg.csv")

  fix_age_group <- function(age_group) {
    age_group <- age_group %>%
      str_remove("AgeGroup_")
    case_when(
      age_group %in% c("0-19", "20-24", "25-29", "30-34", "35-39") ~ "0-39",
      age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69") ~ "40-69",
      TRUE ~ "70+"
    )
  }


  case_data <- bind_rows(
    nsw_cases_B %>%
      mutate(age_group = fix_age_group(age_group)) %>%
      group_by(age_group, date = notification_date) %>%
      summarise(n = sum(confirmed_cases_count)),
    nsw_cases_A %>%
      mutate(age_group = fix_age_group(age_group)) %>%
      filter(notification_date < min(nsw_cases_B$notification_date)) %>%
      group_by(age_group, date = notification_date) %>%
      summarise(n = n())
  ) %>%
    mutate(age_group = factor(age_group, levels = c("70+", "40-69", "0-39"))) %>%
    filter(date <= ymd("2022-02-07"))


  plots_common <- list(
    theme_minimal(),
    scale_fill_manual(
      name = "Age group     ",
      values = RColorBrewer::brewer.pal(4, "Blues")[2:4]
    ),
    scale_x_date(
      date_breaks = "months",
      labels = scales::label_date_short(format = c("%Y", "%B")),
      expand = expansion(mult = c(0.1, 0.05))
    ),
    scale_y_continuous(
      position = "right",
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    ),
    xlab(NULL), ylab("Count"),
    geom_vline(
      xintercept = ymd(date_variant_cutoff),
      color = "grey20",
      linetype = "dashed"
    ),
    theme(
      legend.position = "bottom",
      axis.title.y.right = element_text(margin = margin(l = 0.5, unit = "cm")),
      text = element_text(family = "Helvetica")
    ),
    coord_cartesian(xlim = c(ymd("2021-07-04"), ymd("2022-02-07")))
  )

  cowplot::plot_grid(

    ggplot(case_data) +
      geom_col(aes(x = date, y = n, fill = age_group),
        width = 0.75
      ) +
      plots_common +
      annotate("label",
        x = ymd("2021-09-23"), y = 55000,
        label = "Delta epidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      annotate("label",
        x = ymd("2022-Jan-12"), y = 55000,
        label = "Omicron/Delta\nepidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      theme(
        legend.position = c(0.1, 0.55),
        legend.background = element_rect(
          fill = "white",
          color = "grey90"
        )
      ) +
      ggtitle("Notified cases and hospital bed occupancy over the Delta and Omicron/Delta epidemic periods", "Notified cases"),
    ggplot(all_occupancy %>% filter(group == "ward")) +
      geom_col(aes(x = date, y = count, fill = age_group),
        width = 0.75
      ) +
      plots_common +
      annotate("label",
        x = ymd("2021-09-23"), y = 3500,
        label = "Delta epidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      annotate("label",
        x = ymd("2022-Jan-12"), y = 3500,
        label = "Omicron/Delta\nepidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      theme(
        legend.position = c(0.1, 0.55),
        legend.background = element_rect(
          fill = "white",
          color = "grey90"
        )
      ) +
      ggtitle(NULL, "Ward bed occupancy"),
    ggplot(all_occupancy %>% filter(group == "ICU")) +
      geom_col(aes(x = date, y = count, fill = age_group),
        width = 0.75
      ) +
      plots_common +
      annotate("label",
        x = ymd("2021-09-23"), y = 290,
        label = "Delta epidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      annotate("label",
        x = ymd("2022-Jan-12"), y = 290,
        label = "Omicron/Delta\nepidemic period",
        label.r = unit(0, "cm"),
        size = 3.5,
        hjust = 0.5, vjust = 1
      ) +
      ggtitle(NULL, "ICU bed occupancy") +
      theme(
        legend.position = c(0.1, 0.55),
        legend.background = element_rect(
          fill = "white",
          color = "grey90"
        )
      ),
    ncol = 1,
    align = "v",
    rel_heights = c(0.95, 0.92, 0.92),
    axis = "lrtb"
  )

  ggsave(
    paste0(results_dir, "/burden.png"),
    bg = "white",
    width = 9, height = 9, dpi = 400
  )
}
