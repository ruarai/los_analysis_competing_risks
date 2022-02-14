
make_burden_figure <- function(
  linelist_raw,
  date_data_load,
  date_variant_cutoff,
  results_dir
) {
  
  linelist_data <- read_NSW_linelist(
    linelist_raw
  )  %>%
    mutate(age_class_narrow = cut_age(age, get_narrow_age_table()))
  
  
  make_ll_counts <- function(linelist) {
    days <- seq(
      ymd("2021-07-07"), ymd(date_data_load), by = "days"
    )
    
    expand_grid(
      date = days,
      age_class = get_narrow_age_table()$labels
    ) %>%
      rowwise() %>%
      
      mutate(count_ward = linelist %>%
               filter(age_class_narrow == age_class,
                      dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                      dt_hosp_admission < date,
                      
                      is.na(dt_first_icu) | dt_first_icu > date | dt_last_icu < date) %>%
               nrow(),
             
             count_ICU = linelist %>%
               drop_na(dt_first_icu) %>%
               filter(age_class_narrow == age_class,
                      dt_last_icu >= date | is.na(dt_last_icu),
                      dt_first_icu <= date,) %>%
               nrow(),
             
             count_hosp = count_ward + count_ICU)
  }
  
  counts_strict <- make_ll_counts(linelist_data) %>%
    
    mutate(age_class = factor(age_class, levels = c("70+", "40-69", "0-39")))
  
  
  
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
    mutate(age_group = factor(age_group, levels = c("70+", "40-69", "0-39")))
  
  
  plots_common <- list(
    theme_minimal(),
    scale_fill_manual(name = "Age group     ",
                      values = RColorBrewer::brewer.pal(4, "Blues")[2:4]),
    scale_x_date(
      date_breaks = "months",
      labels = scales::label_date_short()
    ),
    
    scale_y_continuous(
      position = "right",
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    ),
    
    xlab(NULL), ylab("Count"),
    
    geom_vline(xintercept = ymd(date_variant_cutoff),
               color = 'grey20',
               linetype = "dashed"),
    
    
    theme(legend.position = "bottom",
          axis.title.y.right = element_text(margin = margin(l = 0.5, unit = "cm"))),
    coord_cartesian(xlim = c(ymd("2021-07-04"), ymd("2022-02-15")))
  )
  
  cowplot::plot_grid(
    
    ggplot(case_data) +
      
      geom_col(aes(x = date, y = n, fill = age_group),
               width = 0.75)+
      
      plots_common +
      
      annotate("label",
               x = ymd("2021-09-23"), y = 55000,
               label = "Delta epidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0.5, vjust = 1)  +
      
      annotate("label",
               x = ymd("2021-Dec-18"), y = 55000,
               label = "Omicron/Delta\nepidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0, vjust = 1) +
      
      theme(legend.position = c(0.1, 0.55),
            legend.background = element_rect(fill = "white",
                                             color = "grey90")) +
      
      ggtitle("Notified cases and hospital bed occupancy over the Delta and Omicron/Delta epidemic periods", "Notified cases"),
    
    ggplot(counts_strict) +
      
      geom_col(aes(x = date, y = count_ward, fill = age_class),
               width = 0.75)+
      
      plots_common +

      annotate("label",
               x = ymd("2021-09-23"), y = 2400,
               label = "Delta epidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0.5, vjust = 1)  +

      annotate("label",
               x = ymd("2021-Dec-18"), y = 2400,
               label = "Omicron/Delta\nepidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0, vjust = 1) +
      
      theme(legend.position = c(0.1, 0.55),
            legend.background = element_rect(fill = "white",
                                             color = "grey90")) +
      
      ggtitle(NULL, "Ward bed occupancy")
    ,
    
    ggplot(counts_strict) +
      
      geom_col(aes(x = date, y = count_ICU, fill = age_class),
               width = 0.75) +
      
      plots_common +
      
      annotate("label", 
               x = ymd("2021-09-23"), y = 270,
               label = "Delta epidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0.5, vjust = 1) +
      
      annotate("label", 
               x = ymd("2021-Dec-18"), y = 270,
               label = "Omicron/Delta\nepidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0, vjust = 1) +
      
      ggtitle(NULL, "ICU bed occupancy") +
      
      theme(legend.position = c(0.1, 0.55),
            legend.background = element_rect(fill = "white",
                                             color = "grey90"))
    ,
    
    ncol = 1,
    align = 'v',
    rel_heights = c(0.95,0.92, 0.92),
    axis = 'lrtb'
  )
  
  ggsave(
    paste0(results_dir, "/burden.png"),
    bg = "white",
    width = 9, height = 9, dpi = 400
  )

}



