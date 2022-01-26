
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
  
  
  plots_common <- list(
    theme_minimal(),
    scale_fill_brewer("Age group   ", palette = "Blues"),
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
    coord_cartesian(xlim = c(ymd("2021-07-04"), ymd("2022-01-29")))
  )
  
  
  cowplot::plot_grid(
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
               x = ymd("2021-Dec-18"), y = 2400-30,
               label = "Omicron/Delta\nepidemic period",
               label.r = unit(0, "cm"),
               size = 3.5,
               hjust = 0, vjust = 1) +
      
      theme(legend.position = c(0.1, 0.55),
            legend.background = element_rect(fill = "white",
                                             color = "grey90")) +
      
      ggtitle("Hospital bed occupancy over the Delta and Omicron/Delta epidemic periods", "Ward bed occupancy")
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
    rel_heights = c(0.85,0.95),
    axis = 'lrtb'
  )
  
  ggsave(
    paste0(results_dir, "/burden.png"),
    bg = "white",
    width = 9, height = 7, dpi = 400
  )

}



