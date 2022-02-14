
export_fit_samples <- function(
  surv_ward_to_next,
  surv_ICU_to_next,
  surv_postICU_to_next,
  
  results_dir
) {
  
  get_fit_samples <- function(
    surv_fit,
    age_classes,
    event_names
  ) {
    if(is.null(surv_fit))
      return(tibble())
    
    map_dfr(
      1:1000,
      function(i) {
        resampled_fit <- flexsurv:::resample_pars(surv_fit)
        
        
        bind_rows(
          map_dfr(
            event_names,
            function(i_event) {
              flexsurv:::get_basepars(
                resampled_fit,
                event = which(surv_fit$evnames == i_event),
                newdata = tibble(age_class_narrow = age_classes, age_class_wide = age_classes)
                
              ) %>%
                as_tibble() %>%
                mutate(age_class = age_classes,
                       scale = 1 / rate,
                       coding = i_event) %>%
                select(-rate)
            }
          ) %>%
            pivot_longer(cols = c(shape, scale))
          ,
          
          flexsurv:::get_probpars(
            resampled_fit,
            newdata = tibble(age_class_narrow = age_classes, age_class_wide = age_classes)
          ) %>%
            rename_with(~ case_when(. == "val" ~ "value",
                                    . == "age_class_narrow" ~ "age_class",
                                    . == "age_class_wide" ~ "age_class",
                                    . == "event" ~ "coding",
                                    TRUE ~ .)) %>%
            mutate(name = "prob")
        )
        
      },
      .id = "sample"
    ) %>%
      filter(coding %in% event_names) %>%
      mutate(age_class = replace_na(age_class, "all"))
    
  }
  
  
  narrow_ages <- get_narrow_age_table()$labels
  wide_ages <- get_wide_age_table()$labels
  
  samples <- bind_rows(
    get_fit_samples(surv_ward_to_next$fit_narrow, narrow_ages, c("ward_to_discharge", "ward_to_ICU")),
    get_fit_samples(surv_ward_to_next$fit_singular, "all", "ward_to_death"),
    
    get_fit_samples(surv_ICU_to_next$fit_wide, wide_ages, c("ICU_to_death", "ICU_to_discharge", "ICU_to_postICU")),
    
    get_fit_samples(surv_postICU_to_next$fit_narrow, narrow_ages, c("postICU_to_discharge")),
    get_fit_samples(surv_postICU_to_next$fit_singular, "all", "postICU_to_death"),
  )
  
  
  ggplot(samples %>% pivot_wider()) +
    geom_point(aes(x = shape, y = scale, color = age_class),
               size = 0.2, alpha = 0.2) +
    
    facet_wrap(~coding) +
    
    ggokabeito::scale_color_okabe_ito(order = 3:7, name = "Age group") +
    
    scale_x_log10() +
    scale_y_log10() +
    
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    
    theme_minimal() +
    theme(legend.position = "bottom") +
    
    ggtitle("Length of stay distribution parameter estimate samples") 
  
  
  ggsave(
    paste0(results_dir, "/estimate_samples.png"),
    bg = "white",
    width = 9, height = 7, dpi = 200
  )
  
  
  
  ggplot(samples %>% pivot_wider()) +
    geom_point(aes(x = age_class, y = prob),
               position = position_jitter(width = 0.5),
               size = 0.2, alpha = 0.2) +
    
    facet_wrap(~coding,
               scales = "free_x") +
    
    ggokabeito::scale_color_okabe_ito(order = 3:7, name = "Age group") +
    
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    
    theme_minimal() +
    theme(legend.position = "bottom") +
    
    ggtitle("Length of stay distribution parameter estimate samples") 
  
  
  
  
  age_class_to_10yr <- function(age_class) {
    case_when(
      age_class == "0-39" ~ list(c("0-9", "10-19", "20-29", "30-39")),
      age_class == "40-69" ~ list(c("40-49", "50-59", "60-69")),
      age_class == "70+" ~ list(c("70-79", "80+")),
      age_class == "0-69" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69")),
      
      age_class == "all" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
    )
  }
  
  
  consistent_col_names <- c(
    "sample", "age_group", "pr_ward_to_discharge", "pr_ward_to_ICU", "pr_ICU_to_discharge", 
    "pr_ICU_to_postICU", "pr_postICU_to_death", "scale_onset_to_ward", 
    "scale_ward_to_discharge", "scale_ward_to_ICU", "scale_ward_to_death", 
    "scale_ICU_to_discharge", "scale_ICU_to_death", "scale_ICU_to_postICU", 
    "scale_postICU_to_discharge", "scale_postICU_to_death", "shape_onset_to_ward", 
    "shape_ward_to_discharge", "shape_ward_to_ICU", "shape_ward_to_death", 
    "shape_ICU_to_discharge", "shape_ICU_to_death", "shape_ICU_to_postICU", 
    "shape_postICU_to_discharge", "shape_postICU_to_death"
  )
  
  samples %>%
    mutate(age_class = age_class_to_10yr(age_class),
           name = if_else(name == "prob", "pr", name)) %>%
    unnest(age_class) %>%
    pivot_wider(names_from = c(name, coding)) %>%
    rename(age_group = age_class) %>%
    select(any_of(consistent_col_names)) %>%
    write_csv(paste0(results_dir, "/estimate_samples_share_wide.csv"))
  
  
  all_means <- tar_read(all_means)
  round2 <- function(x) if_else(is.na(x), NA_character_, format(round(x, 2), nsmall = 2))
  
  tbl_data <- left_join(
    
    samples %>% 
      filter(name != "prob") %>%
      group_by(age_class, coding, name) %>% 
      summarise(mean = mean(value),
                u95 = exp(quantile(log(value), p = 0.975)),
                l95 = exp(quantile(log(value), p = 0.025)),
                
                .groups = "drop") %>%
      
      pivot_wider(names_from = name,
                  values_from = c(mean, u95, l95),
                  names_glue = "{name}_{.value}") %>%
      
      select(coding, age_class,
             scale_mean, scale_l95, scale_u95,
             shape_mean, shape_l95, shape_u95),
    
    
    
    samples %>%
      group_by(coding, age_class) %>%
      filter(name %in% c("shape", "scale")) %>%
      
      pivot_wider() %>%
      
      summarise(corr = cor(log(scale), log(shape)))
  ) %>%
    
    left_join(
      all_means %>% 
        mutate(age_class = replace_na(age_class, "all")) %>%
        select(coding, age_class, n) %>%
        group_by(coding, age_class) %>%
        slice(1)) %>%
    
    mutate(coding = factor(coding, levels = fit_meta$i_comp)) %>%
    
    mutate(across(
      c(scale_mean, scale_l95, scale_u95, shape_mean, shape_l95, shape_u95, corr),
      round2
    )) %>%
    
    relocate(coding, age_class, n) %>%
    
    arrange(coding, age_class)
  
  require(kableExtra)
  
  
  kbl(
    tbl_data,
    col.names = rep("", times = 10),
    align = c("l", "l", rep("r", times = 8)),
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  ) %>% 
    collapse_rows(1)  %>%
    
    add_header_above(c("Pathway" = 1, "Age group" = 1, "n" = 1,
                       "Scale" = 3, "Shape" = 3, "Cor." = 1),
                     align = "l") %>%
    
    write_file(paste0(results_dir, "/tbl_full_params.tex"))
}


