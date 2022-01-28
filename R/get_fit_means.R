
get_fit_means <- function(
  surv_ward_to_next,
  surv_ICU_to_next,
  surv_postICU_to_next
) {
  age_table_wide <- get_wide_age_table()
  age_table_narrow <- get_narrow_age_table()
  
  
  surv_fits <- list(
    surv_ward_to_next,
    surv_ICU_to_next,
    surv_postICU_to_next
  )
  
  get_fit_mean <- function(surv_fit, newdata) {
    if(is.null(surv_fit))
      return(tibble())
    
    flexsurv::mean_flexsurvmix(
      surv_fit,
      
      newdata = newdata,
      B = 50
    ) %>%
      as_tibble() %>%
      rename_with(~ case_when(. == "age_class_wide" ~ "age_class",
                              . == "age_class_narrow" ~ "age_class",
                              . == "val" ~ "mean",
                              . == "lower" ~ "mean_lower",
                              . == "upper" ~ "mean_upper",
                              . == "event" ~ "coding",
                              TRUE ~ .)) %>%
      
      left_join(
        flexsurv::quantile_flexsurvmix(
          surv_fit,
          newdata = newdata,
          B  = 50,
          probs = 0.9
        ) %>%
          as_tibble() %>%
          select(-p) %>%
          rename_with(~ case_when(. == "age_class_wide" ~ "age_class",
                                  . == "age_class_narrow" ~ "age_class",
                                  . == "val" ~ "q90",
                                  . == "lower" ~ "q90_lower",
                                  . == "upper" ~ "q90_upper",
                                  . == "event" ~ "coding",
                                  TRUE ~ .))
      )
    
    
  }
  
  
  fit_data <- bind_rows(
    surv_ward_to_next$data,
    surv_ICU_to_next$data,
    surv_postICU_to_next$data,
  ) %>%
    drop_na(coding)
  
  
  fit_sample_counts <- bind_rows(
    fit_data %>%
      group_by(coding, age_class_wide) %>%
      summarise(n = n(), .groups = "drop") %>%
      rename(age_class = age_class_wide) %>%
      mutate(age_type = "wide"),
    
    fit_data %>%
      group_by(coding, age_class_narrow) %>%
      summarise(n = n(), .groups = "drop") %>%
      rename(age_class = age_class_narrow) %>%
      mutate(age_type = "narrow"),
    
    fit_data %>%
      group_by(coding) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(age_type = "singular")
  )
  
  surv_fits %>%
    map_dfr(function(fits_ls) { 
      bind_rows(
        get_fit_mean(
          fits_ls$fit_wide,
          newdata = tibble(age_class_wide = age_table_wide$labels)
        ) %>%
          mutate(age_type = "wide"),
        get_fit_mean(
          fits_ls$fit_narrow,
          newdata = tibble(age_class_narrow = age_table_narrow$labels)
        ) %>%
          mutate(age_type = "narrow"),
        
        get_fit_mean(
          fits_ls$fit_singular,
          newdata = NULL
        ) %>%
          mutate(age_type = "singular"))
    }) %>%
    
    full_join(fit_sample_counts)
  
}
              