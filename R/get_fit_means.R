
get_fit_means <- function(
  trunc_onset_to_ward,
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
                              TRUE ~ .))
  }
  
  bind_rows(
    trunc_onset_to_ward$fit %>%
      select(age_class, mean, mean_lower, mean_upper) %>%
      mutate(coding = "onset_to_ward", age_type = "narrow"),
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
            mutate(age_type = "narrow"))
      })
  )
  
}
              