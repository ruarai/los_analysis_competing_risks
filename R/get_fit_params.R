
get_fit_params <- function(
  trunc_onset_to_ward,
  surv_ward_to_next,
  surv_ICU_to_next,
  surv_postICU_to_next
) {
  age_table_wide <- get_wide_age_table()
  age_table_narrow <- get_narrow_age_table()
  
  
  get_dist_parameters <- function(surv_fit, age_class, event_name) {
    
    # Using internal function from flexsurv
    flexsurv:::get_basepars(
      surv_fit,
      
      newdata = tibble(age_class_wide = age_class, age_class_narrow = age_class),
      
      event = which(surv_fit$evnames == event_name)
    ) %>%
      as_tibble() %>%
      mutate(age_class = !!age_class, coding = event_name) %>%
      
      select(coding, age_class, shape, rate)
  }
  
  get_param_table <- function(surv_fit, age_classes) {
    if(is.null(surv_fit))
      return(tibble())
    
    
    expand_grid(
      age_class = age_classes,
      coding = surv_fit$evnames
    ) %>%
      pmap_dfr(
        function(age_class, coding) {
          
          get_dist_parameters(
            surv_fit,
            age_class,
            coding
          )
        }
      ) %>%
      
      left_join(
        flexsurv:::get_probpars(
          surv_fit,
          newdata= tibble(
            age_class_narrow = age_classes,
            age_class_wide = age_classes
          )) %>%
          rename_with(~ case_when(. == "age_class_wide" ~ "age_class",
                                  . == "age_class_narrow" ~"age_class",
                                  . == "val" ~ "prob_trans",
                                  . == "event" ~ "coding",
                                  TRUE ~ .))
      )
  }
  
  surv_fits <- list(
    surv_ward_to_next,
    surv_ICU_to_next,
    surv_postICU_to_next
  )
  
  bind_rows(
    surv_fits %>%
      map_dfr(function(fits_ls) { 
        bind_rows(
          get_param_table(
            fits_ls$fit_wide,
            age_table_wide$labels
          ) %>%
            mutate(age_type = "wide"),
          get_param_table(
            fits_ls$fit_narrow,
            age_table_narrow$labels
          ) %>%
            mutate(age_type = "narrow"),
          get_param_table(
            fits_ls$fit_singular,
            ""
          ) %>%
            mutate(age_type = "singular",
                   age_class = "all"))
      }),
    
    trunc_onset_to_ward$fit %>% 
      select(age_class, shape, rate) %>%
      mutate(prob_trans = 1,
             coding = "onset_to_ward",
             age_type = "narrow")
  )
  
  
}
