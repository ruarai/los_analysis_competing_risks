
make_surv_ICU_to_next <- function(
  linelist_data
) {
  require(flexsurv)
  
  code_ICU_compartment <- function(is_still_in_icu, received_postICU_care, patient_died) {
    case_when(is_still_in_icu ~ "censored",
              
              received_postICU_care ~ "ICU_to_postICU",
              
              !patient_died ~ "ICU_to_discharge",
              
              patient_died ~ "ICU_to_death",
              
              TRUE ~ "unknown")
  }
  
  ICU_modelling <- linelist_data %>%
    filter(ever_in_icu) %>%
    mutate(
      LoS = true_icu_hours / 24,
      LoS_naive = time_diff_to_days(dt_last_icu - dt_first_icu),
      
      received_postICU_care = time_diff_to_days(dt_hosp_discharge - dt_last_icu) > 0.01,
      
      coding = code_ICU_compartment(is_still_in_icu, received_postICU_care, patient_died),
      
      censor_code = if_else(coding == "censored", 0, 1),
      
      
      age_class_narrow = cut_age(age, get_narrow_age_table()),
      age_class_wide = cut_age(age, get_wide_age_table())) %>%
    
    mutate(
      LoS = if_else(LoS == 0, 0.01, LoS),
      
    ) %>%
    
    filter(coding != "unknown") %>%
    
    mutate(coding = if_else(coding == "censored", NA_character_, coding)) %>%
    
    select(coding, censor_code, LoS, age_class_narrow, age_class_wide) %>%
    
    filter(LoS > 0)
  
  
  n_bootstraps_fit <- 50
  
  get_fit_los <- function(data) {
    data %>%
      pull(LoS) %>%
      fitdistrplus::fitdist(distr = "gamma") %>%
      fitdistrplus::bootdist(niter = n_bootstraps_fit, parallel = "multicore", ncpus = 32) %>%
      pluck("estim") %>%
      as_tibble() %>%
      mutate(bootstrap = row_number(), .before = 1) %>%
      
      mutate(scale = 1 / rate,
             mean = shape * scale,
             q90 = qgamma(0.9, shape = shape, rate = rate)) %>%
      select(-rate)
  }
  
  get_fit_prob <- function(data) {
    map_dfr(
      1:n_bootstraps_fit, 
      function(...) {
        data %>%
          sample_n(nrow(.), replace = TRUE) %>%
          count(coding) %>%
          mutate(n = n / sum(n)) %>%
          pivot_wider(names_from = coding, values_from = n)
      }
    ) %>% 
      mutate(bootstrap = row_number()) %>%
      pivot_longer(-bootstrap, names_to = "coding", values_to = "prob")
  }
  
  fit_narrow <- left_join(
    ICU_modelling %>%
      group_by(coding, age_class_narrow) %>%
      filter(n() > 1) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    ICU_modelling %>%
      group_by(age_class_narrow) %>%
      do(fit = get_fit_prob(.)) %>%
      unnest(fit), 
    by = c("coding", "age_class_narrow", "bootstrap")
  )
  
  fit_wide <- left_join(
    ICU_modelling %>%
      group_by(coding, age_class_wide) %>%
      filter(n() > 1) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    ICU_modelling %>%
      group_by(age_class_wide) %>%
      do(fit = get_fit_prob(.)) %>%
      unnest(fit), 
    by = c("coding", "age_class_wide", "bootstrap")
  )
  
  list(
    fit_narrow = fit_narrow,
    fit_wide = fit_wide
  )
}


