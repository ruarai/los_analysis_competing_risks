

make_surv_ward_to_next <- function(
  linelist_data
) {
  require(flexsurv)
  
  code_ward_compartment <- function(is_still_in_hosp, ever_in_icu, patient_died) {
    case_when(
      ever_in_icu ~ "ward_to_ICU",
      is_still_in_hosp  ~ "censored",
      patient_died ~ "ward_to_death",
      TRUE ~ "ward_to_discharge"
    )
  }
  
  
  ward_modelling <- linelist_data %>%
    mutate(
      coding = code_ward_compartment(is_still_in_hosp,
                                     ever_in_icu,
                                     patient_died),
      censor_code = if_else(coding == "censored", 0, 1),
      
      
      LoS = case_when(
        coding == "ward_to_ICU" ~ time_diff_to_days(dt_first_icu - dt_hosp_admission),
        TRUE ~ time_diff_to_days(dt_hosp_discharge - dt_hosp_admission)
      ),
      LoS = if_else(LoS == 0, 0.01, LoS),
      
      age_class_narrow = cut_age(age, get_narrow_age_table()),
      age_class_wide = cut_age(age, get_wide_age_table())
    ) %>%
    
    mutate(coding = if_else(coding == "censored", NA_character_, coding),
           coding = factor(coding, levels = c("ward_to_death", "ward_to_discharge", "ward_to_ICU"))) %>%
    
    filter(LoS > 0) %>%
    
    select(coding, censor_code, LoS, age_class_narrow, age_class_wide)
  
  
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
    ward_modelling %>%
      group_by(coding, age_class_narrow) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    ward_modelling %>%
      group_by(age_class_narrow) %>%
      do(fit = get_fit_prob(.)) %>%
      unnest(fit), 
    by = c("coding", "age_class_narrow", "bootstrap")
  )

  fit_wide <- left_join(
    ward_modelling %>%
      group_by(coding, age_class_wide) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    ward_modelling %>%
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


