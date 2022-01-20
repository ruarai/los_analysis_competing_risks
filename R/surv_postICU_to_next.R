

make_surv_postICU_to_next <- function(
  linelist_data
) {
  require(flexsurv)
  
  code_postICU_compartment <- function(is_still_in_hosp, patient_died) {
    case_when(is_still_in_hosp ~ "censored",
              patient_died ~ "postICU_to_death",
              TRUE ~ "postICU_to_discharge")
  }
  
  
  postICU_modelling <- linelist_data %>%
    filter(ever_in_icu) %>%
    mutate(
      LoS = time_diff_to_days(dt_hosp_discharge - dt_last_icu),
      
      received_postICU_care = LoS > 0.01) %>%
    
    filter(received_postICU_care) %>%
    
    mutate(
      LoS = if_else(LoS == 0, 0.01, LoS),
      age_class_narrow = cut_age(age, get_narrow_age_table()),
      age_class_wide = cut_age(age, get_wide_age_table())
    ) %>%
    
    mutate(
      coding = code_postICU_compartment(is_still_in_hosp, patient_died),
      censor_code = if_else(coding == "censored", 0, 1)) %>%
    
    mutate(coding = if_else(coding == "censored", NA_character_, coding)) %>%
    
    select(coding, censor_code, LoS, age_class_narrow, age_class_wide)
  
  
  dist_vec <- c(postICU_to_discharge = "gamma", postICU_to_death = "gamma")
  
  optim_control_list <- list(
    fnscale = 10000,
    
    reltol = 1e-8,
    
    maxit = 1000,
    trace = 3
  )
  
  
  surv_fit_narrow <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    
    pformula = ~age_class_narrow,
    
    anc = list(
      postICU_to_discharge = list(shape = ~age_class_narrow),
      postICU_to_death = list(shape = ~age_class_narrow)
    ),
    
    dists = dist_vec,
    
    data = postICU_modelling,
    
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)
  
  surv_fit_wide <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    
    pformula = ~age_class_wide,
    
    anc = list(
      postICU_to_discharge = list(shape = ~age_class_wide),
      postICU_to_death = list(shape = ~age_class_wide)
    ),
    
    dists = dist_vec,
    
    data = postICU_modelling,
    
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    
    data = postICU_modelling
  )
}