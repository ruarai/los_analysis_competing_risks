

make_surv_postICU_to_next <- function(
  linelist_data,
  age_table_narrow,
  age_table_wide
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
      age_class_narrow = cut_age(age, age_table_narrow),
      age_class_wide = cut_age(age, age_table_wide)
    ) %>%
    
    mutate(
      coding = code_postICU_compartment(is_still_in_hosp, patient_died),
      censor_code = if_else(coding == "censored", 0, 1))
  
  
  
  surv_fit_narrow <- flexsurvreg(
    Surv(LoS, censor_code)  ~ coding + shape(age_class_narrow),
    data = postICU_modelling,
    
    dist = "gamma"
  )
  
  surv_fit_wide <- flexsurvreg(
    Surv(LoS, censor_code)  ~ coding + shape(age_class_wide),
    data = postICU_modelling,
    
    dist = "gamma"
  )
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    
    data = postICU_modelling
  )
}