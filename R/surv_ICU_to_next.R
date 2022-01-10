
make_surv_ICU_to_next <- function(
  linelist_data,
  age_table_narrow,
  age_table_wide
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
      LoS = time_diff_to_days(dt_last_icu - dt_first_icu),
      
      received_postICU_care = time_diff_to_days(dt_hosp_discharge - dt_last_icu) > 0.01,
           
      coding = code_ICU_compartment(is_still_in_icu, received_postICU_care, patient_died),
           
      censor_code = if_else(coding == "censored", 0, 1),
      
      
      age_class_narrow = cut_age(age, age_table_narrow),
      age_class_wide = cut_age(age, age_table_wide)) %>%
    
    mutate(
      LoS = if_else(LoS == 0, 0.01, LoS)
    )
  
  
  surv_fit_narrow <- flexsurvreg(
    Surv(LoS, censor_code) ~ age_class_narrow + coding + shape(age_class_narrow) + shape(coding),
    data = ICU_modelling,
    
    dist = "gamma"
  )
  
  surv_fit_wide <- flexsurvreg(
    Surv(LoS, censor_code) ~ age_class_wide + coding + shape(age_class_wide) + shape(coding),
    data = ICU_modelling,
    
    dist = "gamma"
  )
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    
    data = ICU_modelling
  )
}


