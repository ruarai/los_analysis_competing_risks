

make_surv_ward_to_next <- function(
  linelist_data,
  age_table_narrow,
  age_table_wide
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
      
      age_class_narrow = cut_age(age, age_table_narrow),
      age_class_wide = cut_age(age, age_table_wide)
    )
  
  
  
  surv_fit_narrow <- flexsurvreg(
    Surv(LoS, censor_code) ~ age_class_narrow + coding + shape(age_class_narrow) + shape(coding),
    data = ward_modelling,
    
    dist = "gamma"
  )
  
  surv_fit_wide <- flexsurvreg(
    Surv(LoS, censor_code) ~ age_class_wide + coding + shape(age_class_wide) + shape(coding),
    data = ward_modelling,
    
    dist = "gamma"
  )
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    
    data = ward_modelling
  )

}


