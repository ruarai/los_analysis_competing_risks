



make_surv_onset_to_ward <- function(
  linelist_data,
  age_table_narrow,
  age_table_wide
) {
  require(flexsurv)
  
  
  data_LoS_onset_to_ward <- linelist_data %>%
    mutate(
      LoS = time_diff_to_days(dt_hosp_admission - as_datetime(date_onset)),
      LoS = if_else(LoS == 0, 0.01, LoS),
      
      age_class_narrow = cut_age(age, age_table_narrow),
      age_class_wide = cut_age(age, age_table_wide),
      
      coding = "onset_to_ward"
    )
  
  surv_fit_narrow <- flexsurvreg(
    Surv(time = LoS) ~ 1,
    anc = list(shape = ~age_class_narrow),
    dist = "gamma",
    data = data_LoS_onset_to_ward
  )
  surv_fit_wide <- flexsurvreg(
    Surv(time = LoS) ~ 1,
    anc = list(shape = ~age_class_wide),
    dist = "gamma",
    data = data_LoS_onset_to_ward
  )
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    
    data = data_LoS_onset_to_ward
  )
}
