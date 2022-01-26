

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
  
  print("Fitting narrow estimates...")
  
  
  dist_vec <- c(ward_to_discharge = "gamma", ward_to_ICU = "gamma", ward_to_death = "gamma")
  
  optim_control_list <- list(
    fnscale = 10000,
    
    reltol = 1e-8,
    
    maxit = 1000,
    trace = 3
  )
  
  
  surv_fit_narrow <- flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    
    pformula = ~age_class_narrow,
    
    anc = list(
      ward_to_discharge = list(shape = ~age_class_narrow, rate = ~age_class_narrow),
      ward_to_ICU = list(shape = ~age_class_narrow, rate = ~age_class_narrow),
      ward_to_death = list(shape = ~age_class_narrow, rate = ~age_class_narrow)
    ),
    
    dists = dist_vec,
    
    data = ward_modelling,
    
    method = "direct",
    optim.control = optim_control_list
  )
  
  print("Fitting wide estimates...")
  surv_fit_wide <- flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    
    pformula = ~age_class_wide,

    anc = list(
      ward_to_discharge = list(shape = ~age_class_wide, rate = ~age_class_wide),
      ward_to_ICU = list(shape = ~age_class_wide, rate = ~age_class_wide),
      ward_to_death = list(shape = ~age_class_wide, rate = ~age_class_wide)
    ),
    
    dists = dist_vec,
    
    data = ward_modelling,
    
    optim.control = optim_control_list
  )
  
  print("Fitting complete")
  
  
  ## Perform a funky two-step estimation process to try and get sensible ward-to-death
  ## length of stay estimations. Normally, ward_to_death would be drastically over-estimated
  ## since it seems that the likelihood of high length of stay and high probability of death
  ## is /slightly/ greater than the likelihood of medium length of stay and low probability
  ## of death
  
  ## We use Aalen-Johansen non-parametric estimates of transition probabilities
  ## (which are produced from the first surv_fit_singular object, with the actual
  ## gamma dist. fit being discarded). We then take that non-parametric pr_ward_to_death
  ## and use it as a fixed parameter in the next fitting process, such that resultant length of
  ## stay is somewhat more reasonable
  
  surv_fit_singular <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    
    dists = dist_vec,
    
    data = ward_modelling,
    
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)
  
  surv_fit_singular_fixed_pr <- tryCatch({
    fixed_pr_ward_to_death <- ajfit_flexsurvmix(surv_fit_singular) %>%
      filter(state == "ward_to_death",
             model == "Aalen-Johansen") %>%
      pull(val) %>%
      max()
    
    flexsurvmix(
      Surv(LoS, censor_code) ~ 1,
      event = coding,
      
      dists = dist_vec,
      
      data = ward_modelling,
      
      fixedpars = c(1),
      initp = c(fixed_pr_ward_to_death, (1 - fixed_pr_ward_to_death) * 0.8, (1 - fixed_pr_ward_to_death) * 0.2),
      
      method = "direct",
      optim.control = optim_control_list
      
    )}, error = function(e) NULL)
  
  
  
  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    fit_singular = surv_fit_singular_fixed_pr,
    
    data = ward_modelling
  )

}


