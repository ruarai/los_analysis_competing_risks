
make_surv_ICU_to_next <- function(linelist_data) {
  require(flexsurv)

  code_ICU_compartment <- function(is_still_in_icu, received_postICU_care, patient_died) {
    case_when(
      is_still_in_icu ~ "censored",
      received_postICU_care ~ "ICU_to_postICU",
      !patient_died ~ "ICU_to_discharge",
      patient_died ~ "ICU_to_death",
      TRUE ~ "unknown"
    )
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
      age_class_wide = cut_age(age, get_wide_age_table())
    ) %>%
    mutate(
      LoS = if_else(LoS == 0, 0.01, LoS),
    ) %>%
    filter(coding != "unknown") %>%
    mutate(coding = if_else(coding == "censored", NA_character_, coding)) %>%
    select(coding, censor_code, LoS, age_class_narrow, age_class_wide) %>%
    filter(LoS > 0)


  dist_vec <- c(ICU_to_discharge = "gamma", ICU_to_postICU = "gamma", ICU_to_death = "gamma")

  optim_control_list <- list(
    fnscale = 100,
    reltol = 1e-9,
    maxit = 500,
    trace = 3
  )


  surv_fit_narrow <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    pformula = ~age_class_narrow,
    anc = list(
      ICU_to_discharge = list(shape = ~age_class_narrow),
      ICU_to_postICU = list(shape = ~age_class_narrow),
      ICU_to_death = list(shape = ~age_class_narrow)
    ),
    dists = dist_vec,
    data = ICU_modelling,
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)

  surv_fit_wide <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    pformula = ~age_class_wide,
    anc = list(
      ICU_to_discharge = list(shape = ~age_class_wide),
      ICU_to_postICU = list(shape = ~age_class_wide),
      ICU_to_death = list(shape = ~age_class_wide)
    ),
    dists = dist_vec,
    data = ICU_modelling,
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)

  surv_fit_singular <- tryCatch(flexsurvmix(
    Surv(LoS, censor_code) ~ 1,
    event = coding,
    dists = dist_vec,
    data = ICU_modelling,
    method = "direct",
    optim.control = optim_control_list
  ), error = function(e) NULL)

  list(
    fit_narrow = surv_fit_narrow,
    fit_wide = surv_fit_wide,
    fit_singular = surv_fit_singular,
    data = ICU_modelling
  )
}
