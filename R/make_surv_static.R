

make_surv_static <- function(linelist_data, compartment, date_data_load) {
  require(flexsurv)

  
  modelling_data <- linelist_data %>%
    filter(compartment == !!compartment, dt_admit <= date_data_load - days(14)) %>%
    mutate(
      coding = str_c(compartment, "_to_", trans),
      age_class_narrow = cut_age(age, get_narrow_age_table()),
      age_class_wide = cut_age(age, get_wide_age_table()),
      censor_code = if_else(still_in_hospital, 0, 1)
    ) %>%
    select(coding, censor_code, LoS = los, age_class_narrow, age_class_wide)


  n_bootstraps_fit <- 1000

  get_fit_los <- function(data) {
    data %>%
      pull(LoS) %>%
      fitdistrplus::fitdist(distr = "gamma") %>%
      fitdistrplus::bootdist(niter = n_bootstraps_fit, parallel = "multicore", ncpus = 32) %>%
      pluck("estim") %>%
      as_tibble() %>%
      mutate(bootstrap = row_number(), .before = 1) %>%
      mutate(
        scale = 1 / rate,
        mean = shape * scale,
        q90 = qgamma(0.9, shape = shape, rate = rate)
      ) %>%
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
      pivot_longer(-bootstrap, names_to = "coding", values_to = "prob") %>%
      mutate(prob = replace_na(prob, 0))
  }

  fit_narrow <- left_join(
    modelling_data %>%
      group_by(coding, age_class_narrow) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    modelling_data %>%
      group_by(age_class_narrow) %>%
      do(fit = get_fit_prob(.)) %>%
      unnest(fit),
    by = c("coding", "age_class_narrow", "bootstrap")
  )

  fit_wide <- left_join(
    modelling_data %>%
      group_by(coding, age_class_wide) %>%
      do(fit = get_fit_los(.)) %>%
      unnest(fit),
    modelling_data %>%
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
