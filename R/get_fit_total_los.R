

get_fit_total_los <- function(surv_ward_to_next,
                              surv_ICU_to_next,
                              surv_postICU_to_next) {
  age_table_wide <- get_wide_age_table()

  if (is.null(surv_ward_to_next$fit_wide) ||
    is.null(surv_ICU_to_next$fit_wide) ||
    is.null(surv_postICU_to_next$fit_wide)) {
    return(tibble())
  }

  mix_model <- flexsurv::fmixmsm(
    admission = surv_ward_to_next$fit_wide,
    ward_to_ICU = surv_ICU_to_next$fit_wide,
    ICU_to_postICU = surv_postICU_to_next$fit_wide
  )


  final_paths <- flexsurv::ppath_fmixmsm(
    mix_model,
    final = TRUE,
    newdata = tibble(age_class_wide = age_table_wide$labels),
    B = 50
  ) %>%
    rename(prob = val, prob_lower = lower, prob_upper = upper) %>%
    left_join(
      flexsurv::meanfinal_fmixmsm(
        mix_model,
        final = TRUE,
        newdata = tibble(age_class_wide = age_table_wide$labels),
        B = 50
      ) %>%
        rename(mean = val, mean_lower = lower, mean_upper = upper)
    ) %>%
    as_tibble()





  final_paths %>%
    group_by(age_class_wide) %>%
    summarise(
      mean = sum(prob * mean),
      mean_upper = sum(prob_upper * mean_upper),
      mean_lower = sum(prob_lower * mean_lower)
    )
}
