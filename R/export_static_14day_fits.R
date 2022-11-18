
export_static_14day_fits <- function(
    ward_fit_omi_BA5,
    ICU_fit_omi_BA5,
    results_dir
) {
  
  all_fits <- bind_rows(
    ward_fit$fit_narrow, ICU_fit$fit_narrow
  )
  
  all_fits %>%
    group_by(age_class_narrow, bootstrap) %>% 
    summarise(
      mean_ward = sum(mean[str_starts(coding, "ward_to_")] * prob[str_starts(coding, "ward_to_")]),
      mean_ICU = sum(mean[str_starts(coding, "ICU_to_")] * prob[str_starts(coding, "ICU_to_")]),
    ) %>%
    group_by(age_class_narrow) %>%
    summarise(
      mean_ward = max(mean_ward),
      mean_ICU = max(mean_ICU)
    )
  
  
  age_class_to_10yr <- function(age_class) {
    case_when(
      age_class == "0-39" ~ list(c("0-9", "10-19", "20-29", "30-39")),
      age_class == "40-69" ~ list(c("40-49", "50-59", "60-69")),
      age_class == "70+" ~ list(c("70-79", "80+")),
      age_class == "0-69" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69")),
      age_class == "all" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
    )
  }
  
  all_fits_aged <- all_fits %>%
    mutate(age_group = age_class_to_10yr(age_class_narrow), .before = 1) %>%
    unnest(age_group) %>%
    select(-age_class_narrow)
  
  consistent_col_names <- c(
    "sample", "age_group", "pr_ward_to_discharge", "pr_ward_to_ICU", "pr_ICU_to_discharge",
    "pr_ICU_to_postICU", "pr_postICU_to_death", "scale_onset_to_ward",
    "scale_ward_to_discharge", "scale_ward_to_ICU", "scale_ward_to_death",
    "scale_ICU_to_discharge", "scale_ICU_to_death", "scale_ICU_to_postICU",
    "scale_postICU_to_discharge", "scale_postICU_to_death", "shape_onset_to_ward",
    "shape_ward_to_discharge", "shape_ward_to_ICU", "shape_ward_to_death",
    "shape_ICU_to_discharge", "shape_ICU_to_death", "shape_ICU_to_postICU",
    "shape_postICU_to_discharge", "shape_postICU_to_death"
  )
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  all_fits_wide <- all_fits_aged %>%
    select(age_group, coding, bootstrap, prob) %>%
    pivot_wider(names_from = coding, values_from = prob, names_prefix = "pr_") %>%
    left_join(
      all_fits_aged %>%
        select(age_group, coding, bootstrap, scale) %>%
        pivot_wider(names_from = coding, values_from = scale, names_prefix = "scale_")
    ) %>%
    left_join(
      all_fits_aged %>%
        select(age_group, coding, bootstrap, shape) %>%
        pivot_wider(names_from = coding, values_from = shape, names_prefix = "shape_")
    ) %>%
    mutate(
      pr_ICU_to_postICU = 0,
      pr_postICU_to_death = 0,
      
      shape_ICU_to_postICU = 1,
      scale_ICU_to_postICU = 1,
      shape_postICU_to_discharge = 1,
      scale_postICU_to_discharge = 1,
      shape_postICU_to_death = 1,
      scale_postICU_to_death = 1
    ) %>%
    rename(sample = bootstrap) %>%
    
    select(-c(pr_ward_to_death, pr_ICU_to_death)) %>%
    
    mutate(age_group = factor(age_group, levels = age_groups)) %>%
    arrange(sample, age_group)
  
  
  setdiff(colnames(all_fits_wide), consistent_col_names)
  setdiff(consistent_col_names, colnames(all_fits_wide))
  
  
  all_fits_wide_mean <- all_fits_wide %>%
    group_by(age_group) %>%
    summarise(
      across(-c(sample), mean)
    )
  
  
  all_fits_wide %>%
    write_csv(str_c(results_dir, "/estimate_samples_share_wide.csv"))
  
  all_fits_wide_mean %>%
    write_csv(str_c(results_dir, "/clinical_parameters_share.csv"))
}


