
export_fits <- function(
  param_fits,
  results_dir
) {
  
  age_class_to_10yr <- function(age_class) {
    case_when(
      age_class == "0-39" ~ list(c("0-9", "10-19", "20-29", "30-39")),
      age_class == "40-69" ~ list(c("40-49", "50-59", "60-69")),
      age_class == "70+" ~ list(c("70-79", "80+")),
      age_class == "0-69" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69")),
      age_class == "all" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
    )
  }
  
  fit_data <- param_fits %>%
    left_join(
      fit_meta %>% mutate(keep = TRUE),
      by = c("coding" = "i_comp", "age_type" = "i_age_type")
    ) %>%
    
    drop_na(keep) %>%
    
    select(age_class, coding, shape, rate, prob_trans) %>%
    
    mutate(age_class = age_class_to_10yr(age_class)) %>%
    
    unnest(age_class)
  
  consistent_col_names <- c(
    "age_group", "pr_ward_to_discharge", "pr_ward_to_ICU", "pr_ICU_to_discharge", 
    "pr_ICU_to_postICU", "pr_postICU_to_death", "scale_onset_to_ward", 
    "scale_ward_to_discharge", "scale_ward_to_ICU", "scale_ward_to_death", 
    "scale_ICU_to_discharge", "scale_ICU_to_death", "scale_ICU_to_postICU", 
    "scale_postICU_to_discharge", "scale_postICU_to_death", "shape_onset_to_ward", 
    "shape_ward_to_discharge", "shape_ward_to_ICU", "shape_ward_to_death", 
    "shape_ICU_to_discharge", "shape_ICU_to_death", "shape_ICU_to_postICU", 
    "shape_postICU_to_discharge", "shape_postICU_to_death"
  )
  
  fit_export <- fit_data %>%
    select(age_class, coding, shape) %>%
    
    pivot_wider(names_from = coding,
                values_from = shape) %>%
    rename_with(.cols = -age_class, .fn = ~ str_c("shape_", .)) %>%
    
    left_join(
      fit_data %>%
        mutate(scale = 1 / rate) %>%
        select(age_class, coding, scale) %>%
        
        pivot_wider(names_from = coding,
                    values_from = scale) %>%
        rename_with(.cols = -age_class, .fn = ~ str_c("scale_", .))
    ) %>%
    
    left_join(
      fit_data %>%
        select(age_class, coding, prob_trans) %>%
        
        pivot_wider(names_from = coding,
                    values_from = prob_trans) %>%
        rename_with(.cols = -age_class, .fn = ~ str_c("pr_", .))
    ) %>%
    
    rename(age_group = age_class)
  
  export_file <- paste0(results_dir, "/clinical_parameters.csv")
  export_file_share <- paste0(results_dir, "/clinical_parameters_share.csv")
  
  fit_export %>%
    select(any_of(consistent_col_names)) %>% 
    write_csv(export_file)
  
  fit_export %>%
    select(any_of(consistent_col_names)) %>% 
    write_csv(export_file_share)
  
  return(export_file)
}

